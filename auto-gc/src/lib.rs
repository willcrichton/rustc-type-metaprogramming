#![feature(rustc_private, quote, proc_macro, proc_macro_internals)]

extern crate proc_macro;
extern crate rustc;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_driver;
extern crate rustc_trans_utils;
extern crate rustc_resolve;
extern crate rustc_incremental;
extern crate rustc_typeck as typeck;
extern crate syntax_pos;
extern crate syntax;
extern crate syn;
extern crate quote as quoteshim;

use std::sync::mpsc;
use std::rc::Rc;
use std::path::PathBuf;

use rustc::{ty, hir, session};
use rustc_errors::registry::Registry;
use rustc_metadata::cstore::CStore;
use rustc_driver::driver;
use rustc_resolve::MakeGlobMap;
use syntax::{ext, ast};
use syntax_pos::DUMMY_SP;
use syntax::ptr::P;
use syntax::ext::quote::rt::ToTokens;

use syntax::tokenstream;
use quoteshim::ToTokens as OtherToTokens;

// NOTE: if you are seeing "error[E0463]: can't find crate for `std`"
// then you need to change this SYSROOT to your specific machine's.
static SYSROOT: &'static str = "/Users/will/.rustup/toolchains/nightly-x86_64-apple-darwin";

trait Folder<'a> {
    fn fold_expr(&mut self, expr: &'a hir::Expr) -> P<ast::Expr>;
    fn fold_stmt(&mut self, stmt: &'a hir::Stmt) -> ast::Stmt;
}

struct TestFolder<'a, 'tcx: 'a, 'ecx: 'a> {
    ecx: &'a ext::base::ExtCtxt<'ecx>,
    tcx: ty::TyCtxt<'a, 'tcx, 'tcx>,
}

impl<'a, 'tcx: 'a, 'ecx> Folder<'tcx> for TestFolder<'a, 'tcx, 'ecx> {
    fn fold_expr(&mut self, expr: &'tcx hir::Expr) -> P<ast::Expr> {
        use hir::{Expr_, BinOp_};
        match expr.node {
            Expr_::ExprLit(ref lit) => quote_expr!(&self.ecx, $lit),
            Expr_::ExprBinary(ref binop, ref e1, ref e2) => {
                let e1 = self.fold_expr(e1);
                let e2 = self.fold_expr(e2);
                match binop.node {
                    BinOp_::BiAdd => quote_expr!(&self.ecx, Rc::new(*$e1 + *$e2)),
                    _ => panic!("Not yet implemented")
                }
            },
            Expr_::ExprBlock(ref block) => {
                let new_block: Vec<ast::Stmt> =
                    block.stmts.iter().map(|stmt| self.fold_stmt(stmt)).collect();
                quote_expr!(&self.ecx, {$new_block})
            },
            _ => panic!("Not yet implemented")
        }
    }

    fn fold_stmt(&mut self, stmt: &'tcx hir::Stmt) -> ast::Stmt {
        use hir::{Stmt_, Decl_, PatKind};
        use ty::TypeVariants;
        use ast::IntTy;
        match stmt.node {
            Stmt_::StmtDecl(ref decl, _) => {
                match decl.node {
                    Decl_::DeclLocal(ref local) => {
                        let name = if let PatKind::Binding(_, _, ref name, _) = local.pat.node {
                            syntax_pos::symbol::Ident::with_empty_ctxt(name.node)
                        } else {
                            panic!("Not yet implemented")
                        };

                        let expr = &local.init.iter().next().unwrap();
                        let table = self.tcx.typeck_tables_of(expr.hir_id.owner_def_id());
                        let ty = table.node_id_to_type(expr.hir_id);
                        let ty = match ty.sty {
                            TypeVariants::TyInt(intty) =>
                                match intty {
                                    IntTy::I32 => quote_ty!(&self.ecx, i32),
                                    _ => panic!("Not yet implemented")
                                }
                            _ => panic!("Not yet implemented")
                        };

                        let expr = self.fold_expr(expr);
                        quote_stmt!(&self.ecx, let $name: Rc<$ty> = $expr;).unwrap()
                    },
                    _ => panic!("Not yet implemented")
                }
            }
            _ => panic!("Not yet implemented")
        }
    }
}


#[allow(unused_variables)]
#[proc_macro]
pub fn auto_gc(ts: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syntax::with_globals(|| {
        let codemap = syntax::codemap::CodeMap::new(syntax::codemap::FilePathMapping::empty());
        codemap.new_filemap(syntax_pos::FileName::Custom("".to_string()), "".to_string());

        let mut opts = session::config::basic_options();
        opts.maybe_sysroot = Some(PathBuf::from(SYSROOT));
        let sess = session::build_session_with_codemap(
            opts,
            None,
            Registry::new(&[]),
            Rc::new(codemap),
            None,
        );

        let cstore = CStore::new(
            rustc_trans_utils::trans_crate::MetadataOnlyTransCrate::new()
                .metadata_loader(),
        );

        let mut resolver = ext::base::DummyResolver;
        let cx = ext::base::ExtCtxt::new(
            &sess.parse_sess,
            ext::expand::ExpansionConfig::default("".to_string()),
            &mut resolver,
        );
        let krate =
            ast::Crate {
                module: ast::Mod {
                    inner: DUMMY_SP,
                    items: vec![
                        quote_item!(
                            &cx,
                            fn main(){let x: i32 = 1 + 2;}).unwrap(),
                    ],
                },
                attrs: vec![],
                span: DUMMY_SP,
            };


        let driver::ExpansionResult {
            expanded_crate,
            defs,
            analysis,
            resolutions,
            mut hir_forest,
        } = driver::phase_2_configure_and_expand(
            &sess,
            &cstore,
            krate,
            None,
            "test",
            None,
            MakeGlobMap::No,
            |_| Ok(()),
        ).unwrap();

        let arenas = ty::AllArenas::new();

        let hir_map = hir::map::map_crate(&sess, &cstore, &mut hir_forest, &defs);

        let mut local_providers = ty::maps::Providers::default();
        driver::default_provide(&mut local_providers);

        let mut extern_providers = local_providers;
        driver::default_provide_extern(&mut extern_providers);

        let query_result_on_disk_cache = rustc_incremental::load_query_result_cache(&sess);

        let (tx, _rx) = mpsc::channel();

        let input = session::config::Input::Str {
            name: syntax_pos::FileName::Custom("".to_string()),
            input: "".to_string(),
        };
        let output_filenames = driver::build_output_filenames(&input, &None, &None, &[], &sess);

        ty::TyCtxt::create_and_enter(&sess, &cstore, local_providers, extern_providers, &arenas, resolutions, hir_map, query_result_on_disk_cache, "", tx, &output_filenames, |tcx| {
            typeck::check_crate(tcx).expect("typeck failure");
            let mut folder = TestFolder {ecx: &cx, tcx: tcx};
            let expr =
                folder.fold_expr(&tcx.hir.krate().bodies.values().next().unwrap().value);
            let ts: tokenstream::TokenStream = expr.to_tokens(&cx).into_iter().collect();
            let ts = proc_macro::__internal::token_stream_wrap(ts);
            println!("{}", ts);
            ts
        })
    })
}
