#![feature(rustc_private, quote)]

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
use rustc::hir::intravisit;
use rustc::hir::intravisit::Visitor;

// NOTE: if you are seeing "error[E0463]: can't find crate for `std`"
// then you need to change this SYSROOT to your specific machine's.
static SYSROOT: &'static str = "/Users/will/.rustup/toolchains/nightly-x86_64-apple-darwin";

struct TestVisitor<'a, 'tcx: 'a> {
    tcx: ty::TyCtxt<'a, 'tcx, 'tcx>,
}

impl<'a, 'tcx: 'a> Visitor<'tcx> for TestVisitor<'a, 'tcx> {
    fn nested_visit_map<'this>(&'this mut self) -> intravisit::NestedVisitorMap<'this, 'tcx> {
        intravisit::NestedVisitorMap::OnlyBodies(&self.tcx.hir)
    }

    fn visit_expr(&mut self, ex: &'tcx hir::Expr) {
        let table = self.tcx.typeck_tables_of(ex.hir_id.owner_def_id());
        let ty = table.node_id_to_type(ex.hir_id);
        intravisit::walk_expr(self, ex);
    }
}

fn main() {
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

        let krate = {
            let mut resolver = ext::base::DummyResolver;
            let cx = ext::base::ExtCtxt::new(
                &sess.parse_sess,
                ext::expand::ExpansionConfig::default("".to_string()),
                &mut resolver,
            );

            ast::Crate {
                module: ast::Mod {
                    inner: DUMMY_SP,
                    items: vec![
                        quote_item!(
                            &cx,
                            fn main(){let x = 1 + 2;}).unwrap(),
                    ],
                },
                attrs: vec![],
                span: DUMMY_SP,
            }
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
            let mut visitor = TestVisitor{tcx: tcx};
            tcx.hir.krate().visit_all_item_likes(&mut visitor.as_deep_visitor());
        });
    })
}
