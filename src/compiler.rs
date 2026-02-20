use crate::ast::{GlobalStatementVisitor, ImportNode};
use crate::error::AnyResult;
use crate::ir_build_context::IRBuildContext;
use crate::lexer::Lexer;
use crate::symbols::{SymbolPath, SymbolTable};
use crate::token::Location;
use crate::token_reader::TokenReader;
use crate::{ast_passes, parser};
use inkwell::context::Context;
use std::ffi::OsStr;
use std::fs::File;
use std::path::{Path, PathBuf};

pub struct Compiler {
    pub should_print_tokens: bool,
    pub should_print_ast: bool,
    pub should_print_symbols: bool,
    pub should_print_ir: bool,
    pub no_include_root: bool,
    pub import_directories: Vec<String>,
    pub module_name: Option<String>,
}

impl Compiler {
    fn compile_module<'st, 'ctx>(
        &self,
        source_path: &Path,
        module_out: Option<&Path>,
        build_ctx: &mut IRBuildContext<'ctx, 'st>,
    ) -> AnyResult<()> {
        let file_handle = File::open(source_path).map_err(|io_err| (&io_err).into())?;

        let mut lexer = Lexer::new(Box::new(file_handle));
        lexer.read_tokens();

        if self.should_print_tokens {
            println!("TOKENS:");
            let mut print_reader = TokenReader::new(lexer.tokens());
            while print_reader.has_tokens() {
                println!("   {:?}", print_reader.next().unwrap().token_type);
            }
        }

        let mut reader = TokenReader::new(lexer.tokens());
        let mut parser = parser::Parser::new(&mut reader);

        let parsed = parser.parse().map_err(|e| (&e).into())?;

        if self.should_print_ast {
            println!("AST:");
            println!("{:#?}", parsed);
        }

        let module_name = self.module_name.clone().unwrap_or(
            source_path
                .file_stem()
                .unwrap_or(OsStr::new("root"))
                .to_str()
                .unwrap_or("root")
                .to_string(),
        );
        let path = SymbolPath::new(module_name.as_str());

        if let Some(module_out_path) = module_out {
            let module_file = File::create(&module_out_path).map_err(|io_err| (&io_err).into())?;

            let mut pass = ast_passes::export_module_pass::ExportModulePass::new(
                module_name,
                self.import_directories.as_slice(),
            );
            pass.visit_global_statement(&(&parsed).into(), &());

            let module_api = pass.module.to_api();
            serde_json::to_writer(module_file, &module_api)
                .map_err(|serde_err| (&serde_err).into())?;
        }

        let mut symbols_pass = ast_passes::collect_symbols_pass::CollectSymbolsPass::new(
            &mut build_ctx.symbol_table,
            self.import_directories.as_slice(),
        );

        let import_node = ImportNode {
            location: Location { line: 0, column: 0 },
            module_name: "root".into(),
            dst_path: Some(SymbolPath::empty()),
            is_public: false,
        };
        if !self.no_include_root {
            symbols_pass
                .visit_import(&import_node, &SymbolPath::empty())
                .map_err(|e| (&e).into())?;
        }
        symbols_pass
            .visit_global_statement(&(&parsed).into(), &path)
            .map_err(|e| (&e).into())?;

        if self.should_print_symbols {
            build_ctx.symbol_table.print_symbols();
        }

        let mut translation_pass = ast_passes::ir_translation_pass::IRTranslationPass::new(
            build_ctx,
            self.import_directories.as_slice(),
        );

        if !self.no_include_root {
            translation_pass
                .visit_import(&import_node, &SymbolPath::empty())
                .map_err(|e| (&e).into())?;
        }

        translation_pass
            .visit_global_statement(&(&parsed).into(), &path)
            .map_err(|e| (&e).into())?;

        if self.should_print_ir {
            build_ctx.print_module();
        }

        Ok(())
    }
    pub fn compile_to_object(
        &self,
        source_paths: &[PathBuf],
        output_path: &Path,
        module_out: Option<&Path>,
    ) -> AnyResult<()> {
        let mut symbol_table = SymbolTable::new();

        let il_context = Context::create();
        let mut build_ctx = IRBuildContext::new(&il_context, &mut symbol_table);

        for source_path in source_paths {
            self.compile_module(source_path.as_ref(), module_out, &mut build_ctx)?;
        }

        build_ctx.compile(inkwell::targets::FileType::Object, output_path)?;

        Ok(())
    }
}
