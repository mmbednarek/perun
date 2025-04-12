use clap::Parser as ClapParser;
use inkwell::context::Context;
use std::{ffi::OsStr, fs::File, path::Path};

mod token;

mod lexer;
use lexer::Lexer;

#[macro_use]
mod error;

mod token_reader;
use token_reader::TokenReader;

mod ast;

mod parser;

#[macro_use]
mod typing;

mod ir_build_context;
use ir_build_context::IRBuildContext;

mod symbols;
use crate::ast::GlobalStatementVisitor;
use symbols::{SymbolPath, SymbolTable};

mod address_table;

mod collect_symbols_pass;
mod compile_time_evaluation_pass;
mod ir_translation_pass;
mod module_api;
mod type_deduction_pass;

#[derive(ClapParser, Debug)]
#[command(name = "Perun Compiler")]
#[command(version = "0.1")]
#[command(about = "Compiler for the Perun programming language", long_about = None)]
struct CliArgs {
    sources: Vec<String>,

    #[arg(short, long, default_value_t=("object.o").to_string())]
    output: String,

    #[arg(short, long, default_value_t = false)]
    run: bool,
    #[arg(long, default_value_t = false)]
    print_tokens: bool,
    #[arg(long, default_value_t = false)]
    print_ast: bool,
    #[arg(long, default_value_t = false)]
    print_symbols: bool,
    #[arg(long, default_value_t = false)]
    print_ir: bool,
}

fn main() -> std::io::Result<()> {
    let args = CliArgs::parse();
    let il_context = Context::create();

    if args.sources.is_empty() {
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "no sources"));
    }

    for source in args.sources {
        let source_path = Path::new(source.as_str());
        let file_handle = File::open(source_path)?;
        let mut lexer = Lexer::new(Box::new(file_handle));
        lexer.read_tokens();

        if args.print_tokens {
            println!("Printing tokens:");
            let mut print_reader = TokenReader::new(lexer.tokens());
            while print_reader.has_tokens() {
                println!("   {:?}", print_reader.next().unwrap().token_type);
            }
        }

        let mut sym_table = SymbolTable::new();

        let mut reader = TokenReader::new(lexer.tokens());
        let mut parser = parser::Parser::new(&mut reader);

        let parsed_res = parser.parse();
        if let Err(err) = &parsed_res {
            eprintln!(
                "Failed to parse input file (line {}, column {}): {}",
                err.location.line, err.location.column, err.message
            );
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "compilation failed",
            ));
        }

        let parsed = parsed_res.unwrap();
        if args.print_ast {
            println!("Parsed {:#?}", parsed);
        }

        let basename = source_path
            .file_stem()
            .unwrap_or(OsStr::new("root"))
            .to_str()
            .unwrap_or("root");
        let path = SymbolPath::new(basename);

        let mut symbols_pass = collect_symbols_pass::CollectSymbolsPass::new(&mut sym_table);

        let collect_symbols_res = symbols_pass.visit_global_statement(&(&parsed).into(), &path);
        if let Err(err) = &collect_symbols_res {
            eprintln!(
                "Failed to collect symbols (line {}, column {}): {}",
                err.location.line, err.location.column, err.message
            );
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "compilation failed",
            ));
        }

        if args.print_symbols {
            sym_table.print_symbols();
        }

        let mut generator = IRBuildContext::new(&il_context, &sym_table);

        let mut translation_pass = ir_translation_pass::IRTranslationPass::new(&mut generator);

        let generate_res = translation_pass.visit_global_statement(&(&parsed).into(), &path);
        if let Err(err) = &generate_res {
            eprintln!(
                "Failed to compile input file (line {}, column {}): {}",
                err.location.line, err.location.column, err.message
            );
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "compilation failed",
            ));
        }

        if args.print_ir {
            generator.print_module();
        }

        if args.run {
            generator.run();
        } else {
            generator.compile(
                inkwell::targets::FileType::Object,
                Path::new(args.output.as_str()),
            );
        }
    }

    Ok(())
}
