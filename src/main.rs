use clap::Parser as ClapParser;
use std::path::{Path, PathBuf};

mod lexer;
mod token;
#[macro_use]
mod error;
mod ast;
mod parser;
mod token_reader;
#[macro_use]
mod typing;
mod ir_build_context;
mod ir_value_storage;
mod symbols;
mod ast_passes {
    pub mod collect_symbols_pass;
    pub mod compile_time_evaluation_pass;
    pub mod export_module_pass;
    pub mod ir_translation_pass;
    pub mod type_deduction_pass;
}
mod compiler;
mod module;
mod module_api;

#[derive(ClapParser, Debug)]
#[command(name = "Perun Compiler")]
#[command(version = "0.1")]
#[command(about = "Compiler for the Perun programming language", long_about = None)]
struct CliArgs {
    sources: Vec<String>,

    #[arg(short, long, default_value_t=("object.o").to_string())]
    output: String,

    #[arg(short, long, default_value_t = ("").to_string())]
    module_out: String,

    #[arg(short, long, default_value_t = (".").to_string())]
    import_directory: String,

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
    #[arg(long, default_value_t = false)]
    no_include_root: bool,
}

fn main() {
    let args = CliArgs::parse();

    if args.sources.is_empty() {
        eprintln!("No sources provided");
        std::process::exit(1);
    }

    let source_paths: Vec<PathBuf> = args
        .sources
        .iter()
        .map(|path| PathBuf::from(path))
        .collect();
    let compiler = compiler::Compiler {
        should_print_tokens: args.print_tokens,
        should_print_ast: args.print_ast,
        should_print_symbols: args.print_symbols,
        should_print_ir: args.print_ir,
        import_directory: args.import_directory.clone(),
        no_include_root: args.no_include_root,
    };

    let module_out_path = if args.module_out.is_empty() {
        None
    } else {
        Some(Path::new(args.module_out.as_str()))
    };

    let out_path = Path::new(args.output.as_str());
    let result = compiler.compile_to_object(source_paths.as_slice(), out_path, module_out_path);
    if let Err(res) = result {
        eprintln!("{}", res);
        std::process::exit(1);
    }
}
