use std::{fs::File, path::Path};
use inkwell::context::Context;
use clap::Parser as ClapParser;

mod token;

mod lexer;
use lexer::Lexer;

#[macro_use]
mod error;

mod token_reader;
use token_reader::TokenReader;

mod ast;
use ast::GlobalStatementNode;

mod parser;

#[macro_use]
mod typing;

mod ilgen;
use ilgen::IlGenerator;

mod symbols;
use symbols::{SymbolTable, SymbolPath};

mod address_table;

#[derive(ClapParser, Debug)]
#[command(name = "Perun Compiler")]
#[command(version = "0.1")]
#[command(about = "Compiler for the Perun programming language", long_about = None)]
struct CliArgs {
    sources: Vec<String>,

    #[arg(short, long, default_value_t=("object.o".to_string()))]
    output: String,

    #[arg(short, long, default_value_t=false)]
    run: bool,
    #[arg(long, default_value_t=false)]
    print_tokens: bool,
    #[arg(long, default_value_t=false)]
    print_ast: bool,
    #[arg(long, default_value_t=false)]
    print_symbols: bool,
    #[arg(long, default_value_t=false)]
    print_ir: bool,
}

fn main() -> std::io::Result<()> {
    let args = CliArgs::parse();
    let il_context = Context::create();

    if args.sources.is_empty() {
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "no sources"));
    }

    for source in args.sources {
        let file_handle = File::open(source)?;
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
            eprintln!("Failed to parse input file (line {}, column {}): {}", err.location.line, err.location.column, err.message);
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "compilation failed"));
        }

        let parsed = parsed_res.unwrap();
        if args.print_ast {
            println!("Parsed {:?}", parsed);
        }


        let path = SymbolPath::new();
        let collect_symbols_res = parsed.collect_symbols(&path, &mut sym_table);
        if let Err(err) = &collect_symbols_res {
            eprintln!("Failed to collect symbols (line {}, column {}): {}", err.location.line, err.location.column, err.message);
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "compilation failed"));
        }

        if args.print_symbols {
            sym_table.print_symbols();
        }

        let mut generator = IlGenerator::new(&il_context, &sym_table);
        let generatge_res = parsed.generate_il(&mut generator, &path);
        if let Err(err) = &generatge_res {
            eprintln!("Failed to compile input file (line {}, column {}): {}", err.location.line, err.location.column, err.message);
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "compilation failed"));
        }

        if args.print_ir {
            generator.print_module();
        }

        if args.run {
            generator.run();
        } else {
            generator.compile(inkwell::targets::FileType::Object, Path::new(args.output.as_str()));
        }
    }

    Ok(())
}
