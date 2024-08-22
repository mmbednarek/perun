use std::fs::File;

use inkwell::context::Context;

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
use parser::Parser;

#[macro_use]
mod typing;

mod ilgen;
use ilgen::IlGenerator;

mod symbols;
use symbols::{SymbolTable, SymbolPath};

mod address_table;

fn main() -> std::io::Result<()> {
    let file_handle = File::open("fibbonacci.pr")?;
    let mut lexer = Lexer::new(Box::new(file_handle));
    lexer.read_tokens();

    {
        let mut print_reader = TokenReader::new(lexer.tokens());
        while print_reader.has_tokens() {
            println!("token: {:?}", print_reader.next().unwrap().token_type);
        }
    }

    let mut reader = TokenReader::new(lexer.tokens());

    let mut parser = Parser::new(&mut reader);

    let il_context = Context::create();

    let mut sym_table = SymbolTable::new();

    let parsed_res = parser.parse();
    if let Err(err) = &parsed_res {
        eprintln!("Failed to parse input file (line {}, column {}): {}", err.location.line, err.location.column, err.message);
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "compilation failed"));
    }

    let parsed = parsed_res.unwrap();
    println!("Parsed {:?}", parsed);


    let path = SymbolPath::new();
    let collect_symbols_res = parsed.collect_symbols(&path, &mut sym_table);
    if let Err(err) = &collect_symbols_res {
        eprintln!("Failed to collect symbols (line {}, column {}): {}", err.location.line, err.location.column, err.message);
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "compilation failed"));
    }

    let mut generator = IlGenerator::new(&il_context, &sym_table);
    let generatge_res = parsed.generate_il(&mut generator, &path);
    if let Err(err) = &generatge_res {
        eprintln!("Failed to compile input file (line {}, column {}): {}", err.location.line, err.location.column, err.message);
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "compilation failed"));
    }

    generator.print_module();

    generator.run();

    // sym_table.print_symbols();

    Ok(())
}
