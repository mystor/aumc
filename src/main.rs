extern crate docopt;
extern crate rustc_serialize;

use std::io::prelude::*;
use std::fs::File;
use std::path::PathBuf;
use docopt::Docopt;

#[macro_use]
mod err;

mod common;
mod ast;
mod lex;
mod parse;
mod prgm;
mod arena;

// Docopt usage for the program
static USAGE: &'static str = "
Usage: aumc [options] <INPUT>
       aumc (-h | --help)
       aumc (-V | --version)

Options:
    -h, --help       Show this screen.
    -V, --version    Display the version of aumc.
    -o, --out FILE   Output file.
    -e, --emit TYPE  Configure the output that aumc will produce.
                     Valid values: asm, ir, bc, obj, link.
    -O, --opt LEVEL  Optimize the output. Possible levels 0-3.
";

#[allow(non_snake_case)]
#[derive(RustcDecodable, Debug)]
struct Args {
    arg_INPUT: String,
    flag_out: Option<String>,
    flag_emit: Option<Emit>,
    flag_opt: Option<Opt>,
    flag_version: bool,
}

#[derive(RustcDecodable, Debug)]
enum Emit { Asm, Ir, Bc, Obj, Link }

#[derive(Debug)]
enum Opt { None, Less, Default, Agressive }

impl rustc_serialize::Decodable for Opt {
    fn decode<D: rustc_serialize::Decoder>(d: &mut D) -> Result<Opt, D::Error> {
        Ok(match try!(d.read_usize()) {
            0 => Opt::None,
            1 => Opt::Less,
            2 => Opt::Default,
            3 => Opt::Agressive,
            n => {
                return Err(d.error(&format!(
                    "'{}' is not a valid optimization level.", n)));
            }
        })
    }
}

fn main() {
    // Use Docopt to parse the command line usage from the usage string
    let args: Args =
        Docopt::new(USAGE).and_then(|d| d.decode()).unwrap_or_else(|e| e.exit());

    if args.flag_version {
        println!("aumc version: {}",
                 option_env!("CARGO_PKG_VERSION").unwrap_or("UNKNOWN"));
        return
    }

    // Open the input & output files
    let infile = File::open(&args.arg_INPUT).unwrap_or_else(|e| {
        panic!("Error while opening input file: {}", e);
    });
    let outfile = File::create(match args.flag_out {
        Some(ref outfile) => PathBuf::from(outfile),
        None => {
            let mut base = PathBuf::from(args.arg_INPUT);
            match args.flag_emit {
                Some(Emit::Asm) => base.set_extension("s"),
                Some(Emit::Ir) => base.set_extension("ll"),
                Some(Emit::Bc) => base.set_extension("bc"),
                Some(Emit::Obj) => base.set_extension("o"),
                None | Some(Emit::Link) => base.set_extension("out"),
            };

            base
        }
    });
    // get the emit an opt levels
    let emit = args.flag_emit.unwrap_or(Emit::Link);
    let opt = args.flag_opt.unwrap_or(Opt::Default);

    // Call the lexer
    let mut lexer = lex::Lexer::new(infile.bytes().map(|r| {
        match r {
            Ok(c) => c,
            Err(e) => panic!("IO error while reading input file: {}", e),
        }
    }));

    // Create the parser
    let mut parser = parse::Parser::new(lexer.map(|r| {
        match r { // TODO(michael): Maybe handle this a bit better than a thread panic!
            Ok(t) => t,
            Err(e) => panic!("Error while lexing: {}", e),
        }
    }));

    // Parse a single expression
    println!("parser result: {:?}", parser.parse().map(|x| x.pprint()));

    /* for token in lexer {
        println!("{:?}", token);
    } */
}
