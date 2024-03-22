use std::fs;

mod parse;

fn main() {
    let file_path = "./test.vvc";
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");

    println!("{}", contents);

    dbg!(parse::parse_fun(&contents.replace('\n', "")));
}
