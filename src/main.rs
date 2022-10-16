use std::{
    env,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
    rc::Rc,
};

mod asm;
mod colorscheme;
mod consts;
mod error;
mod markup;

use crate::error::Error;

fn main() {
    if let Err(err) = run() {
        println!("{err}");
    }
}

fn run() -> Result<(), Error> {
    let file_path = get_path_arg()?;
    let bytes = read_file_bytes(&file_path)?;
    let disassembled = asm::disassemble(&bytes);
    let analysed = asm::analyse(disassembled, Rc::from(bytes))?;

    Ok(println!("{analysed}"))
}

fn get_path_arg() -> Result<PathBuf, Error> {
    env::args()
        .nth(1)
        .map(|arg_str| PathBuf::from(arg_str))
        .ok_or(Error::new(
            "Path to binary required:",
            Some("use: fueldis <path>"),
        ))
}

fn read_file_bytes(path: &Path) -> Result<Vec<u8>, Error> {
    File::open(path)
        .and_then(|mut file| {
            let mut bytes = Vec::new();
            file.read_to_end(&mut bytes).map(|_| bytes)
        })
        .map_err(|err| {
            Error::new(
                format!("Path '{}':", path.display()),
                Some(format!("{err}")),
            )
        })
}
