use std::{
    env,
    fs::{self, File},
    io::Read,
    path::{Path, PathBuf},
    process::ExitCode,
    rc::Rc,
};

use toml::Value;

mod asm;
mod colorscheme;
mod consts;
mod error;
mod markup;

use crate::error::Error;

fn main() -> ExitCode {
    if let Err(err) = run() {
        println!("{err}");
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

fn run() -> Result<(), Error> {
    let file_path = find_binary_file_path()?;
    let bytes = read_file_bytes(&file_path)?;
    let disassembled = asm::disassemble(&bytes);
    let analysed = asm::analyse(disassembled, Rc::from(bytes))?;

    Ok(println!("{analysed}"))
}

fn find_binary_file_path() -> Result<PathBuf, Error> {
    get_path_arg()
        .or_else(get_pkg_binary_path)
        .ok_or_else(|| Error::new(
            "Unable to determine bytecode binary file path.",
            Some("use: forc-dis [path] or forc dis from within a Sway project"),
        ))
}

fn get_path_arg() -> Option<PathBuf> {
    env::args().nth(1).map(PathBuf::from)
}

fn get_pkg_binary_path() -> Option<PathBuf> {
    // Perform the following, aborting at the first error:
    // - Search for a package dir containing a `Forc.toml` in the current dir ancestors.
    // - Open the `Forc.toml` and parse it.
    // - Get the project::name from the TOML.
    // - Append "out/debug/<name>.bin" to the package dir.

    env::current_dir().ok().and_then(|cur_dir| {
        cur_dir
            .ancestors()
            .find(|path| path.join("Forc.toml").try_exists().unwrap_or(false))
            .and_then(|pkg_dir| {
                fs::read_to_string(pkg_dir.join("Forc.toml"))
                    .ok()
                    .and_then(|pkg_str| pkg_str.parse::<Value>().ok())
                    .and_then(|pkg_toml| {
                        pkg_toml
                            .get("project")
                            .and_then(|pkg_prj_table| pkg_prj_table.get("name"))
                            .and_then(|pkg_name_str| pkg_name_str.as_str().map(|s| s.to_owned()))
                    })
                    .map(|pkg_name| {
                        pkg_dir
                            .join("out")
                            .join("debug")
                            .join(PathBuf::from(pkg_name).with_extension("bin"))
                    })
            })
    })
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
