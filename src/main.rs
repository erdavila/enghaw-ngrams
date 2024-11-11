use std::{env, ffi::OsStr, fs::read_dir, path::PathBuf};

use anyhow::{anyhow, bail, Result};

const DEFAULT_MAX_NGRAM_SIZE: usize = 5;

fn main() -> Result<()> {
    let mut args = env::args();
    args.next();
    let base_dir = args.next().ok_or(anyhow!(
        "Esperado parâmetro que especifica o diretório-base"
    ))?;
    if args.next().is_some() {
        bail!("Parâmetros em excesso");
    }

    for entry in read_dir(&base_dir)? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                process_album(name, entry.path())?;
                continue;
            }
        }

        print_ignoring(entry.path())?;
    }

    Ok(())
}

fn process_album(name: &str, path: PathBuf) -> Result<()> {
    println!("Processando álbum {name:?}");

    for entry in read_dir(path)? {
        let entry = entry?;
        if entry.file_type()?.is_file() {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                process_song(name, entry.path())?;
                continue;
            }
        }

        print_ignoring(entry.path())?;
    }

    Ok(())
}

fn print_ignoring(path: PathBuf) -> Result<()> {
    println!("Ignorando {:?}", os_str_to_str(path.as_os_str())?);
    Ok(())
}

fn process_song(name: &str, path: PathBuf) -> Result<()> {
    println!("  Processando música {name:?}");
    Ok(())
}

fn os_str_to_str(os_str: &OsStr) -> Result<&str> {
    os_str
        .to_str()
        .ok_or_else(|| anyhow!("Não conseguiu converter para String: OsString {os_str:?}"))
}
