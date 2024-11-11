use std::{
    env,
    ffi::OsStr,
    fs::{read_dir, DirEntry},
};

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
        process_album(entry?)?;
    }

    Ok(())
}

fn process_album(entry: DirEntry) -> Result<()> {
    if entry.file_type()?.is_dir() {
        let name = entry.file_name();
        let name = os_str_to_str(&name)?;
        if !name.starts_with('.') {
            println!("Processando álbum {name:?}");
            for entry in read_dir(entry.path())? {
                process_song(entry?)?;
            }
            return Ok(());
        }
    }

    let path = entry.path();
    let path = os_str_to_str(path.as_os_str())?;
    println!("Ignorando {path:?}");
    Ok(())
}

fn process_song(entry: DirEntry) -> Result<()> {
    if entry.file_type()?.is_file() {
        let name = entry.file_name();
        let name = os_str_to_str(&name)?;
        if !name.starts_with('.') {
            println!("  Processando música {name:?}");
            return Ok(());
        }
    }

    let path = entry.path();
    let path = os_str_to_str(path.as_os_str())?;
    println!("  Ignorando {path:?}");
    Ok(())
}

fn os_str_to_str(os_str: &OsStr) -> Result<&str> {
    os_str
        .to_str()
        .ok_or_else(|| anyhow!("Não conseguiu converter para String: OsString {os_str:?}"))
}
