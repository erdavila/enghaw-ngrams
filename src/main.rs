use std::{
    collections::{HashMap, VecDeque},
    env,
    ffi::OsStr,
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::Path,
    rc::Rc,
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
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                process_album(name, &entry.path())?;
                continue;
            }
        }

        print_ignoring(&entry.path())?;
    }

    Ok(())
}

fn process_album(name: &str, path: &Path) -> Result<()> {
    println!("Processando álbum {name:?}");

    for entry in read_dir(path)? {
        let entry = entry?;
        if entry.file_type()?.is_file() {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                let ngram_counts = process_song(name, &entry.path())?;

                for (i, ngram_counts) in ngram_counts.into_iter().enumerate().rev() {
                    let ngram_size = i + 1;
                    let mut ngram_counts: Vec<_> = ngram_counts.into_iter().collect();
                    ngram_counts.sort_by_key(|(_, count)| *count);
                    println!(">>>> n-gram size: {ngram_size}");
                    for (ngram, count) in ngram_counts.into_iter().rev() {
                        println!(">>>>>> {ngram:?}: {count}");
                    }
                }

                continue;
            }
        }

        print_ignoring(&entry.path())?;
    }

    Ok(())
}

fn print_ignoring(path: &Path) -> Result<()> {
    println!("Ignorando {:?}", os_str_to_str(path.as_os_str())?);
    Ok(())
}

fn process_song(name: &str, path: &Path) -> Result<Vec<HashMap<Vec<Word>, usize>>> {
    println!("  Processando música {name:?}");

    let mut ngram_counts = vec![HashMap::new(); DEFAULT_MAX_NGRAM_SIZE];
    let mut ngram_buffer = VecDeque::with_capacity(DEFAULT_MAX_NGRAM_SIZE + 1);
    let mut increment = |s| {
        let word = Word(Rc::from(s));
        ngram_buffer.push_back(word);
        if ngram_buffer.len() > DEFAULT_MAX_NGRAM_SIZE {
            ngram_buffer.pop_front();
        }

        for (i, ngram_counts) in ngram_counts.iter_mut().enumerate() {
            let ngram_size = i + 1;
            if ngram_buffer.len() < ngram_size {
                break;
            }

            let ngram = ngram_buffer.iter().take(ngram_size).cloned().collect();
            ngram_counts
                .entry(ngram)
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }
    };

    let file = File::open(path)?;
    let reader = BufReader::new(file);
    for line in reader.lines() {
        let line = line?;

        let mut s: Option<String> = None;
        for char in line.chars() {
            s = if char.is_alphanumeric() || char == '-' {
                let chars = char.to_lowercase();
                let s = match s {
                    Some(mut s) => {
                        s.extend(chars);
                        s
                    }
                    None => chars.collect(),
                };
                Some(s)
            } else {
                if let Some(s) = s {
                    increment(s);
                }
                None
            };
        }

        if let Some(s) = s {
            increment(s);
        }
    }

    Ok(ngram_counts)
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Word(Rc<str>);

fn os_str_to_str(os_str: &OsStr) -> Result<&str> {
    os_str
        .to_str()
        .ok_or_else(|| anyhow!("Não conseguiu converter para String: OsString {os_str:?}"))
}
