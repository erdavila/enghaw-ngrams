use std::{
    collections::{HashMap, VecDeque},
    ffi::OsStr,
    fmt::Display,
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::Path,
    rc::Rc,
};

use anyhow::{anyhow, Result};
use clap::Parser;

type SongNGrams = HashMap<NGram, usize>;
type AlbumNGrams = Vec<(SongName, SongNGrams)>;

#[derive(Parser, Debug)]
struct Args {
    ngrams_size: usize,
    base_dir: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let data = process_base_dir(&args.base_dir, args.ngrams_size)?;
    let data = aggregate(data);

    let mut ngrams_occurrences: Vec<_> = data.into_iter().collect();
    ngrams_occurrences.sort_by_key(|(_, occurs)| occurs.len());

    let mut max_count = None;
    for (ngram, occurrences) in ngrams_occurrences.into_iter().rev() {
        if max_count.is_some_and(|x| occurrences.len() < x) {
            break;
        }
        max_count = Some(occurrences.len());

        println!("  {ngram}");
        for (album_name, song_name) in occurrences {
            println!("    {album_name} / {song_name}");
        }
    }

    Ok(())
}

fn process_base_dir(path: &str, ngrams_size: usize) -> Result<Vec<(AlbumName, AlbumNGrams)>> {
    let mut data = Vec::new();

    for entry in read_dir(path)? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                let album_ngrams = process_album(name, &entry.path(), ngrams_size)?;
                let album_name = AlbumName(Rc::from(name.to_owned()));
                data.push((album_name, album_ngrams));

                continue;
            }
        }

        print_ignoring(&entry.path())?;
    }

    Ok(data)
}

fn process_album(name: &str, path: &Path, ngrams_size: usize) -> Result<AlbumNGrams> {
    println!("Processando álbum {name:?}");

    let mut album_ngrams = Vec::new();

    for entry in read_dir(path)? {
        let entry = entry?;
        if entry.file_type()?.is_file() {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                let name = name.strip_suffix(".txt").unwrap_or(name);
                let ngram_counts = process_song(name, &entry.path(), ngrams_size)?;
                let song_name = SongName(Rc::from(name.to_owned()));
                album_ngrams.push((song_name, ngram_counts));

                continue;
            }
        }

        print_ignoring(&entry.path())?;
    }

    Ok(album_ngrams)
}

fn print_ignoring(path: &Path) -> Result<()> {
    println!("Ignorando {:?}", os_str_to_str(path.as_os_str())?);
    Ok(())
}

fn process_song(name: &str, path: &Path, ngrams_size: usize) -> Result<SongNGrams> {
    println!("  Processando música {name:?}");

    let mut ngram_counts = HashMap::new();
    let mut ngram_buffer = VecDeque::with_capacity(ngrams_size);
    let mut increment = |s| {
        let word = Word(Rc::from(s));

        if ngram_buffer.len() == ngrams_size {
            ngram_buffer.pop_front();
        }
        ngram_buffer.push_back(word);

        if ngram_buffer.len() == ngrams_size {
            let ngram = NGram(ngram_buffer.iter().cloned().collect());
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

fn aggregate(data: Vec<(AlbumName, AlbumNGrams)>) -> HashMap<NGram, Vec<(AlbumName, SongName)>> {
    let mut aggregated_data = HashMap::<NGram, Vec<(AlbumName, SongName)>>::new();

    for (album_name, album_ngrams) in data {
        for (song_name, song_ngrams) in album_ngrams {
            for ngram in song_ngrams.into_keys() {
                aggregated_data
                    .entry(ngram)
                    .and_modify(|names: &mut _| {
                        names.push((album_name.clone(), song_name.clone()));
                    })
                    .or_insert(vec![(album_name.clone(), song_name.clone())]);
            }
        }
    }

    aggregated_data
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct AlbumName(Rc<str>);
impl Display for AlbumName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct SongName(Rc<str>);
impl Display for SongName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Word(Rc<str>);
impl Display for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct NGram(Vec<Word>);
impl Display for NGram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, word) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{word}")?;
        }
        write!(f, "]")
    }
}

fn os_str_to_str(os_str: &OsStr) -> Result<&str> {
    os_str
        .to_str()
        .ok_or_else(|| anyhow!("Não conseguiu converter para String: OsString {os_str:?}"))
}
