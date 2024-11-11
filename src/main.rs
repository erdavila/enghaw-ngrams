use std::{
    collections::{HashMap, VecDeque},
    env,
    ffi::OsStr,
    fmt::Display,
    fs::{read_dir, File},
    io::{BufRead, BufReader},
    path::Path,
    rc::Rc,
};

use anyhow::{anyhow, bail, Result};

type SongNGrams = Vec<HashMap<NGram, usize>>;
type AlbumNGrams = Vec<(SongName, SongNGrams)>;

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

    let data = process_base_dir(&base_dir)?;
    let data = aggregate(data);

    for (i, ngrams_occurrences) in data.into_iter().enumerate().rev() {
        let ngram_size = i + 1;
        println!("N-Gram de tamanho {ngram_size}");

        let mut ngrams_occurrences: Vec<_> = ngrams_occurrences.into_iter().collect();
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
    }

    Ok(())
}

fn process_base_dir(path: &str) -> Result<Vec<(AlbumName, AlbumNGrams)>> {
    let mut data = Vec::new();

    for entry in read_dir(path)? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                let album_ngrams = process_album(name, &entry.path())?;
                let album_name = AlbumName(Rc::from(name.to_owned()));
                data.push((album_name, album_ngrams));

                continue;
            }
        }

        print_ignoring(&entry.path())?;
    }

    Ok(data)
}

fn process_album(name: &str, path: &Path) -> Result<AlbumNGrams> {
    println!("Processando álbum {name:?}");

    let mut album_ngrams = Vec::new();

    for entry in read_dir(path)? {
        let entry = entry?;
        if entry.file_type()?.is_file() {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                let name = name.strip_suffix(".txt").unwrap_or(name);
                let ngram_counts = process_song(name, &entry.path())?;
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

fn process_song(name: &str, path: &Path) -> Result<SongNGrams> {
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

            let ngram = NGram(ngram_buffer.iter().take(ngram_size).cloned().collect());
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

fn aggregate(
    data: Vec<(AlbumName, AlbumNGrams)>,
) -> Vec<HashMap<NGram, Vec<(AlbumName, SongName)>>> {
    let mut aggregated_data =
        vec![HashMap::<NGram, Vec<(AlbumName, SongName)>>::new(); DEFAULT_MAX_NGRAM_SIZE];

    for (album_name, album_ngrams) in data {
        for (song_name, song_ngrams) in album_ngrams {
            for (i, ngram_counts) in song_ngrams.into_iter().enumerate() {
                let aggregated_data = &mut aggregated_data[i];

                for ngram in ngram_counts.into_keys() {
                    aggregated_data
                        .entry(ngram)
                        .and_modify(|names: &mut _| {
                            names.push((album_name.clone(), song_name.clone()));
                        })
                        .or_insert(vec![(album_name.clone(), song_name.clone())]);
                }
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
