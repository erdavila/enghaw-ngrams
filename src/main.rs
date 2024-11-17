use std::{
    collections::{hash_map::Entry, HashMap, HashSet, VecDeque},
    ffi::OsStr,
    fmt::Display,
    fs::{read_dir, DirEntry, File, FileType},
    io::{BufRead, BufReader},
    path::Path,
    rc::Rc,
};

use anyhow::{anyhow, Result};
use clap::Parser;

type SongNGrams = HashSet<NGram>;
type AlbumNGrams = Vec<(SongName, SongNGrams)>;

#[derive(Parser, Debug)]
struct Args {
    ngrams_size: usize,
    base_dir: String,

    #[arg(short, long, default_value_t = 2, value_name = "N")]
    min_occurrences: usize,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let data = process_base_dir(&args.base_dir, args.ngrams_size)?;
    let data = aggregate(data);

    let mut ngrams_occurrences: Vec<_> = data.into_iter().collect();
    ngrams_occurrences.sort_by(|(ngram_a, occurrs_a), (ngram_b, occurrs_b)| {
        let occurs = occurrs_a.len().cmp(&occurrs_b.len());
        occurs.reverse().then_with(|| ngram_a.cmp(ngram_b))
    });

    for (ngram, occurrences) in ngrams_occurrences {
        if occurrences.len() < args.min_occurrences {
            break;
        }

        println!("  {ngram}");
        for (album_name, song_name) in occurrences {
            println!("    {album_name} / {song_name}");
        }
    }

    Ok(())
}

fn process_base_dir(path: &str, ngrams_size: usize) -> Result<Vec<(AlbumName, AlbumNGrams)>> {
    let mut data = Vec::new();

    process_entries(path, EntryType::Directory, |name, entry| {
        let album_ngrams = process_album(name, &entry.path(), ngrams_size)?;
        let album_name = AlbumName(Rc::from(name.to_owned()));
        data.push((album_name, album_ngrams));
        Ok(())
    })?;

    Ok(data)
}

fn process_album(name: &str, path: &Path, ngrams_size: usize) -> Result<AlbumNGrams> {
    println!("Processando álbum {name:?}");

    let mut album_ngrams = Vec::new();

    process_entries(path, EntryType::File, |name, entry| {
        let name = name.strip_suffix(".txt").unwrap_or(name);
        let ngram_counts = process_song(name, &entry.path(), ngrams_size)?;
        let song_name = SongName(Rc::from(name.to_owned()));
        album_ngrams.push((song_name, ngram_counts));
        Ok(())
    })?;

    Ok(album_ngrams)
}

#[derive(Clone, Copy, Debug)]
enum EntryType {
    File,
    Directory,
}
impl EntryType {
    fn matches(self, file_type: FileType) -> bool {
        match self {
            EntryType::File => file_type.is_file(),
            EntryType::Directory => file_type.is_dir(),
        }
    }
}

fn process_entries(
    path: impl AsRef<Path>,
    entry_type: EntryType,
    mut process_entry: impl FnMut(&str, DirEntry) -> Result<()>,
) -> Result<()> {
    for entry in read_dir(path)? {
        let entry = entry?;
        if entry_type.matches(entry.file_type()?) {
            let name = entry.file_name();
            let name = os_str_to_str(&name)?;
            if !name.starts_with('.') {
                process_entry(name, entry)?;
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

fn process_song(name: &str, path: &Path, ngrams_size: usize) -> Result<SongNGrams> {
    println!("  Processando música {name:?}");

    let mut ngram_counts = HashSet::new();
    let mut ngram_buffer = VecDeque::with_capacity(ngrams_size);
    let mut process_word = |s| {
        let word = Word(Rc::from(s));

        if ngram_buffer.len() == ngrams_size {
            ngram_buffer.pop_front();
        }
        ngram_buffer.push_back(word);

        if ngram_buffer.len() == ngrams_size {
            let ngram = NGram(ngram_buffer.iter().cloned().collect());
            ngram_counts.insert(ngram);
        }
    };

    let file = File::open(path)?;
    let reader = BufReader::new(file);
    for line in reader.lines() {
        let line = line?;

        let mut s: Option<String> = None;
        for char in line.chars() {
            if char.is_alphanumeric() || char == '-' {
                let chars = char.to_lowercase();
                match &mut s {
                    Some(s) => {
                        s.extend(chars);
                    }
                    None => {
                        s = Some(chars.collect());
                    }
                }
            } else if let Some(s) = s.take() {
                process_word(s);
            }
        }

        if let Some(s) = s {
            process_word(s);
        }
    }

    Ok(ngram_counts)
}

fn aggregate(data: Vec<(AlbumName, AlbumNGrams)>) -> HashMap<NGram, Vec<(AlbumName, SongName)>> {
    let mut aggregated_data = HashMap::<_, Vec<_>>::new();

    for (album_name, album_ngrams) in data {
        for (song_name, song_ngrams) in album_ngrams {
            for ngram in song_ngrams {
                let album_and_song_name = (album_name.clone(), song_name.clone());
                match aggregated_data.entry(ngram) {
                    Entry::Occupied(entry) => {
                        entry.into_mut().push(album_and_song_name);
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(vec![album_and_song_name]);
                    }
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
