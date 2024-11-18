use std::{
    collections::{hash_map::Entry, BTreeSet, HashMap, HashSet, VecDeque},
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
type Location = (AlbumName, SongName);

#[derive(Parser, Debug)]
struct Args {
    max_ngrams_size: usize,
    base_dir: String,

    #[arg(short, long, default_value_t = 2, value_name = "N")]
    min_ngrams_size: usize,

    #[arg(short = 'l', long, default_value_t = 2, value_name = "N")]
    min_locations: usize,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let data = process_base_dir(&args.base_dir, args.max_ngrams_size)?;
    let data = aggregate(data);

    let data = prepare_max_size_ngrams(data);
    let mut data = consolidate(data, args.min_locations);
    print_ngrams_with_size(args.max_ngrams_size, &mut data);

    for ngrams_size in (args.min_ngrams_size..args.max_ngrams_size).rev() {
        let shorter_ngrams = generate_shorter_ngrams(ngrams_size, &data);
        data = consolidate(shorter_ngrams, args.min_locations);
        print_ngrams_with_size(ngrams_size, &mut data);
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

fn aggregate(data: Vec<(AlbumName, AlbumNGrams)>) -> HashMap<NGram, BTreeSet<Location>> {
    let mut aggregated_data = HashMap::<_, BTreeSet<_>>::new();

    for (album_name, album_ngrams) in data {
        for (song_name, song_ngrams) in album_ngrams {
            for ngram in song_ngrams {
                let album_and_song_name = (album_name.clone(), song_name.clone());
                match aggregated_data.entry(ngram) {
                    Entry::Occupied(entry) => {
                        entry.into_mut().insert(album_and_song_name);
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(BTreeSet::from([album_and_song_name]));
                    }
                }
            }
        }
    }

    aggregated_data
}

struct ProcessingInfo<'a> {
    locations: BTreeSet<Location>,
    shown_in: HashSet<&'a BTreeSet<Location>>,
}
impl<'a> ProcessingInfo<'a> {
    fn new() -> Self {
        ProcessingInfo {
            locations: BTreeSet::new(),
            shown_in: HashSet::new(),
        }
    }

    fn add(&mut self, info: &'a Info) {
        self.locations.extend(info.locations.iter().cloned());
        match &info.show_decision {
            ShowDecision::Show => {
                self.shown_in.insert(&info.locations);
            }
            ShowDecision::ShownIn(set) => {
                self.shown_in.extend(set);
            }
        }
    }
}

fn prepare_max_size_ngrams(
    data: HashMap<NGram, BTreeSet<Location>>,
) -> HashMap<NGram, ProcessingInfo<'static>> {
    data.into_iter()
        .map(|(ngram, locations)| {
            let pi = ProcessingInfo {
                locations,
                shown_in: HashSet::new(),
            };
            (ngram, pi)
        })
        .collect()
}

struct Info {
    ngram: NGram,
    locations: BTreeSet<Location>,
    show_decision: ShowDecision,
}

#[derive(PartialEq, Eq, Debug)]
enum ShowDecision {
    Show,
    ShownIn(HashSet<BTreeSet<Location>>),
}

fn consolidate(ngrams: HashMap<NGram, ProcessingInfo>, min_locations: usize) -> Vec<Info> {
    ngrams
        .into_iter()
        .map(|(ngram, pi)| {
            let ProcessingInfo {
                locations,
                shown_in,
            } = pi;

            let show_decision = if locations.len() >= min_locations
                && !has_single_element(shown_in.iter(), &&locations)
            {
                ShowDecision::Show
            } else {
                ShowDecision::ShownIn(shown_in.into_iter().cloned().collect())
            };

            Info {
                ngram,
                locations,
                show_decision,
            }
        })
        .collect()
}

fn has_single_element<'a, T: Eq + 'a>(set: impl IntoIterator<Item = &'a T>, elem: &T) -> bool {
    let mut iter = set.into_iter();
    iter.next()
        .is_some_and(|el| el == elem && iter.next().is_none())
}

fn print_ngrams_with_size(ngrams_size: usize, data: &mut Vec<Info>) {
    data.sort_by(|info_a, info_b| {
        let locs = info_a.locations.len().cmp(&info_b.locations.len());
        locs.reverse().then_with(|| info_a.ngram.cmp(&info_b.ngram))
    });

    println!("N-grams de tamanho {ngrams_size}");
    for info in data {
        if info.show_decision == ShowDecision::Show {
            println!("  {}", info.ngram);

            for (album_name, song_name) in &info.locations {
                println!("    {album_name} / {song_name}");
            }
        }
    }
}

fn generate_shorter_ngrams(ngrams_size: usize, data: &Vec<Info>) -> HashMap<NGram, ProcessingInfo> {
    let mut output: HashMap<_, ProcessingInfo> = HashMap::new();

    for info in data {
        assert!(!info.locations.is_empty());

        assert_eq!(info.ngram.0.len(), ngrams_size + 1);
        let prefix_ngram = NGram(info.ngram.0[..ngrams_size].to_vec());
        let suffix_ngram = NGram(info.ngram.0[1..].to_vec());

        for shorter_ngram in [prefix_ngram, suffix_ngram] {
            assert_eq!(shorter_ngram.0.len(), ngrams_size);

            output
                .entry(shorter_ngram)
                .and_modify(|pi| {
                    pi.add(info);
                })
                .or_insert_with(|| {
                    let mut pi = ProcessingInfo::new();
                    pi.add(info);
                    pi
                });
        }
    }

    output
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

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! ngram {
        ($( $ident:ident )+) => {
            NGram(
                vec![
                    $(
                        Word(Rc::from(stringify!($ident)))
                    ),+
                ]
            )
        };
    }

    macro_rules! locations {
        ($( $location:ident )+) => {
            BTreeSet::from([
                $(
                    {
                        let str = Rc::from(stringify!($location));
                        let album_name = AlbumName(Rc::clone(&str));
                        let song_name = SongName(Rc::clone(&str));
                        let location: Location = (album_name, song_name);
                        location
                    }
                ),+
            ])
        };
    }

    macro_rules! assert_infos {
        ($infos:ident, [ $( $ngram:expr, $location:expr, $show:literal );+ $(;)? ]) => {
            {
                let infos = &$infos;
                let mut expected = HashMap::from([
                    $(
                        ($ngram, ($location, $show))
                    ),+
                ]);

                for info in infos {
                    match expected.remove(&info.ngram) {
                        Some((locations, show)) => {
                            assert_eq!(info.locations, locations, "{}", info.ngram);
                            if show {
                                assert_eq!(info.show_decision, ShowDecision::Show, "{}", info.ngram);
                            } else {
                                assert_ne!(info.show_decision, ShowDecision::Show, "{}", info.ngram);
                            }
                        },
                        None => panic!("Not expected: {}", info.ngram),
                    }
                }

                if !expected.is_empty() {
                    panic!("Expected: {:?}", expected);
                }
            }
        };
    }

    #[test]
    fn case_1() {
        const MIN_LOCATIONS: usize = 1;

        let data = HashMap::from([
            (ngram![A B C X Y], locations![L1]),
            (ngram![D E F X Y], locations![L2]),
        ]);
        let data = prepare_max_size_ngrams(data);
        let data = consolidate(data, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X Y], locations![L1], true;
            ngram![D E F X Y], locations![L2], true;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(4, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X], locations![L1], false;
            ngram![B C X Y], locations![L1], false;
            ngram![D E F X], locations![L2], false;
            ngram![E F X Y], locations![L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(3, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C], locations![L1], false;
            ngram![B C X], locations![L1], false;
            ngram![C X Y], locations![L1], false;
            ngram![D E F], locations![L2], false;
            ngram![E F X], locations![L2], false;
            ngram![F X Y], locations![L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(2, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B], locations![L1], false;
            ngram![B C], locations![L1], false;
            ngram![C X], locations![L1], false;
            ngram![D E], locations![L2], false;
            ngram![E F], locations![L2], false;
            ngram![F X], locations![L2], false;
            ngram![X Y], locations![L1 L2], true;
        ]);
    }

    #[test]
    fn case_2() {
        const MIN_LOCATIONS: usize = 1;

        let data = HashMap::from([
            (ngram![A B C X Y], locations![L1]),
            (ngram![D E F X Y], locations![L1 L2]),
        ]);
        let data = prepare_max_size_ngrams(data);
        let data = consolidate(data, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X Y], locations![L1], true;
            ngram![D E F X Y], locations![L1 L2], true;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(4, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X], locations![L1], false;
            ngram![B C X Y], locations![L1], false;
            ngram![D E F X], locations![L1 L2], false;
            ngram![E F X Y], locations![L1 L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(3, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C], locations![L1], false;
            ngram![B C X], locations![L1], false;
            ngram![C X Y], locations![L1], false;
            ngram![D E F], locations![L1 L2], false;
            ngram![E F X], locations![L1 L2], false;
            ngram![F X Y], locations![L1 L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(2, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B], locations![L1], false;
            ngram![B C], locations![L1], false;
            ngram![C X], locations![L1], false;
            ngram![D E], locations![L1 L2], false;
            ngram![E F], locations![L1 L2], false;
            ngram![F X], locations![L1 L2], false;
            ngram![X Y], locations![L1 L2], true;
        ]);
    }

    #[test]
    fn case_3() {
        const MIN_LOCATIONS: usize = 2;

        let data = HashMap::from([
            (ngram![A B C X Y], locations![L1]),
            (ngram![D E F X Y], locations![L1 L2]),
        ]);
        let data = prepare_max_size_ngrams(data);
        let data = consolidate(data, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X Y], locations![L1], false;
            ngram![D E F X Y], locations![L1 L2], true;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(4, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X], locations![L1], false;
            ngram![B C X Y], locations![L1], false;
            ngram![D E F X], locations![L1 L2], false;
            ngram![E F X Y], locations![L1 L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(3, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C], locations![L1], false;
            ngram![B C X], locations![L1], false;
            ngram![C X Y], locations![L1], false;
            ngram![D E F], locations![L1 L2], false;
            ngram![E F X], locations![L1 L2], false;
            ngram![F X Y], locations![L1 L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(2, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B], locations![L1], false;
            ngram![B C], locations![L1], false;
            ngram![C X], locations![L1], false;
            ngram![D E], locations![L1 L2], false;
            ngram![E F], locations![L1 L2], false;
            ngram![F X], locations![L1 L2], false;
            ngram![X Y], locations![L1 L2], false;
        ]);
    }

    #[test]
    fn case_4() {
        const MIN_LOCATIONS: usize = 2;

        let data = HashMap::from([
            (ngram![A B C X Y], locations![L1]),
            (ngram![D E F X Y], locations![L2]),
        ]);
        let data = prepare_max_size_ngrams(data);
        let data = consolidate(data, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X Y], locations![L1], false;
            ngram![D E F X Y], locations![L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(4, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X], locations![L1], false;
            ngram![B C X Y], locations![L1], false;
            ngram![D E F X], locations![L2], false;
            ngram![E F X Y], locations![L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(3, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C], locations![L1], false;
            ngram![B C X], locations![L1], false;
            ngram![C X Y], locations![L1], false;
            ngram![D E F], locations![L2], false;
            ngram![E F X], locations![L2], false;
            ngram![F X Y], locations![L2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(2, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B], locations![L1], false;
            ngram![B C], locations![L1], false;
            ngram![C X], locations![L1], false;
            ngram![D E], locations![L2], false;
            ngram![E F], locations![L2], false;
            ngram![F X], locations![L2], false;
            ngram![X Y], locations![L1 L2], true;
        ]);
    }
}
