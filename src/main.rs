use std::{
    collections::{BTreeSet, HashMap, HashSet},
    ffi::OsStr,
    fmt::Display,
    fs::{read_dir, DirEntry, File, FileType},
    io::{BufRead, BufReader},
    path::Path,
    rc::Rc,
};

use anyhow::{anyhow, Ok, Result};
use clap::Parser;

type AlbumTokens = Vec<(SongName, Vec<Token>)>;
type Location<'loc> = (&'loc AlbumName, &'loc SongName);

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

    let data = load_tokens(&args.base_dir)?;
    let data = extract_ngrams(&data, args.max_ngrams_size);

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

fn load_tokens(path: &str) -> Result<Vec<(AlbumName, AlbumTokens)>> {
    let mut data = Vec::new();

    process_entries(path, EntryType::Directory, |name, entry| {
        let album_tokens = process_album(name, &entry.path())?;
        let album_name = AlbumName(name.to_owned());
        data.push((album_name, album_tokens));
        Ok(())
    })?;

    Ok(data)
}

fn process_album(name: &str, path: &Path) -> Result<Vec<(SongName, Vec<Token>)>> {
    println!("Processando álbum {name:?}");

    let mut album_tokens = Vec::new();

    process_entries(path, EntryType::File, |name, entry| {
        let name = name.strip_suffix(".txt").unwrap_or(name);
        let song_tokens = process_song(name, &entry.path())?;
        let song_name = SongName(name.to_owned());
        album_tokens.push((song_name, song_tokens));
        Ok(())
    })?;

    Ok(album_tokens)
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

fn process_song(name: &str, path: &Path) -> Result<Vec<Token>> {
    println!("  Processando música {name:?}");

    let mut song_tokens = Vec::new();

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
                song_tokens.push(Token(s));
            }
        }

        if let Some(s) = s {
            song_tokens.push(Token(s));
        }
    }

    Ok(song_tokens)
}

fn extract_ngrams(
    data: &Vec<(AlbumName, AlbumTokens)>,
    ngrams_size: usize,
) -> HashMap<NGram, BTreeSet<Location>> {
    let mut ngrams: HashMap<_, BTreeSet<_>> = HashMap::new();

    for (album_name, album_tokens) in data {
        for (song_name, song_tokens) in album_tokens {
            if ngrams_size > song_tokens.len() {
                println!("Tokens insuficientes em {album_name} / {song_name}");
                continue;
            }

            for i in 0..=(song_tokens.len() - ngrams_size) {
                let ngram = &song_tokens[i..(i + ngrams_size)];
                let ngram = NGram(
                    ngram
                        .iter()
                        .map(|token| Word(Rc::from(token.0.clone())))
                        .collect(),
                );

                ngrams
                    .entry(ngram)
                    .and_modify(|locations: &mut _| {
                        locations.insert((album_name, song_name));
                    })
                    .or_insert_with(|| BTreeSet::from([(album_name, song_name)]));
            }
        }
    }

    ngrams
}

struct ProcessingInfo<'loc, 'info> {
    locations: BTreeSet<Location<'loc>>,
    shown_in: HashSet<&'info BTreeSet<Location<'loc>>>,
}
impl<'loc, 'info> ProcessingInfo<'loc, 'info> {
    fn new() -> Self {
        ProcessingInfo {
            locations: BTreeSet::new(),
            shown_in: HashSet::new(),
        }
    }

    fn add(&mut self, info: &'info Info<'loc>) {
        self.locations.extend(&info.locations);
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
) -> HashMap<NGram, ProcessingInfo> {
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

struct Info<'loc> {
    ngram: NGram,
    locations: BTreeSet<Location<'loc>>,
    show_decision: ShowDecision<'loc>,
}

#[derive(PartialEq, Eq, Debug)]
enum ShowDecision<'loc> {
    Show,
    ShownIn(HashSet<BTreeSet<Location<'loc>>>),
}

fn consolidate<'loc>(
    ngrams: HashMap<NGram, ProcessingInfo<'loc, '_>>,
    min_locations: usize,
) -> Vec<Info<'loc>> {
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

fn generate_shorter_ngrams<'loc, 'info>(
    ngrams_size: usize,
    data: &'info Vec<Info<'loc>>,
) -> HashMap<NGram, ProcessingInfo<'loc, 'info>> {
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct AlbumName(String);
impl Display for AlbumName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct SongName(String);
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

#[derive(Debug)]
struct Token(String);

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
        ($( $album_name:ident / $song_name:ident ),+) => {
            BTreeSet::from(
                [
                    $(
                            (&$album_name, &$song_name)
                    ),+
                ]
            )
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
        let album = AlbumName("ALBUM".to_string());
        let song1 = SongName("S1".to_string());
        let song2 = SongName("S2".to_string());

        let data = HashMap::from([
            (ngram![A B C X Y], locations![album / song1]),
            (ngram![D E F X Y], locations![album / song2]),
        ]);
        let data = prepare_max_size_ngrams(data);
        let data = consolidate(data, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X Y], locations![album/song1], true;
            ngram![D E F X Y], locations![album/song2], true;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(4, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X], locations![album/song1], false;
            ngram![B C X Y], locations![album/song1], false;
            ngram![D E F X], locations![album/song2], false;
            ngram![E F X Y], locations![album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(3, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C], locations![album/song1], false;
            ngram![B C X], locations![album/song1], false;
            ngram![C X Y], locations![album/song1], false;
            ngram![D E F], locations![album/song2], false;
            ngram![E F X], locations![album/song2], false;
            ngram![F X Y], locations![album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(2, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B], locations![album/song1], false;
            ngram![B C], locations![album/song1], false;
            ngram![C X], locations![album/song1], false;
            ngram![D E], locations![album/song2], false;
            ngram![E F], locations![album/song2], false;
            ngram![F X], locations![album/song2], false;
            ngram![X Y], locations![album/song1, album/song2], true;
        ]);
    }

    #[test]
    fn case_2() {
        const MIN_LOCATIONS: usize = 1;
        let album = AlbumName("ALBUM".to_string());
        let song1 = SongName("S1".to_string());
        let song2 = SongName("S2".to_string());

        let data = HashMap::from([
            (ngram![A B C X Y], locations![album / song1]),
            (ngram![D E F X Y], locations![album / song1, album / song2]),
        ]);
        let data = prepare_max_size_ngrams(data);
        let data = consolidate(data, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X Y], locations![album/song1], true;
            ngram![D E F X Y], locations![album/song1, album/song2], true;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(4, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X], locations![album/song1], false;
            ngram![B C X Y], locations![album/song1], false;
            ngram![D E F X], locations![album/song1, album/song2], false;
            ngram![E F X Y], locations![album/song1, album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(3, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C], locations![album/song1], false;
            ngram![B C X], locations![album/song1], false;
            ngram![C X Y], locations![album/song1], false;
            ngram![D E F], locations![album/song1, album/song2], false;
            ngram![E F X], locations![album/song1, album/song2], false;
            ngram![F X Y], locations![album/song1, album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(2, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B], locations![album/song1], false;
            ngram![B C], locations![album/song1], false;
            ngram![C X], locations![album/song1], false;
            ngram![D E], locations![album/song1, album/song2], false;
            ngram![E F], locations![album/song1, album/song2], false;
            ngram![F X], locations![album/song1, album/song2], false;
            ngram![X Y], locations![album/song1, album/song2], true;
        ]);
    }

    #[test]
    fn case_3() {
        const MIN_LOCATIONS: usize = 2;
        let album = AlbumName("ALBUM".to_string());
        let song1 = SongName("S1".to_string());
        let song2 = SongName("S2".to_string());

        let data = HashMap::from([
            (ngram![A B C X Y], locations![album / song1]),
            (ngram![D E F X Y], locations![album / song1, album / song2]),
        ]);
        let data = prepare_max_size_ngrams(data);
        let data = consolidate(data, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X Y], locations![album/song1], false;
            ngram![D E F X Y], locations![album/song1, album/song2], true;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(4, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X], locations![album/song1], false;
            ngram![B C X Y], locations![album/song1], false;
            ngram![D E F X], locations![album/song1, album/song2], false;
            ngram![E F X Y], locations![album/song1, album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(3, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C], locations![album/song1], false;
            ngram![B C X], locations![album/song1], false;
            ngram![C X Y], locations![album/song1], false;
            ngram![D E F], locations![album/song1, album/song2], false;
            ngram![E F X], locations![album/song1, album/song2], false;
            ngram![F X Y], locations![album/song1, album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(2, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B], locations![album/song1], false;
            ngram![B C], locations![album/song1], false;
            ngram![C X], locations![album/song1], false;
            ngram![D E], locations![album/song1, album/song2], false;
            ngram![E F], locations![album/song1, album/song2], false;
            ngram![F X], locations![album/song1, album/song2], false;
            ngram![X Y], locations![album/song1, album/song2], false;
        ]);
    }

    #[test]
    fn case_4() {
        const MIN_LOCATIONS: usize = 2;
        let album = AlbumName("ALBUM".to_string());
        let song1 = SongName("song1".to_string());
        let song2 = SongName("S2".to_string());

        let data = HashMap::from([
            (ngram![A B C X Y], locations![album / song1]),
            (ngram![D E F X Y], locations![album / song2]),
        ]);
        let data = prepare_max_size_ngrams(data);
        let data = consolidate(data, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X Y], locations![album/song1], false;
            ngram![D E F X Y], locations![album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(4, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C X], locations![album/song1], false;
            ngram![B C X Y], locations![album/song1], false;
            ngram![D E F X], locations![album/song2], false;
            ngram![E F X Y], locations![album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(3, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B C], locations![album/song1], false;
            ngram![B C X], locations![album/song1], false;
            ngram![C X Y], locations![album/song1], false;
            ngram![D E F], locations![album/song2], false;
            ngram![E F X], locations![album/song2], false;
            ngram![F X Y], locations![album/song2], false;
        ]);

        let shorter_ngrams = generate_shorter_ngrams(2, &data);
        let data = consolidate(shorter_ngrams, MIN_LOCATIONS);
        assert_infos!(data, [
            ngram![A B], locations![album/song1], false;
            ngram![B C], locations![album/song1], false;
            ngram![C X], locations![album/song1], false;
            ngram![D E], locations![album/song2], false;
            ngram![E F], locations![album/song2], false;
            ngram![F X], locations![album/song2], false;
            ngram![X Y], locations![album/song1, album/song2], true;
        ]);
    }
}
