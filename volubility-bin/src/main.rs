use clap::Parser;
use owo_colors::{OwoColorize, Stream};
use std::{
    borrow::Cow,
    ffi::OsStr,
    fs::{read_to_string, File, OpenOptions},
    io::{self, Write},
    path::PathBuf,
};
use thiserror::Error;
use volubility::{check, rewrite_to_string, DEFAULT_SEPARATOR};

/// Errors that can occur while performing Fluent versioning.
#[derive(Debug, Error)]
enum Error {
    #[error("Failed to read reference file")]
    ReadReferenceFile(#[source] io::Error),
    #[error("Failed to read input file")]
    ReadInputFile(#[source] io::Error),
    #[error("Failed to create output file")]
    CreateOutputFile(#[source] io::Error),
    #[error("`--output` cannot be used in check mode")]
    CheckWithOutput,
    #[error("Failed to validate Fluent versioning")]
    Versioning(#[from] volubility::Error),
    #[error("Failed to write output file")]
    WriteOutputFile(#[source] io::Error),
}

/// Convenience alias for `Result`'s of this crate's `Error`.
type Result<T> = std::result::Result<T, Error>;

/// Returns `true` if the file type is a fifo.
#[cfg(not(target_family = "unix"))]
fn is_fifo(_: std::fs::FileType) -> bool {
    false
}

/// Returns `true` if the file type is a fifo.
#[cfg(target_family = "unix")]
fn is_fifo(file_type: std::fs::FileType) -> bool {
    use std::os::unix::fs::FileTypeExt;
    file_type.is_fifo()
}

/// Wrapper around output writer which handles differences between stdout, file and pipe outputs.
pub(crate) enum Output {
    Stdout(io::Stdout),
    File(File),
    Pipe(File),
}

impl Output {
    /// Create a `Output` from the input path (or "-" for stdout).
    pub(crate) fn new(path: &OsStr) -> io::Result<Self> {
        if path == "-" {
            return Ok(Output::Stdout(io::stdout()));
        }

        let file =
            OpenOptions::new().read(true).write(true).create(true).truncate(true).open(path)?;
        if is_fifo(file.metadata()?.file_type()) {
            Ok(Output::File(file))
        } else {
            Ok(Output::Pipe(file))
        }
    }
}

impl Write for Output {
    fn flush(&mut self) -> io::Result<()> {
        match self {
            Output::Stdout(stdout) => stdout.flush(),
            Output::Pipe(pipe) => pipe.flush(),
            Output::File(file) => file.flush(),
        }
    }

    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Output::Stdout(stdout) => stdout.write(buf),
            Output::Pipe(pipe) => pipe.write(buf),
            Output::File(file) => file.write(buf),
        }
    }
}

/// Fluent resource versioning for Pontoon.
///
/// Pontoon does not handle updates to Fluent messages - if some message `foo` is changed in a
/// Fluent resource then translators will not be prompted to update their translation.
///
/// Instead of asking compiler developers to change the message identifier whenever a message is
/// changed, an version number is added as a suffix.
///
/// This utility automatically performs this versioning of Fluent messages given a new version of a
/// resource and an earlier version. It can also check that versioning is valid.
#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Path to the previous version of the `input` Fluent resource - used to determine which
    /// messages have changed and need to be re-versioned.
    #[arg(short, long, value_hint = clap::ValueHint::FilePath)]
    reference: PathBuf,
    /// Path to the input Fluent resource - compared against the reference resource and versioned.
    #[arg(short, long, value_hint = clap::ValueHint::FilePath)]
    input: PathBuf,
    /// Path where versioned Fluent resource should be output.
    #[arg(short, long, value_hint = clap::ValueHint::FilePath)]
    output: Option<PathBuf>,
    /// Run in "check" mode.
    ///
    /// In check mode, an output is not generated and instead, the input is checked that it has been
    /// re-versioned approprivately given the reference.
    #[arg(short, long, default_value_t = true)]
    check: bool,
    /// Separator to use for versioning suffixes.
    ///
    /// e.g. `---` is the suffix separator in message identifier `foo---1`.
    #[arg(long, default_value_t = String::from(DEFAULT_SEPARATOR))]
    suffix_separator: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Providing `--output` in check mode is nonsensical.
    if args.check && args.output.is_some() {
        return Err(Error::CheckWithOutput);
    }

    let reference_str = read_to_string(&args.reference).map_err(Error::ReadReferenceFile)?;
    let input_str = read_to_string(&args.input).map_err(Error::ReadInputFile)?;

    if args.check {
        for ((message_id, attribute_id), error) in
            check(&args.suffix_separator, &reference_str, &input_str)?
        {
            let identifier = match attribute_id {
                Some(id) => Cow::Owned(format!("{message_id}.{id}")),
                None => Cow::Borrowed(message_id),
            };
            let label = "error:".if_supports_color(Stream::Stdout, |text| text.bright_red());
            eprintln!("{label} `{identifier}`: {error}");
        }
        Ok(())
    } else {
        let mut output = args
            .output
            .map(|path| Output::new(path.as_os_str()))
            .unwrap_or_else(|| Output::new(OsStr::new("-")))
            .map_err(Error::CreateOutputFile)?;
        let rewritten_input =
            rewrite_to_string(&args.suffix_separator, &reference_str, &input_str)?;
        output.write_all(rewritten_input.as_bytes()).map_err(Error::WriteOutputFile)
    }
}
