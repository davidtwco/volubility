use fluent_syntax::{
    ast::{Entry, Identifier, Pattern, Resource},
    parser::{parse, ParserError},
};
use std::{collections::HashMap, fmt, num::ParseIntError};
use thiserror::Error;

mod owned;
mod pretty;

pub use crate::pretty::to_string;

/// Default separator used with version suffixes.
pub static DEFAULT_SEPARATOR: &'static str = "---";

/// Errors that can occur while performing Fluent versioning.
#[derive(Debug, Error)]
pub enum Error {
    #[error("Failed to parse reference Fluent resource")]
    ParseReference(#[source] ParserError),
    #[error("Failed to parse input Fluent resource")]
    ParseInput(#[source] ParserError),
    #[error("Failed to parse version suffix on Fluent identifier")]
    InvalidVersion(#[source] ParseIntError),
}

/// Convenience alias for `Result`'s of this crate's `Error`.
type Result<T> = std::result::Result<T, Error>;

/// `Option`-like enum for versions. Only exists for readability.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Versioned {
    Versioned(usize),
    Unversioned,
}

/// Parse a versioned identifier from a Fluent `Identifier` AST node.
///
/// Splits the Fluent message identifier string according to this project's conventions
/// (i.e. a `separator` suffix).
fn parse_versioned_identifier<'a>(
    separator: &str,
    identifier: &Identifier<&'a str>,
) -> Result<(&'a str, Versioned)> {
    match identifier.name.split_once(separator) {
        None => Ok((identifier.name, Versioned::Unversioned)),
        Some((identifier, "")) => Ok((identifier, Versioned::Unversioned)),
        Some((identifier, version)) => Ok((
            identifier,
            Versioned::Versioned(
                usize::from_str_radix(version, 10).map_err(Error::InvalidVersion)?,
            ),
        )),
    }
}

/// Convenience alias for a message identifier.
///
/// "Message" here deviates slightly from what a message is to Fluent, in that this alias can
/// optionally identify a message's attribute, since we consider these the same for the purposes of
/// versioning.
pub type MessageIdentifier<'a> = (&'a str, Option<&'a str>);

/// Convenience alias for a message's content.
type MessageContent<'a> = &'a Pattern<&'a str>;

/// Convenience alias for the map between message identifiers and their version + content.
type ReferenceMessages<'a> =
    HashMap<MessageIdentifier<'a>, (Versioned, Option<MessageContent<'a>>)>;

/// Reason for an incorrect versioning.
#[derive(Debug, Eq, PartialEq)]
pub enum IncorrectVersioning {
    /// Initial version of a message isn't one.
    BadInitialVersion,
    /// Unversioned message changed without being given a version.
    NoInitialVersion,
    /// Versioned message had its version removed.
    VersionRemoved,
    /// Versioned message had its version changed despite the message content not changing.
    UnnecessaryVersionChange,
    /// Versioned message was changed but its version is lower than that of the reference.
    ReferenceVersionGreaterOrEqual,
}

impl fmt::Display for IncorrectVersioning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IncorrectVersioning::BadInitialVersion => write!(f, "initial version must be one"),
            IncorrectVersioning::NoInitialVersion => {
                write!(f, "message has changed without being versioned")
            }
            IncorrectVersioning::VersionRemoved => write!(f, "version was removed"),
            IncorrectVersioning::UnnecessaryVersionChange => {
                write!(f, "unnecessary version change")
            }
            IncorrectVersioning::ReferenceVersionGreaterOrEqual => {
                write!(f, "message changed without version bump")
            }
        }
    }
}

/// Check whether the change in `version` from `reference_version_and_value`'s version is
/// okay or not (considering whether `value` changed from `reference_version_and_value`'s value).
fn check_versioning(
    version: Versioned,
    value: Option<&Pattern<&str>>,
    reference_version_and_value: Option<&(Versioned, Option<MessageContent<'_>>)>,
) -> Option<IncorrectVersioning> {
    let (reference_version, reference_value) = reference_version_and_value
        .map(|(version, value)| (Some(version), *value))
        .unwrap_or((None, None));
    let changed = value != reference_value;

    match (reference_version, version, changed) {
        // If the message is new and unversioned then that's okay.
        (None, Versioned::Unversioned, _) => None,
        // If the message is new and versioned then the version must be one.
        (None, Versioned::Versioned(1), _) => None,
        // If the message is new and versioned but with an initial version greater than one
        // then that's an error.
        (None, Versioned::Versioned(_), _) => Some(IncorrectVersioning::BadInitialVersion),
        // If the message isn't new but hasn't changed then that's okay.
        (Some(Versioned::Unversioned), Versioned::Unversioned, false) => None,
        // If the message isn't new and has changed then that's an error.
        (Some(Versioned::Unversioned), Versioned::Unversioned, true) => {
            Some(IncorrectVersioning::NoInitialVersion)
        }
        // If the message isn't new and is now versioned with an initial version of one,
        // then it doesn't matter if it has changed or not, that's okay.
        (Some(Versioned::Unversioned), Versioned::Versioned(1), _) => None,
        // If the message isn't new and is now versioned with an initial version greater
        // than one, then it doesn't matter if it has changed or not, that's an error
        // (because versioning should start from one).
        (Some(Versioned::Unversioned), Versioned::Versioned(_), _) => {
            Some(IncorrectVersioning::BadInitialVersion)
        }
        // If the message isn't new, was previously versioned and now isn't, that's an
        // error.
        (Some(Versioned::Versioned(_)), Versioned::Unversioned, _) => {
            Some(IncorrectVersioning::VersionRemoved)
        }
        // If the message isn't new, was previously versioned and remains versioned,
        // the versions match, and the message wasn't changed, then that's okay.
        (Some(Versioned::Versioned(reference_version)), Versioned::Versioned(version), false)
            if *reference_version == version =>
        {
            None
        }
        // If the message isn't new, was previously versioned and remains versioned,
        // the reference version is different from the current version, and the message
        // has not changed, then that's an error (because the version shouldn't have
        // changed).
        (Some(Versioned::Versioned(reference_version)), Versioned::Versioned(version), false)
            if *reference_version != version =>
        {
            Some(IncorrectVersioning::UnnecessaryVersionChange)
        }
        // If the message isn't new, was previously versioned and remains versioned,
        // the reference version is lesser, and the message was changed, then that's okay.
        (Some(Versioned::Versioned(reference_version)), Versioned::Versioned(version), true)
            if *reference_version < version =>
        {
            None
        }
        // If the message isn't new, was previously versioned and remains versioned,
        // the reference version is greater or equal, and the message was changed, then
        // that's an error (because the reference version should be lower).
        (Some(Versioned::Versioned(reference_version)), Versioned::Versioned(version), true)
            if *reference_version >= version =>
        {
            Some(IncorrectVersioning::ReferenceVersionGreaterOrEqual)
        }
        // This case isn't reachable because of the if-guards above, but the exhaustiveness
        // checking doesn't know that.
        (Some(Versioned::Versioned(_)), Versioned::Versioned(_), _) => unreachable!(),
    }
}

fn collect_reference_messages<'a>(
    separator: &str,
    reference: &'a Resource<&'a str>,
) -> Result<ReferenceMessages<'a>> {
    let mut map = HashMap::new();
    for entry in &reference.body {
        match entry {
            Entry::Message(message) => {
                let (id, version) = parse_versioned_identifier(separator, &message.id)?;
                map.insert((id, None), (version, message.value.as_ref()));

                for attribute in &message.attributes {
                    let (attribute_id, version) =
                        parse_versioned_identifier(separator, &attribute.id)?;
                    map.insert((id, Some(attribute_id)), (version, Some(&attribute.value)));
                }
            }
            Entry::Term(term) => {
                let (id, version) = parse_versioned_identifier(separator, &term.id)?;
                map.insert((id, None), (version, Some(&term.value)));
            }
            Entry::Comment(_)
            | Entry::GroupComment(_)
            | Entry::ResourceComment(_)
            | Entry::Junk { .. } => continue,
        }
    }
    Ok(map)
}

fn collect_incorrect_versioning<'refr, 'inpt>(
    separator: &str,
    reference_messages: &ReferenceMessages<'refr>,
    input: Resource<&'inpt str>,
) -> Result<Vec<(MessageIdentifier<'inpt>, IncorrectVersioning)>> {
    let mut incorrect_versions = Vec::new();
    for entry in &input.body {
        match entry {
            Entry::Message(message) => {
                let (id, version) = parse_versioned_identifier(separator, &message.id)?;
                let key = (id, None);
                if let Some(error) = check_versioning(
                    version,
                    message.value.as_ref(),
                    reference_messages.get(&key),
                ) {
                    incorrect_versions.push((key, error));
                }

                for attribute in &message.attributes {
                    let (attribute_id, version) =
                        parse_versioned_identifier(separator, &attribute.id)?;
                    let key = (id, Some(attribute_id));
                    if let Some(error) = check_versioning(
                        version,
                        Some(&attribute.value),
                        reference_messages.get(&key),
                    ) {
                        incorrect_versions.push((key, error));
                    }
                }
            }
            Entry::Term(term) => {
                let (id, version) = parse_versioned_identifier(separator, &term.id)?;
                let key = (id, None);
                if let Some(error) =
                    check_versioning(version, Some(&term.value), reference_messages.get(&key))
                {
                    incorrect_versions.push((key, error));
                }
            }
            Entry::Comment(_)
            | Entry::GroupComment(_)
            | Entry::ResourceComment(_)
            | Entry::Junk { .. } => continue,
        }
    }
    Ok(incorrect_versions)
}

/// Check that the Fluent source `input_src` is appropriately versioned given an unversioned or
/// previously versioned `reference_str`.
pub fn check<'refr, 'inpt>(
    separator: &str,
    reference: &'refr str,
    input: &'inpt str,
) -> Result<Vec<(MessageIdentifier<'inpt>, IncorrectVersioning)>> {
    let reference = parse(reference).map_err(|(_, mut errs)| {
        Error::ParseReference(errs.pop().expect("parsing failure w/ no errs"))
    })?;
    let input = parse(input).map_err(|(_, mut errs)| {
        Error::ParseInput(errs.pop().expect("parsing failure w/ no errs"))
    })?;

    let reference_messages = collect_reference_messages(separator, &reference)?;
    collect_incorrect_versioning(separator, &reference_messages, input)
}

#[cfg(test)]
mod versioned_identifier_tests {
    use super::{parse_versioned_identifier, Versioned, DEFAULT_SEPARATOR};
    use fluent_syntax::ast::Identifier;

    #[test]
    fn no_separator_suffix() {
        assert_eq!(
            parse_versioned_identifier(DEFAULT_SEPARATOR, &Identifier { name: "foo" }).unwrap(),
            ("foo", Versioned::Unversioned)
        );
    }

    #[test]
    fn separator_suffix_without_version() {
        assert_eq!(
            parse_versioned_identifier(DEFAULT_SEPARATOR, &Identifier { name: "foo---" }).unwrap(),
            ("foo", Versioned::Unversioned)
        );
    }

    #[test]
    fn separator_suffix_with_version() {
        assert_eq!(
            parse_versioned_identifier(DEFAULT_SEPARATOR, &Identifier { name: "foo---123" })
                .unwrap(),
            ("foo", Versioned::Versioned(123))
        );
    }

    #[test]
    fn separator_suffix_with_invalid_version() {
        assert!(
            parse_versioned_identifier(DEFAULT_SEPARATOR, &Identifier { name: "foo---abc" })
                .is_err(),
        );
    }
}

#[cfg(test)]
mod check_tests {
    use super::{check, IncorrectVersioning, DEFAULT_SEPARATOR};

    #[test]
    fn new_unversioned() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "", "new = content",).unwrap(),
            vec![],
        );
    }

    #[test]
    fn new_initial_version_eq_one() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "", "new---1 = content",).unwrap(),
            vec![],
        );
    }

    #[test]
    fn new_initial_version_gt_one() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "", "new---2 = content",).unwrap(),
            vec![(("new", None), IncorrectVersioning::BadInitialVersion)],
        );
    }

    #[test]
    fn existing_unversioned_unchanged() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "new = content", "new = content",).unwrap(),
            vec![],
        );
    }

    #[test]
    fn existing_unversioned_changed() {
        assert_eq!(
            check(
                DEFAULT_SEPARATOR,
                "new = content",
                "new = different content",
            )
            .unwrap(),
            vec![(("new", None), IncorrectVersioning::NoInitialVersion)],
        );
    }

    #[test]
    fn existing_initial_version_eq_one_unchanged() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "new = content", "new---1 = content",).unwrap(),
            vec![],
        );
    }

    #[test]
    fn existing_initial_version_eq_one_changed() {
        assert_eq!(
            check(
                DEFAULT_SEPARATOR,
                "new = content",
                "new---1 = different content",
            )
            .unwrap(),
            vec![],
        );
    }

    #[test]
    fn existing_initial_version_gt_one_unchanged() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "new = content", "new---2 = content",).unwrap(),
            vec![(("new", None), IncorrectVersioning::BadInitialVersion)],
        );
    }

    #[test]
    fn existing_initial_version_gt_one_changed() {
        assert_eq!(
            check(
                DEFAULT_SEPARATOR,
                "new = content",
                "new---2 = different content",
            )
            .unwrap(),
            vec![(("new", None), IncorrectVersioning::BadInitialVersion)],
        );
    }

    #[test]
    fn existing_version_removed_unchanged() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "new---1 = content", "new = content",).unwrap(),
            vec![(("new", None), IncorrectVersioning::VersionRemoved)],
        );
    }

    #[test]
    fn existing_version_removed_changed() {
        assert_eq!(
            check(
                DEFAULT_SEPARATOR,
                "new---1 = content",
                "new = different content",
            )
            .unwrap(),
            vec![(("new", None), IncorrectVersioning::VersionRemoved)],
        );
    }

    #[test]
    fn existing_matching_version_unchanged() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "new---1 = content", "new---1 = content",).unwrap(),
            vec![],
        );
    }

    #[test]
    fn existing_mismatching_version_ref_unchanged() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "new---2 = content", "new---1 = content",).unwrap(),
            vec![(("new", None), IncorrectVersioning::UnnecessaryVersionChange)],
        );
    }

    #[test]
    fn existing_mismatching_version_inpt_unchanged() {
        assert_eq!(
            check(DEFAULT_SEPARATOR, "new---1 = content", "new---2 = content",).unwrap(),
            vec![(("new", None), IncorrectVersioning::UnnecessaryVersionChange)],
        );
    }

    #[test]
    fn existing_increased_version_changed() {
        assert_eq!(
            check(
                DEFAULT_SEPARATOR,
                "new---1 = content",
                "new---2 = different content",
            )
            .unwrap(),
            vec![],
        );
    }

    #[test]
    fn existing_lower_version_changed() {
        assert_eq!(
            check(
                DEFAULT_SEPARATOR,
                "new---2 = content",
                "new---1 = different content",
            )
            .unwrap(),
            vec![(
                ("new", None),
                IncorrectVersioning::ReferenceVersionGreaterOrEqual
            )],
        );
    }

    #[test]
    fn existing_matching_version_changed() {
        assert_eq!(
            check(
                DEFAULT_SEPARATOR,
                "new---2 = content",
                "new---2 = different content",
            )
            .unwrap(),
            vec![(
                ("new", None),
                IncorrectVersioning::ReferenceVersionGreaterOrEqual
            )],
        );
    }
}
