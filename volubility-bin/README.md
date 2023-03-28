# `volubility`
`volubility` is a utility for versioning messages in Fluent resources, for use with `rustc`'s
diagnostic translation effort.

Pontoon, the web translation platform for Fluent, does not handle updates to Fluent messages - if
some message `foo` is changed in a Fluent resource then translators will not be prompted to update 
their translation.

Instead of asking compiler developers to change the message identifier whenever a message is
changed, an version number is added as a suffix.

This utility automatically performs this versioning of Fluent messages given a new version of a
resource and an earlier version. It can also check that versioning is valid.

## Usage
`volubility` can check the versioning in a Fluent resource or rewrite it with version suffixes given
a previous version of the same resource.

```shell-session
$ cargo install volubility-bin
$ volubility --help
Fluent resource versioning for Pontoon.

Pontoon does not handle updates to Fluent messages - if some message `foo` is changed in a Fluent 
resource then translators will not be prompted to update their translation.

Instead of asking compiler developers to change the message identifier whenever a message is 
changed, an version number is added as a suffix.

This utility automatically performs this versioning of Fluent messages given a new version of a 
resource and an earlier version. It can also check that versioning is valid.

Usage: volubility [OPTIONS] --reference <REFERENCE> --input <INPUT>

Options:
  -r, --reference <REFERENCE>
          Path to the previous version of the `input` Fluent resource - used to determine which
          messages have changed and need to be re-versioned

  -i, --input <INPUT>
          Path to the input Fluent resource - compared against the reference resource and versioned

  -o, --output <OUTPUT>
          Path where versioned Fluent resource should be output

  -c, --check
          Run in "check" mode.

          In check mode, an output is not generated and instead, the input is checked that it has
          been re-versioned approprivately given the reference.

      --suffix-separator <SUFFIX_SEPARATOR>
          Separator to use for versioning suffixes.

          e.g. `---` is the suffix separator in message identifier `foo---1`.

          [default: ---]

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

## Stability
`volubility-bin`'s command-line interface is intended to be stable and have limited breaking changes.

<br>

#### Name
<sup>
<code>volubility</code> is <i>the quality of talking fluently, readily, or incessantly</i>.
</sup>

<br>

<sub>
You could also call this project <code>fluent-versioning</code>, if you'd prefer that.
</sub>

<br>

#### Author and acknowledgements
<sup>
<code>volubility</code> is authored by <a href="https://davidtw.co">David Wood</a> of 
<i>Huawei Technologies Research & Development (UK) Ltd</i>. <code>volubility</code> is 
maintained by the <a href="https://rust-lang.org/governance/teams/compiler">Rust Compiler Team</a>.
</sup>

<br>

#### License
<sup>
Licensed under either of <a href="https://www.apache.org/licenses/LICENSE-2.0">Apache License,
Version 2.0</a> or <a href="https://opensource.org/licenses/MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in
this crate by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without
any additional terms or conditions.
</sub>

<br>

#### Code of conduct
<sup>
When contributing or interacting with this project, we ask abide the
<a href="https://www.rust-lang.org/en-US/conduct.html">Rust Code of Conduct</a> and ask that you do
too.
</sup>
