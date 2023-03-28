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

See the README documents of the [`volubility` crate](volubility/README.md) and the
[`volubility-bin` crate](volubility-bin/README.md) for usage details of the library and binary 
interfaces respectively.

## Contributing to `volubility`
If you want help or mentorship, reach out to us in a GitHub issue, or ask `davidtwco` or in
`#i18n` on the [Rust Zulip instance](https://rust-lang.zulipchat.com/).

`volubility` should always build on stable `rustc`. To build `volubility`:

```shell-session
$ cargo build
```

To run the tests: 

```shell-session
$ cargo test
```

We use `rustfmt` to automatically format and style all of our code. To install and use `rustfmt`:

```shell-session
$ rustup component add rustfmt
$ cargo fmt
```

### Filing an issue
Think you've found a bug? File an issue! To help us understand and reproduce the
issue, provide us with:

* The (preferably minimal) test case
* Steps to reproduce the issue using the test case
* The expected result of following those steps
* The actual result of following those steps

Definitely file an issue if you see an unexpected panic originating from within `volubility`!
`volubility` should never panic unless it is explicitly documented to panic in the specific
circumstances provided.

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
