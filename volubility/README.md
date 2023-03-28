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
To use `volubility` in your own project, add it to your `Cargo.toml`:

```toml
volubility = "0.1.0"
```

See the [`volubility-bin`](../volubility-bin/README.md) crate for an example of using `volubility`'s
library interface.

## Stability
`volubility`'s library interface is intended for use by `rustc` for its diagnostic translation 
support, it currently comes with no stability guarantees and may change at any time.

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
<