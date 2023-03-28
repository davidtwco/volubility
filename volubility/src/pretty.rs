use fluent_syntax::ast::{
    Attribute, CallArguments, Comment, Entry, Expression, Identifier, InlineExpression, Message,
    NamedArgument, Pattern, PatternElement, Resource, Term, Variant, VariantKey,
};
use std::fmt;

static INDENTATION_INCREMENT: usize = 4;

fn print_indentation_at(depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", " ".repeat(depth))
}

/// Wrapper around `Comment` that captures the number of `#` symbols required for the comment,
/// which is normally only encoded in the `Entry` type.
struct HeaderComment<'a, S, const HEADERNESS: usize>(&'a Comment<S>);

/// `Display` but local to this crate so that it can be implemented for `fluent_syntax` types.
pub trait PrettyPrint {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

struct ForwardToPrettyPrint<T: PrettyPrint>(T);

impl<T: PrettyPrint> fmt::Display for ForwardToPrettyPrint<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.print(0, f)
    }
}

/// Pretty-print a Fluent AST node back into surface Fluent syntax.
pub fn to_string<T: PrettyPrint>(node: T) -> String {
    ForwardToPrettyPrint(node).to_string()
}

impl<S: fmt::Display> PrettyPrint for Resource<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let count = self.body.len();
        for (i, entry) in self.body.iter().enumerate() {
            entry.print(indentation_depth, f)?;
            if (i + 1) != count {
                writeln!(f, "")?;
                print_indentation_at(indentation_depth, f)?;
            }
        }
        Ok(())
    }
}

impl<S: fmt::Display> PrettyPrint for Entry<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Entry::Message(message) => message.print(indentation_depth, f),
            Entry::Term(term) => term.print(indentation_depth, f),
            Entry::Comment(comment) => comment.print(indentation_depth, f),
            Entry::GroupComment(comment) => {
                HeaderComment::<_, 2>(comment).print(indentation_depth, f)
            }
            Entry::ResourceComment(comment) => {
                HeaderComment::<_, 3>(comment).print(indentation_depth, f)
            }
            Entry::Junk { .. } => Ok(()),
        }
    }
}

impl<S: fmt::Display> PrettyPrint for Message<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(comment) = &self.comment {
            comment.print(indentation_depth, f)?;
        }
        self.id.print(indentation_depth, f)?;
        write!(f, " = ")?;
        if let Some(value) = &self.value {
            value.print(indentation_depth + INDENTATION_INCREMENT, f)?;
        }
        for attribute in &self.attributes {
            writeln!(f, "")?;
            let indentation_depth = indentation_depth + INDENTATION_INCREMENT;
            print_indentation_at(indentation_depth, f)?;
            attribute.print(indentation_depth, f)?;
        }
        Ok(())
    }
}

impl<S: fmt::Display> PrettyPrint for Term<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(comment) = &self.comment {
            comment.print(indentation_depth, f)?;
            writeln!(f, "")?;
        }
        write!(f, "-")?;
        self.id.print(indentation_depth, f)?;
        writeln!(f, " = ")?;
        self.value.print(indentation_depth + INDENTATION_INCREMENT, f)?;
        for attribute in &self.attributes {
            writeln!(f, "")?;
            let indentation_depth = indentation_depth + INDENTATION_INCREMENT;
            print_indentation_at(indentation_depth, f)?;
            attribute.print(indentation_depth, f)?;
        }
        Ok(())
    }
}

impl<'a, S: fmt::Display, const N: usize> PrettyPrint for HeaderComment<'a, S, N> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for line in &self.0.content {
            writeln!(f, "{} {line}", "#".repeat(N))?;
            print_indentation_at(indentation_depth, f)?;
        }
        Ok(())
    }
}

impl<S: fmt::Display> PrettyPrint for Comment<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        HeaderComment::<_, 1>(self).print(indentation_depth, f)
    }
}

impl<S: fmt::Display> PrettyPrint for Pattern<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // `windows(2)` is used below, so exit early if there's one element.
        if self.elements.len() == 1 {
            self.elements.first().expect("length check was wrong").print(indentation_depth, f)?;
            return Ok(());
        }

        for window in self.elements.windows(2) {
            let (current, next) = match &window {
                &[current, next] => (current, next),
                _ => unreachable!(),
            };

            current.print(indentation_depth, f)?;
            let next_continues_multiline = matches!(
                (current, next),
                (PatternElement::TextElement { .. }, PatternElement::TextElement { .. })
            );
            if next_continues_multiline {
                print_indentation_at(indentation_depth, f)?;
            }
        }

        // For `[a, b, c]`, `windows` will see `[a, b]` then `[b, c]`. Only the first element in
        // each window is printed, so `c` needs to be printed afterwards.
        self.elements.last().expect("must be at least two elements").print(indentation_depth, f)?;

        Ok(())
    }
}

impl<S: fmt::Display> PrettyPrint for PatternElement<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatternElement::TextElement { value } => write!(f, "{value}"),
            PatternElement::Placeable { expression } => {
                write!(f, "{{")?;
                expression.print(indentation_depth, f)?;
                write!(f, "}}")
            }
        }
    }
}

impl<S: fmt::Display> PrettyPrint for Expression<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Select { selector, variants } => {
                selector.print(indentation_depth, f)?;
                writeln!(f, " ->")?;

                {
                    let indentation_depth = indentation_depth + INDENTATION_INCREMENT;
                    print_indentation_at(indentation_depth, f)?;

                    let count = variants.len();
                    for (i, variant) in variants.iter().enumerate() {
                        variant.print(indentation_depth, f)?;
                        if (i + 1) != count {
                            writeln!(f, "")?;
                            print_indentation_at(indentation_depth, f)?;
                        }
                    }
                }

                // Print the final newline outside of the loop with a lesser indentation.
                writeln!(f, "")?;
                print_indentation_at(indentation_depth, f)
            }
            Expression::Inline(expr) => expr.print(indentation_depth, f),
        }
    }
}

impl<S: fmt::Display> PrettyPrint for Variant<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.default {
            write!(f, "*")?;
        }
        write!(f, "[")?;
        self.key.print(indentation_depth, f)?;
        write!(f, "] ")?;
        self.value.print(indentation_depth, f)
    }
}

impl<S: fmt::Display> PrettyPrint for VariantKey<S> {
    fn print(&self, _: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VariantKey::Identifier { name } => write!(f, "{name}"),
            VariantKey::NumberLiteral { value } => write!(f, "{value}"),
        }
    }
}

impl<S: fmt::Display> PrettyPrint for InlineExpression<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InlineExpression::StringLiteral { value } => write!(f, "\"{value}\""),
            InlineExpression::NumberLiteral { value } => write!(f, "{value}"),
            InlineExpression::FunctionReference { id, arguments } => {
                id.print(indentation_depth, f)?;
                arguments.print(indentation_depth, f)
            }
            InlineExpression::MessageReference { id, attribute } => {
                id.print(indentation_depth, f)?;
                if let Some(attribute) = attribute {
                    write!(f, ".")?;
                    attribute.print(indentation_depth, f)?;
                }
                Ok(())
            }
            InlineExpression::TermReference { id, attribute, arguments } => {
                write!(f, "-")?;
                id.print(indentation_depth, f)?;
                if let Some(attribute) = attribute {
                    write!(f, ".")?;
                    attribute.print(indentation_depth, f)?;
                }
                if let Some(arguments) = arguments {
                    arguments.print(indentation_depth, f)?;
                }
                Ok(())
            }
            InlineExpression::VariableReference { id } => {
                write!(f, "$")?;
                id.print(indentation_depth, f)
            }
            InlineExpression::Placeable { expression } => {
                write!(f, "{{ ")?;
                expression.print(indentation_depth, f)?;
                write!(f, " }}")
            }
        }
    }
}

impl<S: fmt::Display> PrettyPrint for CallArguments<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        let total_args = self.positional.len() + self.named.len();
        for (i, positional_arg) in self.positional.iter().enumerate() {
            positional_arg.print(indentation_depth, f)?;
            if (i + 1) != total_args {
                write!(f, ", ")?;
            }
        }
        for (i, named_arg) in self.named.iter().enumerate() {
            named_arg.print(indentation_depth, f)?;
            if (i + 1) != total_args {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")
    }
}

impl<S: fmt::Display> PrettyPrint for NamedArgument<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.name.print(indentation_depth, f)?;
        write!(f, ": ")?;
        self.value.print(indentation_depth, f)
    }
}

impl<S: fmt::Display> PrettyPrint for Attribute<S> {
    fn print(&self, indentation_depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".")?;
        self.id.print(indentation_depth, f)?;
        write!(f, " = ")?;
        self.value.print(indentation_depth, f)
    }
}

impl<S: fmt::Display> PrettyPrint for Identifier<S> {
    fn print(&self, _: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[cfg(test)]
mod pretty_print_tests {
    use super::ForwardToPrettyPrint;
    use fluent_syntax::parser::parse;

    /// Helper macro for constructing tests which assert that the pretty printed input parses to
    /// the same AST as the original input.
    macro_rules! round_trip_test {
        ($name:ident, $code:literal) => {
            #[test]

            fn $name() {
                let trimmed_code = $code.trim();

                let parsed = parse(trimmed_code).unwrap();
                let pretty = ForwardToPrettyPrint(parsed.clone()).to_string();
                let pretty_parsed = parse(pretty.as_ref()).unwrap();
                assert_eq!(parsed, pretty_parsed);
            }
        };
    }

    round_trip_test!(
        attributes,
        r#"
attribute =
    .attr = attribute
multiline-attribute =
    .attr =
        multiline
        attribute
two-attributes =
    .attr-a = attribute A
    .attr-b = attribute B
value-and-attributes = value
    .attr-a = attribute A
    .attr-b = attribute B
multiline-value-and_attributes =
    value A
    value B
    .attr-a = attribute A
    .attr-b = attribute B
        "#
    );

    round_trip_test!(
        call_expressions,
        r#"
call-expression = { FOO() }
call-expression-with-string-expression = { FOO("bar") }
call-expression-with-number-expression = { FOO(1) }
call-expression-with-message-reference = { FOO(bar) }
call-expression-with-external-argument = { FOO($bar) }
call-expression-with-number-named-argument = { FOO(bar: 1) }
call-expression-with-string-named-argument = { FOO(bar: "bar") }
call-expression-with-two-positional-arguments = { FOO(bar, baz) }
call-expression-with-positional-and-named-arguments = { FOO(bar, 1, baz: "baz") }
macro-call = { -term() }
nested-placeables = {{ FOO() }}
        "#
    );

    round_trip_test!(
        comments,
        r#"
# standalone comment


### multiline
### resource comment


## multiline
## group comment

# multiline
# message comment
foo = bar
        "#
    );

    round_trip_test!(
        escapes,
        r#"
backslash_in_text_element = \{ placeable }
excaped_special_char_in_string_literal = { "Escaped \" quote" }
unicode_escape_sequence = { "\u0065" }
        "#
    );

    round_trip_test!(
        inline_expressions,
        r#"
simple-reference = simple { reference }
term-reference = term { -reference }
external-reference = external { $reference }
number-element = number { 1 }
string-element = string { "element" }
attribute-expression = attribute { ex.pression }
        "#
    );

    round_trip_test!(
        messages,
        r#"
simple = simple
multiline =
    multi
    line
        "#
    );

    round_trip_test!(
        select_expression,
        r#"
select-expression =
    { $sel ->
       *[a] A
        [b] B
    }
multiline-variant =
    { $sel ->
       *[a]
            AAA
            BBBB
    }
variant-key-number =
    { $sel ->
       *[a] A
        [b] B
    }
select-expression-in-block-value =
    Foo { $sel ->
       *[a] A
        [b] B
    }
select-expression-in-multiline-value =
    Foo
    Bar { $sel ->
       *[a] A
        [b] B
    }
nested-select-expression =
    { $a ->
       *[a]
            { $b ->
               *[b] Foo
            }
    }
selector-external-argument =
    { $bar ->
       *[a] A
    }
selector-number-expression =
    { 1 ->
       *[a] A
    }
selector-string-expression =
    { "bar" ->
       *[a] A
    }
selector-attribute-expression =
    { -bar.baz ->
       *[a] A
    }
        "#
    );
}
