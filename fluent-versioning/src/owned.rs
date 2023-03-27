use fluent_syntax::ast::{
    Attribute, CallArguments, Comment, Entry, Expression, Identifier, InlineExpression, Message,
    NamedArgument, Pattern, PatternElement, Resource, Term, Variant, VariantKey,
};
use std::borrow::ToOwned as StdToOwned;

/// Local version of `ToOwned` so it can be implemented on `fluent_syntax`'s types.
pub(crate) trait ToOwned {
    /// Works around lack of a generic `Self`, ideally there would be a `S: std::borrow::ToOwned`
    /// generic and `to_owned` would return `Self<<S as std::borrow::ToOwned>::Owned>`.
    type Owned;

    fn to_owned(&self) -> Self::Owned;
}

/// Convert a Fluent AST node parameterized by a borrowed type to the equivalent node parameterized
// by a owned type.
pub(crate) fn to_owned<N: ToOwned>(node: N) -> <N as ToOwned>::Owned {
    node.to_owned()
}

impl<S: StdToOwned> ToOwned for Resource<S> {
    type Owned = Resource<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        Resource {
            body: self.body.iter().map(|entry| entry.to_owned()).collect(),
        }
    }
}

impl<S: StdToOwned> ToOwned for Entry<S> {
    type Owned = Entry<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        match self {
            Entry::Message(message) => Entry::Message(message.to_owned()),
            Entry::Term(term) => Entry::Term(term.to_owned()),
            Entry::Comment(comment) => Entry::Comment(comment.to_owned()),
            Entry::GroupComment(comment) => Entry::GroupComment(comment.to_owned()),
            Entry::ResourceComment(comment) => Entry::ResourceComment(comment.to_owned()),
            Entry::Junk { content } => Entry::Junk {
                content: content.to_owned(),
            },
        }
    }
}

impl<S: StdToOwned> ToOwned for Comment<S> {
    type Owned = Comment<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        Comment {
            content: self.content.iter().map(|line| line.to_owned()).collect(),
        }
    }
}

impl<S: StdToOwned> ToOwned for Identifier<S> {
    type Owned = Identifier<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        Identifier {
            name: self.name.to_owned(),
        }
    }
}

impl<S: StdToOwned> ToOwned for Message<S> {
    type Owned = Message<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        Message {
            id: self.id.to_owned(),
            value: self.value.as_ref().map(|val| val.to_owned()),
            attributes: self.attributes.iter().map(|attr| attr.to_owned()).collect(),
            comment: self.comment.as_ref().map(|comment| comment.to_owned()),
        }
    }
}

impl<S: StdToOwned> ToOwned for Term<S> {
    type Owned = Term<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        Term {
            id: self.id.to_owned(),
            value: self.value.to_owned(),
            attributes: self.attributes.iter().map(|attr| attr.to_owned()).collect(),
            comment: self.comment.as_ref().map(|comment| comment.to_owned()),
        }
    }
}

impl<S: StdToOwned> ToOwned for Attribute<S> {
    type Owned = Attribute<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        Attribute {
            id: self.id.to_owned(),
            value: self.value.to_owned(),
        }
    }
}

impl<S: StdToOwned> ToOwned for Pattern<S> {
    type Owned = Pattern<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        Pattern {
            elements: self.elements.iter().map(|el| el.to_owned()).collect(),
        }
    }
}

impl<S: StdToOwned> ToOwned for PatternElement<S> {
    type Owned = PatternElement<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        match self {
            PatternElement::TextElement { value } => PatternElement::TextElement {
                value: value.to_owned(),
            },
            PatternElement::Placeable { expression } => PatternElement::Placeable {
                expression: expression.to_owned(),
            },
        }
    }
}

impl<S: StdToOwned> ToOwned for Expression<S> {
    type Owned = Expression<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        match self {
            Expression::Select { selector, variants } => Expression::Select {
                selector: selector.to_owned(),
                variants: variants.iter().map(|variant| variant.to_owned()).collect(),
            },
            Expression::Inline(expression) => Expression::Inline(expression.to_owned()),
        }
    }
}

impl<S: StdToOwned> ToOwned for Variant<S> {
    type Owned = Variant<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        Variant {
            key: self.key.to_owned(),
            value: self.value.to_owned(),
            default: self.default,
        }
    }
}

impl<S: StdToOwned> ToOwned for VariantKey<S> {
    type Owned = VariantKey<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        match self {
            VariantKey::Identifier { name } => VariantKey::Identifier {
                name: name.to_owned(),
            },
            VariantKey::NumberLiteral { value } => VariantKey::NumberLiteral {
                value: value.to_owned(),
            },
        }
    }
}

impl<S: StdToOwned> ToOwned for InlineExpression<S> {
    type Owned = InlineExpression<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        match self {
            InlineExpression::StringLiteral { value } => InlineExpression::StringLiteral {
                value: value.to_owned(),
            },
            InlineExpression::NumberLiteral { value } => InlineExpression::NumberLiteral {
                value: value.to_owned(),
            },
            InlineExpression::FunctionReference { id, arguments } => {
                InlineExpression::FunctionReference {
                    id: id.to_owned(),
                    arguments: arguments.to_owned(),
                }
            }
            InlineExpression::MessageReference { id, attribute } => {
                InlineExpression::MessageReference {
                    id: id.to_owned(),
                    attribute: attribute.as_ref().map(|attr| attr.to_owned()),
                }
            }
            InlineExpression::TermReference {
                id,
                attribute,
                arguments,
            } => InlineExpression::TermReference {
                id: id.to_owned(),
                attribute: attribute.as_ref().map(|attr| attr.to_owned()),
                arguments: arguments.as_ref().map(|args| args.to_owned()),
            },
            InlineExpression::VariableReference { id } => {
                InlineExpression::VariableReference { id: id.to_owned() }
            }
            InlineExpression::Placeable { expression } => InlineExpression::Placeable {
                expression: Box::new((*expression).to_owned()),
            },
        }
    }
}

impl<S: StdToOwned> ToOwned for CallArguments<S> {
    type Owned = CallArguments<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        CallArguments {
            positional: self.positional.iter().map(|arg| arg.to_owned()).collect(),
            named: self.named.iter().map(|arg| arg.to_owned()).collect(),
        }
    }
}

impl<S: StdToOwned> ToOwned for NamedArgument<S> {
    type Owned = NamedArgument<<S as StdToOwned>::Owned>;

    fn to_owned(&self) -> Self::Owned {
        NamedArgument {
            name: self.name.to_owned(),
            value: self.value.to_owned(),
        }
    }
}
