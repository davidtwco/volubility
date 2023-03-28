use fluent_syntax::ast::{
    Attribute, CallArguments, Comment, Entry, Expression, Identifier, InlineExpression, Message,
    NamedArgument, Pattern, PatternElement, Resource, Term, Variant, VariantKey,
};
use std::ops::Deref;

pub(crate) trait DerefNode {
    type Target<'a>
    where
        Self: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a>;
}

impl<S: Deref> DerefNode for Resource<S> {
    type Target<'a> = Resource<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        Resource {
            body: self.body.iter().map(|entry| entry.deref_node()).collect(),
        }
    }
}

impl<S: Deref> DerefNode for Entry<S> {
    type Target<'a> = Entry<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        match self {
            Entry::Message(message) => Entry::Message(message.deref_node()),
            Entry::Term(term) => Entry::Term(term.deref_node()),
            Entry::Comment(comment) => Entry::Comment(comment.deref_node()),
            Entry::GroupComment(comment) => Entry::GroupComment(comment.deref_node()),
            Entry::ResourceComment(comment) => Entry::ResourceComment(comment.deref_node()),
            Entry::Junk { content } => Entry::Junk {
                content: content.deref(),
            },
        }
    }
}

impl<S: Deref> DerefNode for Comment<S> {
    type Target<'a> = Comment<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        Comment {
            content: self.content.iter().map(|line| line.deref()).collect(),
        }
    }
}

impl<S: Deref> DerefNode for Identifier<S> {
    type Target<'a> = Identifier<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        Identifier {
            name: self.name.deref(),
        }
    }
}

impl<S: Deref> DerefNode for Message<S> {
    type Target<'a> = Message<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        Message {
            id: self.id.deref_node(),
            value: self.value.as_ref().map(|val| val.deref_node()),
            attributes: self
                .attributes
                .iter()
                .map(|attr| attr.deref_node())
                .collect(),
            comment: self.comment.as_ref().map(|comment| comment.deref_node()),
        }
    }
}

impl<S: Deref> DerefNode for Term<S> {
    type Target<'a> = Term<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        Term {
            id: self.id.deref_node(),
            value: self.value.deref_node(),
            attributes: self
                .attributes
                .iter()
                .map(|attr| attr.deref_node())
                .collect(),
            comment: self.comment.as_ref().map(|comment| comment.deref_node()),
        }
    }
}

impl<S: Deref> DerefNode for Attribute<S> {
    type Target<'a> = Attribute<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        Attribute {
            id: self.id.deref_node(),
            value: self.value.deref_node(),
        }
    }
}

impl<S: Deref> DerefNode for Pattern<S> {
    type Target<'a> = Pattern<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        Pattern {
            elements: self.elements.iter().map(|el| el.deref_node()).collect(),
        }
    }
}

impl<S: Deref> DerefNode for PatternElement<S> {
    type Target<'a> = PatternElement<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        match self {
            PatternElement::TextElement { value } => PatternElement::TextElement {
                value: value.deref(),
            },
            PatternElement::Placeable { expression } => PatternElement::Placeable {
                expression: expression.deref_node(),
            },
        }
    }
}

impl<S: Deref> DerefNode for Expression<S> {
    type Target<'a> = Expression<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        match self {
            Expression::Select { selector, variants } => Expression::Select {
                selector: selector.deref_node(),
                variants: variants
                    .iter()
                    .map(|variant| variant.deref_node())
                    .collect(),
            },
            Expression::Inline(expression) => Expression::Inline(expression.deref_node()),
        }
    }
}

impl<S: Deref> DerefNode for Variant<S> {
    type Target<'a> = Variant<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        Variant {
            key: self.key.deref_node(),
            value: self.value.deref_node(),
            default: self.default,
        }
    }
}

impl<S: Deref> DerefNode for VariantKey<S> {
    type Target<'a> = VariantKey<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        match self {
            VariantKey::Identifier { name } => VariantKey::Identifier { name: name.deref() },
            VariantKey::NumberLiteral { value } => VariantKey::NumberLiteral {
                value: value.deref(),
            },
        }
    }
}

impl<S: Deref> DerefNode for InlineExpression<S> {
    type Target<'a> = InlineExpression<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        match self {
            InlineExpression::StringLiteral { value } => InlineExpression::StringLiteral {
                value: value.deref(),
            },
            InlineExpression::NumberLiteral { value } => InlineExpression::NumberLiteral {
                value: value.deref(),
            },
            InlineExpression::FunctionReference { id, arguments } => {
                InlineExpression::FunctionReference {
                    id: id.deref_node(),
                    arguments: arguments.deref_node(),
                }
            }
            InlineExpression::MessageReference { id, attribute } => {
                InlineExpression::MessageReference {
                    id: id.deref_node(),
                    attribute: attribute.as_ref().map(|attr| attr.deref_node()),
                }
            }
            InlineExpression::TermReference {
                id,
                attribute,
                arguments,
            } => InlineExpression::TermReference {
                id: id.deref_node(),
                attribute: attribute.as_ref().map(|attr| attr.deref_node()),
                arguments: arguments.as_ref().map(|args| args.deref_node()),
            },
            InlineExpression::VariableReference { id } => InlineExpression::VariableReference {
                id: id.deref_node(),
            },
            InlineExpression::Placeable { expression } => InlineExpression::Placeable {
                expression: Box::new(expression.deref_node()),
            },
        }
    }
}

impl<S: Deref> DerefNode for CallArguments<S> {
    type Target<'a> = CallArguments<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        CallArguments {
            positional: self.positional.iter().map(|arg| arg.deref_node()).collect(),
            named: self.named.iter().map(|arg| arg.deref_node()).collect(),
        }
    }
}

impl<S: Deref> DerefNode for NamedArgument<S> {
    type Target<'a> = NamedArgument<&'a <S as Deref>::Target> where <S as Deref>::Target: 'a, S: 'a;

    fn deref_node<'a>(&'a self) -> Self::Target<'a> {
        NamedArgument {
            name: self.name.deref_node(),
            value: self.value.deref_node(),
        }
    }
}
