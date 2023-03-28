use fluent_syntax::ast::{
    Attribute, CallArguments, Comment, Entry, Expression, Identifier, InlineExpression, Message,
    NamedArgument, Pattern, PatternElement, Resource, Term, Variant, VariantKey,
};

/// Local version of `ToOwned` so it can be implemented on `fluent_syntax`'s types.
pub(crate) trait ToOwnedNode {
    /// Works around lack of a generic `Self`, ideally there would be a `S: std::borrow::ToOwned`
    /// generic and `to_owned` would return `Self<<S as std::borrow::ToOwned>::Owned>`.
    type Owned;

    fn to_owned_node(&self) -> Self::Owned;
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Resource<&S> {
    type Owned = Resource<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        Resource { body: self.body.iter().map(|entry| entry.to_owned_node()).collect() }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Entry<&S> {
    type Owned = Entry<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        match self {
            Entry::Message(message) => Entry::Message(message.to_owned_node()),
            Entry::Term(term) => Entry::Term(term.to_owned_node()),
            Entry::Comment(comment) => Entry::Comment(comment.to_owned_node()),
            Entry::GroupComment(comment) => Entry::GroupComment(comment.to_owned_node()),
            Entry::ResourceComment(comment) => Entry::ResourceComment(comment.to_owned_node()),
            Entry::Junk { content } => Entry::Junk { content: ToOwned::to_owned(*content) },
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Comment<&S> {
    type Owned = Comment<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        Comment { content: self.content.iter().map(|line| ToOwned::to_owned(*line)).collect() }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Identifier<&S> {
    type Owned = Identifier<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        Identifier { name: self.name.to_owned() }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Message<&S> {
    type Owned = Message<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        Message {
            id: self.id.to_owned_node(),
            value: self.value.as_ref().map(|val| val.to_owned_node()),
            attributes: self.attributes.iter().map(|attr| attr.to_owned_node()).collect(),
            comment: self.comment.as_ref().map(|comment| comment.to_owned_node()),
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Term<&S> {
    type Owned = Term<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        Term {
            id: self.id.to_owned_node(),
            value: self.value.to_owned_node(),
            attributes: self.attributes.iter().map(|attr| attr.to_owned_node()).collect(),
            comment: self.comment.as_ref().map(|comment| comment.to_owned_node()),
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Attribute<&S> {
    type Owned = Attribute<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        Attribute { id: self.id.to_owned_node(), value: self.value.to_owned_node() }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Pattern<&S> {
    type Owned = Pattern<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        Pattern { elements: self.elements.iter().map(|el| el.to_owned_node()).collect() }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for PatternElement<&S> {
    type Owned = PatternElement<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        match self {
            PatternElement::TextElement { value } => {
                PatternElement::TextElement { value: ToOwned::to_owned(*value) }
            }
            PatternElement::Placeable { expression } => {
                PatternElement::Placeable { expression: expression.to_owned_node() }
            }
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Expression<&S> {
    type Owned = Expression<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        match self {
            Expression::Select { selector, variants } => Expression::Select {
                selector: selector.to_owned_node(),
                variants: variants.iter().map(|variant| variant.to_owned_node()).collect(),
            },
            Expression::Inline(expression) => Expression::Inline(expression.to_owned_node()),
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for Variant<&S> {
    type Owned = Variant<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        Variant {
            key: self.key.to_owned_node(),
            value: self.value.to_owned_node(),
            default: self.default,
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for VariantKey<&S> {
    type Owned = VariantKey<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        match self {
            VariantKey::Identifier { name } => {
                VariantKey::Identifier { name: ToOwned::to_owned(*name) }
            }
            VariantKey::NumberLiteral { value } => {
                VariantKey::NumberLiteral { value: ToOwned::to_owned(*value) }
            }
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for InlineExpression<&S> {
    type Owned = InlineExpression<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        match self {
            InlineExpression::StringLiteral { value } => {
                InlineExpression::StringLiteral { value: ToOwned::to_owned(*value) }
            }
            InlineExpression::NumberLiteral { value } => {
                InlineExpression::NumberLiteral { value: ToOwned::to_owned(*value) }
            }
            InlineExpression::FunctionReference { id, arguments } => {
                InlineExpression::FunctionReference {
                    id: id.to_owned_node(),
                    arguments: arguments.to_owned_node(),
                }
            }
            InlineExpression::MessageReference { id, attribute } => {
                InlineExpression::MessageReference {
                    id: id.to_owned_node(),
                    attribute: attribute.as_ref().map(|attr| attr.to_owned_node()),
                }
            }
            InlineExpression::TermReference { id, attribute, arguments } => {
                InlineExpression::TermReference {
                    id: id.to_owned_node(),
                    attribute: attribute.as_ref().map(|attr| attr.to_owned_node()),
                    arguments: arguments.as_ref().map(|args| args.to_owned_node()),
                }
            }
            InlineExpression::VariableReference { id } => {
                InlineExpression::VariableReference { id: id.to_owned_node() }
            }
            InlineExpression::Placeable { expression } => {
                InlineExpression::Placeable { expression: Box::new((*expression).to_owned_node()) }
            }
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for CallArguments<&S> {
    type Owned = CallArguments<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        CallArguments {
            positional: self.positional.iter().map(|arg| arg.to_owned_node()).collect(),
            named: self.named.iter().map(|arg| arg.to_owned_node()).collect(),
        }
    }
}

impl<S: ?Sized + ToOwned> ToOwnedNode for NamedArgument<&S> {
    type Owned = NamedArgument<<S as ToOwned>::Owned>;

    fn to_owned_node(&self) -> Self::Owned {
        NamedArgument { name: self.name.to_owned_node(), value: self.value.to_owned_node() }
    }
}
