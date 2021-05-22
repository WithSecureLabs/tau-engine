use std::error::Error as StdError;
use std::fmt;

/// A `Result` alias where `Err` case is `tau_engine::Error`.
pub type Result<T> = std::result::Result<T, Error>;

/// The errors that may occur when using the Tau Engine.
pub struct Error {
    inner: Box<Inner>,
}

pub(crate) type Source = Box<dyn StdError + Send + Sync>;

struct Inner {
    kind: Kind,
    source: Option<Source>,
}

impl Error {
    pub(crate) fn new(kind: Kind) -> Error {
        Error {
            inner: Box::new(Inner { kind, source: None }),
        }
    }

    pub(crate) fn with<S: Into<Source>>(mut self, source: S) -> Error {
        self.inner.source = Some(source.into());
        self
    }

    /// Returns the kind of this error.
    pub fn kind(&self) -> &Kind {
        &self.inner.kind
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut builder = fmt.debug_struct("tau_engine::Error");
        builder.field("kind", &self.inner.kind);
        if let Some(ref source) = self.inner.source {
            builder.field("source", source);
        }
        builder.finish()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let desc = match self.inner.kind {
            Kind::Parse(Parse::InvalidExpression) => {
                "an invalid expression was provided to the parser"
            }
            Kind::Parse(Parse::InvalidToken) => "an invalid token was encountered during parsing",
            Kind::Parse(Parse::LedFollowing) => {
                "an invalid expression was encountered following the LED during parsing"
            }
            Kind::Parse(Parse::LedPreceding) => {
                "an invalid expression was encountered preceding the LED during parsing"
            }
            Kind::Token(Token::InvalidCharacter) => {
                "an invalid character was encountered during tokenisation"
            }
            Kind::Token(Token::InvalidNumber) => {
                "an invalid number was encountered during tokenisation"
            }
        };
        if let Some(ref source) = self.inner.source {
            write!(f, "{}: {}", desc, source)
        } else {
            f.write_str(&desc)
        }
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.inner.source.as_ref().map(|e| &**e as _)
    }
}

/// The `Kind` of `tau_engine::Error`.
#[derive(Debug)]
pub enum Kind {
    /// Parsing Errors
    Parse(Parse),
    /// Tokenising Errors
    Token(Token),
}

/// The `Kind` of `tau_engine::Error` when tokenising.
#[derive(Debug)]
pub enum Parse {
    /// An invalid expression was provided
    InvalidExpression,
    /// An invalid token was encountered
    InvalidToken,
    /// An invalid following expression was encountered
    LedFollowing,
    /// An invalid preceding expression was encountered
    LedPreceding,
}

/// The `Kind` of `tau_engine::Error` when tokenising.
#[derive(Debug)]
pub enum Token {
    /// An invalid character was encountered
    InvalidCharacter,
    /// An invalid number was encountered
    InvalidNumber,
}

// Helpers
#[inline]
pub(crate) fn parse_invalid_expr<E: Into<Source>>(e: E) -> Error {
    Error::new(Kind::Parse(Parse::InvalidExpression)).with(e)
}

#[inline]
pub(crate) fn parse_invalid_token<E: Into<Source>>(e: E) -> Error {
    Error::new(Kind::Parse(Parse::InvalidToken)).with(e)
}

#[inline]
pub(crate) fn parse_led_following<E: Into<Source>>(e: E) -> Error {
    Error::new(Kind::Parse(Parse::LedFollowing)).with(e)
}

#[inline]
pub(crate) fn parse_led_preceding<E: Into<Source>>(e: E) -> Error {
    Error::new(Kind::Parse(Parse::LedPreceding)).with(e)
}

#[inline]
pub(crate) fn token_invalid_char<E: Into<Source>>(e: E) -> Error {
    Error::new(Kind::Token(Token::InvalidCharacter)).with(e)
}

#[inline]
pub(crate) fn token_invalid_num<E: Into<Source>>(e: E) -> Error {
    Error::new(Kind::Token(Token::InvalidNumber)).with(e)
}
