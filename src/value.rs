use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::hash::BuildHasher;

/// A dynamic data type that the solver can reason on.
#[derive(Clone)]
pub enum Value<'a> {
    /// Represents an empty type.
    Null,
    /// Represents a boolean.
    Bool(bool),
    /// Represents a float.
    Float(f64),
    /// Represents an integer.
    Int(i64),
    /// Represents an unsigned integer.
    UInt(u64),
    /// Represents a string.
    String(Cow<'a, str>),
    /// Represents an array.
    Array(&'a dyn Array),
    /// Represents an object.
    Object(&'a dyn Object),
}

impl<'a> Value<'a> {
    /// Returns true if the `Value` is an Array.
    #[inline]
    pub fn is_array(&self) -> bool {
        matches!(self, Self::Array(_))
    }

    /// Returns true if the `Value` is a Bool.
    #[inline]
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(_))
    }

    /// Returns true if the `Value` is a Float.
    #[inline]
    pub fn is_f64(&self) -> bool {
        matches!(self, Self::Float(_))
    }

    /// Returns true if the `Value` is an Int.
    #[inline]
    pub fn is_i64(&self) -> bool {
        matches!(self, Self::Int(_))
    }

    /// Returns true if the `Value` is a Null.
    #[inline]
    pub fn is_null(&self) -> bool {
        matches!(self, Self::Null)
    }

    /// Returns true if the `Value` is an Object.
    #[inline]
    pub fn is_object(&self) -> bool {
        matches!(self, Self::Object(_))
    }

    /// Returns true if the `Value` is a String.
    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }

    /// Returns true if the `Value` is a UInt.
    #[inline]
    pub fn is_u64(&self) -> bool {
        matches!(self, Self::UInt(_))
    }

    /// Return the associated array if the `Value` is an Array.
    #[inline]
    pub fn as_array(&self) -> Option<&dyn Array> {
        match self {
            Self::Array(a) => Some(*a),
            _ => None,
        }
    }

    /// Return the associated boolean if the `Value` is a Bool.
    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Return the associated f64 if the `Value` is a Float.
    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Self::Float(n) => Some(*n),
            _ => None,
        }
    }

    /// Return the associated i64 if the `Value` is a Int.
    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self::Int(n) => Some(*n),
            _ => None,
        }
    }

    /// Return the associated () if the `Value` is a Null.
    #[inline]
    pub fn as_null(&self) -> Option<()> {
        match self {
            Self::Null => Some(()),
            _ => None,
        }
    }

    /// Return the associated object if the `Value` is an Object.
    #[inline]
    pub fn as_object(&self) -> Option<&dyn Object> {
        match self {
            Self::Object(o) => Some(*o),
            _ => None,
        }
    }

    /// Return the associated str if the `Value` is a String.
    #[inline]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(s),
            _ => None,
        }
    }

    /// Return the associated u64 if the `Value` is a UInt.
    #[inline]
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Self::UInt(n) => Some(*n),
            _ => None,
        }
    }

    /// Returns the `Value` as an i64 if possible.
    ///
    /// Currently supports: Int & UInt.
    #[inline]
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            Self::Int(n) => Some(*n),
            Self::UInt(n) => {
                if *n <= i64::MAX as u64 {
                    Some(*n as i64)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Returns the `Value` as a String if possible.
    ///
    /// Currently supports: Bool, Float, Int, String & UInt.
    #[inline]
    pub fn to_string(&self) -> Option<String> {
        match self {
            Self::Bool(b) => Some(b.to_string()),
            Self::Int(i) => Some(i.to_string()),
            Self::UInt(u) => Some(u.to_string()),
            Self::Float(f) => Some(f.to_string()),
            Self::String(s) => Some(s.to_string()),
            _ => None,
        }
    }
}

/// A **data type** that can be represented as a `Value`.
///
/// # Implementations
///
/// As long as the **data type** can be coerced into one of the values provided by `Value` then
/// `AsValue` can be implemented on that type. Below is a contrived example:
///
/// ```
/// use std::borrow::Cow;
///
/// use tau_engine::{AsValue, Value};
///
/// enum Foo {
///     Bar,
///     Baz
/// }
///
/// impl AsValue for Foo {
///     fn as_value(&self) -> Value<'_> {
///         match self {
///             Self::Bar => Value::String(Cow::Borrowed("bar")),
///             Self::Baz => Value::String(Cow::Borrowed("baz")),
///         }
///     }
/// }
/// ```
#[cfg(not(feature = "sync"))]
pub trait AsValue {
    /// Returns the implemented type as a `Value`
    ///
    /// # Example
    ///
    /// ```
    /// use tau_engine::AsValue;
    ///
    /// let value = "foobar".as_value();
    /// ```
    fn as_value(&self) -> Value<'_>;
}
#[cfg(feature = "sync")]
pub trait AsValue: Send + Sync {
    fn as_value(&self) -> Value<'_>;
}

impl AsValue for () {
    #[inline]
    fn as_value(&self) -> Value<'_> {
        Value::Null
    }
}

impl AsValue for bool {
    #[inline]
    fn as_value(&self) -> Value<'_> {
        Value::Bool(*self)
    }
}

impl AsValue for str {
    #[inline]
    fn as_value(&self) -> Value<'_> {
        Value::String(Cow::Borrowed(self))
    }
}

impl AsValue for String {
    #[inline]
    fn as_value(&self) -> Value<'_> {
        Value::String(Cow::Borrowed(self))
    }
}

impl<V> AsValue for HashSet<V>
where
    V: AsValue,
{
    #[inline]
    fn as_value(&self) -> Value<'_> {
        Value::Array(self)
    }
}

impl<V> AsValue for Option<V>
where
    V: AsValue,
{
    #[inline]
    fn as_value(&self) -> Value<'_> {
        self.as_ref().map(|v| v.as_value()).unwrap_or(Value::Null)
    }
}

impl<V> AsValue for Vec<V>
where
    V: AsValue,
{
    #[inline]
    fn as_value(&self) -> Value<'_> {
        Value::Array(self)
    }
}

macro_rules! impl_as_value_float {
    ($ty:ty) => {
        impl AsValue for $ty {
            #[inline]
            fn as_value(&self) -> Value<'_> {
                Value::Float(*self as f64)
            }
        }
    };
}

impl_as_value_float!(f32);
impl_as_value_float!(f64);

macro_rules! impl_as_value_int {
    ($ty:ty) => {
        impl AsValue for $ty {
            #[inline]
            fn as_value(&self) -> Value<'_> {
                Value::Int(*self as i64)
            }
        }
    };
}

impl_as_value_int!(i8);
impl_as_value_int!(i16);
impl_as_value_int!(i32);
impl_as_value_int!(i64);
impl_as_value_int!(isize);

macro_rules! impl_as_value_uint {
    ($ty:ty) => {
        impl AsValue for $ty {
            #[inline]
            fn as_value(&self) -> Value<'_> {
                Value::UInt(*self as u64)
            }
        }
    };
}

impl_as_value_uint!(u8);
impl_as_value_uint!(u16);
impl_as_value_uint!(u32);
impl_as_value_uint!(u64);
impl_as_value_uint!(usize);

/// A **data type** that can be represented as an `Array`.
///
/// This allows more complex array-like data types to be represented in a generic way for use as a
/// `Value`.
///
/// # Implementations
///
/// As long as the **data type** is considered array-like then `Array` can be implemented on that
/// type. Below is a contrived example:
///
/// ```
/// use tau_engine::{Array, Value};
///
/// // NOTE: Implements Iterator
/// #[derive(Clone)]
/// struct Counter {
///    count: usize,
/// }
/// # impl Iterator for Counter {
/// #    // we will be counting with usize
/// #    type Item = usize;
///
/// #    // next() is the only required method
/// #    fn next(&mut self) -> Option<Self::Item> {
/// #        // Increment our count. This is why we started at zero.
/// #        self.count += 1;
///
/// #        // Check to see if we've finished counting or not.
/// #        if self.count < 6 {
/// #            Some(self.count)
/// #        } else {
/// #            None
/// #        }
/// #    }
/// # }
/// impl Array for Counter {
///     fn iter(&self) -> Box<dyn Iterator<Item = Value<'_>> + '_> {
///         Box::new(self.clone().map(|v| Value::UInt(v as u64)))
///     }
///
///     fn len(&self) -> usize {
///         self.clone().count()
///     }
/// }
/// ```
#[allow(clippy::len_without_is_empty)]
#[cfg(not(feature = "sync"))]
pub trait Array {
    /// Returns a boxed iterator of `Value` items.
    ///
    /// # Example
    ///
    /// ```
    /// use std::collections::HashSet;
    /// use tau_engine::{Array, Value};
    ///
    /// let mut set = HashSet::new();
    /// set.insert(1);
    ///
    /// let mut value = Array::iter(&set);
    ///
    /// assert_eq!(value.next().is_some(), true);
    /// ```
    fn iter(&self) -> Box<dyn Iterator<Item = Value<'_>> + '_>;

    /// Returns the length of the array.
    ///
    /// # Example
    ///
    ///```
    /// use std::collections::HashSet;
    /// use tau_engine::{Array, Value};
    ///
    /// let mut set = HashSet::new();
    /// set.insert(1);
    ///
    /// let len = Array::len(&set);
    ///
    /// assert_eq!(len, 1);
    /// ```
    fn len(&self) -> usize;
}
#[cfg(feature = "sync")]
pub trait Array: Send + Sync {
    fn iter(&self) -> Box<dyn Iterator<Item = Value<'_>> + '_>;
    fn len(&self) -> usize;
}

impl<V> Array for HashSet<V>
where
    V: AsValue,
{
    #[inline]
    fn iter(&self) -> Box<dyn Iterator<Item = Value<'_>> + '_> {
        Box::new(self.iter().map(|v| v.as_value()))
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }
}

impl<V> Array for Vec<V>
where
    V: AsValue,
{
    #[inline]
    fn iter(&self) -> Box<dyn Iterator<Item = Value<'_>> + '_> {
        Box::new(self.as_slice().iter().map(|v| v.as_value()))
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }
}

/// A **data type** that can be represented as an `Object`.
///
/// This allows more complex object-like data types to be represented in a generic way for use as a
/// `Value`.
///
/// # Implementations
///
/// As long as the **data type** is considered object-like then `Object` can be implemented on that
/// type. Below is a contrived example:
///j
/// ```
/// use std::borrow::Cow;
///
/// use tau_engine::{Object, Value};
///
/// struct Foo {
///     pub bar: String,
///     pub baz: String,
/// }
///
/// impl Object for Foo {
///     fn get(&self, key: &str) -> Option<Value<'_>> {
///         match key {
///             "bar" => Some(Value::String(Cow::Borrowed(&self.bar))),
///             "baz" => Some(Value::String(Cow::Borrowed(&self.baz))),
///             _ => None,
///         }
///     }
///
///     fn keys(&self) -> Vec<Cow<'_, str>> {
///         ["bar", "baz"].iter().map(|s| Cow::Borrowed(*s)).collect()
///     }
///
///     fn len(&self) -> usize {
///         2
///     }
/// }
/// ```
///
/// # Find
///
/// The `find` function allows for nested access from an `Object`. A default implementation is
/// provided by the trait which assumes the key will split on the `.` character. This can be overriden if
/// required. Below is an example of how find works for a complex data structure.
///
/// ```
/// # use std::borrow::Cow;
/// # use tau_engine::Value;
/// use tau_engine::Object;
///
/// struct Foo {
///     pub bar: String,
/// }
/// # impl Object for Foo {
/// #     fn get(&self, key: &str) -> Option<Value<'_>> {
/// #         match key {
/// #             "bar" => Some(Value::String(Cow::Borrowed(&self.bar))),
/// #             _ => None,
/// #         }
/// #     }
/// #
/// #     fn keys(&self) -> Vec<Cow<'_, str>> {
/// #         ["bar"].iter().map(|s| Cow::Borrowed(*s)).collect()
/// #     }
/// #
/// #     fn len(&self) -> usize {
/// #         1
/// #     }
/// # }
/// struct Baz {
///     pub foo: Foo,
/// }
/// # impl Object for Baz {
/// #     fn get(&self, key: &str) -> Option<Value<'_>> {
/// #         match key {
/// #             "foo" => Some(Value::Object(&self.foo)),
/// #             _ => None,
/// #         }
/// #     }
/// #
/// #     fn keys(&self) -> Vec<Cow<'_, str>> {
/// #         ["foo"].iter().map(|s| Cow::Borrowed(*s)).collect()
/// #     }
/// #
/// #     fn len(&self) -> usize {
/// #         1
/// #     }
/// # }
/// let complex = Baz {
///     foo: Foo {
///         bar: "foobar".to_owned(),
///     }
/// };
///
/// let value = complex.find("foo.bar").unwrap();
///
/// assert_eq!(value.as_str(), Some("foobar"));
/// ```
#[allow(clippy::len_without_is_empty)]
#[cfg(not(feature = "sync"))]
pub trait Object {
    /// Looks for a `Value` by key and returns it if found. The provided implementation will split
    /// the key on `.` to handle nesting.
    fn find(&self, key: &str) -> Option<Value<'_>> {
        let mut v: Option<Value<'_>> = None;
        for k in key.split('.') {
            match v {
                Some(Value::Object(value)) => v = value.get(k),
                Some(_) => return None,
                None => match <Self as Object>::get(self, k) {
                    Some(value) => v = Some(value),
                    None => return None,
                },
            }
        }
        v
    }

    /// Get the `Value` corresponding to the key.
    fn get(&self, key: &str) -> Option<Value<'_>>;

    /// Returns the keys for the object.
    fn keys(&self) -> Vec<Cow<'_, str>>;

    /// Returns the number of elements in the object.
    fn len(&self) -> usize;
}
#[cfg(feature = "sync")]
pub trait Object: Send + Sync {
    fn find(&self, key: &str) -> Option<Value<'_>> {
        let mut v: Option<Value<'_>> = None;
        for k in key.split('.') {
            match v {
                Some(Value::Object(value)) => v = value.get(k),
                Some(_) => return None,
                None => match <Self as Object>::get(self, k) {
                    Some(value) => v = Some(value),
                    None => return None,
                },
            }
        }
        v
    }
    fn get(&self, key: &str) -> Option<Value<'_>>;
    fn keys(&self) -> Vec<Cow<'_, str>>;
    fn len(&self) -> usize;
}

#[cfg(not(feature = "sync"))]
impl<V, S> Object for HashMap<String, V, S>
where
    V: AsValue,
    S: BuildHasher,
{
    #[inline]
    fn get(&self, key: &str) -> Option<Value<'_>> {
        self.get(key).map(|v| v.as_value())
    }

    #[inline]
    fn keys(&self) -> Vec<Cow<'_, str>> {
        self.keys().map(|s| Cow::Borrowed(s.as_str())).collect()
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }
}
#[cfg(feature = "sync")]
impl<V, S> Object for HashMap<String, V, S>
where
    V: AsValue,
    S: BuildHasher + Send + Sync,
{
    #[inline]
    fn get(&self, key: &str) -> Option<Value<'_>> {
        self.get(key).map(|v| v.as_value())
    }

    #[inline]
    fn keys(&self) -> Vec<Cow<'_, str>> {
        self.keys().map(|s| Cow::Borrowed(s.as_str())).collect()
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }
}

impl<O: Object> AsValue for O {
    #[inline]
    fn as_value(&self) -> Value<'_> {
        Value::Object(self)
    }
}
