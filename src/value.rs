use std::borrow::Cow;
use std::collections::{HashMap, HashSet};

pub enum Value<'a> {
    Null,
    Bool(bool),
    Float(f64),
    Int(i64),
    UInt(u64),
    String(Cow<'a, str>),
    Array(&'a dyn Array),
    Object(&'a dyn Object),
}

impl<'a> Value<'a> {
    #[inline]
    pub fn is_array(&self) -> bool {
        match self {
            Self::Array(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        match self {
            Self::Bool(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_f64(&self) -> bool {
        match self {
            Self::Float(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_i64(&self) -> bool {
        match self {
            Self::Int(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        match self {
            Self::Null => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_object(&self) -> bool {
        match self {
            Self::Object(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match self {
            Self::String(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn is_u64(&self) -> bool {
        match self {
            Self::UInt(_) => true,
            _ => false,
        }
    }

    #[inline]
    pub fn as_array(&self) -> Option<&dyn Array> {
        match self {
            Self::Array(a) => Some(*a),
            _ => None,
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(*b),
            _ => None,
        }
    }

    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Self::Float(n) => Some(*n),
            _ => None,
        }
    }

    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self::Int(n) => Some(*n),
            _ => None,
        }
    }

    #[inline]
    pub fn as_null(&self) -> Option<()> {
        match self {
            Self::Null => Some(()),
            _ => None,
        }
    }

    #[inline]
    pub fn as_object(&self) -> Option<&dyn Object> {
        match self {
            Self::Object(o) => Some(*o),
            _ => None,
        }
    }

    #[inline]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Self::UInt(n) => Some(*n),
            _ => None,
        }
    }

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

pub trait AsValue {
    fn as_value_ref(&self) -> Value<'_>;
}

impl AsValue for () {
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        Value::Null
    }
}

impl AsValue for bool {
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        Value::Bool(*self)
    }
}

impl AsValue for str {
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        Value::String(Cow::Borrowed(self))
    }
}

impl AsValue for String {
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        Value::String(Cow::Borrowed(self))
    }
}

impl<V> AsValue for Box<V>
where
    V: AsValue,
{
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        self.as_ref().as_value_ref()
    }
}

impl<V> AsValue for HashMap<String, V>
where
    V: AsValue,
{
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        Value::Object(self)
    }
}

impl<V> AsValue for HashSet<V>
where
    V: AsValue,
{
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        Value::Array(self)
    }
}

impl<V> AsValue for Option<V>
where
    V: AsValue,
{
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        self.as_ref()
            .map(|v| v.as_value_ref())
            .unwrap_or(Value::Null)
    }
}

impl<V> AsValue for Vec<V>
where
    V: AsValue,
{
    #[inline]
    fn as_value_ref(&self) -> Value<'_> {
        Value::Array(self)
    }
}

macro_rules! impl_as_value_ref_float {
    ($ty:ty) => {
        impl AsValue for $ty {
            #[inline]
            fn as_value_ref(&self) -> Value<'_> {
                Value::Float(*self as f64)
            }
        }
    };
}

impl_as_value_ref_float!(f32);
impl_as_value_ref_float!(f64);

macro_rules! impl_as_value_ref_int {
    ($ty:ty) => {
        impl AsValue for $ty {
            #[inline]
            fn as_value_ref(&self) -> Value<'_> {
                Value::Int(*self as i64)
            }
        }
    };
}

impl_as_value_ref_int!(i8);
impl_as_value_ref_int!(i16);
impl_as_value_ref_int!(i32);
impl_as_value_ref_int!(i64);
impl_as_value_ref_int!(isize);

macro_rules! impl_as_value_ref_uint {
    ($ty:ty) => {
        impl AsValue for $ty {
            #[inline]
            fn as_value_ref(&self) -> Value<'_> {
                Value::UInt(*self as u64)
            }
        }
    };
}

impl_as_value_ref_uint!(u8);
impl_as_value_ref_uint!(u16);
impl_as_value_ref_uint!(u32);
impl_as_value_ref_uint!(u64);
impl_as_value_ref_uint!(usize);

pub trait Array {
    fn iter(&self) -> Box<dyn Iterator<Item = Value<'_>> + '_>;
    fn len(&self) -> usize;
}

impl<V> Array for HashSet<V>
where
    V: AsValue,
{
    #[inline]
    fn iter(&self) -> Box<dyn Iterator<Item = Value<'_>> + '_> {
        Box::new(self.iter().map(|v| v.as_value_ref()))
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
        Box::new(self.as_slice().iter().map(|v| v.as_value_ref()))
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }
}

pub trait Object {
    fn get(&self, key: &str) -> Option<Value<'_>>;
    fn keys(&self) -> Vec<&str>;
    fn len(&self) -> usize;
}

impl<V> Object for HashMap<String, V>
where
    V: AsValue,
{
    #[inline]
    fn get(&self, key: &str) -> Option<Value<'_>> {
        self.get(key).map(|v| v.as_value_ref())
    }

    #[inline]
    fn keys(&self) -> Vec<&str> {
        self.keys().map(|k| k.as_str()).collect()
    }

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hi() {
        let v = vec![1, 2, 3];
        let _ = Value::Array(&v);
    }
}
