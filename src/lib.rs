pub use pydataclass_derive::PyDataclass;
use std::{collections::BTreeMap, path::PathBuf};

pub use serialize::Serializer;

mod py_doc;
mod serialize;

#[derive(Debug)]
pub enum PyClass {
    Dataclass(PyDataclass),
    Enum(PyEnum),
}

impl PyClass {
    pub fn name(&self) -> &str {
        match self {
            PyClass::Dataclass(py_dataclass) => &py_dataclass.name,
            PyClass::Enum(py_enum) => &py_enum.name,
        }
    }
}

#[derive(Debug)]
pub struct PyDataclass {
    pub name: String,
    pub fields: Fields,
}

#[derive(Debug)]
pub struct PyEnum {
    pub name: String,
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub enum Fields {
    Named(Vec<NamedField>),
    Anon(Vec<PyType>),
}

#[derive(Debug)]
pub struct NamedField {
    pub name: String,
    pub ty: PyType,
}

#[derive(Debug)]
pub struct Variant {
    pub idx: usize,
    pub name: String,
    pub fields: Option<Fields>,
}

#[derive(Debug)]
pub enum PyType {
    Container(&'static str, Vec<PyType>),
    BuiltIn(&'static str),
    Named(String),
}

#[derive(Default, Debug)]
pub struct PyRegistry(BTreeMap<String, Option<PyClass>>);

impl PyRegistry {
    pub fn insert<F>(&mut self, name: impl ToString + AsRef<str>, py_class_fn: F)
    where
        F: Fn(&mut Self) -> PyClass,
    {
        if !self.0.contains_key(name.as_ref()) {
            self.0.insert(name.to_string(), None);
            let py_class = py_class_fn(self);
            self.0.insert(name.to_string(), Some(py_class));
        }
    }
}

pub trait BuildPyClass {
    fn py_class(registry: &mut PyRegistry) -> PyClass;
}

pub trait BuildPyType {
    fn py_type(registry: &mut PyRegistry) -> PyType;
}

#[macro_export]
macro_rules! py_type {
    // Single type case
    (builtin $type:ty => $py_type:expr) => {
        impl BuildPyType for $type {
            fn py_type(_: &mut PyRegistry) -> PyType {
                $py_type
            }
        }
    };

    // Multiple types case
    (builtin $first_type:ty, $($rest_type:ty),+ => $py_type:expr) => {
        // Implement for the first type
        py_type!(builtin $first_type => $py_type);

        // Recursively implement for the rest of the types
        py_type!(builtin $($rest_type),+ => $py_type);
    };

    // Single type case
    (user $type:ty) => {
        impl $crate::BuildPyType for $type {
            fn py_type(r: &mut $crate::PyRegistry) -> $crate::PyType {
                r.insert(stringify!($type), <$type as $crate::BuildPyClass>::py_class);
                $crate::PyType::Named(stringify!($type).to_string())
            }
        }
    };
}

py_type!(builtin String => PyType::BuiltIn("str"));
py_type!(builtin u8, u16, u32, u64, usize => PyType::BuiltIn("int"));
py_type!(builtin i8, i16, i32, i64, isize => PyType::BuiltIn("int"));
py_type!(builtin bool => PyType::BuiltIn("bool"));

impl<T> BuildPyType for Option<T>
where
    T: BuildPyType,
{
    fn py_type(registry: &mut PyRegistry) -> PyType {
        PyType::Container("Optional", vec![T::py_type(registry)])
    }
}

impl<T> BuildPyType for Box<T>
where
    T: BuildPyType,
{
    fn py_type(registry: &mut PyRegistry) -> PyType {
        T::py_type(registry)
    }
}

impl<T> BuildPyType for Vec<T>
where
    T: BuildPyType,
{
    fn py_type(registry: &mut PyRegistry) -> PyType {
        PyType::Container("List", vec![T::py_type(registry)])
    }
}

impl BuildPyType for PathBuf {
    fn py_type(_r: &mut PyRegistry) -> PyType {
        PyType::BuiltIn("Path")
    }
}

macro_rules! impl_build_py_type_for_tuples {
    // Implement for tuples of size N
    ($($T:ident),+) => {
        impl<$($T),+> BuildPyType for ($($T,)+)
        where
            $($T: BuildPyType),+
        {
            fn py_type(registry: &mut PyRegistry) -> PyType {
                PyType::Container(
                    "Tuple",
                    vec![$($T::py_type(registry)),+]
                )
            }
        }
    };
}

// Implement for tuples of size 0 to 12
impl_build_py_type_for_tuples!(A);
impl_build_py_type_for_tuples!(A, B);
impl_build_py_type_for_tuples!(A, B, C);
impl_build_py_type_for_tuples!(A, B, C, D);
impl_build_py_type_for_tuples!(A, B, C, D, E);
impl_build_py_type_for_tuples!(A, B, C, D, E, F);
impl_build_py_type_for_tuples!(A, B, C, D, E, F, G);
impl_build_py_type_for_tuples!(A, B, C, D, E, F, G, H);
impl_build_py_type_for_tuples!(A, B, C, D, E, F, G, H, I);
impl_build_py_type_for_tuples!(A, B, C, D, E, F, G, H, I, J);
impl_build_py_type_for_tuples!(A, B, C, D, E, F, G, H, I, J, K);
impl_build_py_type_for_tuples!(A, B, C, D, E, F, G, H, I, J, K, L);
