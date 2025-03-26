#![allow(unused)]

use std::io::Write;
use std::{fs, path::PathBuf};

use pydataclass::{PyDataclass, Serializer};

#[derive(PyDataclass, Debug)]
pub struct HarnessMetadata2(Vec<FunctionMetadata>);

#[derive(PyDataclass, Debug)]
pub struct FunctionMetadata {
    target_function_name: String,
    location: InjectionLocation,
    actions: Vec<Action>,
}

#[derive(PyDataclass, Debug)]
pub struct InjectionLocation {
    file: PathBuf,
    line: usize,
    col: usize,
}

#[derive(PyDataclass, Debug)]
pub struct Action {
    kind: ActionKind,
    injections: Vec<Payload>,
}

#[derive(PyDataclass, Debug)]
pub enum ActionKind {
    PanicFree,
    Equality,
    Performance(usize, String),
    PubUseChain,
}

#[derive(PyDataclass, Debug)]
pub struct Payload {
    payload: String,
    location: InjectionLocation,
    x: Option<u32>,
    y: (usize, String),
}

fn main() {
    let r = Serializer::<HarnessMetadata2>::default();
    println!("{r}");
}
