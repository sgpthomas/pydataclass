use std::{
    collections::{btree_map::Entry, BTreeMap, HashSet},
    io::Write,
    marker::PhantomData,
};

use pretty::{Pretty, RcAllocator, RcDoc};

use crate::{
    py_doc::{PrettyExt, PyClassDoc, PyFnDoc},
    BuildPyClass, Fields, NamedField, PyClass, PyDataclass, PyEnum, PyRegistry, PyType, Variant,
};

/// Describes how to build the parse function for `Self`
trait ParseFunction<'a> {
    fn parse(self, ctx: &mut Context) -> PyFnDoc<'a>;
    fn dump(self, ctx: &mut Context) -> PyFnDoc<'a>;
}

/// Describes how to render self to a RcDoc
pub(super) trait Render<'a>: Sized {
    fn render(self, ctx: &mut Context) -> RcDoc<'a>;
}

#[derive(Default)]
pub(super) struct Context {
    ty_imports: BTreeMap<String, HashSet<String>>,
    seen_definitions: HashSet<String>,
    dependencies: HashSet<String>,
}

impl Context {
    fn import(&mut self, module: impl ToString, thing: impl ToString) {
        match self.ty_imports.entry(module.to_string()) {
            Entry::Occupied(mut o) => {
                o.get_mut().insert(thing.to_string());
            }
            Entry::Vacant(v) => {
                v.insert(HashSet::from([thing.to_string()]));
            }
        }
    }

    fn render_parse_fields<'a>(&mut self, fields: &'a Fields) -> RcDoc<'a> {
        let input = move |doc: RcDoc<'a>| RcDoc::text("input").append(doc.brackets());
        match fields {
            Fields::Named(fields) => RcDoc::intersperse(
                fields.iter().map(|f| {
                    RcDoc::text(&f.name).append(" = ").append(
                        self.construct_py_type(&f.ty, input(RcDoc::text(&f.name).double_quotes())),
                    )
                }),
                RcDoc::line(),
            ),
            Fields::Anon(fields) => RcDoc::intersperse(
                fields.iter().enumerate().map(|(idx, ty)| {
                    RcDoc::text(format!("_{idx}"))
                        .append(" = ")
                        .append(self.construct_py_type(ty, input(RcDoc::text(format!("{idx}")))))
                }),
                RcDoc::line(),
            ),
        }
    }

    fn construct_py_type<'a>(&mut self, ty: &'a PyType, doc: RcDoc<'a>) -> RcDoc<'a> {
        match ty {
            PyType::Container("List", args) if args.len() == 1 => RcDoc::nil()
                .append(self.construct_py_type(&args[0], RcDoc::text("item")))
                .append(RcDoc::space())
                .append("for item in")
                .append(RcDoc::space())
                .append(doc)
                .brackets(),
            PyType::Container("Optional", args) if args.len() == 1 => self
                .construct_py_type(&args[0], doc.clone())
                .append(RcDoc::space())
                .append("if")
                .append(RcDoc::space())
                .append(doc)
                .append(RcDoc::space())
                .append("is not None else None"),
            PyType::Container("Tuple", args) => RcDoc::intersperse(
                args.iter().map(|a| self.construct_py_type(a, doc.clone())),
                ", ",
            )
            .parens(),
            PyType::Container(ty, args) => panic!("Don't support {ty}[{args:?}] yet"),
            PyType::BuiltIn("Path") => RcDoc::text("Path").append(doc.parens()),
            PyType::BuiltIn(pyty) => {
                self.import("typing", "cast");
                RcDoc::text("cast").append(RcDoc::text(*pyty).append(", ").append(doc).parens())
            }
            PyType::Named(name) => RcDoc::text(name).append(".parse").append(doc.parens()),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn dump_py_type<'a>(&self, ty: &'a PyType, doc: RcDoc<'a>) -> RcDoc<'a> {
        match ty {
            PyType::Container("List", args) if args.len() == 1 => RcDoc::nil()
                .append("x.dump() for x in self.")
                .append(doc)
                .brackets(),
            PyType::Container("Optional", _) => RcDoc::text("self.").append(doc),
            PyType::Container("Tuple", args) => RcDoc::intersperse(
                args.iter().enumerate().map(|(i, a)| {
                    self.dump_py_type(a, doc.clone())
                        .append(RcDoc::text(format!("{i}")).brackets())
                }),
                ", ",
            )
            .parens(),
            PyType::Container(ty, args) => panic!("Don't support {ty}[{args:?}] yet"),
            PyType::BuiltIn("Path") => {
                RcDoc::text("str").append(RcDoc::text("self.").append(doc).parens())
            }
            PyType::BuiltIn(_) => RcDoc::text("self.").append(doc),
            PyType::Named(_) => RcDoc::text("self.").append(doc).append(".dump()"),
        }
    }
}

impl<'a> Render<'a> for &'a PyClass {
    fn render(self, ctx: &mut Context) -> RcDoc<'a> {
        match self {
            PyClass::Dataclass(py_dataclass) => py_dataclass.render(ctx),
            PyClass::Enum(py_enum) => py_enum.render(ctx),
        }
    }
}

impl<'a> ParseFunction<'a> for (&'a str, &'a Fields) {
    fn parse(self, ctx: &mut Context) -> PyFnDoc<'a> {
        let (name, fields) = self;
        let expected_type = if matches!(fields, Fields::Anon(_vec)) {
            RcDoc::text("list")
        } else {
            RcDoc::text("dict")
        };

        PyFnDoc::new("parse")
            .annotate("@staticmethod")
            .arg_ty("input", "Json")
            .returns(RcDoc::text(name).double_quotes())
            .body(
                RcDoc::nil()
                    .append("assert type(input) is")
                    .append(RcDoc::space())
                    .append(expected_type)
                    .append(RcDoc::hardline())
                    .append(ctx.render_parse_fields(fields))
                    .append(RcDoc::hardline())
                    .append("return")
                    .append(RcDoc::space())
                    .append(name)
                    .append(
                        RcDoc::intersperse(field_names(fields).map(|(name, _)| name), ", ")
                            .parens(),
                    ),
            )
    }

    fn dump(self, ctx: &mut Context) -> PyFnDoc<'a> {
        let (_name, fields) = self;
        PyFnDoc::new("dump")
            .arg("self")
            .returns(RcDoc::text("Json"))
            .body(
                RcDoc::nil().append("return").append(RcDoc::space()).append(
                    RcDoc::nil()
                        .append(
                            RcDoc::hardline()
                                .append(RcDoc::intersperse(
                                    field_names(fields).map(|(field_name, field_ty)| {
                                        field_name
                                            .clone()
                                            .double_quotes()
                                            .append(": ")
                                            .append(ctx.dump_py_type(field_ty, field_name))
                                    }),
                                    RcDoc::text(",").append(RcDoc::hardline()),
                                ))
                                .nest(4)
                                .group(),
                        )
                        .append(RcDoc::hardline())
                        .braces(),
                ),
            )
    }
}

fn field_names<'a>(fields: &'a Fields) -> Box<dyn Iterator<Item = (RcDoc<'a>, &'a PyType)> + 'a> {
    match fields {
        Fields::Named(named) => Box::new(named.iter().map(|f| (RcDoc::text(&f.name), &f.ty))),
        Fields::Anon(anons) => Box::new(
            anons
                .iter()
                .enumerate()
                .map(|(idx, ty)| (RcDoc::text(format!("_{idx}")), ty)),
        ),
    }
}

impl<'a> ParseFunction<'a> for &'a PyDataclass {
    fn parse(self, ctx: &mut Context) -> PyFnDoc<'a> {
        (self.name.as_str(), &self.fields).parse(ctx)
    }

    fn dump(self, ctx: &mut Context) -> PyFnDoc<'a> {
        (self.name.as_str(), &self.fields).dump(ctx)
    }
}

impl<'a> Render<'a> for &'a PyDataclass {
    fn render(self, ctx: &mut Context) -> RcDoc<'a> {
        let mut cls = PyClassDoc::new(&self.name)
            .annotate("@dataclass")
            .function(self.parse(ctx))
            .function(self.dump(ctx));
        cls = match &self.fields {
            Fields::Named(named) => cls.fields(
                named
                    .iter()
                    .map(|f| (&f.name, Some(f.ty.render(ctx)), None)),
            ),
            Fields::Anon(anons) => cls.fields(
                anons
                    .iter()
                    .enumerate()
                    .map(|(idx, ty)| (format!("_{idx}"), Some(ty.render(ctx)), None)),
            ),
        };

        ctx.seen_definitions.insert(self.name.clone());
        cls.render(ctx)
    }
}

impl<'a> Render<'a> for &'a NamedField {
    fn render(self, ctx: &mut Context) -> RcDoc<'a> {
        RcDoc::text(&self.name)
            .append(":")
            .append(RcDoc::space())
            .append(self.ty.render(ctx))
    }
}

impl<'a> Render<'a> for &'a PyType {
    fn render(self, ctx: &mut Context) -> RcDoc<'a> {
        match self {
            PyType::Container(container_name, args) => {
                ctx.import("typing", container_name);
                RcDoc::text(*container_name).append(
                    RcDoc::intersperse(args.iter().map(|arg| arg.render(ctx)), ", ").brackets(),
                )
            }
            PyType::Named(name) => {
                ctx.dependencies.insert(name.clone());
                if ctx.seen_definitions.contains(name) {
                    RcDoc::text(name)
                } else {
                    name.double_quotes()
                }
            }
            PyType::BuiltIn(name) => {
                if name == &"Path" {
                    ctx.import("pathlib", "Path");
                }
                RcDoc::text(*name)
            }
        }
    }
}

impl<'a> Render<'a> for &'a PyEnum {
    fn render(self, ctx: &mut Context) -> RcDoc<'a> {
        PyClassDoc::new(&self.name)
            .function(self.parse(ctx))
            .function(self.dump(ctx))
            .render(ctx)
            .append(RcDoc::hardline())
            .append(RcDoc::hardline())
            .append(RcDoc::intersperse(
                self.variants.iter().map(|v| {
                    let mut cls = PyClassDoc::new(&v.name).parent(&self.name);
                    if let Some(fields) = &v.fields {
                        // create match args
                        cls = cls.field(
                            "__match_args__",
                            None,
                            Some(
                                RcDoc::intersperse(
                                    field_names(fields).map(|(name, _)| name.double_quotes()),
                                    ", ",
                                )
                                .append(",")
                                .parens(),
                            ),
                        );

                        // create init function
                        let mut init_fn = PyFnDoc::new("__init__").arg("self");
                        for (name, ty) in field_names(fields) {
                            init_fn = init_fn.arg_ty(name, ty.render(ctx));
                        }
                        init_fn = init_fn.body(RcDoc::nil().append(RcDoc::intersperse(
                            field_names(fields).map(|(name, _)| {
                                RcDoc::text("self.")
                                    .append(name.clone())
                                    .append(" = ")
                                    .append(name)
                            }),
                            RcDoc::hardline(),
                        )));

                        // create parse function
                        cls = cls
                            .function(init_fn)
                            .function((v.name.as_str(), fields).parse(ctx))
                            .function(
                                (v.name.as_str(), fields)
                                    .dump(ctx)
                                    .override_name("dump_inner"),
                            )
                    }
                    cls.render(ctx)
                }),
                RcDoc::hardline().append(RcDoc::hardline()),
            ))
    }
}

enum PyStmtDoc<'a> {
    IfStmt {
        conds: Vec<(RcDoc<'a>, RcDoc<'a>)>,
        else_: Option<RcDoc<'a>>,
    },
}

impl<'a> PyStmtDoc<'a> {
    fn if_(cond: impl Pretty<'a, RcAllocator>) -> PyIfStmtBuilder<'a, ()> {
        PyIfStmtBuilder {
            cond: cond.build(),
            then: (),
        }
    }

    fn else_(self, stmt: impl Pretty<'a, RcAllocator>) -> Self {
        let PyStmtDoc::IfStmt { conds, else_: _ } = self;
        PyStmtDoc::IfStmt {
            conds,
            else_: Some(stmt.build()),
        }
    }
}

impl<'a> Render<'a> for PyStmtDoc<'a> {
    fn render(self, _ctx: &mut Context) -> RcDoc<'a> {
        match self {
            PyStmtDoc::IfStmt { conds, else_ } => RcDoc::intersperse(
                conds.into_iter().map(|(cond, then)| {
                    RcDoc::text("if")
                        .append(RcDoc::space())
                        .append(cond)
                        .append(":")
                        .append(RcDoc::hardline().append(then).nest(4).group())
                }),
                RcDoc::hardline().append("el"),
            )
            .append(else_.map(|else_| {
                RcDoc::hardline()
                    .append("else:")
                    .append(RcDoc::hardline().append(else_).nest(4).group())
            })),
        }
    }
}

struct PyIfStmtBuilder<'a, T> {
    cond: RcDoc<'a>,
    then: T,
}

impl<'a> PyIfStmtBuilder<'a, ()> {
    fn then(self, stmt: impl Pretty<'a, RcAllocator>) -> PyIfStmtBuilder<'a, RcDoc<'a>> {
        PyIfStmtBuilder {
            cond: self.cond,
            then: stmt.build(),
        }
    }
}

impl<'a> FromIterator<PyIfStmtBuilder<'a, RcDoc<'a>>> for PyStmtDoc<'a> {
    fn from_iter<T: IntoIterator<Item = PyIfStmtBuilder<'a, RcDoc<'a>>>>(iter: T) -> Self {
        let mut conds = vec![];

        for i in iter {
            conds.push((i.cond, i.then));
        }

        Self::IfStmt { conds, else_: None }
    }
}

impl<'a> ParseFunction<'a> for &'a PyEnum {
    fn parse(self, ctx: &mut Context) -> PyFnDoc<'a> {
        PyFnDoc::new("parse")
            .annotate("@staticmethod")
            .arg_ty("input", "Json")
            .returns(RcDoc::text(&self.name).double_quotes())
            .body(
                self.variants
                    .iter()
                    .map(|v| {
                        let vname = &v.name;
                        match &v.fields {
                            Some(_) => PyStmtDoc::if_(format!(
                                "type(input) is dict and \"{vname}\" in input",
                            ))
                            .then(format!("return {vname}.parse(input[\"{vname}\"])")),
                            None => PyStmtDoc::if_(format!(
                                "type(input) is str and \"{vname}\" == input"
                            ))
                            .then(format!("return {vname}()")),
                        }
                    })
                    .collect::<PyStmtDoc>()
                    .else_("raise Exception(\"Can't parse input\")")
                    .render(ctx),
            )
    }

    fn dump(self, ctx: &mut Context) -> PyFnDoc<'a> {
        PyFnDoc::new("dump")
            .arg("self")
            .returns(RcDoc::text("Json"))
            .body(
                self.variants
                    .iter()
                    .map(|v| {
                        let vname = &v.name;
                        match &v.fields {
                            Some(_) => PyStmtDoc::if_(format!("isinstance(self, {vname})",))
                                .then(format!("return {{\"{vname}\": self.dump_inner()}}")),
                            None => PyStmtDoc::if_(format!(
                                "type(input) is str and \"{vname}\" == input"
                            ))
                            .then(format!("return {vname}()")),
                        }
                    })
                    .collect::<PyStmtDoc>()
                    .else_("raise Exception(\"unreachable\")")
                    .render(ctx),
            )
    }
}

impl<'a> Render<'a> for &'a Vec<Variant> {
    fn render(self, ctx: &mut Context) -> RcDoc<'a> {
        RcDoc::intersperse(
            self.iter().map(|variant| variant.render(ctx)),
            RcDoc::hardline(),
        )
    }
}

impl<'a> Render<'a> for &'a Variant {
    fn render(self, _ctx: &mut Context) -> RcDoc<'a> {
        RcDoc::text(&self.name)
            .append(RcDoc::space())
            .append("=")
            .append(RcDoc::space())
            .append(self.idx.to_string())
    }
}

#[derive(Debug)]
pub struct Serializer<T>(PhantomData<T>);

impl<T> Default for Serializer<T> {
    fn default() -> Self {
        Serializer(PhantomData)
    }
}

impl<T> std::fmt::Display for Serializer<T>
where
    T: BuildPyClass,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut registry = PyRegistry::default();
        let root_class = T::py_class(&mut registry);

        let mut ctx = Context::default();
        let mut rendered_py_classes: Vec<u8> = vec![];
        root_class
            .render(&mut ctx)
            .render(80, &mut rendered_py_classes)
            .unwrap();

        writeln!(&mut rendered_py_classes, "\n\n").unwrap();

        for v in registry.0.into_values().flatten() {
            v.render(&mut ctx)
                .render(80, &mut rendered_py_classes)
                .unwrap();
            writeln!(&mut rendered_py_classes, "\n\n").unwrap();
        }

        ctx.import("dataclasses", "dataclass");
        ctx.import("typing", "List");
        ctx.import("typing", "Dict");
        for (module, imports) in &ctx.ty_imports {
            writeln!(
                f,
                "from {module} import {}",
                imports.iter().cloned().collect::<Vec<_>>().join(", ")
            )?;
        }

        writeln!(f)?;
        writeln!(f, "Json = List[\"Json\"] | Dict | str | int")?;
        writeln!(f)?;

        write!(
            f,
            "\n{}",
            String::from_utf8(rendered_py_classes).unwrap().trim_end()
        )?;

        Ok(())
    }
}

impl<A, B> std::fmt::Display for Serializer<(A, B)>
where
    A: BuildPyClass,
    B: BuildPyClass,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut registry = PyRegistry::default();
        let root_class_a = A::py_class(&mut registry);
        let root_class_b = B::py_class(&mut registry);

        let mut ctx = Context::default();
        let mut rendered_py_classes: Vec<u8> = vec![];
        root_class_a
            .render(&mut ctx)
            .render(80, &mut rendered_py_classes)
            .unwrap();
        writeln!(&mut rendered_py_classes, "\n\n").unwrap();
        root_class_b
            .render(&mut ctx)
            .render(80, &mut rendered_py_classes)
            .unwrap();

        writeln!(&mut rendered_py_classes, "\n\n").unwrap();

        for v in registry.0.into_values().flatten() {
            v.render(&mut ctx)
                .render(80, &mut rendered_py_classes)
                .unwrap();
            writeln!(&mut rendered_py_classes, "\n\n").unwrap();
        }

        ctx.import("dataclasses", "dataclass");
        ctx.import("typing", "List");
        ctx.import("typing", "Dict");
        for (module, imports) in &ctx.ty_imports {
            writeln!(
                f,
                "from {module} import {}",
                imports.iter().cloned().collect::<Vec<_>>().join(", ")
            )?;
        }

        writeln!(f)?;
        writeln!(f, "Json = List[\"Json\"] | Dict | str | int")?;
        writeln!(f)?;

        write!(
            f,
            "\n{}",
            String::from_utf8(rendered_py_classes).unwrap().trim_end()
        )?;

        Ok(())
    }
}
