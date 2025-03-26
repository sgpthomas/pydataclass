//! Some utilities to make it easier to print formatted python code.

use pretty::{BuildDoc, DocBuilder, Pretty, RcAllocator, RcDoc};

use crate::serialize::{Context, Render};

pub(super) trait PrettyExt<'a>: Sized {
    type Output;
    fn build(self) -> Self::Output;
    fn enclose<B, A>(self, before: B, after: A) -> RcDoc<'a>
    where
        B: Pretty<'a, RcAllocator>,
        A: Pretty<'a, RcAllocator>;
    fn double_quotes(self) -> RcDoc<'a> {
        self.enclose("\"", "\"")
    }
    fn parens(self) -> RcDoc<'a> {
        self.enclose("(", ")")
    }
    fn brackets(self) -> RcDoc<'a> {
        self.enclose("[", "]")
    }
}

impl<'a, P> PrettyExt<'a> for P
where
    P: Pretty<'a, RcAllocator>,
{
    type Output = RcDoc<'a>;

    fn build(self) -> Self::Output {
        DocBuilder(&RcAllocator, BuildDoc::nil())
            .append(self)
            .into_doc()
    }

    fn enclose<B, A>(self, before: B, after: A) -> RcDoc<'a>
    where
        B: Pretty<'a, RcAllocator>,
        A: Pretty<'a, RcAllocator>,
    {
        before.build().append(self).append(after.build())
    }
}

pub(super) struct PyClassDoc<'a> {
    annotation: Option<RcDoc<'a>>,
    name: RcDoc<'a>,
    parents: Vec<RcDoc<'a>>,
    fields: Vec<(RcDoc<'a>, Option<RcDoc<'a>>, Option<RcDoc<'a>>)>,
    functions: Vec<PyFnDoc<'a>>,
}

impl<'a> PyClassDoc<'a> {
    pub fn new(name: &'a str) -> Self {
        Self {
            annotation: None,
            name: RcDoc::text(name),
            parents: vec![],
            fields: vec![],
            functions: vec![],
        }
    }

    pub fn annotate(mut self, annotation: &'a str) -> Self {
        self.annotation = Some(RcDoc::text(annotation));
        self
    }

    pub fn parent(mut self, parent: &'a str) -> Self {
        self.parents.push(RcDoc::text(parent));
        self
    }

    pub fn field(
        mut self,
        name: impl Pretty<'a, RcAllocator>,
        ty: Option<RcDoc<'a>>,
        init: Option<RcDoc<'a>>,
    ) -> Self {
        let name = name.build();
        self.fields.push((name, ty, init));
        self
    }

    pub fn fields(
        mut self,
        fields_iter: impl Iterator<
            Item = (
                impl Pretty<'a, RcAllocator>,
                Option<RcDoc<'a>>,
                Option<RcDoc<'a>>,
            ),
        >,
    ) -> Self {
        self.fields
            .extend(fields_iter.map(|(name, ty, init)| (name.build(), ty, init)));
        self
    }

    pub fn function(mut self, function: PyFnDoc<'a>) -> Self {
        self.functions.push(function);
        self
    }
}

impl<'a> Render<'a> for PyClassDoc<'a> {
    fn render(self, ctx: &mut Context) -> RcDoc<'a> {
        let parents = if !self.parents.is_empty() {
            RcDoc::intersperse(self.parents, ", ").parens()
        } else {
            RcDoc::nil()
        };

        let body = if !self.fields.is_empty() || !self.functions.is_empty() {
            RcDoc::intersperse(
                [
                    (!self.fields.is_empty()).then_some(RcDoc::intersperse(
                        self.fields.into_iter().map(|(name, ty, init)| {
                            name.append(
                                ty.map(|ty| RcDoc::text(": ").append(ty))
                                    .unwrap_or(RcDoc::nil()),
                            )
                            .append(init.map(|init| RcDoc::text(" = ").append(init)))
                        }),
                        RcDoc::hardline(),
                    )),
                    (!self.functions.is_empty()).then_some(RcDoc::intersperse(
                        self.functions.into_iter().map(|func| func.render(ctx)),
                        RcDoc::hardline().append(RcDoc::hardline()),
                    )),
                ]
                .into_iter()
                .filter(|doc| doc.is_some()),
                RcDoc::hardline().append(RcDoc::hardline()),
            )
        } else {
            RcDoc::text("pass")
        };

        RcDoc::nil()
            .append(
                self.annotation
                    .map(|annot| annot.append(RcDoc::hardline()))
                    .unwrap_or(RcDoc::nil()),
            )
            .append("class")
            .append(RcDoc::space())
            .append(self.name)
            .append(parents)
            .append(":")
            .append(RcDoc::hardline().append(body).nest(4).group())
    }
}

pub(super) struct PyFnDoc<'a> {
    annotation: Option<RcDoc<'a>>,
    name: &'a str,
    args: Vec<(RcDoc<'a>, Option<RcDoc<'a>>)>,
    return_ty: Option<RcDoc<'a>>,
    body: Option<RcDoc<'a>>,
}

impl<'a> PyFnDoc<'a> {
    pub fn new(name: &'a str) -> Self {
        Self {
            annotation: None,
            name,
            args: vec![],
            return_ty: None,
            body: None,
        }
    }

    pub fn annotate(mut self, annotation: impl Pretty<'a, RcAllocator>) -> Self {
        self.annotation = Some(annotation.build());
        self
    }

    pub fn arg(mut self, arg_name: impl Pretty<'a, RcAllocator>) -> Self {
        self.args.push((arg_name.build(), None));
        self
    }

    pub fn arg_ty(
        mut self,
        arg_name: impl Pretty<'a, RcAllocator>,
        ty: impl Pretty<'a, RcAllocator>,
    ) -> Self {
        self.args.push((arg_name.build(), Some(ty.build())));
        self
    }

    pub fn returns(mut self, ty: impl Pretty<'a, RcAllocator>) -> Self {
        self.return_ty = Some(ty.build());
        self
    }

    pub fn body(mut self, body: RcDoc<'a>) -> Self {
        self.body = Some(body);
        self
    }
}

impl<'a> Render<'a> for PyFnDoc<'a> {
    fn render(self, _ctx: &mut Context) -> RcDoc<'a> {
        RcDoc::nil()
            .append(
                self.annotation
                    .map(|annot| annot.append(RcDoc::hardline()))
                    .unwrap_or(RcDoc::nil()),
            )
            .append("def")
            .append(RcDoc::space())
            .append(self.name)
            .append(
                RcDoc::intersperse(
                    self.args
                        .into_iter()
                        .map(|(arg, ty)| arg.append(ty.map(|ty| RcDoc::text(": ").append(ty)))),
                    ", ",
                )
                .parens(),
            )
            .append(
                self.return_ty
                    .as_ref()
                    .map(|ty| RcDoc::text(" -> ").append(ty.clone()))
                    .unwrap_or(RcDoc::nil()),
            )
            .append(":")
            .append(
                RcDoc::hardline()
                    .append(self.body.clone().unwrap_or(RcDoc::text("pass")))
                    .nest(4)
                    .group(),
            )
    }
}
