use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(PyDataclass)]
pub fn derive_py_dataclass(item: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(item as syn::DeriveInput);

    let root = quote!(::pydataclass);
    let name = &parsed.ident;

    let construct = match &parsed.data {
        syn::Data::Struct(data_struct) => expand_struct(&root, name, data_struct),
        syn::Data::Enum(data_enum) => expand_enum(&root, name, data_enum),
        syn::Data::Union(_data_union) => panic!("We don't support unions."),
    };

    quote! {
        impl #root::BuildPyClass for #name {
            fn py_class(r: &mut #root::PyRegistry) -> #root::PyClass {
                #construct
            }
        }
        #root::py_type!(user #name);
    }
    .into()
}

fn expand_struct(
    root: &proc_macro2::TokenStream,
    ty_name: &syn::Ident,
    data_struct: &syn::DataStruct,
) -> proc_macro2::TokenStream {
    let fields = expand_fields(root, &data_struct.fields);
    quote! {
        #root::PyClass::Dataclass(#root::PyDataclass {
            name: stringify!(#ty_name).to_string(),
            fields: #fields
        })
    }
}

fn expand_enum(
    root: &proc_macro2::TokenStream,
    ty_name: &syn::Ident,
    data_enum: &syn::DataEnum,
) -> proc_macro2::TokenStream {
    let variants = data_enum.variants.iter().enumerate().map(|(idx, v)| {
        let fields = if !v.fields.is_empty() {
            let fields = expand_fields(root, &v.fields);
            quote!(Some(#fields))
        } else {
            quote!(None)
        };

        let v_name = &v.ident;
        quote! {
            #root::Variant {
                idx: #idx,
                name: stringify!(#v_name).to_string(),
                fields: #fields
            }
        }
    });
    quote! {
        #root::PyClass::Enum(#root::PyEnum {
            name: stringify!(#ty_name).to_string(),
            variants: vec![#(#variants),*]
        })
    }
}

fn expand_fields(
    root: &proc_macro2::TokenStream,
    fields: &syn::Fields,
) -> proc_macro2::TokenStream {
    let named = fields.iter().all(|f| f.ident.is_some());

    if named {
        let named = fields.iter().map(|f| {
            let ty = &f.ty;
            let fname = f.ident.as_ref().unwrap();
            quote! {
                #root::NamedField {
                    name: stringify!(#fname).to_string(),
                    ty: <#ty as #root::BuildPyType>::py_type(r),
                }
            }
        });
        quote!(#root::Fields::Named(vec![#(#named),*]))
    } else {
        let fields = fields.iter().map(|f| {
            let ty = &f.ty;
            quote!(<#ty as #root::BuildPyType>::py_type(r))
        });
        quote!(#root::Fields::Anon(vec![#(#fields),*]))
    }
}
