# Stonemason

A functional, typed data language.

## Design

Stonemason data may be of the following types:
* Primitive
    * Unit: `()`; a zero-sized type.
    * Boolean
    * Numeric
        * Integer: Unsigned & signed integers, of arbitrary bit width.
        * Floats: 32- & 64-bit floating-point numerics.
    * Character: A UTF-8 [unicode scalar value](https://www.unicode.org/glossary/#unicode_scalar_value); equivalent to Rust's [char](https://doc.rust-lang.org/nightly/std/primitive.char.html) type.
* Sequence
    * Tuple: Fixed-length, heterogeneous sequence of data.
    * Array: Homogeneous sequence of data.
    * String: A sequence of UTF-8 characters. Equivalent to `[u8]`, but with convenient syntax and required to be valid UTF-8.
* User-defined
    * Struct: Heterogeneous product of other types (the 'fields' of the type).
    * Enum: Heterogeneous disjoint union type.
* Collection
    * Map:

A stonemason definition always resolves to a struct of type `Module`, defined as:
```stonemason
struct Module {
    modules: Map<String, Module>,
    variables: Map<String, dyn>,
    result: dyn
}
```
The `result` field is the output of the last expression in the definition. Expressions with no output evaluate to `()`.
