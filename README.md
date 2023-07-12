# HasWasm
A WebAssembly-like Embedded DSL in Haskell

## Introduction
HasWasm is a domain-specific language (DSL) that is integrated into Haskell, which is also based on the course project of [Domain Specific Languages](https://www.cs.uu.nl/docs/vakken/dsl/index.html) at Utrecht University. 

It makes use of the strong type system in Haskell to offer expressive and type-safe abstractions for writing WebAssembly code, which also includes a code generator that turns the parsed Abstract Syntax Tree (AST) into a WebAssembly module in text (WAT) format. 

HasWasm supports two basic types, blocks and control flows, functions, local and global variables, and export and imports.

This project is aimed to create a type-safe, expressive embedded DSL for WebAssembly programming in Haskell. Our effort is concentrated on offering expressive, type-safe, and user-friendly language abstractions for writing WebAssembly code.


## Usage
Please refer to the **Results and Discussion section** of [doc](./docs/DSL_Assignment2_Report.pdf).


## Contributors
- [faizilham](https://github.com/faizilham)
- [largeword](https://github.com/largeword)
