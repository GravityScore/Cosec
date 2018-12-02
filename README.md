
The Cosec C Compiler
--------------------

Cosec is a toy optimising C compiler, written in Python. The goals of the
project are:

* **Maintainable**: the source code is well commented and documented, written 
in a clean, modular, easily maintainable, and extensible fashion.
* **Complete**: the compiler doesn't strictly adhere to any of the C
standards, but implements the most commonly used features of the C language.
* **Technically unique**: the compiler uses a select set of complex algorithms
for parsing, optimisation, and code generation.
* **Standalone**: the compiler doesn't have any external dependencies, and is 
completely self-contained.

Cosec is unique because it's one of the only hobby compilers to implement 
complex optimisations only using an SSA form intermediate representation.

## Architecture Overview

The major components of Cosec are:

Component | Description | Reference
-----------------------------------
Lexer | Converts source code into a series of tokens | `lexer.py`
Pre-processor | Deals with directives and comments | `preprocessor.py`
AST generator | Converts a series of tokens into a structured syntax tree | `ast.py`
IR generator | Converts an AST into a graph of basic blocks containing IR instructions | `ir.py`
IR optimiser | Optimises the IR graph by running a series of optimisation passes | `opt/opt.py`
Assembly generator | Converts an IR graph into assembly code | `asm.py`
Assembler and linker | GNU binutils are used to assemble and link the assembly code | `cosec.py`

## Usage

Cosec has no dependencies, so running the compiler is as easy as:

```bash
$ python cosec.py example.c
```

Use the built in help function for more details about the command line options:

```bash
$ python cosec.py --help
```

## License

Cosec is under the MIT license. See the `LICENSE` file for more details.
