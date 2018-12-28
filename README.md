
The Cosec C Compiler
--------------------

Cosec is a toy optimising C compiler, written in Python. My goals for the 
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

* **Lexer**: converts C source code into an array of individual tokens.
* **Pre-processor**: deals with pre-processor directives (e.g. `#include`) and
  comments.
* **Parser**: converts tokens produced by the lexer into an abstract syntax
  tree.
* **Compiler**: converts an AST into a graph of basic blocks containing
  IR instructions.
* **Optimiser**: optimises an IR graph to produce faster assembly code.
* **Assembly Generator**: outputs architecture-specific assembly code from an 
  IR graph.
* **Assembler**: converts assembly code into machine code in the form of object
  files. Cosec uses the GNU assembler on OSX and Linux.
* **Linker**: links a list of object files together into a final executable. 
  Cosec uses the GNU linker on OSX and Linux.

## Usage

Cosec has no dependencies, so running the compiler is as easy as:

```bash
$ python cosec.py example.c
```

Use the built in help function for more details about the command line options:

```bash
$ python cosec.py --help
```

Note that Python 3.7 is the minimum required version.

## License

Cosec is under the MIT license. See the `LICENSE` file for more details.
