
# The Cosec C Compiler
# By Ben Anderson
# December 2018

import sys
import os.path
import platform
import subprocess
import argparse

from lexer import Lexer
from ast import AstGenerator
from ir import IrGenerator
from asm import AsmGenerator
from err import CompilerError, CompilerErrorBuilder


def main():
    """
    The main entry point for the compiler's command line utility, which
    compiles, assembles, and links all provided files together to produce an
    executable.
    :return: The compiler's exit status as an integer; 0 for success and
    non-zero for error.
    """
    # Wrap the entire compiler in a try-except for compiler errors. Any other
    # errors indicate an internal bug in the compiler
    try:
        assert_platform()
        args = parse_args()
        process_files(args.files, args.output)
    except CompilerError as err:
        err.pretty_print()
        return 1
    return 0


def assert_platform():
    """
    The compiler only works on Intel x86_64 systems with the GCC assembler and
    linker installed. Such systems only include macOS and Linux.
    """
    # Check operating system
    if sys.platform != "darwin":
        raise CompilerError("Only macOS is supported (for now)")

    # Check processor architecture
    if platform.machine() != "x86_64":
        raise CompilerError("Only x86-64 platforms are supported (for now)")


def parse_args():
    """
    Parses the command line arguments passed to the compiler.
    :return: The set of command line arguments.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs="+",
                        help="compile and link the source or object file")
    parser.add_argument("-v", "--version", action="version",
                        version="Cosec 0.0.1")
    parser.add_argument("-o", "--output", default="a.out",
                        help="write output executable to OUTPUT")
    return parser.parse_args()


def process_files(files, exec_file):
    """
    Checks that each file is either a C source code file or a compiled object
    file. Compiles each source code file into an object file, and links these
    object files (plus any others included in the files list) into an
    executable.
    :param files: A set of C source code files or compiled object files.
    :param exec_file: The path to write the output executable to.
    """
    # Check that each file is either a .c or .o file. We perform this check on
    # all files PRIOR to compiling any .c files because we don't want to
    # generate unnecessary .o files before discovering an error with the input
    # arguments
    for file in files:
        extension = os.path.splitext(file)[1]
        if extension != ".c" and extension != ".o":
            msg = f"'{file}' must be a '.c' or '.o' file"
            raise CompilerErrorBuilder(msg).file(file).build()

    # Compile each .c file into a .o file with the same base file name
    object_files = []
    for file in files:
        file_name, extension = os.path.splitext(file)
        if extension == ".c":
            # Compile and assemble the .c file into an object file
            asm_file = file_name + ".s"
            obj_file = file_name + ".o"
            compile(file, asm_file)
            assemble(asm_file, obj_file)
            object_files.append(obj_file)
        elif extension == ".o":
            object_files.append(file)

    # Link all the object files into an executable
    link(object_files, exec_file)


def compile(src_file, asm_file):
    """
    Compiles a C source code file into an assembly file.
    :param src_file: The .c file to compile.
    :param asm_file: The path to write the .s output file to.
    """
    source_code = read_file(src_file)

    # Source code -> tokens
    lexer = Lexer(source_code)
    tokens = lexer.tokenize()

    # Tokens -> AST
    ast = AstGenerator(tokens)
    ast_root = ast.gen()

    # AST -> IR
    ir = IrGenerator(ast_root)
    ir_graph = ir.gen()

    # IR -> Assembly
    asm = AsmGenerator(ir_graph)
    asm_code = asm.gen()

    # Write the assembly code to the output file
    try:
        with open(asm_file, "w") as file:
            file.write(asm_code)
    except IOError as io_err:
        msg = f"failed to write assembly to '{asm_file}'"
        raise CompilerErrorBuilder(msg).file(asm_file).build() from io_err


def assemble(asm_file, obj_file):
    """
    Assembles an assembly file into machine code using the system's native
    assembler (GCC on Linux and LLVM on macOS).
    :param asm_file: The .s file to assemble.
    :param obj_file: The path to write the .o output file to.
    """
    try:
        subprocess.check_call(["as", "-o", obj_file, asm_file])
    except subprocess.CalledProcessError as err:
        # TODO: more descriptive error
        raise CompilerError("assembler failed") from err


def link(obj_files, exec_file):
    """
    Links a series of object files together using the system's native linker
    (GCC on Linux and LLVM on macOS).
    :param obj_files: The set of object files to link.
    :param exec_file: The path to write the output executable to.
    """
    try:
        subprocess.check_call(["ld", "-arch", "x86_64", "-macosx_version_min",
                               "10.7", "-no_pie", "-o", exec_file] + obj_files)
    except subprocess.CalledProcessError as err:
        # TODO: more descriptive error
        raise CompilerError("linker failed") from err


def read_file(path):
    """
    Reads the contents of a file as a string, or None if the file can't be read.
    :param path: The path to the file to read.
    :return: The contents of the file.
    """
    try:
        with open(path, "r") as file:
            return file.read()
    except IOError as err:
        msg = f"failed to read file '{path}'"
        raise CompilerError(msg).file(path).build() from err


if __name__ == "__main__":
    sys.exit(main())
