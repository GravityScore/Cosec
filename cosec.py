#!/usr/bin/python

# The Cosec C Compiler
# By Ben Anderson
# December 2018

import sys
import os.path
import platform
import subprocess

from lexer import Tokens
from parser import Parser
from error import Error, Color, print_color


def main():
    """
    The main entry point for the compiler's command line utility, which
    compiles, assembles, and links all provided files together to produce an
    executable.

    :return: The program's exit status. 0 for success and non-zero for error.
    """
    # Catch all compiler errors (any other exception represents an internal
    # compiler error)
    try:
        # Parse command line arguments
        args = parse_args()

        # Check if we're to show help or version information
        if args["help"]:
            show_help()
        elif args["version"]:
            show_version()
        else:
            # Otherwise, proceed with compilation
            assert_platform()
            process_files(args["files"], args["output"])
    except Error as err:
        # Pretty print all compiler errors to the standard output
        err.print()
        return 1
    return 0


def parse_args():
    """
    Parses the command line arguments passed to the compiler.

    :return: The set of command line arguments.
    """
    # Start with a reasonable set of default arguments
    args = {"help": False, "version": False, "output": "a.out", "files": []}

    # Iterate over all the arguments, starting at the 1st argument since the
    # 0th is just the script name
    idx = 1
    while idx < len(sys.argv):
        arg = sys.argv[idx]
        if arg == "--help" or arg == "-h":
            args["help"] = True
        elif arg == "--version" or arg == "-v":
            args["version"] = True
        elif arg == "-o":
            # Check there's another argument to follow
            if idx >= len(sys.argv) - 1:
                raise Error("missing argument to '-o'")
            idx += 1
            args["output"] = sys.argv[idx]
        elif arg[0] == "-":
            # Unknown command line option (wasn't handled above)
            raise Error(f"unknown option '{arg}'")
        else:
            args["files"].append(arg)
        idx += 1
    return args


def show_version():
    """
    Prints version text to the standard output.
    """
    print_color(Color.GREEN)
    print_color(Color.BOLD)
    print("Cosec C Compiler", end="")
    print_color(Color.RESET)
    print(" version 0.1.0")
    print("By Ben Anderson")


def show_help():
    """
    Prints help text to the standard output.
    """
    show_version()
    print("")
    print("Usage:")
    print("  cosec [options] <files>")
    print("")
    print("Options:")
    print("  --help, -h     Show this help text")
    print("  --version, -v  Show the compiler version")
    print("  -o <file>      Write output to <file>")


def assert_platform():
    """
    The compiler only works on Intel x86_64 systems with the GCC assembler and
    linker installed. Such systems only include macOS and Linux. We only support
    macOS for now, since I haven't worked out the linker arguments to use for
    Linux yet.
    """
    # Check operating system
    if sys.platform != "darwin":
        raise Error("Only macOS is supported (for now)")

    # Check processor architecture
    if platform.machine() != "x86_64":
        raise Error("Only x86-64 platforms are supported (for now)")


def process_files(files, exec_file):
    """
    Checks that each file is either a C source code file or a compiled object
    file. Compiles each source code file into an object file, and links these
    object files (plus any others included in the files list) into an
    executable.

    :param files:     A set of C source code files or compiled object files.
    :param exec_file: The path to write the output executable to.
    """
    # Check the user actually supplied at least one file to compile
    if len(files) == 0:
        raise Error("no input files")

    # Compile each .c file into a .o file with the same base file name
    object_files = []
    for file in files:
        file_name, extension = os.path.splitext(file)
        if extension == ".c":
            # Compile and assemble the .c file into an object file
            asm_file = file_name + ".s"
            obj_file = file_name + ".o"
            compile(file, asm_file)
            # assemble(asm_file, obj_file)
            object_files.append(obj_file)
        elif extension == ".o":
            object_files.append(file)
        else:
            raise Error(f"'{file}' must be a '.c' or '.o' file")

    # Link all the object files into an executable
    # link(object_files, exec_file)


def compile(src_file, asm_file):
    """
    Compiles a C source code file into an assembly file.

    :param src_file: The .c file to compile.
    :param asm_file: The path to write the .s output file to.
    """
    source = read_file(src_file)

    # Source code -> tokens
    tokens = Tokens(src_file, source)

    # Tokens -> AST
    parser = Parser(tokens)
    roots = parser.gen()

    # # AST -> IR
    # ir = IrGenerator(ast_root)
    # ir_graph = ir.gen()
    #
    # # IR -> Assembly
    # asm = AsmGenerator(ir_graph)
    # asm_code = asm.gen()
    #
    # # Write the assembly code to the output file
    # try:
    #     with open(asm_file, "w") as file:
    #         file.write(asm_code)
    # except IOError as io_err:
    #     raise Error(f"failed to write to '{asm_file}'") from io_err


def assemble(asm_file, obj_file):
    """
    Assembles an assembly file into machine code using the system's native
    assembler (GCC on Linux and LLVM on macOS).
    :param asm_file: The .s file to assemble.
    :param obj_file: The path to write the .o output file to.
    """
    # Use different assembler options depending on the platform
    if sys.platform == "darwin":
        assembler_options = []
    else:
        raise Error("Only macOS is supported")

    # Invoke the assembler
    try:
        subprocess.check_call(["as"] + assembler_options +
                              ["-o", obj_file, asm_file])
    except subprocess.CalledProcessError as err:
        # TODO: more descriptive error
        raise Error(f"assembler failed for '{asm_file}'") from err


def link(obj_files, exec_file):
    """
    Links a series of object files together using the system's native linker
    (GCC on Linux and LLVM on macOS).
    :param obj_files: The set of object files to link.
    :param exec_file: The path to write the output executable to.
    """
    # Use different linker options depending on the platform
    if sys.platform == "darwin":
        assembler_options = ["-macosx_version_min", "10.7", "-no_pie"]
    else:
        raise Error("Only macOS is supported")

    # Invoke the linker
    try:
        subprocess.check_call(["ld", "-arch", "x86_64"] + assembler_options +
                              ["-o", exec_file] + obj_files)
    except subprocess.CalledProcessError as err:
        # TODO: more descriptive error
        raise Error(f"linker failed for '{exec_file}'") from err


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
        raise Error(f"failed to read file '{path}'") from err


if __name__ == "__main__":
    sys.exit(main())
