# Design Document for PL/I to HLASM Generator

## Overview

The PL/I to HLASM generator project aims to provide a complete toolchain for converting PL/I source code into HLASM assembly code. This project includes a lexer, parser, code generator, and an interpreter for executing the generated assembly code.

## Components

### Lexer

- **File**: `src/pl1_lexer.mll`
- **Description**: Utilizes OCamllex to tokenize PL/I source code. The lexer identifies keywords, identifiers, literals, and operators, producing a stream of tokens for the parser.

### Parser

- **File**: `src/pl1_parser.mly`
- **Description**: Implements the syntax analysis of PL/I code using OCamlly. It takes the token stream from the lexer and constructs an abstract syntax tree (AST) that represents the structure of the PL/I program.

### Abstract Syntax Tree (AST)

- **Files**: `src/ast.ml`, `src/ast.mli`
- **Description**: Defines the data structures for the AST, which is used to represent the hierarchical structure of the PL/I program. The interface file (`ast.mli`) exposes the necessary types and functions for interacting with the AST.

### Code Generation

- **File**: `src/codegen.ml`
- **Description**: Transforms the AST into HLASM assembly code. This module contains the logic for translating high-level constructs into low-level assembly instructions.

### HLASM AST

- **File**: `src/hlasm_ast.ml`
- **Description**: Represents the HLASM-specific constructs in an abstract syntax tree format. This allows for easier manipulation and generation of HLASM code.

### HLASM Printer

- **File**: `src/hlasm_printer.ml`
- **Description**: Responsible for outputting the generated HLASM code in a readable format. This module formats the assembly code for clarity and correctness.

### Interpreter

- **File**: `src/interpreter.ml`
- **Description**: Implements an interpreter that reads and executes the generated HLASM code. This module simulates the execution of the assembly instructions, providing a runtime environment for the generated code.

### Runtime

- **File**: `src/runtime.ml`
- **Description**: Contains functions and data structures necessary for the execution of the interpreter. This includes memory management, instruction handling, and state management.

### Utilities

- **File**: `src/utils.ml`
- **Description**: Provides utility functions that are used throughout the project. These functions assist with common tasks such as string manipulation, error handling, and logging.

## Testing

- **Directory**: `tests/`
- **Description**: Contains test cases for the interpreter and sample PL/I and HLASM files. The tests ensure that the lexer, parser, code generator, and interpreter work correctly and produce the expected results.

## Conclusion

This design document outlines the structure and components of the PL/I to HLASM generator project. Each module plays a crucial role in the overall functionality, from parsing the source code to executing the generated assembly. The project aims to provide a robust and efficient toolchain for PL/I developers.