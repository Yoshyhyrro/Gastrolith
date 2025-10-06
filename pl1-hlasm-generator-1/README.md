# PL/I to HLASM Generator

This project is a PL/I to HLASM (High-Level Assembler) code generator implemented in OCaml. It includes a lexer, parser, code generator, and an interpreter for executing the generated assembly code.

## Features

- **Lexer**: Uses OCamllex to tokenize PL/I source code.
- **Parser**: Utilizes OCamlly to parse tokens into an abstract syntax tree (AST).
- **Code Generation**: Transforms the AST into HLASM code.
- **Interpreter**: Reads and executes the generated HLASM code.

## Installation

1. Ensure you have OCaml and Dune installed on your system.
2. Clone the repository:
   ```
   git clone <repository-url>
   cd pl1-hlasm-generator
   ```
3. Build the project using Dune:
   ```
   dune build
   ```

## Usage

To run the PL/I to HLASM generator, use the following command:

```
dune exec ./src/main.exe <path-to-pl1-file>
```

Replace `<path-to-pl1-file>` with the path to your PL/I source code file.

## Testing

To run the tests, execute:

```
dune runtest
```

## Contributing

Contributions are welcome! Please open an issue or submit a pull request for any enhancements or bug fixes.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.