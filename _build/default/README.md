Gastrolith - lightweight HLASM/PL1 parser and linter

This workspace contains a refactored HLASM parser and a small `pl1-lint` binary
that demonstrates how to run parsing and emit modern console-style warnings.

Build

  dune build

Run linter

  dune exec -- ./bin/pl1_lint.exe -- <file.pl1>

Notes

- The linter currently implements a few sample checks (undefined symbols, long lines).
- The parser modules are in the repository (hlasm_*.ml). The lint binary reuses them.
- Future: add more lint rules, integrate with CI, output JSON/clang-format compatible reports.
