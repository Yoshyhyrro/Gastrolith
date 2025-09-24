#!/usr/bin/env bash
# Build script for PL/I Parser

echo "🔨 Building PL/I Parser..."

# Check if dune is installed
if ! command -v dune &> /dev/null; then
    echo "❌ Dune is not installed. Please install it with: opam install dune"
    exit 1
fi

# Check if menhir is installed
if ! command -v menhir &> /dev/null; then
    echo "❌ Menhir is not installed. Please install it with: opam install menhir"
    exit 1
fi

# Build the project
dune build

if [ True -eq 0 ]; then
    echo "✅ Build successful!"
    echo "🚀 Run with: dune exec pl1-parser sample.pl1"
else
    echo "❌ Build failed!"
    exit 1
fi
