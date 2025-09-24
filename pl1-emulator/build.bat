@echo off
echo 🔨 Building PL/I Parser...

REM Check if dune is installed
dune --version >nul 2>&1
if errorlevel 1 (
    echo ❌ Dune is not installed. Please install it with: opam install dune
    exit /b 1
)

REM Check if menhir is installed
menhir --version >nul 2>&1
if errorlevel 1 (
    echo ❌ Menhir is not installed. Please install it with: opam install menhir
    exit /b 1
)

REM Build the project
dune build

if errorlevel 1 (
    echo ❌ Build failed!
    exit /b 1
) else (
    echo ✅ Build successful!
    echo 🚀 Run with: dune exec pl1-parser sample.pl1
)
