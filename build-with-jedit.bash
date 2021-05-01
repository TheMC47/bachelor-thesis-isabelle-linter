#!/usr/bin/env bash
set -euo pipefail
echo "Building jEdit..."
./isabelle/bin/isabelle jedit -b
echo "Building the project..."
./sbt "project linter" package
echo "Launching jEdit with extras..."
./isabelle/bin/isabelle jedit -e
