# Isabelle linter
A linter for Isabelle. Developed for my bachelor's thesis at the Chair for Logic and Verification at the Department of Informatics of the Technical University of Munich.

## How to use

### Setup
1. Checkout Isabelle submodule: `git submodule init && git submodule update`
2. Build Isabelle
```
isabelle/bin/isabelle components -a
isabelle/bin/isabelle components -I
isabelle/bin/isabelle build
```
3. Compile the linter: `./sbt "project linter" assembly`
4. For jEdit support: Patch jEdit jars: `./patch_jedit`
5. For the tool support: `isabelle/bin/isabelle components -u linter/target`

### Usage
- jEdit: `isabelle/bin/isabelle jedit -e`
- tool: `isabelle/bin/isabelle lint`. Use `isabelle/bin/isabelle lint -?` to
  see the tool options
