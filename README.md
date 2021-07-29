# Isabelle linter
A linter for isabelle

## How to use

### Setup
1. Checkout Isabelle submodule: `git submodule init && git submodule update`
2. Compile the linter: `./sbt "project linter" assembly`
3. For jEdit support: Patch jEdit jars: `./patch_jedit`
4. For the tool support: `isabelle/bin/isabelle components -u linter/target`

### Usage
- jEdit: `isabelle/bin/isabelle jedit -e`
- tool: `isabelle/bin/isabelle lint`. Use `isabelle/bin/isabelle lint -?` to
  see the tool options
