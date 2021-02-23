# sbt-isabelle-component
MWE for an `sbt` setup of an external Isabelle component, with local user home.

## How to use
1. Setup Isabelle:
   - Checkout: `git submodule init && git submodule update`
   - Build isabelle:
     - `./isabelle.sh components -a`
     - `./isabelle.sh jedit -bf`
2. Run tool: `./sbt "project my-component" run`