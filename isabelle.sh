#!/bin/bash


LAUNCHER=$(readlink -f "$0")
LAUNCHER_DIR=$(dirname "$LAUNCHER")

export USER_HOME=$LAUNCHER_DIR/

$LAUNCHER_DIR/isabelle/bin/isabelle "$@"