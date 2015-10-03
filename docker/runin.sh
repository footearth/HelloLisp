#!/usr/bin/env bash

set -e

docker run \
  --name=hello-common-lisp \
  --rm \
  -ti \
  -v `pwd`:/root/commonlisp \
  mooxe/commonlisp \
  /bin/bash
