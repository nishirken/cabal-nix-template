#!/usr/bin/env bash

ormolu --mode inplace $(find . -name '*.hs')
