#!/bin/bash

# Functions for transitioning a tree between old-style and submodule libraries/*
ghc79_libs="base ghc-prim integer-gmp integer-simple template-haskell"

function to_ghc79() {
        for d in $ghc79_libs; do
                rm -Rf libraries/$d/.git
        done
        ./sync-all checkout master
}

function from_ghc79() {
        for d in $ghc79_libs; do
                rm -Rf libraries/$d
        done
        ./sync-all get
        ./sync-all checkout ghc-7.8
        ./boot
}

