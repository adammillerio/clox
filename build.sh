#!/usr/bin/env bash
ACTION=${1:-'release'}

function build_clean() {
    rm -rf build
}

function build_release() {
    MODE=release NAME=clox SOURCE_DIR=src make
}

function build_debug() {
    MODE=debug NAME=cloxd SOURCE_DIR=src make
}

function clone_repo() {
    if ! [ -d "craftinginterpreters" ]; then
        git clone 'https://github.com/munificent/craftinginterpreters.git'
    fi
}

function build_install_dart() {
    brew install dart-lang/dart/dart@2.19
    brew link dart-lang/dart/dart@2.19
}

function build_test() {
    build_release
    clone_repo

    pushd craftinginterpreters || return

    make get
    make debug build/test.dart.snapshot
    dart 'build/test.dart.snapshot' -i '../build/clox' clox

    popd || return
}

BUILD_ACTION="build_${ACTION}"
${BUILD_ACTION}
