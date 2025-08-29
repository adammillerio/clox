#!/usr/bin/env bash
SCRIPT_DIR=$(dirname "${BASH_SOURCE[0]}")

if ! [ -f "${SCRIPT_DIR}/build/clox" ]; then
    "${SCRIPT_DIR}/build.sh" release
fi

"${SCRIPT_DIR}/build/clox" ${@}

