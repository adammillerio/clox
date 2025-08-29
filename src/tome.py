#!/usr/bin/env python3
from pathlib import Path
from typing import Tuple

import click
from smn import Context, pass_context, tome
from smn.utils import DefaultGroup

from aecode.lib.base.config import AECODE_LOCAL_DIR

# //local/craftinginterpreters
REPO_DIR: Path = AECODE_LOCAL_DIR / "craftinginterpreters"
REPO_URL = "https://github.com/munificent/craftinginterpreters.git"

CLOX_TARGET = "//aemisc/clox:cli"


@tome.group(
    name="clox",
    cls=DefaultGroup,
    default_if_no_args=True,
    short_help="clox programming language",
)
def clox() -> None:
    pass


@clox.command(
    "smn-run",
    default=True,
    help="run clox cli",
    context_settings={"ignore_unknown_options": True},
)
@click.argument("command", nargs=-1, type=click.UNPROCESSED)
@pass_context
def smn_run(ctx: Context, command: Tuple[str, ...]) -> None:
    ctx.exec_buck_entrypoint("//aemisc/clox:cli", command)


@clox.group("test", short_help="clox test suite")
def clox_test() -> None:
    pass


@clox_test.command("clone")
@pass_context
def clox_test_clone(ctx: Context) -> None:
    """clone munificent/craftinginterpreters to run tests"""

    ctx.run(f"git clone '{REPO_URL}' '{REPO_DIR}'", echo=True)


@clox_test.command("setup")
@pass_context
def clox_test_setup(ctx: Context) -> None:
    """install the dart language for tests

    The last version of Dart 2 is installed as 3 requires null safety:

    https://github.com/munificent/craftinginterpreters/issues/1122
    """

    ctx.run("brew install dart-lang/dart/dart@2.19", echo=True)
    ctx.run("brew link dart-lang/dart/dart@2.19", echo=True)


@clox_test.command("build")
@pass_context
def clox_test_build(ctx: Context) -> None:
    """build the dart test binary"""

    with ctx.cd(REPO_DIR):
        ctx.run("make get", echo=True)
        ctx.run("make debug build/test.dart.snapshot", echo=True)


@clox_test.command("run")
@pass_context
def clox_test_run(ctx: Context) -> None:
    """run clox test suite"""

    with ctx.from_aesource():
        bin = ctx.run(
            f"buck2 run --emit-shell {CLOX_TARGET}",
            pty=False,
            echo=True,
            hide=ctx.show_debug,
        ).stdout.rstrip()

    with ctx.cd(REPO_DIR):
        ctx.run(f"dart 'build/test.dart.snapshot' -i '{bin}' clox", echo=True)
