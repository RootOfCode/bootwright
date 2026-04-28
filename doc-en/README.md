# Bootwright Documentation

This directory contains the English documentation for Bootwright.

## Contents

- [Getting Started](getting-started.md)
- [Core Concepts](core-concepts.md)
- [How the DSL Works](how-the-dsl-works.md)
- [DSL Reference](dsl-reference.md)
- [Examples Guide](examples-guide.md)

## What Bootwright Is

Bootwright is a Common Lisp framework for building bare-metal operating systems. It lets you describe:

- OS images with `.bwo`
- target machines with `.bwm`
- DSL extensions with `.bwp`

The framework itself is implemented in SBCL/Common Lisp, but the operating systems you write in Bootwright still use normal Lisp syntax.

## Current Scope

Bootwright is currently centered on BIOS-compatible x86 systems. The shipped toolchain and examples focus on:

- boot sectors
- real mode
- protected mode
- VGA text mode
- paging
- interrupts
- timer, UART, keyboard, ATA, memory map, and execution-context primitives

It is not yet a general-purpose UEFI or multi-architecture production OS platform.

## Recommended Reading Order

1. [Getting Started](getting-started.md)
2. [Core Concepts](core-concepts.md)
3. [How the DSL Works](how-the-dsl-works.md)
4. [DSL Reference](dsl-reference.md)
5. [Examples Guide](examples-guide.md)
