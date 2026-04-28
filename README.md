# Bootwright

Bootwright is an SBCL/Common Lisp framework for bare-metal operating system development. It includes:

- a custom assembler
- a Bootwright OS source format with `.bwo`
- a machine-description format with `.bwm`
- a personality format with `.bwp`
- a growing DSL for BIOS x86 OS work

This root `README.md` is the documentation hub.

## Documentation

- English: [doc-en/README.md](doc-en/README.md)
- Português (Brasil): [doc-ptbr/README.md](doc-ptbr/README.md)

## Quick Links

- Machine description: [machines/pc-bios.bwm](machines/pc-bios.bwm)
- Build script: [build.lisp](build.lisp)
- Test script: [test.lisp](test.lisp)
- Core DSL: [src/dsl.lisp](src/dsl.lisp)
- Assembler: [src/assembler.lisp](src/assembler.lisp)
- Lisp-OS example: [examples/lisp-os/demo-lisp-os.bwo](examples/lisp-os/demo-lisp-os.bwo)

## What Bootwright Targets Today

Bootwright is currently focused on legacy BIOS x86 development:

- 16-bit boot entry
- 32-bit protected mode
- VGA text mode
- BIOS disk/video services
- PIC, PIT, UART, keyboard, paging, memory maps, and related low-level primitives

It is appropriate for hobby OSes, experiments, teaching, and systems research on BIOS-compatible x86 machines or emulators such as QEMU.
