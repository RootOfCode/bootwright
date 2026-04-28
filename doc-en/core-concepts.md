# Core Concepts

## File Types

### `.bwo`

Bootwright OS source files.

They define images with `defos` and are loaded automatically into the `bootwright.os` package, so they do not need an explicit `in-package`.

### `.bwm`

Bootwright machine files.

These describe the target machine model: ISA, boot protocol, memory regions, interrupt controller, timer, serial devices, and framebuffer devices.

### `.bwp`

Bootwright personality files.

A personality adds higher-level DSL forms and section abstractions on top of the core system.

## Image Model

A Bootwright image is usually composed of:

- one boot sector
- one or more kernel or generic sections

Sections have:

- a kind
- a load address
- an entry label
- their own compiled body

The boot sector typically loads later sections and transfers control to the kernel entry.

There is an important separation between:

- image-level structure, which decides what sections exist and where they live on disk and in memory
- section-level structure, which decides what bytes get emitted inside each section

At the image level, `boot-sector`, `kernel-section`, and `section` are the main building blocks. At the section level, forms like `routine`, `string`, `label`, `db`, `gdt`, and raw instructions define the actual contents.

## Execution Model

Bootwright currently emphasizes BIOS x86:

- boot starts in 16-bit real mode
- code may switch into 32-bit protected mode
- paging and interrupts can then be configured

The shipped machine description `pc-bios.bwm` models that environment.

The compiler therefore works across two address models at once:

- disk layout, where sections occupy sectors in the final image
- runtime layout, where sections are loaded to memory segments or origins

Forms such as `load-section` and `jump-section` depend on that precomputed layout.

## DSL Philosophy

The DSL is Lisp syntax over low-level machine work. It tries to provide:

- direct control over layout and instructions
- reusable machine-backed helpers
- explicit forms for things like GDT, IDT, paging, IRQs, block I/O, and memory maps

Where the DSL does not abstract something, you can still fall back to the assembler layer.

In practice, Bootwright behaves like a mixed language:

- some forms are declarations consumed at build time
- some forms emit bytes directly
- some forms generate target-aware helper code
- unknown forms in a routine body fall through to the assembler

That last rule is what makes the DSL practical for OS work: you are not trapped when the high-level layer does not have a dedicated form yet.

## Build Pipeline

The normal path from source to image is:

1. SBCL loads the `.bwo` file.
2. `defos` records an image specification.
3. Bootwright resolves the target machine.
4. Section forms are parsed into section specs.
5. Bootwright computes section layout and load addresses.
6. Each section body is compiled in order into bytes.
7. Labels and fixups are resolved.
8. The final image is written to `out/` or the requested output path.

This is why order matters inside sections, and why label-based references work even when the label is defined later in the file.

## Compile-Time Versus Run-Time

Inside Bootwright sources, ordinary Lisp still exists. You can define helper macros, constants, or include files at load time.

But forms inside section bodies should be read as image-building forms:

- `const` is a compile-time substitution
- `routine` emits a label and inline code
- `string`, `db`, `dw`, `dd`, `resb`, `resw`, `resd` emit bytes
- `initialize-real-mode`, `framebuffer-print-at`, `memory-map-probe` emit machine code

If you need runtime state, store it in emitted data, not in Lisp variables.

## Structured Control

Bootwright already had labels, jumps, mutable memory, and calls. The DSL now also exposes structured control forms for unbounded computation:

- `forever`
- `while-zero`
- `while-nonzero`
- `if-zero`
- `if-nonzero`

These are convenience forms over the same low-level execution model.

Their semantics are intentionally simple:

- `if-zero` and `if-nonzero` compare an operand against zero
- `while-zero` and `while-nonzero` loop by testing an operand against zero
- `forever` emits an infinite loop

They are useful when you want the readability of structured flow without giving up exact machine-oriented control.

## Current Hardware Scope

Bootwright is not a universal hardware abstraction layer. Current examples assume:

- BIOS-compatible x86
- VGA text mode
- BIOS disk/video services
- PC-style PIC/PIT/UART/PS2 behavior

That means the framework is well suited for emulator-driven OS development and selected legacy or BIOS-compatible hardware.
