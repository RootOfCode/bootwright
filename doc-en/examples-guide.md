# Examples Guide

## Basics

### `examples/basics/demo-real.bwo`

Small real-mode example with BIOS text output.

Use it when you want to understand:

- boot sector flow
- real-mode setup
- BIOS screen output

### `examples/basics/demo-assets.bwo`

Shows source-relative asset inclusion with `include-binary`.

### `examples/basics/demo-scroll.bwo`

Shows framebuffer scrolling helpers.

## Protected Mode

### `examples/protected/demo-protected.bwo`

Protected-mode bootstrap with GDT, IDT, paging, and a software interrupt path.

### `examples/protected/demo-paging.bwo`

Focused paging example using `page-structures`, `map-page`, and `unmap-page`.

## Devices

### `examples/devices/demo-timer.bwo`

Uses IRQ0 and timer helpers to update a visible timer.

### `examples/devices/demo-probe.bwo`

Combines VGA, UART, speaker, and control-register inspection.

### `examples/devices/demo-keyboard.bwo`

Interactive keyboard echo example.

### `examples/devices/demo-ata.bwo`

Block-device demo for ATA PIO reads.

### `examples/devices/demo-memmap.bwo`

BIOS E820 probing and iteration.

### `examples/devices/demo-physalloc.bwo`

Bootstrap physical-page allocator example.

## CPU and Runtime

### `examples/cpu/demo-cpuinfo.bwo`

Uses `cpuid` and screen printing helpers.

### `examples/cpu/demo-bench.bwo`

Uses `rdtsc` for a small boot-time benchmark.

### `examples/cpu/demo-memops.bwo`

Exercises memory operands and runtime copy/fill helpers.

### `examples/cpu/demo-runtime-utils.bwo`

Focused runtime-helper coverage image.

### `examples/cpu/demo-sysinstr.bwo`

System-instruction demo for CPL=0 behavior.

## Execution

### `examples/exec/demo-context.bwo`

Cooperative context switching with execution slots.

### `examples/exec/demo-syscalls.bwo`

Syscall-table dispatch through INT 0x80.

### `examples/exec/demo-userspace.bwo`

User/kernel transition-related example.

### `examples/exec/demo-sysenter.bwo`

System-call fastpath setup example.

## Personality System

### `examples/personality/demo-personality.bwo`

Reference example for `.bwp` personalities.

## Lisp-OS

### `examples/lisp-os/demo-lisp-os.bwo`

The largest shipped example.

Use it as the reference for:

- multi-file organization
- UI composition
- keyboard input path
- a Bootwright-based shell
- a small Lisp-style interpreter

Related files:

- `examples/lisp-os/lib/architecture.bwo`
- `examples/lisp-os/lib/config.bwo`
- `examples/lisp-os/lib/interpreter.bwo`
- `examples/lisp-os/lib/ui.bwo`
- `examples/lisp-os/lib/filesystem.bwo`
- `examples/lisp-os/lib/keyboard.bwo`
- `examples/lisp-os/lib/shell.bwo`
