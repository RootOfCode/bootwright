# Bootwright

Bootwright is an SBCL/Common Lisp bare-metal OS framework with its own assembler and its own source-file format for OS images.

The framework itself is implemented in `.lisp`, but operating systems can now be written as Bootwright source files with the `.bwo` extension. They still use ordinary Lisp syntax and are loaded with the normal Common Lisp reader.

## Bootwright Source Files

Bootwright OS source files are loaded into the `bootwright.os` package automatically, so they do not need an explicit `in-package` form. They use `defos` directly:

```lisp
(defos *demo-os* (:target (:x86-bios :kernel-segment #x1000))
  (boot-sector (:entry boot)
    (routine boot
      (initialize-real-mode :stack #x7C00)
      (set-video-mode 3)
      (enable-a20-fast)
      (bios-print "Bootwright stage 1 loading kernel...")
      (load-section kernel :on-error disk-error)
      (jump-section kernel))
    (routine disk-error
      (bios-print "Disk read failed.")
      (hang)))
  (kernel-section kernel (:load-segment #x1000 :entry kernel-main)
    (const kernel-stack #x9000)
    (string banner "Hello from Bootwright.")
    (routine kernel-main
      (initialize-real-mode :stack kernel-stack)
      (bios-print banner)
      (hang))))
```

The included examples are:

- [demo-real.bwo](examples/demo-real.bwo)
- [demo-protected.bwo](examples/demo-protected.bwo)
- [demo-assets.bwo](demo-assets.bwo)
- [demo-timer.bwo](examples/demo-timer.bwo)

## DSL Features

Bootwright currently supports:

- Boot sectors, kernel sections, generic sections, routines, labels, and raw data directives.
- A built-in x86 assembler for the early-boot instruction subset used by the framework.
- Real-mode setup, protected-mode entry, GDT/IDT declarations, paging structures, and boot-info handoff.
- BIOS text output, VGA text output, QEMU debug output, and VGA screen clearing.
- Real-mode cursor placement via `bios-set-cursor` and `bios-print-at`.
- Compile-time constants via `const`, reusable zero-terminated strings via `string`, and simple code/data repetition via `repeat`.
- Source-relative asset embedding via `include-binary`.
- Screen layout helpers via `vga-print-at` and `vga-box`.
- Real-mode helpers like `set-video-mode` and `enable-a20-fast`.
- Interrupt-controller helpers like `pic-remap`, `pic-mask-all`, `pic-mask`, `pic-unmask`, and `pic-eoi`.
- PIT programming via `pit-set-frequency`.
- Bootwright-native source loading/building through `load-os-source` and `build-os-source-file`.

The assembler supports:

- 8-bit, 16-bit, and 32-bit immediate/register moves for the subset used by the framework.
- Relative branches and calls in 16-bit and 32-bit code.
- Control-register moves, `lgdt`, `lidt`, `iret`, `in`, `out`, far jumps, and string ops used for early boot work.

## Build

From `bootwright/`:

```bash
sbcl --script build.lisp
```

That writes:

- `out/bootwright-demo.img`
- `out/bootwright-protected.img`
- `out/bootwright-assets.img`
- `out/bootwright-timer.img`

You can also build a `.bwo` file directly from Lisp:

```lisp
(bootwright:build-os-source-file #P"examples/demo-protected.bwo")
```

## Run

Normal VGA window:

```bash
qemu-system-i386 -drive format=raw,file=out/bootwright-protected.img,if=floppy
```

Headless debug output:

```bash
qemu-system-i386 \
  -display none \
  -drive format=raw,file=out/bootwright-protected.img,if=floppy \
  -monitor none -serial none -parallel none \
  -global isa-debugcon.iobase=0xe9 \
  -debugcon stdio
```

## Example Notes

- `demo-assets.bwo` shows `include-binary` loading [kernel-banner.txt](/home/bruno/Documentos/codex/bootwright/examples/assets/kernel-banner.txt) relative to the source file.
- `demo-protected.bwo` uses `vga-box` and `vga-print-at` for its protected-mode status panel.
- `demo-timer.bwo` remaps the PIC, programs the PIT, unmasks IRQ0, and keeps a live `MM:SS` timer updated on the VGA screen from IRQ0.
