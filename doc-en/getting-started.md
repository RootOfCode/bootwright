# Getting Started

## Requirements

- SBCL
- ASDF
- QEMU for emulator-based testing

Bootwright is designed around SBCL. The framework is loaded as a normal Common Lisp system.

## Build All Shipped Images

From the project root:

```bash
sbcl --script build.lisp
```

This builds the shipped example images into `out/`.

## Run the Test Suite

```bash
sbcl --script test.lisp
```

The test suite builds images and boots the supported examples headlessly under QEMU, then checks debug output or serial output.

## Boot an Example

Example:

```bash
qemu-system-i386 -drive format=raw,file=out/protected/bootwright-protected.img,if=floppy
```

For `lisp-os`:

```bash
qemu-system-i386 -drive format=raw,file=out/lisp-os/bootwright-lisp-os.img,if=floppy
```

## Build a Single `.bwo` Source File

You can build one source file directly:

```lisp
(bootwright:build-os-source-file #P"examples/protected/demo-protected.bwo")
```

If no output is specified, Bootwright writes the result under `out/`.

## Minimal Bootwright OS

```lisp
(use-machine #P"../machines/pc-bios.bwm")

(defos *demo-os* (:machine pc-bios)
  (boot-sector (:entry boot)
    (routine boot
      (initialize-real-mode :stack #x7C00)
      (set-video-mode 3)
      (bios-print "Booting...")
      (load-section kernel :on-error disk-error)
      (jump-section kernel))
    (routine disk-error
      (bios-print "Disk read failed.")
      (hang)))
  (kernel-section kernel (:load-segment #x1000 :entry kernel-main)
    (routine kernel-main
      (initialize-real-mode :stack #x9000)
      (bios-print "Hello from Bootwright.")
      (hang))))
```

## Practical Advice

- Start from `examples/basics` or `examples/protected`.
- Use `demo-lisp-os.bwo` as the reference for a larger multi-file OS layout.
- Prefer building and testing through QEMU first.
- Treat the current target as BIOS x86, not “all real hardware”.
