# Bootwright

Bootwright is an SBCL/Common Lisp bare-metal OS framework with its own assembler, machine-description format, and source-file format for OS images.

The framework itself is implemented in `.lisp`, but operating systems can be written as Bootwright source files with the `.bwo` extension, while hardware targets live in `.bwm` machine files. Both still use ordinary Lisp syntax and are loaded with the normal Common Lisp reader.

## Bootwright Source Files

Bootwright OS source files are loaded into the `bootwright.os` package automatically, so they do not need an explicit `in-package` form. They can load a machine descriptor, then use `defos` directly:

```lisp
(use-machine #P"../machines/pc-bios.bwm")

(defos *demo-os* (:machine pc-bios)
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

The included [pc-bios.bwm](machines/pc-bios.bwm) machine file describes the current x86 BIOS target: boot protocol, memory regions, PIC, PIT, COM1, and the VGA text framebuffer.

The included examples are:

- [demo-real.bwo](examples/basics/demo-real.bwo) — real-mode kernel with BIOS text output.
- [demo-protected.bwo](examples/protected/demo-protected.bwo) — protected-mode entry, paging, IDT, and a software interrupt handler.
- [demo-assets.bwo](examples/basics/demo-assets.bwo) — embedding a binary asset relative to the source file.
- [demo-timer.bwo](examples/devices/demo-timer.bwo) — PIT-driven IRQ0 with a live `MM:SS` clock on screen.
- [demo-probe.bwo](examples/devices/demo-probe.bwo) — system probe that samples CR0 in hex, mirrors a structured boot log to COM1, and runs an uptime counter.
- [demo-cpuinfo.bwo](examples/cpu/demo-cpuinfo.bwo) — issues `CPUID(0)` and `CPUID(1)` and renders the live vendor string, signature, and feature flags on screen.
- [demo-bench.bwo](examples/cpu/demo-bench.bwo) — boot-time microbench: times an iterative Fibonacci F(20) loop with `rdtsc`, prints the cycle count in decimal, and uses `include` to pull its GDT body from [lib/gdt-flat.bwo](examples/cpu/lib/gdt-flat.bwo).
- [demo-memops.bwo](examples/cpu/demo-memops.bwo) — exercises `(:mem ...)`, `lea`, `movzx`, `movsx`, `rep movsd`, `rep stosd`, and the generic timer helpers in one protected-mode image.
- [demo-sysinstr.bwo](examples/cpu/demo-sysinstr.bwo) — exercises CPL=0 system instructions: `RDMSR(IA32_TSC)`, an atomic `LOCK ADD` on a counter, `INVLPG` via `tlb-flush-page`, `MOV CR3,CR3` via `tlb-flush-all`, and `MFENCE` via `memory-barrier`.
- [demo-keyboard.bwo](examples/devices/demo-keyboard.bwo) — interactive PS/2 keyboard echo: declares a `keyboard-driver`, wires IRQ1, blocks on `keyboard-read`, then mirrors each pressed key to a VGA cell and to debugcon. Run in a QEMU window and type.
- [demo-ata.bwo](examples/devices/demo-ata.bwo) — declares a primary-bus ATA-PIO LBA28 `block-device`, reads sector 0 of the IDE companion disk, and verifies the `0xAA55` boot signature.
- [demo-memmap.bwo](examples/devices/demo-memmap.bwo) — declares a `memory-map` backed by INT 15h E820, runs `memory-map-probe` in real mode before `enter-protected-mode`, then renders the entry count plus the first region's base/length/type on screen.

## DSL Features

Bootwright currently supports:

- Boot sectors, kernel sections, generic sections, routines, labels, and raw data directives.
- A built-in x86 assembler for the early-boot instruction subset used by the framework.
- Direct memory operands via `(:mem ...)`, including absolute addresses, base+displacement, and base+index*scale+displacement in 32-bit code.
- Machine-backed targets via `.bwm` files and `defmachine`, with `use-machine` and `:machine` support in `.bwo` files.
- Real-mode setup, protected-mode entry, GDT/IDT declarations, paging structures, and boot-info handoff.
- BIOS text output, VGA text output, QEMU debug output, and VGA screen clearing.
- Real-mode cursor placement via `bios-set-cursor` and `bios-print-at`.
- Compile-time constants via `const`, reusable zero-terminated strings via `string`, and simple code/data repetition via `repeat`.
- Source-relative asset embedding via `include-binary`.
- Screen layout helpers via `vga-print-at` and `vga-box`.
- Machine-resolved framebuffer helpers via `framebuffer-clear`, `framebuffer-print-at`, and `framebuffer-box`.
- Live 32-bit hex printing via `vga-print-hex` (literal or 32-bit register source).
- Live 32-bit decimal printing via `vga-print-decimal` (10-digit zero-padded; uses the new `div`).
- 4-character ASCII printing from a 32-bit register via `vga-print-ascii4` (useful for `CPUID` vendor strings).
- VGA hardware cursor placement via `vga-set-cursor`.
- COM1 serial output via `serial-init` and `serial-print` — pairs with `-serial stdio` for headless logging.
- Machine-resolved UART forms via `uart-init`, `uart-print`, `uart-write-byte`, and `uart-read-byte`.
- PC speaker tones via `pc-speaker-tone` and `pc-speaker-off` (PIT channel 2).
- Real-mode helpers like `set-video-mode` and `enable-a20-fast`.
- Interrupt-controller helpers like `pic-remap`, `pic-mask-all`, `pic-mask`, `pic-unmask`, and `pic-eoi`.
- PIT programming via `pit-set-frequency`.
- Generic machine-backed IRQ and timer helpers via `irq-remap`, `irq-mask`, `irq-unmask`, `irq-end-of-interrupt`, `timer-set-frequency`, `timer-enable`, `timer-disable`, and `timer-read-counter`.
- ISA-generic memory and TLB primitives via `memory-barrier` (`:load`/`:store`/full), `tlb-flush-page`, `tlb-flush-all`, and `load-tr` (Phase 4 starter set).
- PS/2 keyboard support via `keyboard-driver` (declares ring buffer + scancode table + IRQ1 handler), `keyboard-read` (blocking), and `keyboard-poll` (non-blocking with `:empty` jump target). US-QWERTY scancode set 1; user wires the handler in their own IDT.
- ATA-PIO LBA28 disk support via `block-device` (primary/secondary bus, 512-byte sectors), `block-read` (sector → buffer), and `block-write` (buffer → sector + cache flush). The bundled testing harness attaches every image as both floppy (boot) and IDE master (data), so `block-read` against the image's own sector 0 round-trips the boot signature.
- BIOS E820 memory probing via `memory-map` (declares count + entry buffer), `memory-map-probe` (real-mode INT 15h fill), `memory-map-base` / `memory-map-count` (load entries pointer / count into a register).
- Bootwright-native source loading/building through `load-os-source` and `build-os-source-file`.
- Headless QEMU verification via `test-os` and `test-os-source-file`.
- Multi-file source composition via `(include "other.bwo" ...)` — includes are resolved relative to the calling file and run in the same package, so a library file can define Lisp helpers that the main file splices into a `defos` body with `#.` (read-time eval).

The assembler supports:

- 8-bit, 16-bit, and 32-bit immediate/register moves for the subset used by the framework.
- Register/memory moves and arithmetic using `(:mem ...)` operands in 32-bit code, plus direct absolute memory references in 16-bit code.
- Relative branches and calls in 16-bit and 32-bit code.
- Control-register moves, `lgdt`, `lidt`, `iret`, `in`, `out`, far jumps, and string ops used for early boot work.
- `lea`, `movzx`, `movsx`, `xchg`, `movsd`, `stosd`, `rep`, `pushad`, `popad`, and explicit near conditional branches like `jne-near`.
- Arithmetic (`add`, `sub`, `mul`, `div`, `not`, `neg`), bitwise (`xor`, `or`, `and`, `test`), shifts (`shl`, `shr`), and the unsigned/signed conditional-jump family (`je`/`jne`/`jb`/`jae`/`jbe`/`ja`/`jl`/`jge`/`jle`/`jg`/`js`/`jns` plus the `jz`/`jnz`/`jc`/`jnc` aliases).
- `loop`, `pusha`/`popa`, `pushfd`/`popfd` for tighter handler/loop bodies.
- `cpuid` for CPU identification and `rdtsc` for cycle counting.
- CPL=0 system ops: `wrmsr`/`rdmsr`, `clts`, `ltr`, `invlpg`, `lret`/`retf`, `cmpxchg`, the `lock` prefix wrapper, and the `mfence`/`lfence`/`sfence` memory fences.
- String I/O ops: `insb`/`insw`/`insd` and `outsb`/`outsw`/`outsd` (16/32-bit operand size selected by mode + prefix).

Memory operands follow Lisp syntax and can optionally declare width when needed:

```lisp
(mov eax (:mem :dword scratch-value))
(mov (:mem :dword scratch-value) eax)
(mov esi (:mem :dword ebx ecx 4 0))
(movzx ebp (:mem :byte unsigned-byte))
(rep movsd)
```

## Build

From `bootwright/`:

```bash
sbcl --script build.lisp
```

That writes:

- `out/basics/bootwright-demo.img`
- `out/protected/bootwright-protected.img`
- `out/basics/bootwright-assets.img`
- `out/devices/bootwright-timer.img`
- `out/devices/bootwright-probe.img`
- `out/cpu/bootwright-cpuinfo.img`
- `out/cpu/bootwright-bench.img`
- `out/cpu/bootwright-memops.img`
- `out/cpu/bootwright-sysinstr.img`
- `out/devices/bootwright-keyboard.img`
- `out/devices/bootwright-ata.img`
- `out/devices/bootwright-memmap.img`

You can also build a `.bwo` file directly from Lisp:

```lisp
(bootwright:build-os-source-file #P"examples/protected/demo-protected.bwo")
```

## Test

Bootwright can also build and boot an image under headless QEMU, then assert on debugcon or serial output:

```lisp
(bootwright:test-os-source-file
 #P"examples/protected/demo-protected.bwo"
 :timeout-ms 5000
 :expect-debugcon '("stage1: loading protected-mode demo"
                    "kernel: protected mode + paging active"
                    "kernel: returned from int 0x30"))
```

There is also a ready-made script:

```bash
sbcl --script test.lisp
```

## Run

Normal VGA window:

```bash
qemu-system-i386 -drive format=raw,file=out/protected/bootwright-protected.img,if=floppy
```

Headless debug output:

```bash
qemu-system-i386 \
  -display none \
  -drive format=raw,file=out/protected/bootwright-protected.img,if=floppy \
  -monitor none -serial none -parallel none \
  -global isa-debugcon.iobase=0xe9 \
  -debugcon stdio
```

## Example Notes

- `demo-assets.bwo` shows `include-binary` loading [kernel-banner.txt](examples/basics/assets/kernel-banner.txt) relative to the source file.
- `demo-protected.bwo` uses the machine-backed `framebuffer-*` and `irq-remap` forms for its protected-mode status panel.
- `demo-timer.bwo` uses the generic `timer-set-frequency`, `irq-unmask`, and `irq-end-of-interrupt` forms and keeps a live `MM:SS` timer updated on the VGA screen from IRQ0.
- `demo-probe.bwo` reads the live `CR0` value via `mov eax, cr0`, prints it with `vga-print-hex`, mirrors a labelled boot log over COM1 via `uart-init`/`uart-print` (run with `-serial stdio` to capture), beeps once via `pc-speaker-tone`, and parks the VGA hardware cursor in the corner.
- `demo-cpuinfo.bwo` runs `cpuid` twice and uses `vga-print-ascii4` to render the 12-byte vendor string (e.g. `GenuineIntel`) plus the signature and feature dwords as hex; the values reflect the underlying CPU and respond to QEMU's `-cpu` option.
- `demo-bench.bwo` brackets a 20-iteration Fibonacci loop with `rdtsc`, prints the resulting cycle delta in 10-digit zero-padded decimal via `vga-print-decimal`, and pulls its GDT body from `lib/gdt-flat.bwo` via `(include "lib/gdt-flat.bwo")` plus `#.(flat-gdt 'gdt :pointer 'gdt-descriptor)`.
- `demo-memops.bwo` validates runtime dword stores/loads, `REP MOVSD`, `REP STOSD`, `MOVZX`, `MOVSX`, and a PIT counter latch/read, then reports success over QEMU debugcon.
- `demo-sysinstr.bwo` issues `RDMSR(0x10)`, builds an atomic counter with `LOCK ADD` followed by `MEMORY-BARRIER`, then runs `TLB-FLUSH-PAGE`/`TLB-FLUSH-ALL` to exercise the Phase 4 memory and TLB primitives.
- `demo-keyboard.bwo` is interactive: it remaps the PIC to unmask IRQ1 only, hands off to a `keyboard-driver`-emitted scancode handler, and runs `keyboard-read` in a tight loop that echoes each typed character to row 2 column 22 and to debugcon. Verified under QEMU by sending `sendkey a b c ret` through the monitor pipe.
- `demo-ata.bwo` issues a one-sector ATA-PIO read of LBA 0 (boot sector of the IDE companion disk attached by `testing.lisp`), then validates that the word at offset 510 of the 512-byte buffer equals `0xAA55`.
- `demo-memmap.bwo` runs `memory-map-probe` (INT 15h E820) in the kernel's real-mode entry routine, before `enter-protected-mode`, then renders the entry count and first region in protected mode. Asserts that QEMU's BIOS returned at least one entry.
