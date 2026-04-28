# DSL Reference

This is a practical reference to the Bootwright DSL. It focuses on meaning and usage, not a formal grammar.

## Reading the DSL Correctly

Inside a section body, most forms are not ordinary Lisp computation. They are compile directives that:

- emit bytes
- emit instructions
- register named machine subsystems
- request helper routines
- influence section layout

If a form is not recognized as a Bootwright DSL form or a personality form, Bootwright tries to assemble it as a raw instruction.

## Top-Level Source Forms

### `use-machine`

Loads a `.bwm` machine file so the image can target a named machine descriptor.

```lisp
(use-machine #P"../../machines/pc-bios.bwm")
```

### `include`

Loads additional `.bwo` source files relative to the current file.

```lisp
(include #P"lib/config"
         #P"lib/console"
         #P"lib/interpreter")
```

### `use-personality`

Loads a `.bwp` personality that adds custom section, data, or routine forms.

### `defos`

Defines one bootable image.

```lisp
(defos *demo* (:machine pc-bios)
  ...)
```

The `:machine` option is the normal way to bind the image to a target from a `.bwm` file.

## Section Forms

### `boot-sector`

Defines the stage-1 boot section.

```lisp
(boot-sector (:entry boot)
  ...)
```

Typical properties:

- origin comes from the boot protocol, usually `0x7C00`
- sector alignment comes from the machine boot protocol
- the entry label is usually the first boot routine

### `kernel-section`

Defines a loadable kernel section.

```lisp
(kernel-section kernel32 (:load-segment #x1000 :entry kernel-main)
  ...)
```

Common options:

- `:entry`
- `:load-segment`
- `:load-offset`
- `:origin`
- `:sector-align`

### `section`

Defines a more generic section when you do not want the defaults of `kernel-section`.

## Core Structural Forms

### `routine`

Emits a label and compiles the body inline at that position.

```lisp
(routine kernel-main
  ...)
```

`routine` is not a separate code object. It is just a named point in the current section.

### `label`

Marks the current offset inside the current section.

```lisp
(label loop-start)
```

### `const`

Compile-time substitution only.

```lisp
(const kernel-stack #x90000)
```

Use emitted data, not `const`, for mutable runtime state.

### `bits`

Changes instruction width for later emitted code in the same section.

```lisp
(bits 32)
```

### `arch`

Changes the assembler ISA token if needed for a section.

## Data and Layout Forms

### `string`

Emits an ASCII string, zero-terminated by default.

```lisp
(string banner "Kernel loaded.")
(string raw-text "ABC" :zero-terminated nil)
```

Options include:

- `:zero-terminated`
- `:z`
- `:align`

### `db`, `dw`, `dd`

Emit raw byte, word, or dword data.

```lisp
(db #xEB #x3C)
(dw #xAA55)
(dd #x0010F000)
```

### `resb`, `resw`, `resd`

Emit zero-filled storage.

```lisp
(resb 128)
(resw 16)
(resd 32)
```

### `ascii`, `ascii-z`

Emit ASCII bytes directly.

### `align`

Pads the current section offset to the requested alignment.

```lisp
(align 16)
```

### `pad-to`

Pads until the current section reaches a specific offset.

```lisp
(pad-to 510 0)
```

### `fill`

Emit a repeated fill byte.

### `repeat`

Repeat a body at build time.

```lisp
(repeat 4
  (db 0))
```

### `include-binary`

Embed a file's bytes directly into the current section.

## Control Flow Forms

### Raw Assembler Flow

You can always write:

- `jmp`
- `call`
- `ret`
- `iret`
- `cmp`
- `jz`
- `jnz`
- `jne-near`
- `ljmp16`
- `ljmp32`

### `forever`

Infinite loop around the body.

```lisp
(forever
  (hlt))
```

### `if-zero`, `if-nonzero`

Structured branching based on `cmp operand, 0`.

```lisp
(if-zero eax
  (:then
    (bios-print "zero"))
  (:else
    (bios-print "nonzero")))
```

### `while-zero`, `while-nonzero`

Structured loops based on repeated comparison with zero.

```lisp
(while-nonzero ecx
  (dec ecx))
```

### `hang`, `idle-forever`

Convenience stop/park loops.

## Boot and CPU Mode Forms

### `initialize-real-mode`

Sets up the common real-mode execution environment, especially stack state.

```lisp
(initialize-real-mode :stack #x7C00)
```

### `initialize-protected-mode`

Sets up data segments and stack after entering protected mode.

```lisp
(initialize-protected-mode :data-selector #x10
                           :stack-linear #x90000)
```

### `enter-protected-mode`

Switches from real mode to protected mode and transfers control to an entry label.

### `load-gdt`, `load-idt`, `load-tr`

Load descriptor tables or the task register.

### `enable-paging`

Enables paging with the given page-directory state.

### `sysenter-setup`, `syscall-setup`, `privilege-drop`

Lower-level system-entry and privilege-transition helpers.

## Descriptor and Paging Forms

### `gdt`

Defines a GDT and optionally its pointer structure.

```lisp
(gdt gdt (:pointer gdt-descriptor)
  (:null)
  (:code32 :base :section :limit #xAFFFF)
  (:data32 :base :section :limit #xAFFFF))
```

### `idt`

Defines an IDT and its entries.

### `boot-info`

Builds a boot information structure for handoff to later code.

### `page-structures`, `page-directory`, `page-table`

Define paging structures.

### `map-page`, `unmap-page`

Emit runtime page mapping updates.

### `memory-barrier`, `tlb-flush-page`, `tlb-flush-all`

Emit paging and ordering support operations.

## Display and Output Forms

### BIOS Text Output

- `bios-print`
- `bios-set-cursor`
- `bios-print-at`

These are useful in real mode, especially early stage boot code.

### Debug Output

- `debug-print`

This is especially useful in QEMU because Bootwright examples use the debugcon path.

### VGA Text Helpers

- `vga-print`
- `vga-print-at`
- `vga-box`
- `vga-print-hex`
- `vga-print-decimal`
- `vga-print-ascii4`
- `vga-set-cursor`

### Framebuffer-Aware Helpers

- `framebuffer-clear`
- `framebuffer-print-at`
- `framebuffer-box`
- `framebuffer-fill-region`
- `framebuffer-window`
- `framebuffer-scroll`
- `framebuffer-scroll-region`

These use framebuffer definitions from the current machine description.

## Memory and String Runtime Helpers

### Raw Memory

- `copy-memory`
- `fill-memory`
- `zero-memory`
- `compare-memory`

### String Utilities

- `copy-string`
- `string-length`
- `string-equal`

These are useful for kernels, shells, and simple in-memory subsystems.

## Interrupt and Timer Forms

### Interrupt Control

- `irq-remap`
- `irq-mask`
- `irq-unmask`
- `irq-end-of-interrupt`
- `irq-handler`

`irq-handler` is a convenience wrapper for the common "service routine + EOI + `iret`" path.

### PIC/PIT Control

- `pic-remap`
- `pic-mask`
- `pic-unmask`
- `pic-mask-all`
- `pic-eoi`
- `pit-set-frequency`

### Generic Timer Layer

- `timer-set-frequency`
- `timer-enable`
- `timer-disable`
- `timer-read-counter`

The generic timer layer routes through the machine descriptor instead of hardcoding one exact device path in every OS.

## Keyboard, Serial, and Block Device Forms

### Keyboard

- `keyboard-driver`
- `keyboard-read`
- `keyboard-poll`

`keyboard-driver` is partly declarative. It registers a keyboard subsystem during the section pre-scan so later code can use the generated structures and helpers.

### Serial / UART

- `serial-init`
- `serial-print`
- `uart-init`
- `uart-print`
- `uart-write-byte`
- `uart-read-byte`
- `uart-tx-ready-p`

### Block and Volume I/O

- `block-device`
- `block-read`
- `block-write`
- `partition-table`
- `partition-table-read`
- `partition-find`
- `volume`
- `volume-bind`
- `volume-read`
- `volume-write`

This is the storage side of the DSL for simple kernels, loaders, and OS experiments.

## Memory Management Forms

### Machine Memory Maps

- `memory-map`
- `memory-map-probe`
- `memory-map-base`
- `memory-map-count`
- `memory-map-iterate`

### Physical Allocation

- `phys-allocator-bootstrap`
- `phys-allocator-init`
- `phys-alloc`
- `phys-free`

### Page Management

- `page-structures`
- `map-page`
- `unmap-page`
- `memory-barrier`
- `tlb-flush-page`
- `tlb-flush-all`

## Execution Context and Syscall Forms

- `execution-slot-type`
- `execution-slot`
- `context-save`
- `context-restore`
- `context-init`
- `syscall-table`
- `tss`
- `privilege-drop`
- `sysenter-setup`
- `syscall-setup`

These are the lower-level runtime forms for task context and userspace/kernel transition experiments.

## Assembler Interop

Bootwright includes its own assembler. Instruction forms can appear directly inside routines:

```lisp
(mov eax (:mem :dword value))
(add eax ebx)
(cmp eax 0)
(jne-near retry)
```

### Memory Operands

Use `(:mem ...)` to form explicit memory operands:

```lisp
(:mem label)
(:mem :dword label)
(:mem :byte buffer 7)
(:mem :dword table ecx 4 8)
(:mem eax)
(:mem :word ebx 2)
```

Common patterns:

- `(:mem label)` for direct label-addressed memory
- `(:mem :dword label)` for typed label memory
- `(:mem :byte buffer 7)` for label plus displacement
- `(:mem :dword table ecx 4 8)` for label plus scaled index plus displacement
- `(:mem eax)` for base-register indirect memory

### Best Practice

- use DSL forms when they clearly describe the hardware action you want
- use raw instructions when you need exact instruction selection or special-case behavior
- keep mode switches, stack setup, and machine assumptions explicit

## A Small Real Example

```lisp
(use-machine #P"../../machines/pc-bios.bwm")

(defos *small-demo* (:machine pc-bios)
  (boot-sector (:entry boot)
    (routine boot
      (initialize-real-mode :stack #x7C00)
      (set-video-mode 3)
      (bios-print "Loading kernel...")
      (load-section kernel :on-error disk-error)
      (jump-section kernel))
    (routine disk-error
      (bios-print "Disk read failed.")
      (hang)))
  (kernel-section kernel (:load-segment #x1000 :entry kernel-main)
    (string ready "Kernel ready.")
    (routine kernel-main
      (initialize-real-mode :stack #x9000)
      (bios-print ready)
      (hang))))
```

This example shows the three main layers working together:

- source-level image structure
- section-level DSL code and data
- machine-backed boot loading between sections
