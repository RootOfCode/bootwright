# How the DSL Works

This document explains the Bootwright DSL as a language for describing a bootable machine image, not just as a list of available forms.

## The Short Version

Bootwright source is normal Lisp syntax, but the important forms do not "run the OS" while the file is loading. Instead:

1. the `.bwo` file is loaded by SBCL
2. `defos` records an image specification
3. the image sections are parsed
4. each section body is compiled into bytes by Bootwright's assembler
5. the resulting sections are packed into a bootable disk image

That means a Bootwright source file mixes two worlds:

- ordinary Common Lisp evaluation at load time
- Bootwright DSL forms that describe machine code, data, and image layout

## Source File Model

A typical source file looks like this:

```lisp
(use-machine #P"../../machines/pc-bios.bwm")

(defos *demo* (:machine pc-bios)
  (boot-sector (:entry boot)
    (routine boot
      (initialize-real-mode :stack #x7C00)
      (bios-print "Booting...")
      (load-section kernel)
      (jump-section kernel)))
  (kernel-section kernel (:load-segment #x1000 :entry kernel-main)
    (routine kernel-main
      (initialize-real-mode :stack #x9000)
      (bios-print "Hello from the kernel.")
      (hang))))
```

The top-level pieces are:

- `use-machine`: load a `.bwm` machine description so names like `pc-bios` exist
- `defos`: define one bootable image
- section forms such as `boot-sector`, `kernel-section`, or generic `section`

You can also use:

- `include` to split a larger OS into multiple `.bwo` files
- `use-personality` to load DSL extensions from a `.bwp` personality

## Build-Time Versus Run-Time

This distinction matters a lot.

### Build-Time

These happen while SBCL is loading or building the image:

- `use-machine`
- `include`
- `use-personality`
- `defos`
- Lisp helper definitions such as `defun`, `defmacro`, `defparameter`

### Run-Time

These become machine code or raw bytes inside a section:

- `routine`
- `label`
- `string`
- `db`, `dw`, `dd`
- `initialize-real-mode`
- `enter-protected-mode`
- `framebuffer-print-at`
- plain instructions such as `mov`, `cmp`, `jmp`, `int`, `iret`

Inside a section body, you should think in terms of emitted bytes, not Lisp function calls.

## A Section Is a Linear Byte Stream

Each section body is compiled in order. Bootwright does not treat `routine` as a separate compiled function object. It simply emits a label and then compiles the body inline.

For example:

```lisp
(kernel-section kernel (:load-segment #x1000 :entry kernel-main)
  (string banner "hello")
  (routine kernel-main
    (mov si banner)
    (call print-string)
    (hang))
  (routine print-string
    ...))
```

This compiles into one linear section:

1. bytes for `banner`
2. label `kernel-main` and its instructions
3. label `print-string` and its instructions

Order matters for:

- where labels land
- what data is adjacent to what code
- alignment and padding
- the final size of the section

## Labels, Constants, and Fixups

### `label`

`label` marks the current byte position inside the current section.

```lisp
(label retry)
```

### `const`

`const` is a compile-time substitution mechanism, not a mutable runtime variable.

```lisp
(const screen-cols 80)
(mov ax screen-cols)
```

If you need runtime storage, use emitted data:

```lisp
(label tick-count)
(dd 0)
```

### Fixups

When an instruction or data item references a label, Bootwright records a fixup and resolves it when the section is finalized. That is why forward references work:

```lisp
(jmp later)
...
(label later)
```

## `routine` Bodies

Inside a `routine`, Bootwright accepts both:

- higher-level DSL forms
- raw assembler instructions

Example:

```lisp
(routine kernel-main
  (initialize-protected-mode :data-selector #x10 :stack-linear #x90000)
  (framebuffer-clear screen0 :attribute #x17)
  (mov eax 1234)
  (cmp eax 0)
  (jne-near ready)
  (hang)
  (label ready)
  (vga-print "ready" :row 2 :column 2)
  (ret))
```

Any form that is not recognized as a Bootwright DSL form or a personality form falls through to the assembler. That is why instruction forms like `mov`, `cmp`, `push`, `hlt`, `iret`, and `ljmp32` work directly.

## Data and Layout Forms

These forms emit bytes directly into the current section.

### Strings

```lisp
(string hello "Hello")
(string path "SYS:STARTUP" :zero-terminated nil)
```

By default, `string` emits a zero-terminated byte string.

### Raw Data

```lisp
(db #xEB #x3C)
(dw #xAA55)
(dd #x0010F000)
```

### Reserved Space

```lisp
(resb 64)
(resw 8)
(resd 16)
```

These emit zero-filled space.

### Alignment and Padding

```lisp
(align 16)
(pad-to 510 0)
```

Use these when layout matters, especially in boot sectors, tables, and paging structures.

## CPU Mode and Encoding Width

Bootwright starts section assembly in 16-bit mode by default. Use `bits` to switch the assembler width for subsequent emitted instructions:

```lisp
(routine kernel-real-entry
  (initialize-real-mode :stack #x9000)
  (enter-protected-mode :gdt gdt-descriptor
                        :entry protected-main
                        :code-selector #x08))

(bits 32)

(routine protected-main
  (initialize-protected-mode :data-selector #x10 :stack-linear #x90000)
  ...)
```

Important:

- `bits` affects instruction encoding from that point onward in the same section
- it does not create a new section
- some DSL forms require a specific mode and will error if used in the wrong width

## Structured Control Forms

Bootwright supports raw control flow through labels and jumps, but it also has structured zero/nonzero forms:

```lisp
(if-zero eax
  (:then
    (bios-print "zero"))
  (:else
    (bios-print "nonzero")))

(while-nonzero ecx
  (dec ecx))

(forever
  (hlt))
```

The zero/nonzero forms work by emitting `cmp <operand>, 0` and branching on the resulting flags. In other words:

- `if-zero` means "run the `:then` branch if operand == 0"
- `if-nonzero` means "run the `:then` branch if operand != 0"
- `while-zero` repeats while operand == 0
- `while-nonzero` repeats while operand != 0

They are convenience forms over the same branch model you would write manually.

## Memory Operands and `(:mem ...)`

Bootwright's assembler uses `(:mem ...)` for explicit memory operands.

Common forms:

```lisp
(:mem label)
(:mem :byte label)
(:mem :word label 2)
(:mem :dword buffer ecx 4 8)
(:mem eax)
(:mem :dword ebx 12)
```

Read these as:

- direct memory at a label
- typed memory at a label
- label plus displacement
- label plus index register times scale plus displacement
- memory through a base register
- base register plus displacement

Example:

```lisp
(mov al (:mem :byte keyboard-state))
(mov eax (:mem :dword table ecx 4 0))
(mov (:mem :word cursor-pos) ax)
```

This is one of the most important pieces of the DSL, because real kernels spend a lot of time moving data between registers and memory.

## Machine-Backed Forms

Many Bootwright forms do not hardcode ports or addresses directly. Instead they consult the current `.bwm` machine descriptor.

Examples:

- `framebuffer-clear` uses the named framebuffer device
- `uart-init` and `uart-print` use the selected serial device
- `timer-set-frequency` uses the machine timer device
- `irq-remap` and `irq-end-of-interrupt` use the machine interrupt controller

That means the DSL is not just a macro layer over instructions. It also contains target-aware code generators.

## Helper-Emitting Forms

Some forms expand into direct instructions immediately. Others mark helper routines as needed and Bootwright emits those helpers later in the same section.

Typical examples:

- `bios-print`
- `debug-print`
- `vga-print`
- `serial-print`

As a user, the main thing to know is:

- you can call the high-level form from your routines
- Bootwright will add the required helper code automatically

## Declarations That Register Subsystems

Some forms do more than emit bytes once. They also register a named subsystem during a pre-scan pass so later forms can refer to it.

Examples:

- `keyboard-driver`
- `block-device`
- `memory-map`
- `phys-allocator-bootstrap`
- `page-structures`
- `syscall-table`
- `partition-table`
- `volume`

That is why this works:

```lisp
(memory-map mmap ...)
...
(memory-map-count mmap :into eax)
```

The first form registers the named object, and later forms compile against it.

## Image-Level Control

These forms tie sections together:

```lisp
(load-section kernel32 :on-error disk-error)
(jump-section kernel32)
```

`load-section` uses the already computed section layout to read the target section from the disk image into memory. `jump-section` transfers control to the loaded section entry point.

This is an important point: Bootwright computes the disk layout before it compiles these calls, so the loader code can refer to real LBAs and entry offsets.

## Multi-File Projects

`include` loads more `.bwo` files into the current source context:

```lisp
(include #P"lib/config"
         #P"lib/console"
         #P"lib/interpreter"
         #P"lib/shell")
```

Included files:

- run in the same package
- can define Lisp helpers
- can define macros
- can contribute reusable Bootwright routines and data

This is how `examples/lisp-os/` is structured.

## Personalities

A `.bwp` personality can add:

- new section forms
- new data forms
- new routine forms

The shipped `personalities/simple-console.bwp` is the reference example. It defines:

- `personality-kernel` as a custom section type
- `panel-string` as a custom data form
- `panel-box` and `panel-line` as custom routine forms

Use personalities when you want a project-specific dialect on top of Bootwright without modifying the core DSL.

## A Practical Mental Model

When writing Bootwright code, think in this order:

1. What machine am I targeting?
2. What sections will exist in the image?
3. In what order should bytes appear inside each section?
4. What labels and data must be addressable?
5. Which parts should use raw instructions and which parts should use machine-backed helpers?

If you keep that model in mind, the DSL becomes much easier to reason about.
