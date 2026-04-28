# Referência da DSL

Esta é uma referência prática da DSL do Bootwright. O foco está no significado e no uso, não em uma gramática formal.

## Como Ler a DSL Corretamente

Dentro de um corpo de seção, a maior parte das formas não é computação Lisp comum. Elas são diretivas de compilação que:

- emitem bytes
- emitem instruções
- registram subsistemas nomeados da máquina
- pedem rotinas auxiliares
- influenciam o layout da seção

Se uma forma não for reconhecida como forma de DSL do Bootwright nem como forma de personalidade, o Bootwright tenta montá-la como instrução crua.

## Formas de Topo do Arquivo-Fonte

### `use-machine`

Carrega um arquivo de máquina `.bwm` para que a imagem possa mirar um descritor de máquina nomeado.

```lisp
(use-machine #P"../../machines/pc-bios.bwm")
```

### `include`

Carrega arquivos-fonte `.bwo` adicionais relativos ao arquivo atual.

```lisp
(include #P"lib/config"
         #P"lib/console"
         #P"lib/interpreter")
```

### `use-personality`

Carrega uma personalidade `.bwp` que adiciona formas customizadas de seção, dados ou rotina.

### `defos`

Define uma imagem inicializável.

```lisp
(defos *demo* (:machine pc-bios)
  ...)
```

A opção `:machine` é a forma normal de ligar a imagem a um alvo vindo de um arquivo `.bwm`.

## Formas de Seção

### `boot-sector`

Define a seção de boot de estágio 1.

```lisp
(boot-sector (:entry boot)
  ...)
```

Propriedades típicas:

- a origem vem do protocolo de boot, geralmente `0x7C00`
- o alinhamento em setores vem do protocolo de boot da máquina
- o label de entrada costuma ser a primeira rotina de boot

### `kernel-section`

Define uma seção de kernel carregável.

```lisp
(kernel-section kernel32 (:load-segment #x1000 :entry kernel-main)
  ...)
```

Opções comuns:

- `:entry`
- `:load-segment`
- `:load-offset`
- `:origin`
- `:sector-align`

### `section`

Define uma seção mais genérica quando você não quer os padrões de `kernel-section`.

## Formas Estruturais Centrais

### `routine`

Emite um label e compila o corpo inline naquela posição.

```lisp
(routine kernel-main
  ...)
```

`routine` não é um objeto de código separado. Ela é apenas um ponto nomeado na seção atual.

### `label`

Marca o offset atual dentro da seção atual.

```lisp
(label loop-start)
```

### `const`

Substituição em tempo de compilação apenas.

```lisp
(const kernel-stack #x90000)
```

Use dados emitidos, e não `const`, para estado mutável em runtime.

### `bits`

Muda a largura das instruções para o código emitido depois dentro da mesma seção.

```lisp
(bits 32)
```

### `arch`

Muda o token de ISA do montador se necessário para uma seção.

## Formas de Dados e Layout

### `string`

Emite uma string ASCII, terminada em zero por padrão.

```lisp
(string banner "Kernel loaded.")
(string raw-text "ABC" :zero-terminated nil)
```

Opções incluem:

- `:zero-terminated`
- `:z`
- `:align`

### `db`, `dw`, `dd`

Emitem dados crus em byte, word ou dword.

```lisp
(db #xEB #x3C)
(dw #xAA55)
(dd #x0010F000)
```

### `resb`, `resw`, `resd`

Emitem armazenamento preenchido com zero.

```lisp
(resb 128)
(resw 16)
(resd 32)
```

### `ascii`, `ascii-z`

Emitem bytes ASCII diretamente.

### `align`

Faz padding do offset atual até o alinhamento solicitado.

```lisp
(align 16)
```

### `pad-to`

Faz padding até que a seção atual atinja um offset específico.

```lisp
(pad-to 510 0)
```

### `fill`

Emite um byte de preenchimento repetido.

### `repeat`

Repete um corpo em tempo de build.

```lisp
(repeat 4
  (db 0))
```

### `include-binary`

Incorpora os bytes de um arquivo diretamente na seção atual.

## Formas de Fluxo de Controle

### Fluxo Cru do Montador

Você sempre pode escrever:

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

Loop infinito em volta do corpo.

```lisp
(forever
  (hlt))
```

### `if-zero`, `if-nonzero`

Desvio estruturado com base em `cmp operando, 0`.

```lisp
(if-zero eax
  (:then
    (bios-print "zero"))
  (:else
    (bios-print "nonzero")))
```

### `while-zero`, `while-nonzero`

Loops estruturados com base em comparação repetida com zero.

```lisp
(while-nonzero ecx
  (dec ecx))
```

### `hang`, `idle-forever`

Loops de parada/estacionamento por conveniência.

## Formas de Boot e Modo da CPU

### `initialize-real-mode`

Configura o ambiente comum de execução em modo real, especialmente o estado da pilha.

```lisp
(initialize-real-mode :stack #x7C00)
```

### `initialize-protected-mode`

Configura segmentos de dados e pilha depois de entrar em modo protegido.

```lisp
(initialize-protected-mode :data-selector #x10
                           :stack-linear #x90000)
```

### `enter-protected-mode`

Muda de modo real para protegido e transfere o controle para um label de entrada.

### `load-gdt`, `load-idt`, `load-tr`

Carregam tabelas de descritores ou o task register.

### `enable-paging`

Ativa paginação com o estado de diretório de páginas informado.

### `sysenter-setup`, `syscall-setup`, `privilege-drop`

Helpers de baixo nível para entrada em sistema e transições de privilégio.

## Formas de Descritores e Paginação

### `gdt`

Define uma GDT e opcionalmente sua estrutura de ponteiro.

```lisp
(gdt gdt (:pointer gdt-descriptor)
  (:null)
  (:code32 :base :section :limit #xAFFFF)
  (:data32 :base :section :limit #xAFFFF))
```

### `idt`

Define uma IDT e suas entradas.

### `boot-info`

Constrói uma estrutura de informações de boot para handoff ao código posterior.

### `page-structures`, `page-directory`, `page-table`

Definem estruturas de paginação.

### `map-page`, `unmap-page`

Emitem atualizações de mapeamento em runtime.

### `memory-barrier`, `tlb-flush-page`, `tlb-flush-all`

Emitem operações de suporte para paginação e ordenação.

## Formas de Tela e Saída

### Saída de Texto via BIOS

- `bios-print`
- `bios-set-cursor`
- `bios-print-at`

São úteis em modo real, especialmente no código inicial de boot.

### Saída de Debug

- `debug-print`

Isso é especialmente útil no QEMU porque os exemplos do Bootwright usam o caminho de debugcon.

### Helpers de Texto VGA

- `vga-print`
- `vga-print-at`
- `vga-box`
- `vga-print-hex`
- `vga-print-decimal`
- `vga-print-ascii4`
- `vga-set-cursor`

### Helpers Conscientes de Framebuffer

- `framebuffer-clear`
- `framebuffer-print-at`
- `framebuffer-box`
- `framebuffer-fill-region`
- `framebuffer-window`
- `framebuffer-scroll`
- `framebuffer-scroll-region`

Esses usam definições de framebuffer da descrição de máquina atual.

## Helpers de Runtime para Memória e Strings

### Memória Crua

- `copy-memory`
- `fill-memory`
- `zero-memory`
- `compare-memory`

### Utilitários de Strings

- `copy-string`
- `string-length`
- `string-equal`

São úteis para kernels, shells e subsistemas simples em memória.

## Formas de Interrupção e Timer

### Controle de Interrupções

- `irq-remap`
- `irq-mask`
- `irq-unmask`
- `irq-end-of-interrupt`
- `irq-handler`

`irq-handler` é um wrapper de conveniência para o caminho comum "rotina de serviço + EOI + `iret`".

### Controle de PIC/PIT

- `pic-remap`
- `pic-mask`
- `pic-unmask`
- `pic-mask-all`
- `pic-eoi`
- `pit-set-frequency`

### Camada Genérica de Timer

- `timer-set-frequency`
- `timer-enable`
- `timer-disable`
- `timer-read-counter`

A camada genérica de timer roteia pela descrição de máquina em vez de codificar um único caminho de dispositivo em cada SO.

## Formas de Teclado, Serial e Dispositivos de Bloco

### Teclado

- `keyboard-driver`
- `keyboard-read`
- `keyboard-poll`

`keyboard-driver` é parcialmente declarativo. Ele registra um subsistema de teclado durante a pré-análise da seção para que o código posterior possa usar as estruturas e helpers gerados.

### Serial / UART

- `serial-init`
- `serial-print`
- `uart-init`
- `uart-print`
- `uart-write-byte`
- `uart-read-byte`
- `uart-tx-ready-p`

### Bloco e Volumes

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

Essa é a parte de armazenamento da DSL para kernels simples, loaders e experimentos de SO.

## Formas de Gerência de Memória

### Mapas de Memória da Máquina

- `memory-map`
- `memory-map-probe`
- `memory-map-base`
- `memory-map-count`
- `memory-map-iterate`

### Alocação Física

- `phys-allocator-bootstrap`
- `phys-allocator-init`
- `phys-alloc`
- `phys-free`

### Gerência de Páginas

- `page-structures`
- `map-page`
- `unmap-page`
- `memory-barrier`
- `tlb-flush-page`
- `tlb-flush-all`

## Formas de Contexto de Execução e Syscalls

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

Essas são as formas de runtime mais baixas para contexto de tarefa e experimentos de transição entre userspace e kernel.

## Interop com o Montador

O Bootwright inclui seu próprio montador. Formas de instrução podem aparecer diretamente dentro de rotinas:

```lisp
(mov eax (:mem :dword valor))
(add eax ebx)
(cmp eax 0)
(jne-near repetir)
```

### Operandos de Memória

Use `(:mem ...)` para formar operandos explícitos de memória:

```lisp
(:mem label)
(:mem :dword label)
(:mem :byte buffer 7)
(:mem :dword tabela ecx 4 8)
(:mem eax)
(:mem :word ebx 2)
```

Padrões comuns:

- `(:mem label)` para memória direta endereçada por label
- `(:mem :dword label)` para memória tipada por label
- `(:mem :byte buffer 7)` para label mais deslocamento
- `(:mem :dword tabela ecx 4 8)` para label mais índice escalado mais deslocamento
- `(:mem eax)` para memória indireta por registrador base

### Boa Prática

- use formas da DSL quando elas descrevem claramente a ação de hardware desejada
- use instruções cruas quando precisar de seleção exata de instrução ou comportamento especial
- mantenha trocas de modo, configuração de pilha e suposições de máquina explícitas

## Um Pequeno Exemplo Real

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

Esse exemplo mostra as três camadas principais trabalhando juntas:

- estrutura da imagem no nível do fonte
- código e dados da DSL no nível da seção
- carregamento entre seções baseado na máquina
