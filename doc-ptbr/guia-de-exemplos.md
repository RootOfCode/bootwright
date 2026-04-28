# Guia de Exemplos

## Básicos

### `examples/basics/demo-real.bwo`

Exemplo pequeno em modo real com saída de texto da BIOS.

Útil para entender:

- fluxo do setor de boot
- inicialização em modo real
- saída na tela pela BIOS

### `examples/basics/demo-assets.bwo`

Mostra inclusão de assets relativos ao arquivo-fonte com `include-binary`.

### `examples/basics/demo-scroll.bwo`

Mostra os helpers de scroll de framebuffer.

## Modo Protegido

### `examples/protected/demo-protected.bwo`

Bootstrap de modo protegido com GDT, IDT, paginação e caminho de interrupção por software.

### `examples/protected/demo-paging.bwo`

Exemplo focado em paginação com `page-structures`, `map-page` e `unmap-page`.

## Dispositivos

### `examples/devices/demo-timer.bwo`

Usa IRQ0 e helpers de timer para atualizar um relógio visível.

### `examples/devices/demo-probe.bwo`

Combina VGA, UART, speaker e inspeção de registradores de controle.

### `examples/devices/demo-keyboard.bwo`

Exemplo interativo de eco de teclado.

### `examples/devices/demo-ata.bwo`

Demonstra E/S em bloco com leitura ATA PIO.

### `examples/devices/demo-memmap.bwo`

Mostra probing E820 da BIOS e iteração sobre o mapa de memória.

### `examples/devices/demo-physalloc.bwo`

Exemplo do alocador bootstrap de páginas físicas.

## CPU e Runtime

### `examples/cpu/demo-cpuinfo.bwo`

Usa `cpuid` e helpers de impressão na tela.

### `examples/cpu/demo-bench.bwo`

Usa `rdtsc` para um microbenchmark no boot.

### `examples/cpu/demo-memops.bwo`

Exercita operandos de memória e helpers de cópia/preenchimento.

### `examples/cpu/demo-runtime-utils.bwo`

Imagem focada em cobrir helpers de runtime.

### `examples/cpu/demo-sysinstr.bwo`

Demonstra instruções de sistema em CPL=0.

## Execução

### `examples/exec/demo-context.bwo`

Troca cooperativa de contexto com execution slots.

### `examples/exec/demo-syscalls.bwo`

Despacho de syscalls via tabela e INT 0x80.

### `examples/exec/demo-userspace.bwo`

Exemplo relacionado à transição usuário/kernel.

### `examples/exec/demo-sysenter.bwo`

Exemplo de setup de fastpath para syscall.

## Sistema de Personalidade

### `examples/personality/demo-personality.bwo`

Exemplo de referência para personalidades `.bwp`.

## Lisp-OS

### `examples/lisp-os/demo-lisp-os.bwo`

É o maior exemplo incluído.

Use-o como referência para:

- organização em múltiplos arquivos
- composição de interface
- caminho de entrada do teclado
- shell baseado em Bootwright
- pequeno interpretador em estilo Lisp

Arquivos relacionados:

- `examples/lisp-os/lib/architecture.bwo`
- `examples/lisp-os/lib/config.bwo`
- `examples/lisp-os/lib/interpreter.bwo`
- `examples/lisp-os/lib/ui.bwo`
- `examples/lisp-os/lib/filesystem.bwo`
- `examples/lisp-os/lib/keyboard.bwo`
- `examples/lisp-os/lib/shell.bwo`
