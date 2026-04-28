# Como a DSL Funciona

Este documento explica a DSL do Bootwright como uma linguagem para descrever uma imagem de máquina inicializável, não apenas como uma lista de formas disponíveis.

## Resumo Curto

O código-fonte do Bootwright usa sintaxe Lisp comum, mas as formas importantes não "executam o sistema operacional" enquanto o arquivo está sendo carregado. Em vez disso:

1. o arquivo `.bwo` é carregado pelo SBCL
2. `defos` registra uma especificação de imagem
3. as seções da imagem são analisadas
4. o corpo de cada seção é compilado em bytes pelo montador do Bootwright
5. as seções resultantes são empacotadas em uma imagem de disco inicializável

Isso significa que um arquivo-fonte do Bootwright mistura dois mundos:

- avaliação Common Lisp normal em tempo de carga
- formas da DSL do Bootwright que descrevem código de máquina, dados e layout de imagem

## Modelo do Arquivo-Fonte

Um arquivo típico se parece com isto:

```lisp
(use-machine #P"../../machines/pc-bios.bwm")

(defos *demo* (:machine pc-bios)
  (boot-sector (:entry boot)
    (routine boot
      (initialize-real-mode :stack #x7C00)
      (bios-print "Inicializando...")
      (load-section kernel)
      (jump-section kernel)))
  (kernel-section kernel (:load-segment #x1000 :entry kernel-main)
    (routine kernel-main
      (initialize-real-mode :stack #x9000)
      (bios-print "Ola do kernel.")
      (hang))))
```

As peças de topo são:

- `use-machine`: carrega uma descrição de máquina `.bwm` para que nomes como `pc-bios` existam
- `defos`: define uma imagem inicializável
- formas de seção como `boot-sector`, `kernel-section` ou `section`

Você também pode usar:

- `include` para dividir um SO maior em vários arquivos `.bwo`
- `use-personality` para carregar extensões da DSL vindas de uma personalidade `.bwp`

## Tempo de Build Versus Tempo de Execução

Essa distinção é muito importante.

### Tempo de Build

Estas coisas acontecem enquanto o SBCL está carregando ou construindo a imagem:

- `use-machine`
- `include`
- `use-personality`
- `defos`
- definições auxiliares em Lisp como `defun`, `defmacro`, `defparameter`

### Tempo de Execução

Estas se tornam código de máquina ou bytes crus dentro de uma seção:

- `routine`
- `label`
- `string`
- `db`, `dw`, `dd`
- `initialize-real-mode`
- `enter-protected-mode`
- `framebuffer-print-at`
- instruções normais como `mov`, `cmp`, `jmp`, `int`, `iret`

Dentro do corpo de uma seção, o jeito certo de pensar é em bytes emitidos, não em chamadas de função Lisp.

## Uma Seção é um Fluxo Linear de Bytes

Cada corpo de seção é compilado em ordem. O Bootwright não trata `routine` como um objeto de função compilada separado. Ele apenas emite um label e compila o corpo naquele ponto.

Por exemplo:

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

Isso é compilado em uma única seção linear:

1. bytes de `banner`
2. label `kernel-main` e suas instruções
3. label `print-string` e suas instruções

A ordem importa para:

- onde os labels caem
- quais dados ficam adjacentes a qual código
- alinhamento e padding
- tamanho final da seção

## Labels, Constantes e Fixups

### `label`

`label` marca a posição atual de bytes dentro da seção atual.

```lisp
(label retry)
```

### `const`

`const` é um mecanismo de substituição em tempo de compilação, não uma variável mutável em runtime.

```lisp
(const screen-cols 80)
(mov ax screen-cols)
```

Se você precisa de armazenamento em runtime, emita dados:

```lisp
(label tick-count)
(dd 0)
```

### Fixups

Quando uma instrução ou dado referencia um label, o Bootwright registra um fixup e o resolve quando a seção é finalizada. É por isso que referências futuras funcionam:

```lisp
(jmp later)
...
(label later)
```

## Corpos de `routine`

Dentro de uma `routine`, o Bootwright aceita tanto:

- formas de DSL de nível mais alto
- instruções cruas do montador

Exemplo:

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

Qualquer forma que não seja reconhecida como forma de DSL do Bootwright ou forma de personalidade cai para o montador. É por isso que instruções como `mov`, `cmp`, `push`, `hlt`, `iret` e `ljmp32` funcionam diretamente.

## Formas de Dados e Layout

Essas formas emitem bytes diretamente na seção atual.

### Strings

```lisp
(string hello "Hello")
(string path "SYS:STARTUP" :zero-terminated nil)
```

Por padrão, `string` emite uma string terminada em zero.

### Dados Crus

```lisp
(db #xEB #x3C)
(dw #xAA55)
(dd #x0010F000)
```

### Espaço Reservado

```lisp
(resb 64)
(resw 8)
(resd 16)
```

Essas formas emitem espaço preenchido com zero.

### Alinhamento e Padding

```lisp
(align 16)
(pad-to 510 0)
```

Use isso quando o layout importa, especialmente em setores de boot, tabelas e estruturas de paginação.

## Modo da CPU e Largura de Codificação

O Bootwright começa a montagem de cada seção em modo de 16 bits por padrão. Use `bits` para mudar a largura das instruções seguintes:

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

Importante:

- `bits` afeta a codificação das instruções a partir daquele ponto na mesma seção
- ele não cria uma nova seção
- algumas formas da DSL exigem um modo específico e geram erro se usadas na largura errada

## Formas de Controle Estruturado

O Bootwright suporta fluxo de controle cru com labels e jumps, mas também fornece formas estruturadas baseadas em zero/não-zero:

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

As formas zero/não-zero funcionam emitindo `cmp <operando>, 0` e desviando conforme as flags. Em outras palavras:

- `if-zero` significa "rode o ramo `:then` se o operando == 0"
- `if-nonzero` significa "rode o ramo `:then` se o operando != 0"
- `while-zero` repete enquanto o operando == 0
- `while-nonzero` repete enquanto o operando != 0

São formas de conveniência sobre o mesmo modelo de desvios que você escreveria manualmente.

## Operandos de Memória e `(:mem ...)`

O montador do Bootwright usa `(:mem ...)` para operandos explícitos de memória.

Formas comuns:

```lisp
(:mem label)
(:mem :byte label)
(:mem :word label 2)
(:mem :dword buffer ecx 4 8)
(:mem eax)
(:mem :dword ebx 12)
```

Leia assim:

- memória direta em um label
- memória tipada em um label
- label mais deslocamento
- label mais registrador de índice vezes escala mais deslocamento
- memória por registrador base
- registrador base mais deslocamento

Exemplo:

```lisp
(mov al (:mem :byte keyboard-state))
(mov eax (:mem :dword table ecx 4 0))
(mov (:mem :word cursor-pos) ax)
```

Essa é uma das partes mais importantes da DSL, porque kernels reais gastam muito tempo movendo dados entre registradores e memória.

## Formas Baseadas na Máquina

Muitas formas do Bootwright não codificam portas ou endereços diretamente. Em vez disso, consultam a descrição de máquina `.bwm` atual.

Exemplos:

- `framebuffer-clear` usa o dispositivo de framebuffer nomeado
- `uart-init` e `uart-print` usam o dispositivo serial selecionado
- `timer-set-frequency` usa o dispositivo de timer da máquina
- `irq-remap` e `irq-end-of-interrupt` usam o controlador de interrupção da máquina

Isso significa que a DSL não é apenas uma camada de macros sobre instruções. Ela também contém geradores de código conscientes do alvo.

## Formas que Emitem Helpers

Algumas formas expandem para instruções diretamente. Outras apenas marcam helpers como necessários, e o Bootwright emite esses helpers depois na mesma seção.

Exemplos típicos:

- `bios-print`
- `debug-print`
- `vga-print`
- `serial-print`

Como usuário, o principal a saber é:

- você pode chamar a forma de alto nível nas suas rotinas
- o Bootwright adiciona o código auxiliar necessário automaticamente

## Declarações que Registram Subsistemas

Algumas formas fazem mais do que emitir bytes uma vez. Elas também registram um subsistema nomeado durante uma passada de pré-análise, para que formas posteriores possam referenciá-lo.

Exemplos:

- `keyboard-driver`
- `block-device`
- `memory-map`
- `phys-allocator-bootstrap`
- `page-structures`
- `syscall-table`
- `partition-table`
- `volume`

É por isso que isto funciona:

```lisp
(memory-map mmap ...)
...
(memory-map-count mmap :into eax)
```

A primeira forma registra o objeto nomeado, e as formas posteriores compilam em cima dele.

## Controle no Nível da Imagem

Estas formas conectam seções entre si:

```lisp
(load-section kernel32 :on-error disk-error)
(jump-section kernel32)
```

`load-section` usa o layout de seção já calculado para ler a seção-alvo da imagem de disco para a memória. `jump-section` transfere o controle para o ponto de entrada da seção carregada.

Esse ponto é importante: o Bootwright calcula o layout do disco antes de compilar essas chamadas, então o código do loader pode referenciar LBAs e offsets de entrada reais.

## Projetos com Múltiplos Arquivos

`include` carrega mais arquivos `.bwo` no contexto atual:

```lisp
(include #P"lib/config"
         #P"lib/console"
         #P"lib/interpreter"
         #P"lib/shell")
```

Arquivos incluídos:

- executam no mesmo pacote
- podem definir helpers em Lisp
- podem definir macros
- podem contribuir com rotinas e dados reutilizáveis do Bootwright

É assim que `examples/lisp-os/` está estruturado.

## Personalidades

Uma personalidade `.bwp` pode adicionar:

- novas formas de seção
- novas formas de dados
- novas formas de rotina

A `personalities/simple-console.bwp` enviada com o projeto é o exemplo de referência. Ela define:

- `personality-kernel` como tipo de seção customizado
- `panel-string` como forma de dados customizada
- `panel-box` e `panel-line` como formas de rotina customizadas

Use personalidades quando você quiser um dialeto específico do projeto em cima do Bootwright sem modificar a DSL central.

## Modelo Mental Prático

Ao escrever código em Bootwright, pense nesta ordem:

1. Qual máquina eu estou mirando?
2. Quais seções vão existir na imagem?
3. Em que ordem os bytes devem aparecer dentro de cada seção?
4. Quais labels e dados precisam ser endereçáveis?
5. Quais partes devem usar instruções cruas e quais partes devem usar helpers baseados na máquina?

Se você mantiver esse modelo em mente, a DSL fica muito mais fácil de entender.
