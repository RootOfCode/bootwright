# Conceitos Centrais

## Tipos de Arquivo

### `.bwo`

Arquivos-fonte de sistema operacional do Bootwright.

Eles definem imagens com `defos` e são carregados automaticamente no pacote `bootwright.os`, então não precisam de `in-package` explícito.

### `.bwm`

Arquivos de máquina do Bootwright.

Eles descrevem o modelo de máquina-alvo: ISA, protocolo de boot, regiões de memória, controlador de interrupção, timer, dispositivos seriais e framebuffer.

### `.bwp`

Arquivos de personalidade do Bootwright.

Uma personalidade adiciona formas de DSL e abstrações de seção mais altas sobre o núcleo do sistema.

## Modelo de Imagem

Uma imagem do Bootwright normalmente é composta por:

- um setor de boot
- uma ou mais seções de kernel ou seções genéricas

Cada seção tem:

- um tipo
- um endereço de carga
- um rótulo de entrada
- um corpo compilado próprio

O setor de boot normalmente carrega as seções posteriores e transfere o controle para a entrada do kernel.

Existe uma separação importante entre:

- estrutura no nível da imagem, que decide quais seções existem e onde elas vivem no disco e na memória
- estrutura no nível da seção, que decide quais bytes serão emitidos dentro de cada seção

No nível da imagem, `boot-sector`, `kernel-section` e `section` são os blocos principais. No nível da seção, formas como `routine`, `string`, `label`, `db`, `gdt` e instruções cruas definem o conteúdo real.

## Modelo de Execução

Hoje o foco do Bootwright é BIOS x86:

- o boot começa em modo real de 16 bits
- o código pode mudar para modo protegido de 32 bits
- depois disso paginação e interrupções podem ser configuradas

O arquivo `pc-bios.bwm` modela esse ambiente.

Por isso o compilador trabalha com dois modelos de endereçamento ao mesmo tempo:

- layout no disco, onde as seções ocupam setores na imagem final
- layout em runtime, onde as seções são carregadas em segmentos ou origens de memória

Formas como `load-section` e `jump-section` dependem desse layout pré-calculado.

## Filosofia da DSL

A DSL usa sintaxe Lisp sobre trabalho de baixo nível. A ideia é oferecer:

- controle direto sobre layout e instruções
- helpers reutilizáveis baseados na máquina
- formas explícitas para GDT, IDT, paginação, IRQs, E/S em bloco e mapas de memória

Quando a DSL não abstrai algo, você ainda pode cair diretamente na camada do montador.

Na prática, o Bootwright se comporta como uma linguagem mista:

- algumas formas são declarações consumidas em tempo de build
- algumas formas emitem bytes diretamente
- algumas formas geram código auxiliar consciente da máquina-alvo
- formas desconhecidas dentro de uma rotina caem para o montador

Essa última regra é o que torna a DSL prática para desenvolvimento de SO: você não fica preso quando a camada de mais alto nível ainda não tem uma forma dedicada.

## Pipeline de Build

O caminho normal do fonte até a imagem é:

1. o SBCL carrega o arquivo `.bwo`
2. `defos` registra uma especificação de imagem
3. o Bootwright resolve a máquina-alvo
4. as formas de seção são transformadas em especificações de seção
5. o Bootwright calcula layout de seção e endereços de carga
6. o corpo de cada seção é compilado em ordem para bytes
7. labels e fixups são resolvidos
8. a imagem final é gravada em `out/` ou no caminho solicitado

É por isso que a ordem importa dentro das seções, e por isso referências por label funcionam mesmo quando o label aparece depois no arquivo.

## Tempo de Compilação Versus Tempo de Execução

Dentro dos fontes Bootwright, Lisp comum continua existindo. Você pode definir macros auxiliares, constantes ou incluir arquivos em tempo de carga.

Mas as formas dentro dos corpos de seção devem ser lidas como formas de construção de imagem:

- `const` é uma substituição em tempo de compilação
- `routine` emite um label e código inline
- `string`, `db`, `dw`, `dd`, `resb`, `resw`, `resd` emitem bytes
- `initialize-real-mode`, `framebuffer-print-at`, `memory-map-probe` emitem código de máquina

Se você precisa de estado em runtime, armazene em dados emitidos, não em variáveis Lisp.

## Controle Estruturado

O Bootwright já tinha labels, jumps, memória mutável e chamadas. Agora a DSL também expõe formas estruturadas para computação não limitada:

- `forever`
- `while-zero`
- `while-nonzero`
- `if-zero`
- `if-nonzero`

Essas formas são conveniências sobre o mesmo modelo de execução de baixo nível.

As semânticas são propositalmente simples:

- `if-zero` e `if-nonzero` comparam um operando com zero
- `while-zero` e `while-nonzero` fazem loop testando um operando contra zero
- `forever` emite um loop infinito

Elas são úteis quando você quer a legibilidade de um fluxo estruturado sem abrir mão do controle exato orientado à máquina.

## Escopo Atual de Hardware

O Bootwright não é uma camada universal de abstração de hardware. Os exemplos atuais assumem:

- x86 compatível com BIOS
- modo texto VGA
- serviços de disco e vídeo da BIOS
- comportamento de PIC, PIT, UART e PS/2 de PC clássico

Por isso ele é especialmente adequado para desenvolvimento de SO em emuladores e em hardware legado ou compatível com BIOS.
