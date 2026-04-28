# Documentação do Bootwright

Este diretório contém a documentação em português do Brasil para o Bootwright.

## Conteúdo

- [Começando](comecando.md)
- [Conceitos Centrais](conceitos-centrais.md)
- [Como a DSL Funciona](como-a-dsl-funciona.md)
- [Referência da DSL](referencia-da-dsl.md)
- [Guia de Exemplos](guia-de-exemplos.md)

## O que é o Bootwright

Bootwright é um framework em Common Lisp para construir sistemas operacionais bare metal. Ele permite descrever:

- imagens de SO com `.bwo`
- máquinas-alvo com `.bwm`
- extensões da DSL com `.bwp`

O framework em si é implementado em SBCL/Common Lisp, mas os sistemas escritos nele continuam usando sintaxe Lisp comum.

## Escopo Atual

Hoje o Bootwright é focado em sistemas x86 compatíveis com BIOS. Os exemplos e a toolchain incluídos giram em torno de:

- setor de boot
- modo real
- modo protegido
- modo texto VGA
- paginação
- interrupções
- primitivas de timer, UART, teclado, ATA, mapa de memória e contexto de execução

Ele ainda não é uma plataforma pronta para UEFI geral ou múltiplas arquiteturas em ambiente de produção.

## Ordem Recomendada de Leitura

1. [Começando](comecando.md)
2. [Conceitos Centrais](conceitos-centrais.md)
3. [Como a DSL Funciona](como-a-dsl-funciona.md)
4. [Referência da DSL](referencia-da-dsl.md)
5. [Guia de Exemplos](guia-de-exemplos.md)
