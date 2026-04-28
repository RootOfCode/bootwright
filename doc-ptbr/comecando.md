# Começando

## Requisitos

- SBCL
- ASDF
- QEMU para testes via emulador

O Bootwright foi projetado em torno do SBCL. O framework é carregado como um sistema Common Lisp normal.

## Construir Todas as Imagens

Na raiz do projeto:

```bash
sbcl --script build.lisp
```

Isso gera as imagens de exemplo em `out/`.

## Rodar a Suíte de Testes

```bash
sbcl --script test.lisp
```

A suíte constrói imagens, inicializa os exemplos suportados em QEMU headless e verifica saída de debug ou de serial.

## Inicializar um Exemplo

Exemplo:

```bash
qemu-system-i386 -drive format=raw,file=out/protected/bootwright-protected.img,if=floppy
```

Para o `lisp-os`:

```bash
qemu-system-i386 -drive format=raw,file=out/lisp-os/bootwright-lisp-os.img,if=floppy
```

## Construir um Único Arquivo `.bwo`

Você também pode construir um arquivo diretamente:

```lisp
(bootwright:build-os-source-file #P"examples/protected/demo-protected.bwo")
```

Se você não informar saída, o Bootwright grava o resultado em `out/`.

## Exemplo Mínimo

```lisp
(use-machine #P"../machines/pc-bios.bwm")

(defos *demo-os* (:machine pc-bios)
  (boot-sector (:entry boot)
    (routine boot
      (initialize-real-mode :stack #x7C00)
      (set-video-mode 3)
      (bios-print "Inicializando...")
      (load-section kernel :on-error disk-error)
      (jump-section kernel))
    (routine disk-error
      (bios-print "Falha na leitura do disco.")
      (hang)))
  (kernel-section kernel (:load-segment #x1000 :entry kernel-main)
    (routine kernel-main
      (initialize-real-mode :stack #x9000)
      (bios-print "Ola do Bootwright.")
      (hang))))
```

## Dicas Práticas

- Comece por `examples/basics` ou `examples/protected`.
- Use `demo-lisp-os.bwo` como referência para um SO maior e dividido em vários arquivos.
- Prefira construir e validar no QEMU primeiro.
- Considere o alvo atual como BIOS x86, não como “qualquer hardware real”.
