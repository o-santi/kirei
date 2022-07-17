# kirei

uma pequena linguagem criada com o singelo propósito de aprender como são implementadas linguagens com tipos dependentes.


para rodar, é necessário utilizar a ferramenta [dune](https://github.com/ocaml/dune) para ocaml:
```bash
git clone (...)
cd kirei
dune build
```
o comando `build` irá criar um executável `_build/default/bin/main.exe`. ao rodá-lo, ele irá ler o conteúdo de `example.ki`, normalizar o termo e cuspir o termo normalizado no terminal.

também é possível executar usando `dune exec kirei`.



