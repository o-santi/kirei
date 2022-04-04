# kirei

uma pequena linguagem criada com o singelo propósito de aprender como são implementadas linguagens com tipos dependentes.

é dividida em 3 fases: sem tipos, tipos simples e com tipos dependentes. cada uma é implementada na sua própria file (atualmente temos apenas a versão sem tipos).

para rodar, instale [Kind](https://github.com/Kindelia/Kind) e depos faça:
```bash
kind untyped.kind --run
```


## sintaxe
a linguagem aceita apenas 3 tipos de expressões:
```ocaml
let id = exp in body (* expressões let: define o valor de uma variável no environment *)
(λvar exp)           (* define uma expressão lambda *)
exp1 exp2            (* define uma aplicação de função, equivale à (exp1 exp2), ou em linguagens tipo c, exp1(exp2) *)
```
aplicações de funções são consideradas associativas à esquerda (na ausência de parênteses).

ou seja, a expressão
```ocaml
a b c
```
é interpretada como
```ocaml
((a b) c)
```
isso foi feito para facilitar aplicações como:
```ocaml
let sum = λx λy (...) in
  sum two three 
```
já que queremos que isso seja interpretado como
```ocaml
((sum two) three)
```
