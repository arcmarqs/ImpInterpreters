# Interpretador de IMP

## Fundamentos de linguagens de progamação trabalho 2

### Amadeu Marques

Para correr o interpretador basta correr o comando cabal run. 
Estão disponiveis alguns testes na pasta test, a maior parte destes foram retirados das Notas apresentadas nas aulas. Para correr estes testes o comando é cabal run < tests/ test\<1, 2 ou 3\>.imp

A sintaxe do interpretador é a seguinte:

- Primeiro indica-se na primeira linha o mapa de estados inicial no formato indicado nas aulas \[valor/variavel, ... , valor/variavel\]. Contudo também é possivel correr o programa com o mapa de estados vazio \[ \].

- Na linha seguinte especifica-se o comando a avaliar, delimitada por \[\[ \]\].

- Os comandos If then else, While e or requerem parenteses no seu ultimo comando, (por exemplo \[\[ while x<=2 do (x:=x+1) or (if x=1 then y:=x+2 else (x:=3))) \]\] ).

- O output do programa é a lista que contém o estado das variaveis após a avaliação do comando.

#### Exemplo: 
cabal run

\[3/x\]\
\[\[ y:=1;while \!x=1 do (y := y*x;x:=x-1) \]\]

output: \[("x",1),("y",6)\]
