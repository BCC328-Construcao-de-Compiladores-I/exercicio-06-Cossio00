[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/k_hkOCd1)
Ambiente de desenvolvimento para BCC328
=======================================

Pré-requisitos
-------------- 

Este repositório utiliza o [Docker](https://www.docker.com/) para 
garantir que o código utilize a versão 
correta de suas dependências.

Para executar, instale o Docker e o 
Docker compose e execute, no terminal, 
os seguintes comandos na pasta principal 
do projeto (a que contém o `dockerfile`) 
no terminal:

```
docker-compose up -d 
```

Em seguida, entre no terminal com as ferramentas
de desenvolvimento usando:

```
docker exec -it haskell-dev bash 
```

Esses passos irão instalar as ferramentas de
desenvolvimento Haskell (compilador GHC e Cabal) 
e ferramentas para compilação.

Instalando Alex e Happy
----------------------- 

Após a execução dos passos anteriores, instale 
os geradores de analisadores léxico e sintático,
Alex e Happy, usando: 

```
apt-get install alex happy 
```

Com isso, você terá o ambiente necessário para 
desenvolvimento das atividades da disciplina 
BCC328 - Construção de Compiladores I.

Após a instalação destes componentes, entre na 
pasta `workspace` e nela execute: 

```
cabal build
```

que irá compilar todo o projeto.

Observação
----------

Caso o comando `apt-get` retorne erro, tente executar usando `sudo`.

Gerando o Lexer com Alex:
```
alex src/L/L1/Frontend/Lexer.x -o src/L/L1/Frontend/Lexer.hs
```

Gerando o Parser com happy:
```
happy src/L/L1/Frontend/Parser.y -o src/L/L1/Frontend/LALRParser.hs
```


Executando os exercícios L1:

Acesse o diretório raiz (bcc328) e digite:
```
cabal run l1 -- --lexer-only test.l1
cabal run l1 -- --recursive test.l1
cabal run l1 -- --lalr test.l1
```

Executando os exercícios L2:

Acesse o diretório raiz (bcc328) e digite:
```
cabal run l2 -- --lexer-only test.l2
cabal run l2 -- --parse-only test.l2
cabal run l2 -- --interpret test.l2
```
