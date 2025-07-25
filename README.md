[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/8MxTER4w)
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


