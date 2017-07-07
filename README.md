# Projeto de Estatística 2017.1

Este repositório contém os arquivos do projeto da disciplina "Princípios e
Técnicas da Análise Estatística Experimental" do curso de Mestrado em Ciência
da Computação da Universidade Federal de Pernambuco. Semestre 2017.1.

## Autores

Este projeto foi autorado por Vítor de Albuquerque Torreão (vat@cin.ufpe.br).

## Instalação

Para executar os experimentos e análises desse projeto, é necessário primeiro
instalar o R. Isso pode ser feito acessando o
[site oficial da linguagem](https://www.r-project.org/).

## Execução do Experimento

Para gerar as imagens utilizadas na apresentação e no relatório, execute o
script R, `projeto.R`. Para isso, execute o seguinte comando no terminal (ou
prompt de comando no caso do Windows):

```
$ RScript projeto.R
```

Isso irá gerar as imagens dentro das pastas `histograms`, `boxplots` e
`qqplots`, assim como irá imprimir na tela as estatísticas e resultados dos
testes apresentados nos documentos.

## Geração do documento de apresentação (LaTeX)

Para gerar o documento de apresentação (slides) do projeto, você precisa
primeiro executar os scripts R na pasta raiz e na pasta "horario_de_pico", pois
o documento de apresentação utiliza as imagens geradas por esses scripts.
