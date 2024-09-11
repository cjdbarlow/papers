# Libraries
library(tidyverse)
library(forestploter)
library(DiagrammeR)
library(DiagrammeRsvg)


# 1 Process plot
process = grViz("
    digraph G {
    graph [layout = dot, rankdir = LR, fontname = 'Helvetica']
    node [shape = rectangle, fontname = 'Helvetica']
 
    d [label = <Data <BR/><BR/> Table: Pre-imputed>];
 
    subgraph cluster_1 {
        label = 'Imputation';
        graph[style = dashed];
        a0 [label=<I<SUB>1</SUB>>];
        a1 [label=<I<SUB>2</SUB>>];
        a2 [label=<I<SUB>n</SUB>>];
        a3 [label=<I<SUB>26</SUB>>];
        a4 [label=<Figure: Convergence>, shape = none];
    }

    subgraph cluster_2 {
        label = 'Matching';
        graph[style = dashed];
        b0 [label=<M<SUB>1</SUB>>];
        b1 [label=<M<SUB>2</SUB>>];
        b2 [label=<M<SUB>n</SUB>>];
        b3 [label=<M<SUB>26</SUB>>];
        b4 [label=<Table: Balance>, shape = none];
    }
  
    subgraph cluster_3 {
        label = 'Modelling';
        graph[style = dashed];
        c0 [label=<R<SUB>1</SUB>>];
        c1 [label=<R<SUB>2</SUB>>];
        c2 [label=<R<SUB>n</SUB>>];
        c3 [label=<R<SUB>26</SUB>>];
        #c4 [label=<Figure D>, shape = none];
    }
  
    d -> a0 [headport = 'w'];
    d -> a1 [headport = 'w'];
    d -> a2 [headport = 'w', style = 'dotted'];
    d -> a3 [headport = 'w'];
  
    a0 -> b0 [dir = 'right'];
    a1 -> b1 [dir = 'right'];
    a2 -> b2 [dir = 'right', style = 'dotted'];
    a3 -> b3 [dir = 'right'];
  
    b0 -> c0 [dir = 'right'];
    b1 -> c1 [dir = 'right'];
    b2 -> c2 [dir = 'right', style = 'dotted'];
    b3 -> c3 [dir = 'right'];
    
    p [label = <Pooled Results <BR/><BR/> Figure: Forest Plot>];

    c0 -> p [tailport = 'e'];
    c1 -> p [tailport = 'e'];
    c2 -> p [tailport = 'e', style = 'dotted'];
    c3 -> p [tailport = 'e'];

}")


