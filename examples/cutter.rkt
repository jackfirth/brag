#lang yaragg
top : expr (/"," expr)*
expr : "x" | list
list : "(" expr ("," expr)* ")"