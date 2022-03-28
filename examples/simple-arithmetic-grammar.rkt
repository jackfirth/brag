#lang yaragg

expr : term ('+' term)*
term : factor ('*' factor)*
factor : INT
