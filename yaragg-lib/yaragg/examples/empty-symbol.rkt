#lang yaragg

top : xs | ys | zs
xs : () | "x" xs
ys : Ø | "y" /ys
zs : ∅ | "z" @zs