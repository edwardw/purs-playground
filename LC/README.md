## Lambda Calculus in Purescript

Initially translated from [a Haskell project](https://crypto.stanford.edu/~blynn/lambda/), then made significant change to adopt [De Bruijn Index](https://en.wikipedia.org/wiki/De_Bruijn_index) rather than messy variable renaming. It so happens that it also performs much better in computing factorial!

The revised De Bruijn Index naming scheme is also inspired by another Haskell project: [Haskell-Morte-Library](https://github.com/Gabriel439/Haskell-Morte-Library). It is revised to include variable names in order to produce more friendly pretty-printing result; the original De Bruijn Index is number only and doesn't need to name variables.

Specifically, its [issue #1](https://github.com/Gabriel439/Haskell-Morte-Library/issues/1) has detailed β-reduction, substitution and shifting algorithm under De Bruijn Index, which the Wikipedia page lacks:

### β-reduction
```
(λ. t) u ~> ↑(-1,0)(t[0 := ↑(1,0)u])
```

### Substitution

The substitution of a term `s` for variable number `j` in a term `t`, written `t[j := s]`, is defined as follows:

```
k[j := s]       =  | s  if k = j
                   | k  otherwise
(λ. t)[j := s]  =  λ. t[j+1 := ↑(1,0)s]
(t u)[j := s]   =  (t[j := s]  u[j := s])
```

### Shifting

The `d`-place shift of a term `t` above cutoff `c`, written `↑(d,c)t`, is defined as follows:

```
↑(d,c)k       =  | k     if k < c
                 | k + d if k >= c
↑(d,c)(λ. t)  =  λ. ↑(d,c+1)t
↑(d,c)(t u)   =  (↑(d,c)t ↑(d,c)u)
```
