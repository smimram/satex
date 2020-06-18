Satex is a [string diagram](https://en.wikipedia.org/wiki/String_diagram)
generator for LaTeX. It takes as input a formula such as
```
(1 * delta) * (mu * 1)
```
(appropriately written in the LaTeX file) and produces a diagram such as

![](fig/frobl.svg)

generated using TikZ.

It is inspired from [Catex](https://webusers.imj-prg.fr/~yves.guiraud/) (see
also the
[documentation](https://webusers.imj-prg.fr/~yves.guiraud/catex/catex.pdf)) for
which the sources where unfortunately not available.

# General usage

In order to use satex in your LaTeX file, you should first include the style
file:

```
\usepackage{satex}
```

You should then declare the operators you want to use in the format

```
\deftwocell[options]{name : m -> n}
```

which declares an operator named `name` with `m` inputs and `n` outputs. The
options in `options` allow changing the way the operator is displayed and so
on. For instance,

```
\deftwocell[triangle]{mu : 2 -> 1}
```

will declare the following operator:

![](fig/mu.svg)

One can then generate diagrams by using commands of the form
