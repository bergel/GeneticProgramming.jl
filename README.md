# GeneticProgramming.jl

## Features

GeneticProgramming.jl has the following characteristics:

- Grammar-based specification
- Expression type
- Customizable notation:
  - prefix notation (also called *function call notation*), e.g., `f(a, b)`
  - infix notation, e.g., `a + b`
  - postfix notation, e.g., `a b +`

Current limitations:

- Does not support multi-objective fitness functions. Multi-objective search is supported by composing sub-fitness functions into a single fitness function.

## Example

Consider a simple programming language to do arithmetics that supports intergers, boolean values, and the function `+`, `-`, `<`, `if(cond, then, else)`. This language can be modelled using the following grammar:

```Julia
function value_if(c, then_value, else_value)
    return c ? then_value : else_value
end

rules = [
    :expr => [:number],
    :expr => [Atom(; print=infix_print(), value_factory=()->+), :number, :expr],
    :expr => [Atom(; print=infix_print(), value_factory=()->-), :number, :expr],
    :expr => [Atom(; value_factory=()->"IF"), :condition, :expr, :expr],
    :condition => [Atom(; print=infix_print(), value_factory=()-><), :number, :number],
    :number => [Atom(; id=:number, value_factory=()->rand(-10:10))],
]
```

```Julia
config = GPConfig(rules; maximum_width = 4)
gp_print(build_individual(config))
```

An individual can be built from any designated rule name:

```Julia
gp_print(build_individual(config, :number))
```

If no rule name is provided, then the name of the first rule is used, `:expr` in this example.

Replacing part of the individual:
```Julia
i = build_individual(config)
gp_print(i)

gp_replace!(:number, (index)->index + 10, i)
gp_print(i)

```

### Specifying inputs

### Customizable notation

```Julia
rules = [
    :expr => [Atom(; value_factory=()->1)],
    :expr => [Atom(; value_factory=()->+), :expr, :expr],
]
config = GPConfig(rules)
gp_print(build_individual(config))
```

The script above prints `"+( 1, 1 )"`. The default notation is postfix with parenthesis.

We can use an infix notation such as follow:
```Julia
rules = [
    :expr => [Atom(; value_factory=()->1)],
    :expr => [Atom(; value_factory=()->+, print=infix_print()), :expr, :expr],
]
config = GPConfig(rules)
gp_print(build_individual(config))
gp_print(build_individual(config))
```

This prints `"1 + 1 + 1 + 1 + 1 + 1 + 1 + 1"`.

The infix element can be enclosed with lexical elements, e.g.:

```Julia
rules = [
    :expr => [Atom(; value_factory=()->1)],
    :expr => [Atom(; value_factory=()->+, print=infix_print("(", ")")), :expr, :expr],
]
config = GPConfig(rules)
gp_print(build_individual(config))
gp_print(build_individual(config))
```

The script now prints `"((1 + ((1 + 1) + ((1 + 1) + (1 + 1)))) + 1)"`

The way each element is printed by be configured with a customized printing function. Consider:

```Julia
function my_print(n::GPNode, res::Vector{String})
    push!(res, "val")
    push!(res, string(gp_value(n)))
end
rules = [
    :expr => [Atom(; value_factory=()->1, print=my_print)],
    :expr => [Atom(; value_factory=()->+, print=infix_print("(", ")")), :expr, :expr],
]
config = GPConfig(rules)
gp_print(build_individual(config))
```

which prints `"(val1 + val1)"`. The function `my_print` takes two arguments:

  - `n` corresponds to node to be printed,
  - `res` corresponds to the result of the printing. It is a vector of string values.

```Julia
function def_print(n::GPNode, res::Vector{String})
    push!(res, "def ")
    gp_print(gp_children(n)[1], res)
    push!(res, " = ")
    gp_print(gp_children(n)[2], res)
end

rules = [
    :def => [Atom(; print=def_print), :name, :number],
    :name => [Atom(; value_factory=()->rand(["a", "b", "c"]))],
    :number => [Atom(; value_factory=()->rand(0:10))],
]
config = GPConfig(rules)
gp_print(build_individual(config))
```

The script prints `"def b = 7"`. The function `gp_children` is used to access a particular children of the provided node.

### Generating random program

## Related work

Julia has a number of package that provides closely related techniques. For example, https://github.com/wildart/Evolutionary.jl support genetic programming with the `TreeGP` structure. However, `TreeGP` requires a configuration in terms of terminal and non-terminal (i.e., functions) nodes. Although this way to model GP is efficient in many situations (e.g, solving mathematical problem by searching for a particular equation), it does not seem to be enough go beyond finding untyped and multistatements programs. GeneticProgramming.jl does not suffer from this restriction and can generate correctly-typed programs.