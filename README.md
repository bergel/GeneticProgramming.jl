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
    :number => [Atom(; value_factory=()->rand(-10:10))],
]
```

```Julia
config = GPConfig(rules; maximum_width = 4)
gp_print(build_individual(config))
```



### Specifying inputs

### Generating random program

## Related work

Julia has a number of package that provides closely related techniques. For example, https://github.com/wildart/Evolutionary.jl support genetic programming with the `TreeGP` structure. However, `TreeGP` requires a configuration in terms of terminal and non-terminal (i.e., functions) nodes. Although this way to model GP is efficient in many situations (e.g, solving mathematical problem by searching for a particular equation), it does not seem to be enough go beyond finding untyped and multistatements programs. GeneticProgramming.jl does not suffer from this restriction and can generate correctly-typed programs.