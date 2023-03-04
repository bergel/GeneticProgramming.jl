@testset "Utility function for mutation" begin
    rules = [
        :expr => [:expr, Atom(infix_print("(", ")"), :+, ()->"+"), :expr]
        :expr => [Atom(:number, ()->rand(-10:10))]
    ]

    gp = GPConfig(rules; minimum_depth = 1, maximum_width=5)
    ind = build_individual(gp)
    @test gp_print(ind) == "((4 + -7) + 4)"

    all_nodes::Vector{GPNode} = GeneticProgramming.enumerate_nodes(ind)
    @test length(all_nodes) == 5
    @test map(gp_print, all_nodes) == ["((4 + -7) + 4)", "(4 + -7)", "4", "-7", "4"]

    @test first(all_nodes).producing_rule == first(rules)
    @test last(all_nodes).producing_rule == last(rules)

    ind_copy = gp_copy(ind)
    all_nodes_copy::Vector{GPNode} = GeneticProgramming.enumerate_nodes(ind_copy)
    @test map(gp_print, all_nodes_copy) == ["((4 + -7) + 4)", "(4 + -7)", "4", "-7", "4"]
    GeneticProgramming.replace_node!(ind_copy, all_nodes_copy[2], gp_copy(all_nodes_copy[end]))
    @test map(gp_print, all_nodes_copy) == ["(4 + 4)", "(4 + -7)", "4", "-7", "4"]
end

@testset "Mutation" begin
    @testset "Basic case" begin
        rules = [
            :expr => [:expr, Atom(infix_print("(", ")"), :+, ()->"+"), :expr]
            :expr => [Atom(:number, ()->rand(-10:10))]
        ]

        gp = GPConfig(rules; minimum_depth=1, maximum_width=5)
        ind = build_individual(gp)
        @test gp_print(ind) == "((4 + -7) + 4)"

        ind_mutated = mutate(gp, ind)
        @test gp_print(ind_mutated) == "(((3 + (0 + -5)) + -7) + 4)"
        @test gp_print(ind) == "((4 + -7) + 4)"

        @test gp_print(mutate(gp, ind)) == "((0 + 2) + (-1 + -4))"
        @test gp_print(mutate(gp, ind)) == "((((-3 + -9) + 0) + -7) + 4)"
    end

    @testset "Small tree" begin
        rules = [
            :expr => [:number],
            :expr => [:variable],

            :expr => [Atom(; print=infix_print(), value_factory=()->+), :expr, :expr],
            :expr => [Atom(; print=infix_print(), value_factory=()->*), :expr, :expr],

            :number => [Atom(; value_factory=() -> rand(-10:10))],
            :variable => [Atom(; id=:variable, value_factory=() -> "x")]
        ]
        config = GPConfig(rules)

        v1 = GPNode(:variable, "x", GPNode[], gp_default_print, last(rules), nothing)
        v2 = GPNode(:variable, "x", GPNode[], gp_default_print, last(rules), nothing)
        ind = GPNode(:UNK, +, [v1, v2], gp_default_print, rules[3], nothing)

        ind_mutated = mutate(config, ind)
        @test gp_print(ind) != gp_print(ind_mutated)
    end

    @testset "Without type" begin
        rules = Pair{Symbol, Vector{Any}}[
            :expr => [Atom(; value_factory=()->10)],
            :expr => [Atom(; value_factory=()->20)],
        ]
        config = GPConfig(rules; minimum_depth = 0)
        ind = build_individual(config)
        @test match_constraints(config, ind)

        mutated_ind = mutate(config, ind)
        @test match_constraints(config, mutated_ind)

        @test gp_print(ind) !== gp_print(mutated_ind)
    end
end

@testset "Crossover 1" begin
    rules = [
        :expr => [:expr, Atom(infix_print("(", ")"), :+, ()->"+"), :expr]
        :expr => [Atom(:number, ()->rand(-10:10))]
    ]

    gp = GPConfig(rules; minimum_depth=2, maximum_width=5, seed=10)
    ind1 = build_individual(gp)
    @test gp_print(ind1) == "((-7 + 1) + (4 + 4))"
    ind2 = build_individual(gp)
    @test gp_print(ind2) == "((7 + 7) + (-1 + 0))"

    @test gp_print(crossover(gp, ind1, ind2)) == "((-7 + 1) + (4 + -1))"
    @test gp_print(crossover(gp, ind1, ind2)) == "((7 + 1) + (4 + 4))"
    @test gp_print(crossover(gp, ind1, ind2)) == "((-1 + 0) + (4 + 4))"

    # Originals are preserved
    @test gp_print(ind1) == "((-7 + 1) + (4 + 4))"
    @test gp_print(ind2) == "((7 + 7) + (-1 + 0))"
end

@testset "Crossover 2" begin
    rules = [
        :expr => [:expr, Atom(infix_print("(", ")"), :+, ()->"+"), :expr]
        :expr => [Atom(:number, ()->rand(-10:10))]
    ]

    gp = GPConfig(rules; minimum_depth=1, maximum_width=5, seed=10)
    ind1 = build_individual(gp)
    @test gp_print(ind1) == "((-7 + 1) + 2)"
    ind2 = build_individual(gp)
    @test gp_print(ind2) == "(4 + (-10 + -9))"

    @test gp_print(crossover(gp, ind1, ind2)) == "((-7 + 1) + -9)"
    @test gp_print(crossover(gp, ind1, ind2)) == "((4 + (-10 + -9)) + 2)"
    @test gp_print(crossover(gp, ind1, ind2)) == "((4 + (-10 + -9)) + 2)"

    # Originals are preserved
    @test gp_print(ind1) == "((-7 + 1) + 2)"
    @test gp_print(ind2) == "(4 + (-10 + -9))"
end

@testset "Printing infix" begin
    rules = [
        :nbits => [Atom(;value_factory=()->rand([8, 16, 32, 64, 128]))],
        :value => [Atom(;id=:SignedInt, print=infix_print("int[", "]", ", ")), :nbits, :numberInt],

        :numberInt => [Atom(:number, ()->rand(-10:10))],
    ]
    gp = GPConfig(rules)
    ind = build_individual(gp, :value)
    @test gp_print(ind) == "int[32, 4]"
end
