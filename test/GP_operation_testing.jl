@testset "Utility function for mutation" begin
    rules = [
        :expr => [:expr, Atom(infix_print("(", ")"), :+, ()->"+"), :expr]
        :expr => [Atom(:number, ()->rand(-10:10))]
    ]

    gp = GPConfig(rules; maximum_width=5)
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
    GeneticProgramming.replace_node(ind_copy, all_nodes_copy[2], gp_copy(all_nodes_copy[end]))
    @test map(gp_print, all_nodes_copy) == ["(4 + 4)", "(4 + -7)", "4", "-7", "4"]
end

@testset "Mutation" begin
    rules = [
        :expr => [:expr, Atom(infix_print("(", ")"), :+, ()->"+"), :expr]
        :expr => [Atom(:number, ()->rand(-10:10))]
    ]

    gp = GPConfig(rules; maximum_width=5)
    ind = build_individual(gp)
    @test gp_print(ind) == "((4 + -7) + 4)"

    ind_mutated = mutate(gp, ind)
    @test gp_print(ind_mutated) == "(((3 + (0 + -5)) + -7) + 4)"
    @test gp_print(ind) == "((4 + -7) + 4)"

    @test gp_print(mutate(gp, ind)) == "(((0 + 2) + (-1 + -4)) + 4)"
    @test gp_print(mutate(gp, ind)) == "((4 + ((-3 + -9) + 0)) + 4)"
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
