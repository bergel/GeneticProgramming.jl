@testset "Utility function for mutation" begin
    rules = [
        :expr => [:expr, Atom(:+, ()->"+", gp_print_infix_parent), :expr]
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
        :expr => [:expr, Atom(:+, ()->"+", gp_print_infix_parent), :expr]
        :expr => [Atom(:number, ()->rand(-10:10))]
    ]

    gp = GPConfig(rules; maximum_width=5)
    ind = build_individual(gp)
    @test gp_print(ind) == "((4 + -7) + 4)"

    ind_mutated = mutate(gp, ind)
    @test gp_print(ind_mutated) == "(((3 + (0 + -5)) + -7) + 4)"
    @test gp_print(ind) == "((4 + -7) + 4)"
end
