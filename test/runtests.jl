using GeneticProgramming
using Test


@testset "Tree" begin
    include("tree_testing.jl")
end


@testset "GP" begin
    @testset "Basic" begin
        rules = [
            :expr => :number,
            :expr => [:number , :+ , :expr],
            :number => gp_number,
            :+ => +
        ]
        gp_config = GPConfig(
                        42,
                        :expr,
                        rules
                    )

        @test number_of_terminal_rules(gp_config) == 2
        @test gp_config.terminal_rules == rules[3:4]

        @test number_of_non_terminal_rules(gp_config) == 2
        @test gp_config.non_terminal_rules == rules[1:2]

        @test GeneticProgramming.candidate_rules(gp_config, :expr) == rules[1:2]
        @test GeneticProgramming.candidate_rules(gp_config, :number)[1] == rules[3]

        number_node = build_individual(gp_config, :number)
        @test number_node isa GPNode
        @test gp_type(number_node) == :number
        @test typeof(gp_value(number_node)) == Int64

        #ind = build_individual(gp_config)
        #@test ind isa GPNode
    end
end
