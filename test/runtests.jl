using GeneticProgramming
using Test


@testset "Tree" begin
    @testset "Empty node" begin
        n = GPNode()
        @test number_of_children(n) == 0
        @test gp_type(n) == :UNDEFINED
        @test isnothing(gp_value(n))
        @test isempty(gp_children(n))
    end

    @testset "Simple node" begin
        n = GPNode(:Number, 42)
        @test number_of_children(n) == 0
        @test gp_type(n) == :Number
        @test gp_value(n) == 42
    end

    @testset "Addition" begin
        n_number1 = GPNode(:Number, 40)
        n_number2 = GPNode(:Number, 2)
        n_add = GPNode(:Addition, +, [n_number1, n_number2])
        @test number_of_children(n_add) == 2
        @test gp_children(n_add) == [n_number1, n_number2]
        @test gp_value(n_add) == +

        # evaluation
        @test gp_eval(n_number1) == gp_value(n_number1)
        @test gp_eval(n_number2) == gp_value(n_number2)
        @test gp_eval(n_add) == 42

        n_mult = GPNode(:Mult, *, [n_add, GPNode(:Number, 10)])
        @test gp_eval(n_mult) == 420
    end
end


#= @testset "GP" begin
    @testset "Basic" begin
        gp_config = GPConfig(
                        seed = 42,
                        root = :expr,
                        rules = [
                            :expr => number,
                            :number => gp_number
                            ]
                    )
        build_individual(gp_config)
    end
end
 =#
