using GeneticProgramming
using Test


@testset "Tree" begin
    include("tree_testing.jl")
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
