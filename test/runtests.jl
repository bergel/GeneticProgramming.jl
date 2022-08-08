using GeneticProgramming
using Test
using Random

@testset "Tree" begin
    include("tree_testing.jl")
end


@testset "GP" begin
    include("GP_testing.jl")
end
