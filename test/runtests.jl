using GeneticProgramming
using Test
using Random

@testset "Tree" begin
    include("tree_testing.jl")
end


@testset "GP" begin
    include("GP_structure_testing.jl")
    include("GP_operation_testing.jl")
    include("GP_algorithm_testing.jl")
end
