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

    @test gp_print(n_mult) == "*( +( 40, 2 ), 10 )"
end

@testset "Hybrid printing" begin
    n_number1 = GPNode(:Number, 40)
    n_number2 = GPNode(:Number, 2)

    function gp_print_add(n::GPNode, res::Vector{String})
        push!(res, "(")
        gp_print(gp_children(n)[1], res)
        push!(res, " + ")
        gp_print(gp_children(n)[2], res)
        push!(res, ")")
    end
    n_add = GPNode(:Addition, +, [n_number1, n_number2], gp_print_add)
    @test number_of_children(n_add) == 2
    @test gp_children(n_add) == [n_number1, n_number2]
    @test gp_value(n_add) == +

    # evaluation
    @test gp_eval(n_number1) == gp_value(n_number1)
    @test gp_eval(n_number2) == gp_value(n_number2)
    @test gp_eval(n_add) == 42

    n_mult = GPNode(:Mult, *, [n_add, GPNode(:Number, 10)])
    @test gp_eval(n_mult) == 420

    @test gp_print(n_mult) == "*( (40 + 2), 10 )"
end

@testset "Printing" begin
    n_number1 = GPNode(:Number, 40)
    n_number2 = GPNode(:Number, 2)
    function gp_print_add(n::GPNode, res::Vector{String})
        push!(res, "(")
        gp_print(gp_children(n)[1], res)
        push!(res, " + ")
        gp_print(gp_children(n)[2], res)
        push!(res, ")")
    end
    function gp_print_mult(n::GPNode, res::Vector{String})
        gp_print(gp_children(n)[1], res)
        push!(res, " * ")
        gp_print(gp_children(n)[2], res)
    end
    n_add = GPNode(:Addition, +, [n_number1, n_number2], gp_print_add)
    n_mult = GPNode(:Mult, *, [n_add, GPNode(:Number, 10)], gp_print_mult)
    @test gp_eval(n_mult) == 420

    @test gp_print(n_mult) == "(40 + 2) * 10"
end

@testset "Copying" begin
    n_number1 = GPNode(:Number, 40)
    n_number2 = GPNode(:Number, 2)
    n_add = GPNode(:Addition, +, [n_number1, n_number2])
    n_mult = GPNode(:Mult, *, [n_add, GPNode(:Number, 10)])

    n_copy = gp_copy(n_mult)
    @test !(n_copy === n_mult)
end
