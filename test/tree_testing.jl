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

@testset "Parent" begin
    n_number1 = GPNode(:Number, 40)
    n_number2 = GPNode(:Number, 2)

    @test !has_parent(n_number1)
    @test !has_parent(n_number2)

    n_add = GPNode(:Addition, +, [n_number1, n_number2])

    @test has_parent(n_number1)
    @test has_parent(n_number2)
    @test n_number1.parent == n_add
    @test n_number2.parent == n_add
    @test !has_parent(n_add)

    n_mult = GPNode(:Mult, *, [n_add, GPNode(:Number, 10)])
    @test !has_parent(n_mult)
    @test has_parent(n_add)
    @test n_add.parent == n_mult
end

@testset "No more than one parent" begin
    node = GPNode(:Number, 42)
    another_node = GPNode(:Number, 41, [node])
    @test node.parent == another_node

    local t::Bool = false
    try
        GPNode(:Number, 40, [node])
    catch
        t = true
    end
    @test t == true
end

@testset "Copying and parent" begin
    node = GPNode(:Number, 42)
    another_node = GPNode(:Number, 41, [node])
    @test another_node.children[1].parent == another_node

    another_node_copy = gp_copy(another_node)
    @test another_node_copy.children[1].parent == another_node_copy
end

@testset "Replace node" begin
    @testset "Inner node" begin
        n_number1 = GPNode(:Number, 40)
        n_number2 = GPNode(:Number, 2)
        n_add = GPNode(:Addition, +, [n_number1, n_number2])

        @test gp_print(n_add) == "+( 40, 2 )"
        r = replace_node!(n_add, n_number1, GPNode(:Number, 41))
        @test gp_print(n_add) == "+( 41, 2 )"
        @test r === n_add
    end

    @testset "Root node" begin
        n_number1 = GPNode(:Number, 40)
        n_number2 = GPNode(:Number, 2)
        n_add = GPNode(:Addition, +, [n_number1, n_number2])

        @test gp_print(n_add) == "+( 40, 2 )"
        r = replace_node!(n_add, n_add, GPNode(:Number, 41))
        @test gp_print(r) == "41"
        @test gp_print(n_add) == "+( 40, 2 )"
        @test r !== n_add
    end

    @testset "Not existing node" begin
        n_number1 = GPNode(:Number, 40)
        n_number2 = GPNode(:Number, 2)
        n_add = GPNode(:Addition, +, [n_number1, n_number2])

        r = replace_node!(n_add, GPNode(:Number, 41), GPNode(:Number, 42))
        @test gp_print(n_add) == "+( 40, 2 )"
        @test r === n_add
    end

    @testset "Collect" begin
        n_number1 = GPNode(:Number, 40)
        n_number2 = GPNode(:Number, 2)
        n_add = GPNode(:Addition, +, [n_number1, n_number2])

        r = gp_collect(n_add, :Number)
        @test r == [n_number1, n_number2]
    end

    @testset "Replace node" begin
        n_number1 = GPNode(:Number, 40)
        n_number2 = GPNode(:Number, 2)
        n_add = GPNode(:Addition, +, [n_number1, n_number2])

        r = replace_node!(n_add, n_number1, GPNode(:Number, 42))
        @test gp_print(r) == "+( 42, 2 )"
    end
end
