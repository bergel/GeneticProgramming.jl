@testset "Basic" begin
    rules = [
        :expr => [:number],
        :expr => [:number , Atom(:+, (()->+)) , :expr],
        :number => [Atom(:number, ()->rand(-10:10))]
    ]
    gp_config = GPConfig(
                    rules
                )

    @test GeneticProgramming.candidate_rules(gp_config, :expr) == rules[1:2]
    @test GeneticProgramming.candidate_rules(gp_config, :number) == rules[3:3]

    number_node = build_individual(gp_config, :number)
    @test number_node isa GPNode
    @test gp_type(number_node) == :number
    @test typeof(gp_value(number_node)) == Int64

    Random.seed!(42)
    expr_node = build_individual(gp_config, :expr)
    @test expr_node isa GPNode
    @test gp_print(expr_node) == "+( 0, +( -7, +( -1, 3 ) ) )"

    Random.seed!(41)
    @test gp_print(build_individual(gp_config, :expr)) == "+( -1, -10 )"
end

@testset "With printing" begin
    function gp_print_add(n::GPNode, res::Vector{String})
        push!(res, "(")
        gp_print(gp_children(n)[1], res)
        push!(res, " + ")
        gp_print(gp_children(n)[2], res)
        push!(res, ")")
    end

    rules = [
        :expr => [:number],
        :expr => [:number , Atom(:+, (()->+), gp_print_add) , :expr],
        :number => [Atom(:number, ()->rand(-10:10))]
    ]
    gp_config = GPConfig(
                    rules
                )

    @test GeneticProgramming.candidate_rules(gp_config, :expr) == rules[1:2]
    @test GeneticProgramming.candidate_rules(gp_config, :number) == rules[3:3]

    number_node = build_individual(gp_config, :number)
    @test number_node isa GPNode
    @test gp_type(number_node) == :number
    @test typeof(gp_value(number_node)) == Int64

    Random.seed!(42)
    expr_node = build_individual(gp_config, :expr)
    @test expr_node isa GPNode
    @test gp_print(expr_node) == "(0 + (-7 + (-1 + 3)))"
    @test gp_eval(expr_node) == (0 + (-7 + (-1 + 3)))

    Random.seed!(41)
    @test gp_print(build_individual(gp_config, :expr)) == "(-1 + -10)"
end

@testset "Another example" begin
    function my_print(n::GPNode, res::Vector{String})
        gp_print(gp_children(n)[1], res)
        push!(res, ",")
        gp_print(gp_children(n)[2], res)
    end
    rules = [
        :chain => [:char],
        :chain => [:char, Atom(:seq,  ()->',', my_print), :chain],
        :char => [Atom(:char, ()->'*')]
    ]
    gp_config = GPConfig(
                    rules
                )

    ind = build_individual(gp_config, :chain)
    @test gp_print(ind) == "*,*"

    ind = build_individual(gp_config, :chain)
    @test gp_print(ind) == "*,*,*"

    ind = build_individual(gp_config, :chain)
    @test gp_print(ind) == "*,*"
end


@testset "Another example with minimum and maximum depth" begin
    function my_print(n::GPNode, res::Vector{String})
        gp_print(gp_children(n)[1], res)
        push!(res, ",")
        gp_print(gp_children(n)[2], res)
    end
    rules = [
        :chain => [:char],
        :chain => [:char, Atom(:seq,  ()->',', my_print), :chain],
        :char => [Atom(:char, ()->'*')]
    ]
    gp_config = GPConfig(
                    rules;
                    minimum_depth=7,
                    maximum_depth=10
                )

    ind = build_individual(gp_config, :chain)
    @test gp_print(ind) == "*,*,*,*,*,*,*,*,*"

    ind = build_individual(gp_config, :chain)
    @test gp_print(ind) == "*,*,*,*,*,*,*,*,*"

    ind = build_individual(gp_config, :chain)
    @test gp_print(ind) == "*,*,*,*,*,*,*,*"
end

@testset "Another example with minimum and maximum depth (02)" begin
    function my_print(n::GPNode, res::Vector{String})
        gp_print(gp_children(n)[1], res)
        gp_print(gp_children(n)[2], res)
    end
    rules = [
        :chain => [:char],
        :chain => [Atom(:seq,  ()->',', my_print), :chain, :chain, :chain],
        :char => [Atom(:char, ()->'*')]
    ]
    gp_config = GPConfig(
                    rules;
                    minimum_depth = 4,
                    maximum_depth = 5
                )

    @test gp_print(build_individual(gp_config, :chain)) == "****************"
    @test gp_print(build_individual(gp_config, :chain)) == "****************"
end

@testset "Another example with minimum and maximum width (02)" begin
    function my_print(n::GPNode, res::Vector{String})
        gp_print(gp_children(n)[1], res)
        gp_print(gp_children(n)[2], res)
    end
    rules = [
        :chain => [:char],
        :chain => [Atom(:seq,  ()->',', my_print), :chain, :chain, :chain],
        :char => [Atom(:char, ()->'*')]
    ]
    gp_config = GPConfig(
                    rules;
                    minimum_width = 4,
                    maximum_width = 5
                )

    @test gp_print(build_individual(gp_config, :chain)) == "****"
    @test gp_print(build_individual(gp_config, :chain)) == "****"
end

@testset "ETF example" begin
    function print_parent(n::GPNode, res::Vector{String})
        push!(res, "(")
        gp_print(gp_children(n)[1], res)
        push!(res, ")")
    end
    rules = [
        :expr => [:term],
        :expr => [:term, Atom(:+, ()->+, gp_print_infix), :term],
        :expr => [:term, Atom(:+, ()->-, gp_print_infix), :term],

        :term => [:factor],
        :term => [:factor, Atom(:*, ()->*, gp_print_infix), :factor],
        :term => [:factor, Atom(:/, ()->/, gp_print_infix), :factor],

        :factor => [Atom(:number, ()->rand(-10:10))],
        :factor => [Atom(:parent, ()->"()", print_parent), :expr]
    ]
    gp_config = GPConfig(
                    rules
                )

    @test gp_print(build_individual(gp_config, :expr)) == "4 * (((0) + -6) * 0) + (-4 * 8)"
    @test gp_print(build_individual(gp_config, :expr)) == "-7 * (3 + (7 * -4) * ((1) / (7) + 3 / -3)) + (-6 * ((-8) / (9) + 6 / (1)) + (-1 * 6 + -5 / (0))) / 6"
    @test gp_print(build_individual(gp_config, :expr)) == "(((2) / -5 + 1) / 9 + ((7) / 10 + (-2))) + (((-6) * 9 + -10) + -9 / -6)"
    @test gp_print(build_individual(gp_config, :expr)) == "(((5) / -8 + (-7) / 2) + -10 * -4) * (((4) / (8) + (0) / 5)) + (-8 * ((-10) * 1 + 1 / -6) + (-9 / 7) * -3) / (((-10) / 1) + ((-7) / (2) + (9) * (-10)))"
end
