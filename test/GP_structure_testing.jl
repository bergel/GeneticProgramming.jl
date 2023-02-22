@testset "Basic" begin
    rules = [
        :expr => [:number],
        :expr => [:number , Atom(value_factory=(()->+)) , :expr],
        :number => [Atom(id=:number, value_factory = ()->rand(-10:10))]
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
    rules = [
        :expr => [:number],
        :expr => [:number , Atom(; value_factory=(()->+), print=infix_print("(", ")")) , :expr],
        :number => [Atom(value_factory=()->rand(-10:10))]
    ]
    gp_config = GPConfig(
                    rules
                )

    @test GeneticProgramming.candidate_rules(gp_config, :expr) == rules[1:2]
    @test GeneticProgramming.candidate_rules(gp_config, :number) == rules[3:3]

    number_node = build_individual(gp_config, :number)
    @test number_node isa GPNode
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
    rules = [
        :chain => [:char],
        :chain => [:char, Atom(; value_factory=()->',', print=minimal_infix_print()), :chain],
        :char => [Atom(; value_factory = ()->'*')]
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
    rules = [
        :chain => [:char],
        :chain => [:char, Atom(; value_factory=()->',', print=minimal_infix_print()), :chain],
        :char => [Atom(; value_factory=()->'*')]
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
        :chain => [Atom(my_print, :seq,  ()->','), :chain, :chain, :chain],
        :char => [Atom(gp_default_print, :char, ()->'*')]
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
        :chain => [Atom(my_print, :seq,  ()->','), :chain, :chain, :chain],
        :char => [Atom(gp_default_print, :char, ()->'*')]
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
        :expr => [:term, Atom(infix_print(), :+, ()->+), :term],
        :expr => [:term, Atom(infix_print(), :-, ()->-), :term],

        :term => [:factor],
        :term => [:factor, Atom(infix_print(), :*, ()->*), :factor],
        :term => [:factor, Atom(infix_print(), :/, ()->/), :factor],

        :factor => [Atom(gp_default_print, :number, ()->rand(-10:10))],
        :factor => [Atom(infix_print("(", ")"), :parent, ()->"()"), :expr]
    ]
    gp_config = GPConfig(rules)

    @test gp_print(build_individual(gp_config, :expr)) == "4 * (((0) + -6) * 0) - (-4 * 8)"
    @test gp_print(build_individual(gp_config, :expr)) == "-7 * (3 + (7 * -4) * ((1) / (7) + 3 / -3)) + (-6 * ((-8) / (9) + 6 / (1)) - (-1 * 6 - -5 / (0))) / 6"
    @test gp_print(build_individual(gp_config, :expr)) == "(((2) / -5 - 1) / 9 - ((7) / 10 + (-2))) + (((-6) * 9 + -10) - -9 / -6)"
    @test gp_print(build_individual(gp_config, :expr)) == "(((5) / -8 - (-7) / 2) - -10 * -4) * (((4) / (8) + (0) / 5)) + (-8 * ((-10) * 1 + 1 / -6) + (-9 / 7) * -3) / (((-10) / 1) + ((-7) / (2) - (9) * (-10)))"
end

@testset "Local variables" begin
    function print_block(n::GPNode, res::Vector{String})
        push!(res, "[:")
        gp_print(gp_children(n)[1], res)
        push!(res, " | ")
        gp_print(gp_children(n)[2], res)
        push!(res, "]")

    end

    rules = [
        :expr => [:variable],
        :expr => [:sum_of_two_variables],
        #:expr => [ Atom(infix_print(), :+), :variable, :variable, :variable],
        :expr => [:block],
        :tmp_variable => [Atom(gp_default_print, :tmp_variable, () -> "X")],

        :sum_of_two_variables => [Atom(infix_print(), :+, () -> "+"), :variable, :variable],

        :block => [Atom(print_block), :tmp_variable, :expr],

        :number => [Atom(gp_default_print, :number, ()->rand(-10:10))],
        :variable => [Atom(gp_default_print, :variable, () -> "X")],
    ]

    # RAW generation, without local variables
    gp_config = GPConfig(rules)
    @test gp_print(build_individual(gp_config, :expr)) == "X + X"
    @test gp_print(build_individual(gp_config, :expr)) == "[:X | [:X | X + X]]"

    # Renaming some variables
    gp_config = GPConfig(rules)
    ast = build_individual(gp_config, :expr)
    gp_replace!(:variable, (index) -> string(collect('A':'Z')[index]), ast)
    @test gp_print(ast) == "A + B"

    # Renaming a block of a block
    original_ast = build_individual(gp_config, :expr)
    ast = gp_copy(original_ast)
    gp_replace!(:tmp_variable, (index) -> string(collect('A':'Z')[index]), ast)
    @test gp_print(ast) == "[:A | [:B | X + X]]"

    # We collect tmp_variables and replace :variable by any collected tmp_variables
    a = gp_collect_and_replace(ast, :tmp_variable, :variable, (set_of_tmp_variable) -> rand(set_of_tmp_variable))
    @test gp_print(a) == "[:A | [:B | B + A]]"

    a = gp_collect_and_replace(ast, :tmp_variable, :variable, (set_of_tmp_variable) -> rand(set_of_tmp_variable))
    @test gp_print(a) == "[:A | [:B | A + A]]"

    a = gp_collect_and_replace(ast, :tmp_variable, :variable, (set_of_tmp_variable) -> rand(set_of_tmp_variable))
    @test gp_print(a) == "[:A | [:B | A + A]]"

    a = gp_collect_and_replace(ast, :tmp_variable, :variable, (set_of_tmp_variable) -> rand(set_of_tmp_variable))
    @test gp_print(a) == "[:A | [:B | A + B]]"
end
