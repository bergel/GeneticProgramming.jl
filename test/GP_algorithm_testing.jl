
@testset "Examples" begin
    @testset "Sum 1" begin
        rules = [
            :expr => [:number],
            :expr => [Atom(; print=infix_print(), value_factory=()->+), :number, :expr],
            :number => [Atom(; value_factory=()->rand([1, 3, 7]))]
        ]
        config = GPConfig(rules; maximum_width = 4)

        gp = GPSearch(config, (ind)->abs(21 - gp_eval(ind)), <,
                        30, 50, iszero)
        gp_result = rungp(gp)
        @test gp_width(gp_result.best) == 3
        @test gp_result.fitness == 0
        @test gp_print(gp_result.best) == "7 + 7 + 7"
    end

    @testset "Sum 2" begin
        rules = [
            :expr => [:number],
            :expr => [Atom(; print=infix_print(), value_factory=()->+), :number, :expr],
            :number => [Atom(; value_factory=()->rand([1, 3, 7]))]
        ]
        config = GPConfig(rules; maximum_width = 10)

        gp = GPSearch(config, (ind)->abs(21 - gp_eval(ind)), <,
                        30, 50, iszero)
        gp_result = rungp(gp)
        @test gp_width(gp_result.best) == 5
        @test gp_result.fitness == 0
        @test gp_print(gp_result.best) == "7 + 7 + 3 + 1 + 3"
    end

    @testset "Function 1" begin
        function myfitness(ind)
            f(x) = x*x + 3*x + 1
            r::Int64 = 0
            for x in -10:10
                modified_ind = gp_replace(:variable, (_) -> x, ind)
                delta = abs(gp_eval(modified_ind) - f(x))
                r += min(abs(delta * delta), 5000)
            end
            return r
        end

        rules = [
            :expr => [:number],
            :expr => [:variable],

            :expr => [Atom(; print=infix_print(), value_factory=()->+), :expr, :expr],
            :expr => [Atom(; print=infix_print(), value_factory=()->*), :expr, :expr],

            :number => [Atom(; value_factory=() -> rand(-10:10))],
            :variable => [Atom(; id=:variable, value_factory=() -> "x")]
        ]
        config = GPConfig(rules; maximum_depth=10, maximum_width=20)
        gp = GPSearch(config, myfitness, <, 200, 30, iszero, 1.0, 1.0)
        gp_result = rungp(gp);

        @test gp_result.fitnesses[1] > gp_result.fitnesses[6]
        @test gp_print(gp_result.best) == "x + x + x * x + x + 7 + -6"
        @test gp_result.fitness == 0
    end

    @testset "Function 2" begin
        function myfitness(ind)
            f(x) = x*x + 4*x + 1
            r::Int64 = 0
            for x in -10:10
                modified_ind = gp_replace(:variable, (_) -> x, ind)
                delta = abs(gp_eval(modified_ind) - f(x))
                r += min(abs(delta * delta), 5000)
            end
            return r
        end

        rules = [
            :expr => [:number],
            :expr => [:variable],

            :expr => [Atom(; print=infix_print(), value_factory=()->+), :expr, :expr],
            :expr => [Atom(; print=infix_print(), value_factory=()->*), :expr, :expr],

            :number => [Atom(; value_factory=() -> rand(-10:10))],
            :variable => [Atom(; id=:variable, value_factory=() -> "x")]
        ]
        config = GPConfig(rules; maximum_depth=10, maximum_width=20)
        gp = GPSearch(config, myfitness, <, 200, 30, iszero, 1.0, 1.0)
        gp_result = rungp(gp);

        @test gp_result.fitnesses[1] > gp_result.fitnesses[end]
        @test gp_print(gp_result.best) == "x + x + 1 + x + x * x + x"
    end
end
