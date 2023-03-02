
@testset "Examples" begin
    @testset "Sum" begin
        rules = [
            :expr => [:number],
            :expr => [Atom(; print=infix_print(), value_factory=()->+), :number, :expr],
            :number => [Atom(; value_factory=() -> rand([1, 3, 7]))]
        ]
        config = GPConfig(rules)

        gp = GPSearch(config, (ind)->abs(21 - gp_eval(ind)), <)
        gp_result = rungp(gp)
        @test gp_result.fitness == 0
        # @test gp_print(gp_result.best) == "7 + 7 + 7"
    end
end