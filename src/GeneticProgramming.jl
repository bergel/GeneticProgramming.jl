module GeneticProgramming

using Random

export GPNode
export number_of_children, gp_type, gp_value, gp_children
export gp_eval, gp_print, gp_copy, gg_type
export gp_number
export has_parent
export gp_replace, gp_replace!, gp_collect_and_replace!, gp_collect_and_replace
export match_constraints
export gp_depth, gp_width
export replace_node!

export GPConfig
export Atom
export build_individual, mutate, crossover
export infix_print, minimal_infix_print, prefix_print, gp_default_print

export GPSearch
export rungp

export print_population # Good for debugging

struct UseContext end
# Atom in the grammar (contained in the rules)
struct Atom
    id::Symbol
    value_factory::Function
    print::Function
    use_context::Bool

    function Atom(;print::Function=gp_default_print, id::Symbol=Symbol("UNK"), value_factory::Function=()->nothing)
        return Atom(print, id, value_factory)
    end

    function Atom(print::Function=gp_default_print, id::Symbol=Symbol("UNK"), value_factory::Function=()->nothing)
        return new(id, value_factory, print, false)
    end

    function Atom(id::Symbol=Symbol("UNK"), value_factory::Function=()->nothing)
        return new(id, value_factory, gp_default_print, false)
    end

    function Atom(uc::UseContext; print::Function=gp_default_print, id::Symbol=Symbol("UNK"), value_factory::Function=()->nothing)
        return Atom(uc, print, id, value_factory)
    end

    function Atom(::UseContext, print::Function=gp_default_print, id::Symbol=Symbol("UNK"), value_factory::Function=()->nothing)
        return new(id, value_factory, print, true)
    end

    function Atom(::UseContext, id::Symbol=Symbol("UNK"), value_factory::Function=()->nothing)
        return new(id, value_factory, gp_default_print, true)
    end
end

# GPNode is set as mutable since we need to set the parent link
mutable struct GPNode
    type::Symbol
    value::Any
    children::Vector{GPNode}
    print::Function
    producing_rule::Pair{Symbol, Vector{Any}}
    parent::Union{GPNode,Nothing}

    function GPNode(
            type::Symbol=:UNDEFINED,
            value::Any=nothing,
            children::Vector{GPNode}=GPNode[],
            print::Function=gp_default_print,
            producing_rule::Pair{Symbol, Vector{Any}}=:UNDEFINED=>[],
            parent::Union{GPNode,Nothing}=nothing
        )
        new_node = new(type, value, children, print, producing_rule, parent)

        # We set the parent node
        for child in children
            @assert !has_parent(child)
            child.parent = new_node
        end
        return new_node
    end

    function GPNode(
        atom::Atom,
        children::Vector{GPNode}=GPNode[],
        producing_rule::Pair{Symbol, Vector{Any}}=:UNDEFINED=>[]
    )
        return new(atom.id, atom.value_factory(), children, atom.print, producing_rule)
    end

     function GPNode(
        parent::GPNode,
        atom::Atom,
        children::Vector{GPNode}=GPNode[],
        producing_rule::Pair{Symbol, Vector{Any}}=:UNDEFINED=>[],
    )
        return new(atom.id, atom.value_factory(GPContext(parent)), children, atom.print, producing_rule, parent)
    end
end

struct GPContext
    parent::GPNode
end

has_parent(n::GPNode) = !isnothing(n.parent)
gp_type(n::GPNode) = n.type
gp_value(n::GPNode) = n.value
gp_children(n::GPNode) = n.children
number_of_children(n::GPNode) = length(gp_children(n))

# Return a collection of strings
function gp_default_print(n::GPNode, res::Vector{String})
    return prefix_print("( ", ", ", " )")(n, res)
end

function gp_eval(n::GPNode)
    return gp_eval(n::GPNode, Dict{Symbol,Any}())
end

function gp_eval(n::GPNode, context::Dict{Symbol,Any})
    if(gp_value(n) isa Function)
        evaluated_children = [gp_eval(a_node, context) for a_node in gp_children(n)]
        return gp_value(n)(evaluated_children...)
    else
        return gp_value(n)
    end
end

function gp_print(n::GPNode)
    res = String[]
    gp_print(n, res)
    return join(res)
end

function gp_print(n::GPNode, res::Vector{String})
    n.print(n, res)
end

function gp_copy(n::GPNode)
    copied_children = GPNode[gp_copy(a_node) for a_node in gp_children(n)]
    return GPNode(gp_type(n), gp_value(n), copied_children, n.print, n.producing_rule)
end

# GENETIC PROGRAMMING
struct GPConfig
    root::Symbol
    rules::Vector{Pair{Symbol, Vector{Any}}}
    minimum_depth::Int64
    maximum_depth::Int64
    minimum_width::Int64
    maximum_width::Int64
    seed::Int64

    function GPConfig(
            rules::Vector{Pair{Symbol,Vector{Any}}};
            root::Symbol=first(first(rules)),
            seed::Int64=42,
            minimum_depth=0,
            maximum_depth=10,
            minimum_width=0,
            maximum_width=20
        )
        Random.seed!(seed)
        return new(
            root,
            rules,
            minimum_depth,
            maximum_depth,
            minimum_width,
            maximum_width,
            seed)
    end
end

struct GPSearch
    config::GPConfig
    fitness::Function
    comparison::Function

    population_size::Int64
    max_generations::Int64

    rate_mutation::Float64
    rate_crossover::Float64

    termination::Function

    GPSearch(config::GPConfig) = GPSearch(config, fitness=(ind)->gp_eval(ind), comparison=<)
    GPSearch(config::GPConfig, fitness::Function, comparison::Function) =
        GPSearch(config, fitness, comparison, 10, 10, 0.2, 0.6)
    GPSearch(
        config::GPConfig,
        fitness::Function,
        comparison::Function,
        population_size::Int64,
        max_generations::Int64,
    ) =
        new(config, fitness, comparison, population_size, max_generations, 0.2, 0.6)
    GPSearch(
        config::GPConfig,
        fitness::Function,
        comparison::Function,
        population_size::Int64=10,
        max_generations::Int64=10,
        termination::Function=iszero,
        rate_crossover::Float64=1.0,
        rate_mutation::Float64=0.8,
    ) =
        new(config, fitness, comparison, population_size, max_generations, rate_mutation, rate_crossover, termination)

    GPSearch(config::GPConfig, fitness::Function;
            comparison::Function=<, population_size::Int64=10, max_generations::Int64=10,
            rate_mutation::Float64=0.8, rate_crossover::Float64=1.0,
            termination::Function=(f)->f==0
            ) =
        new(config, fitness, comparison, population_size, max_generations, rate_mutation, rate_crossover, termination)


end

mutable struct GPResult
    fitnesses::Vector{Number}
    best_individuals::Vector{GPNode}
    fitness::Number
    best::GPNode
    last_population::Vector{GPNode}
    GPResult() = new(Number[], GPNode[], 0, GPNode())
end

function select(population::Vector{GPNode}, gp::GPSearch, k::Int64=5)
    local best::GPNode = population[1]
    local best_fitness = gp.fitness(best)
    for i in 1:k
        ind = rand(population)
        f = gp.fitness(ind)
        if gp.comparison(f, best_fitness)
            best = ind
            best_fitness = f
        end
    end
    return best
end

function best_of_population(gp::GPSearch, population::Vector{GPNode})
    local best_ind = nothing
    for ind in population
        if isnothing(best_ind)
            best_ind = ind
        else
            if gp.comparison(gp.fitness(ind), gp.fitness(best_ind))
                best_ind = ind
            end
        end
    end
    return best_ind
end

function rungp(gp::GPSearch)
    local result = GPResult()
    local initial_population::Vector{GPNode} #MAYBE NOT NECESSARY
    local new_population::Vector{GPNode} #MAYBE NOT NECESSARY
    local config = gp.config
    local old_population
    local ind1

    Random.seed!(gp.config.seed)
    initial_population = GPNode[build_individual(gp.config) for _ in 1:gp.population_size]

    # Sanity check
    for i in initial_population
        if !match_constraints(gp.config, i)
            isdefined(Main, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
        end
        @assert match_constraints(gp.config, i)
    end

    old_population = initial_population

    local result_crossover, ind1
    for _ in 1:gp.max_generations
        new_population = GPNode[]
        local best_ind = nothing
        push!(new_population, best_of_population(gp, old_population))

       #print_population(old_population)

        for __ in 1:(gp.population_size-1)
            #@assert match_constraints(gp.config, ind1)
            #@assert match_constraints(gp.config, ind2)
            #printstyled("$(gp_print(ind1))\n", color=:red)
            #printstyled("$(gp_print(ind2))\n", color=:blue)

            # Performing cross over
            local result_crossover
            local ind1
            if rand() < gp.rate_crossover
                for _ in 1:5
                    ind1 = select(old_population, gp)
                    ind2 = select(old_population, gp)

                    result_crossover = crossover(config, ind1, ind2)
                    !isnothing(result_crossover) && break
                end
                if isnothing(result_crossover)
                    #@warn "Cannot perform a crossover between\nind1=$(gp_print(ind1))\n\nind2=$(gp_print(ind2))"
                    result_crossover = gp_copy(ind1)
                end
            else
                result_crossover = select(old_population, gp)
            end
            @assert match_constraints(gp.config, result_crossover)

            if rand() < gp.rate_mutation
                new_ind = mutate(config, result_crossover)
            else
                new_ind = result_crossover
            end
            @assert match_constraints(gp.config, new_ind)

            push!(new_population, new_ind)
        end
        best_ind = gp_copy(best_of_population(gp, new_population))
        push!(result.best_individuals, best_ind)
        push!(result.fitnesses, gp.fitness(best_ind))
        old_population = new_population
        gp.termination(gp.fitness(best_ind)) && break
    end
    result.best = last(result.best_individuals)
    result.fitness = last(result.fitnesses)
    result.last_population = old_population
    return result
end

function candidate_rules(gp::GPConfig, id::Symbol)
    return filter(a_rule -> first(a_rule) == id, gp.rules)
end

function build_individual(gp::GPConfig)
    return build_individual(gp, first(first(gp.rules)))
end

function build_individual(gp::GPConfig, id::Symbol)
    local ind
    for _ in 1:5
        ind = _build_individual(gp, id, 1, 1)
        match_constraints(gp, ind) && return ind
    end
    return ind
    #error("Cannot create constrained individual")
end

function _build_individual(gp::GPConfig, id::Symbol, depth::Int64, width::Int64)
    candidate_r = candidate_rules(gp, id)
    @assert !isempty(candidate_r) "No candidate rule found for \"$id\""

    if(depth <= gp.minimum_depth || width <= gp.minimum_width)
        size_of_candidate_rules = map(r->length(last(r)), candidate_r)
        smallest_rule_size = min(size_of_candidate_rules...)
        candidate_r_for_minimum = filter(r->length(last(r)) > smallest_rule_size, candidate_r)
        if(isempty(candidate_r_for_minimum))
            candidate_r_for_minimum = candidate_r
        end
        selected_rule = rand(candidate_r_for_minimum)
    elseif(depth >= gp.maximum_depth || width >= gp.maximum_width)
        size_of_candidate_rules = map(r->length(last(r)), candidate_r)
        smallest_rule_size = min(size_of_candidate_rules...)
        candidate_r_for_maximum = filter(r->length(last(r)) == smallest_rule_size, candidate_r)
        selected_rule = rand(candidate_r_for_maximum)
    else
        selected_rule = rand(candidate_r)
    end

    terminal_atoms = filter(elem -> elem isa Atom, last(selected_rule))
    if isempty(terminal_atoms)
        # We simply do a recursion
        @assert length(last(selected_rule)) == 1
        return _build_individual(gp, last(selected_rule)[1], depth+1, width)
    else
        @assert length(terminal_atoms) == 1
        terminal_atom = first(terminal_atoms)
        non_atoms = filter(elem -> !(elem isa Atom), last(selected_rule))
        #@infiltrate !isempty(filter(x -> !(x isa Symbol), non_atoms))
        children::Vector{GPNode} = [_build_individual(gp, an_id, depth+1, width+length(non_atoms)) for an_id in non_atoms]
        # return GPNode(terminal_atom.id, terminal_atom.value_factory(), children, terminal_atom.print, selected_rule)
        return GPNode(terminal_atom, children, selected_rule)
    end
end

# Produce:
#   outer_before CHILD1 inner_before VALUE inner_after CHILD2 outer_after
#   outer_before CHILD1 inner_before VALUE inner_after CHILD2 inner_before VALUE inner_after CHILD3 outer_after
minimal_infix_print(outer_before::String="", outer_after::String="") =
    infix_print(outer_before, outer_after, "", "")

#function infix_print(outer_before::String="", inner_before::String=" ", inner_after::String=" ", outer_after::String="")
function infix_print(outer_before::String="", outer_after::String="", inner_before::String=" ", inner_after::String=inner_before)
    return (n::GPNode, res::Vector{String}) -> begin
        push!(res, outer_before)
        #@infiltrate
        foreach((index, child)->
            begin
                gp_print(child, res)
                if(index !== number_of_children(n))
                    push!(res, inner_before)
                    if !isnothing(gp_value(n))
                        push!(res, string(gp_value(n)))
                        push!(res, inner_after)
                    end
                end
            end,
            1:number_of_children(n), gp_children(n))
        push!(res, outer_after)
    end
end

function prefix_print(outer_before::String, inner::String, outer_after::String)
    return (n::GPNode, res::Vector{String}) -> begin
        push!(res, string(gp_value(n)))
        number_of_children(n) == 0 && return

        push!(res, outer_before)
        foreach((index, child)->
            begin
                gp_print(child, res)
                index !== number_of_children(n) && push!(res, inner)
            end,
            1:number_of_children(n), gp_children(n))
        push!(res, outer_after)
    end
end


# Genetic operation
function enumerate_nodes(n::GPNode)
    res = GPNode[]
    enumerate_nodes(n, res)
    return res
end

function enumerate_nodes(n::GPNode, res::Vector{GPNode})
    push!(res, n)
    foreach(nn->enumerate_nodes(nn, res), gp_children(n))
end

function are_from_same_production_rule(ind1::GPNode, ind2::GPNode)
    return ind1.producing_rule == ind2.producing_rule
end

function crossover(gp::GPConfig, ind1::GPNode, ind2::GPNode)
    # We try several time before returning an error
    for _ in 1:5
        t = _crossover(gp, ind1, ind2)
        !isnothing(t) && match_constraints(gp, t) && return t
    end
    return nothing
end

function _crossover(gp::GPConfig, ind1::GPNode, ind2::GPNode)
    # Select a node in ind1
    copy_ind1 = gp_copy(ind1)
    all_subnodes_ind1 = enumerate_nodes(copy_ind1)[1:end]
    selected_remplacement_node_in_ind1 = rand(all_subnodes_ind1)

    # Select a corresponding node in ind2
    all_subnodes_ind2 = enumerate_nodes(gp_copy(ind2))[1:end]
    all_matching_subnodes_ind2 = filter((node)->are_from_same_production_rule(node, selected_remplacement_node_in_ind1), all_subnodes_ind2)

    # Check if there is a node to be selected. If none, then simply return nothing to retry
    isempty(all_matching_subnodes_ind2) && return nothing

    # Pick the portion of ind2
    selected_node_in2 = rand(all_matching_subnodes_ind2)

    # We produce the new node
    return replace_node!(copy_ind1, selected_remplacement_node_in_ind1, selected_node_in2)
end

function match_constraints(gp::GPConfig, node::GPNode)
    return  gp.minimum_depth <= gp_depth(node) <= gp.maximum_depth &&
            gp.minimum_width <= gp_width(node) <= gp.maximum_width
end

# This seems like an impossible problem to solve. Some ind cannot be properly mutated.
function mutate(gp::GPConfig, n::GPNode)
    local t
    # We try several time before returning an error
    for _ in 1:20
        t = _mutate(gp, n)
        gp_print(t) != gp_print(n) && match_constraints(gp, t) && return t
    end
    return t
#=     isdefined(Main, :Infiltrator) && Main.infiltrate(@__MODULE__, Base.@locals, @__FILE__, @__LINE__)
    error("Cannot perform a mutation")
    return nothing
 =#
end

function _mutate(gp::GPConfig, n::GPNode)
    n_copy = gp_copy(n)
    all_subnodes = enumerate_nodes(n_copy)[1:end]
    selected_remplacement_node = rand(all_subnodes)
    new_ind = build_individual(gp, first(selected_remplacement_node.producing_rule))
    return replace_node!(n_copy, selected_remplacement_node, new_ind)
end

# This function modifies the provided tree with root.
# It returns the new tree.
function replace_node!(root::GPNode, from::GPNode, to::GPNode)
    root === from && return to
    if from in gp_children(root)
        children = gp_children(root)
        children[first(findall(x->x==from, gp_children(root)))] = to
        return root
    end
    foreach(nn->replace_node!(nn, from, to), gp_children(root))
    return root
end

function gp_replace(type_to_replace::Symbol, transformation::Function, ast::GPNode, index::Int64=1)
    copy = gp_copy(ast)
    gp_replace!(type_to_replace, transformation, copy)
    return copy
end

function gp_replace!(type_to_replace::Symbol, transformation::Function, ast::GPNode, index::Int64=1)
    if ast.type == type_to_replace
        ast.value = transformation(index)
    end
    global_index = index
    for child in ast.children
        gp_replace!(type_to_replace, transformation, child, global_index)
        global_index = global_index + 1
    end
end

function gp_collect_and_replace!(
    ast::GPNode,
    type_to_be_collected::Symbol,
    type_to_be_replaced::Symbol,
    transformation::Function,
    collected_values::Vector{Any}=[]
)
    # Collect value at the same level of the node
    if ast.type == type_to_be_collected
        push!(collected_values, ast.value)
    end

    # Collect value from children
    for child in ast.children
        if child.type == type_to_be_collected
            push!(collected_values, child.value)
        end
    end

    if ast.type == type_to_be_replaced && !isempty(collected_values)
        ast.value = transformation(collected_values)
    end

    for child in ast.children
        gp_collect_and_replace!(child, type_to_be_collected, type_to_be_replaced, transformation, copy(collected_values))
    end
end

function gp_collect_and_replace(
    ast::GPNode,
    type_to_be_collected::Symbol,
    type_to_be_replaced::Symbol,
    transformation::Function,
)
    ast_copy = gp_copy(ast)
    gp_collect_and_replace!(ast_copy, type_to_be_collected, type_to_be_replaced, transformation)
    return ast_copy
end

function gp_depth(n::GPNode, _accumulator::Int64=0)
    number_of_children(n) == 0 && return _accumulator

    tt = [gp_depth(nn) for nn in n.children]
    return _accumulator + max(tt...) + 1
end

# Number of leaves
function gp_width(n::GPNode)
    local accumulator::Int64
    accumulator = 0
    function _run(n::GPNode)
        if number_of_children(n) == 0
            accumulator += 1
        else
            for nn in n.children
                _run(nn)
            end
        end
    end
    _run(n)
    return accumulator
end

function print_population(population)
    println("Population:")
    for ind in population
        println("   $(gp_print(ind))")
    end
end

end # module
