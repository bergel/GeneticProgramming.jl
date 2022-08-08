module GeneticProgramming

#using Infiltrator
using Random

export GPNode
export number_of_children, gp_type, gp_value, gp_children
export gp_eval, gp_print, gp_copy
export gp_number

export GPConfig
export Atom, gp_print_infix
export build_individual, number_of_terminal_rules, number_of_non_terminal_rules

struct GPNode
    type::Symbol
    value::Any
    children::Vector{GPNode}
    print::Function

    function GPNode(
            type::Symbol=:UNDEFINED,
            value::Any=nothing,
            children::Vector{GPNode}=GPNode[],
            print::Function=gp_default_print)
        return new(type, value, children, print)
    end
end

gp_type(n::GPNode) = n.type
gp_value(n::GPNode) = n.value
gp_children(n::GPNode) = n.children
number_of_children(n::GPNode) = length(gp_children(n))

# Return a collection of strings
function gp_default_print(n::GPNode, res::Vector{String})
    if(!(gp_value(n) isa Function))
        push!(res, string(gp_value(n)))
        return
    end

    push!(res, string(gp_type(n)))
    push!(res, "( ")
    for tuple in enumerate(gp_children(n))
        gp_print(tuple[2], res)
        (tuple[1] < number_of_children(n)) && push!(res, ", ")
    end
    push!(res, " )")
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
    return GPNode(gp_type(n), gp_value(n), copied_children, n.print)
end

# GENETIC PROGRAMMING

struct GPConfig
    root::Symbol
    rules::Vector{Pair{Symbol, Vector{Any}}}
    minimum_depth::Int64
    maximum_depth::Int64
    minimum_width::Int64
    maximum_width::Int64

    function GPConfig(
            seed::Int64,
            root::Symbol,
            rules::Vector{Pair{Symbol,Vector{Any}}};
            minimum_depth=1,
            maximum_depth=10,
            minimum_width=1,
            maximum_width=20
        )
        Random.seed!(seed)
        return new(
            root,
            rules,
            minimum_depth,
            maximum_depth,
            minimum_width,
            maximum_width)
    end
end

function candidate_rules(gp::GPConfig, id::Symbol)
    return filter(a_rule -> first(a_rule) == id, gp.rules)
end

function build_individual(gp::GPConfig)

end

function build_individual(gp::GPConfig, id::Symbol)
    return build_individual(gp, id, 1, 1)
end

function build_individual(gp::GPConfig, id::Symbol, depth::Int64, width::Int64)
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
        return build_individual(gp, last(selected_rule)[1], depth+1, width)
    else
        @assert length(terminal_atoms) == 1
        terminal_atom = first(terminal_atoms)
        non_atoms = filter(elem -> !(elem isa Atom), last(selected_rule))
        children::Vector{GPNode} = [build_individual(gp, an_id, depth+1, width+length(non_atoms)) for an_id in non_atoms]
        return GPNode(terminal_atom.id, terminal_atom.value(), children, terminal_atom.print)
    end
end

# Atom in the grammar (contained in the rules)
struct Atom
    id::Symbol
    value::Any
    print::Function

    function Atom(id::Symbol, value::Any=nothing, print::Function=gp_default_print)
        return new(id, value, print)
    end
end

function gp_print_infix(n::GPNode, res::Vector{String})
    gp_print(gp_children(n)[1], res)
    push!(res, " ")
    push!(res, string(gp_type(n)))
    push!(res, " ")
    gp_print(gp_children(n)[2], res)
end

end # module
