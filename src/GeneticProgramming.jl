module GeneticProgramming

using Random

export GPNode
export number_of_children, gp_type, gp_value, gp_children
export gp_eval, gp_print, gp_copy, gg_type
export gp_number
export has_parent

export GPConfig
export Atom
export build_individual, mutate
export infix_print, minimal_infix_print, prefix_print, gp_default_print

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
        for n in children
            n.parent = new_node
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

    function GPConfig(
            rules::Vector{Pair{Symbol,Vector{Any}}};
            root::Symbol=first(first(rules)),
            seed::Int64=42,
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
    return build_individual(gp, first(first(gp.rules)))
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
        #@infiltrate !isempty(filter(x -> !(x isa Symbol), non_atoms))
        children::Vector{GPNode} = [build_individual(gp, an_id, depth+1, width+length(non_atoms)) for an_id in non_atoms]
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

function mutate(gp::GPConfig, n::GPNode)
    n_copy = gp_copy(n)
    all_subnodes = enumerate_nodes(n_copy)[2:end]
    selected_remplacement_node = rand(all_subnodes)
    new_ind = build_individual(gp, first(selected_remplacement_node.producing_rule))
    replace_node(n_copy, selected_remplacement_node, new_ind)
    return n_copy
end

function replace_node(root::GPNode, from::GPNode, to::GPNode)
    if from in gp_children(root)
        children = gp_children(root)
        children[first(findall(x->x==from, gp_children(root)))] = to
        return
    end
    foreach(nn->replace_node(nn, from, to), gp_children(root))
end

end # module
