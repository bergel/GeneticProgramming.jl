module GeneticProgramming

using Infiltrator
using Random

export GPNode
export number_of_children, gp_type, gp_value, gp_children
export gp_eval, gp_print, gp_copy
export gp_number

export GPConfig
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
    rules::Vector{Pair{Symbol, Any}}
    non_terminal_rules::Vector{Pair{Symbol, Any}}
    terminal_rules::Vector{Pair{Symbol, Any}}

    function GPConfig(seed::Int64, root::Symbol, rules::Vector{Pair{Symbol, Any}})
        Random.seed!(seed)
        non_terminal_rules::Vector{Pair{Symbol, Any}} = filter(r -> !(last(r) isa Function), rules)
        #@infiltrate
        terminal_rules::Vector{Pair{Symbol, Any}} = filter(r -> (last(r) isa Function), rules)
        return new(root, rules, non_terminal_rules, terminal_rules)
    end
end

number_of_terminal_rules(config::GPConfig) = length(config.terminal_rules)
number_of_non_terminal_rules(config::GPConfig) = length(config.non_terminal_rules)


function gp_number()
    return GPNode(:number, rand(-10:10))
end

function candidate_rules(gp::GPConfig, id::Symbol)
    return filter(a_rule -> first(a_rule) == id, gp.rules)
end

function build_individual(gp::GPConfig)

end


is_terminal(gp::GPConfig, rule::Pair{Symbol, Any})=rule in gp.terminal_rules
is_non_terminal(gp::GPConfig, rule::Pair{Symbol, Any})=rule in gp.non_terminal_rules

function build_individual(gp::GPConfig, id::Symbol)
    #@infiltrate
    selected_rule = rand(candidate_rules(gp, id))
    #@infiltrate
    if(is_terminal(gp, selected_rule))
        return last(selected_rule)()
    else
        return "hello"
    end

end

function build_individual(gp::GPConfig, result::GPNode)

end
end # module
