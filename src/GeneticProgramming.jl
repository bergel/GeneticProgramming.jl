module GeneticProgramming

#using Infiltrator

export GPNode
export number_of_children, gp_type, gp_value, gp_children
export gp_eval, gp_print, gp_copy

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


end # module
