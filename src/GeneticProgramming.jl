module GeneticProgramming

using Infiltrator

export GPNode
export number_of_children, gp_type, gp_value, gp_children
export gp_eval

struct GPNode
    type::Symbol
    value::Any
    children::Vector{GPNode}

    function GPNode(type::Symbol=:UNDEFINED, value::Any=nothing, children::Vector{GPNode}=GPNode[])
        return new(type, value, children)
    end
end

gp_type(n::GPNode) = n.type
gp_value(n::GPNode) = n.value
gp_children(n::GPNode) = n.children
number_of_children(n::GPNode) = length(gp_children(n))

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

end # module
