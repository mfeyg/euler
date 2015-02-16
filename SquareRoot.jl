module SquareRoot

export sqrt

using Algebra

Sqrt = fAlgebra(Integer,Integer) do i,j sqrt(i*j) end

sqrt(n::Integer) = if n == 1 Sqrt(1) else ((a,b) -> a*Sqrt(b))(map(prod,zip([(x^div(y,2), x^isodd(y)) for (x,y) in factor(n)]...))...) end

Base.promote_rule{I<:Integer}(::Type{Sqrt}, ::Type{I}) = Sqrt
Base.convert(::Type{Sqrt}, i::Integer) = i*Sqrt(1)

end
