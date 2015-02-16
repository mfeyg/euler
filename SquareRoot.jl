module SquareRoot

export sqrt

using Algebra, Fractions

Sqrt = fAlgebra(Integer,Integer) do i,j sqrt(i*j) end

sqrt(n::Integer) = if n == 1 Sqrt(1) else ((a,b) -> a*Sqrt(b))(map(prod,zip([(x^div(y,2), x^isodd(y)) for (x,y) in factor(n)]...))...) end

Base.promote_rule{I<:Integer}(::Type{Sqrt}, ::Type{I}) = Sqrt
Base.convert(::Type{Sqrt}, i::Integer) = i*Sqrt(1)

Base.show(io::IO, r::Sqrt) = write(io, begin
                                     s = join([if b==1 string(a) elseif a==1 string("√",b) else string(a,"√",b) end for (b,a) in r.coeffs], " + ")
                                     if s == "" "0" else s end
                                   end)

/(a::Sqrt,b::Integer) = Fraction(a,b)
/(b::Integer,a::Sqrt) = Fraction(b,a)

end
