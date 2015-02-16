module SquareRoot

using Fractions

export Sqrt

immutable Sqrt
  n::Int
  function Sqrt(n)
    if n == 1
      (a,b) = (1,1)
    else
    (a,b) = map(prod,zip([(x^div(y,2), x^isodd(y)) for (x,y) in factor(n)]...))
    end
    Multiple(a,new(b))
  end
end

==(a::Sqrt, b::Sqrt) = a.n == b.n
*(a::Sqrt, b::Sqrt) = Sqrt(a.n*b.n)

immutable Multiple{A,B}
  a :: A
  b :: B
end

commute(x::Multiple) = Multiple(x.b, x.a)

*{A,B}(x::Multiple{A,B}, y::Multiple{A,B}) = Multiple{A,B}(x.a*y.a, x.b*y.b)
*{A,B}(x::Multiple{A,B}, y::Multiple{B,A}) = x*commute(y)
*{A,B}(a::A, x::Multiple{A,B}) = Multiple{A,B}(a*x.a, x.b)
*{A,B}(b::B, x::Multiple{A,B}) = Multiple{A,B}(x.a, b*x.b)
*{A,B}(x::Multiple{A,B}, y::Union(A,B)) = y*x

=={A,B}(x::Multiple{A,B}, y::Multiple{A,B}) = x.a == y.a && x.b == y.b
=={A,B}(x::Multiple{A,B}, y::Multiple{B,A}) = x == commute(y)
=={A,B}(x::Multiple{A,B}, a::A) = x.a == a && x.b == 1
=={A,B}(x::Multiple{A,B}, b::B) = x.b == b && x.a == 1
=={A,B}(y::Union(A,B), x::Multiple{A,B}) = x == y

((Sqrt(2)*Sqrt(3)*5)*(6*Sqrt(5)))^2

immutable Adjoin{A,B}
  a :: A
  b :: B
end

Adjoin{A,B}(a::A) = Adjoin{A,B}(a, zero(B))



# function Sqrt(a::Int)
#   r = X_mod(X^2-a)
#   function Fractions.simplify(num::typeof(r), den::typeof(r))
#     if deg(den.v) > 0
#       conj = typeof(den)(X*den.v.coeffs[2] - den.v.coeffs[1])
#       num *= conj
#       den *= conj
#     end
#     g = gcd(num.v.coeffs...,den.v.coeffs...)
#     sim(x) = typeof(r)(Polynomial{Int}([div(a,g) for a in x.v.coeffs]))
#     (sim(num), sim(den))
#   end
#   Base.convert{T<:Number}(::Type{T}, x::typeof(r)) = convert(T, x.v.coeffs[1] + if deg(x.v)>0 sqrt(a)*x.v.coeffs[2] else 0 end)
#   Fraction(r)
# end

end