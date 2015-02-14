module SquareRoot

using PolyRings, Polynomials, Fractions

export Sqrt

function Sqrt(a::Int)
  r = X_mod(X^2-a)
  function Fractions.simplify(num::typeof(r), den::typeof(r))
    if deg(den.v) > 0
      conj = typeof(den)(X*den.v.coeffs[2] - den.v.coeffs[1])
      num *= conj
      den *= conj
    end
    g = gcd(num.v.coeffs...,den.v.coeffs...)
    sim(x) = typeof(r)(Polynomial{Int}([div(a,g) for a in x.v.coeffs]))
    (sim(num), sim(den))
  end
  Base.convert{T<:Number}(::Type{T}, x::typeof(r)) = convert(T, x.v.coeffs[1] + if deg(x.v)>0 sqrt(a)*x.v.coeffs[2] else 0 end)
  Fraction(r)
end

end