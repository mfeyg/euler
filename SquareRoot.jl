module SquareRoot

export sqrt

using Algebra, Fractions

Sqrt = fAlgebra(Integer,Integer) do i,j sqrt(i*j) end

sqrt(n::Integer) = if n == 1 Sqrt(1) else ((a,b) -> a*Sqrt(b))(map(prod,zip([(x^div(y,2), x^isodd(y)) for (x,y) in factor(n)]...))...) end

one(Sqrt) = Sqrt(1)

Base.promote_rule{I<:Integer}(::Type{Sqrt}, ::Type{I}) = Sqrt
Base.convert(::Type{Sqrt}, i::Integer) = i*Sqrt(1)

Base.show(io::IO, r::Sqrt) = write(io, begin
                                     s = join([if b == 1 string(a)
                                             elseif a == 1 string("√",b)
                                             else string(a,"√",b) end
                                               for (b,a) in r.coeffs]
                                              , " + ")
                                     if s == "" "0" else s end
                                   end)

/(a::Sqrt,b::Integer) = Fraction(a,b)
/(b::Integer,a::Sqrt) = Fraction(b,a)
/(a::Sqrt,b::Sqrt) = Fraction(a,b)

isrational(a::Sqrt) = all([b==1||c==0 for (b,c) in a.coeffs])

function rationalize(a::Sqrt)
  # don't try too hard
  if length(a.coeffs) > 20 || any([x > BigInt(10)^100 for x in values(a.coeffs)]) return 1 end
  if isrational(a) 1
  else
    x = Sqrt([k => a*(-1)^i for ((k,a),i) in zip(a.coeffs, 1:length(a.coeffs))])
    x*rationalize(a*x)
  end
end

function Fractions.simplify(num::Sqrt, den::Sqrt)
  function trans(f)
    num = f(num)
    den = f(den)
  end
  r = rationalize(den)
  if isrational(den*r)
    trans(x -> x*r)
  end
  if den == 0 error("Denominator is zero") end
  g = gcd([values(num.coeffs)..., values(den.coeffs)...]...)
  trans(x -> Sqrt([k => div(a,g) for (k,a) in x.coeffs]))
  if den.coeffs[1] < 0
    trans(x -> -x)
  end
  (num,den)
end

parenthesize(r::Sqrt) = if length(r.coeffs) > 1 string("(",r,")") else string(r) end

Base.show(io::IO, f::Fraction{Sqrt}) =
  if f.den == 1
    show(io, f.num)
  else
    write(io, string(parenthesize(f.num), "/", parenthesize(f.den)))
  end

end
