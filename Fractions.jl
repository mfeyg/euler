module Fractions

export Fraction, simplify

immutable Fraction{T <: Number} <: Number
  num :: T
  den :: T
  Fraction{T}(x::T, y::T) = begin x,y = simplify(x,y) ; new(x,y) end
end

Fraction(num,den) = Fraction{promote_type(typeof(num), typeof(den))}(promote(num,den)...)

Base.promote_rule{R,T}(::Type{R}, ::Type{Fraction{T}}) = Fraction{promote_type(R,T)}
Base.promote_rule{R,T}(::Type{Fraction{T}}, ::Type{Fraction{R}}) = Fraction{promote_type(R,T)}
Base.convert{T}(::Type{Fraction{T}}, x) = Fraction{T}(convert(T,x), one(T))
Base.convert{R,T}(::Type{Fraction{T}}, x::Fraction{R}) = Fraction{T}(convert(T,x.num), convert(T,x.den))
Base.convert{T<:Number}(::Type{Fraction}, x::T) = Fraction{T}(x)

Base.convert{T<:Number}(::Type{T}, x::Fraction) = convert(T, convert(T, x.num) / convert(T, x.den))

=={T}(x::Fraction{T}, y::Fraction{T}) = x.num*y.den == y.num*x.den
-{T}(x::Fraction{T}) = Fraction{T}(-x.num,x.den)
+{T}(x::Fraction{T}, y::Fraction{T}) = Fraction{T}(x.num*y.den+y.num*x.den,x.den*y.den)
-(x::Fraction, y::Fraction) = x+(-y)
*{T}(x::Fraction{T}, y::Fraction{T}) = Fraction{T}(x.num*y.num, x.den*y.den)
/{T}(x::Fraction{T}, y::Fraction{T}) = Fraction{T}(x.num*y.den,x.den*y.num)

simplify(x::Integer, y::Integer) = map(a->div(a,gcd(x,y)),(x,y))
simplify(x,y) = (x,y)

end