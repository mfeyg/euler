module Moduli

export modulo

immutable Modulo{T}
  modulus :: T
  val :: T
end

function Base.promote(x::Modulo, y::Modulo)
  if x.modulus == y.modulus
    (x, y)
  else
    error("Moduli must match")
  end
end

Base.promote{T}(x, y::Modulo{T}) = (Modulo(y.modulus, convert(T, x)), y)

Base.promote(x::Modulo, y) = reverse(Base.promote(y, x))

modulo{T}(x::T, m::T) = Modulo(m, x%m)

modulo(a,b) = modulo(promote(a,b)...)

modulus{T}(a::Modulo{T}, b::Modulo{T}) = if a.modulus != b.modulus error("Moduli must match") else b.modulus end

+{T}(a::Modulo{T}, b::Modulo{T}) = modulo(a.val+b.val, modulus(a,b))
-{T}(a::Modulo{T}, b::Modulo{T}) = modulo(a.val-b.val, modulus(a,b))
*{T}(a::Modulo{T}, b::Modulo{T}) = modulo(a.val*b.val, modulus(a,b))

=={T}(a::Modulo{T}, b::Modulo{T}) = (a.val-b.val) % modulus(a,b) == zero(T)

#quot{T}(x::T, a::Modulo{T}) = (modulo(x, a.modulus), a)
#quot{T}(a::Modulo{T}, x::T) = (a, modulo(x, a.modulus))

#promote_rule{T,R}(::Type{Modulo{T}}, ::Type{Modulo{R}}) = Modulo{promote_type(T,R)}##

+(a::Modulo, b) = +(promote(a,b)...)
+(a, b::Modulo) = +(promote(a,b)...)
-(a::Modulo, b) = -(promote(a,b)...)
-(a, b::Modulo) = -(promote(a,b)...)
*(a::Modulo, b) = *(promote(a,b)...)
*(a, b::Modulo) = *(promote(a,b)...)
==(a::Modulo, b) = ==(promote(a,b)...)
==(a, b::Modulo) = ==(promote(a,b)...)

end

