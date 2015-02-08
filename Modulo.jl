module Mod

immutable Modulo{T}
  modulus :: T
  val :: T
end

modulo{T}(x::T, m::T) = Modulo(m, x%m)

modulus{T}(a::Modulo{T}, b::Modulo{T}) = if a.modulus != b.modulus error("Moduli must match") else b.modulus end

+{T}(a::Modulo{T}, b::Modulo{T}) = modulo(a.val+b.val, modulus(a,b))
-{T}(a::Modulo{T}, b::Modulo{T}) = modulo(a.val-b.val, modulus(a,b))
*{T}(a::Modulo{T}, b::Modulo{T}) = modulo(a.val*b.val, modulus(a,b))

=={T}(a::Modulo{T}, b::Modulo{T}) = (a.val-b.val) % modulus(a,b) == zero(T)

quot{T}(x::T, a::Modulo{T}) = (modulo(x, a.modulus), a)
quot{T}(a::Modulo{T}, x::T) = (a, modulo(x, a.modulus))

promote_rule{T,R}(::Type{Modulo{T}}, ::Type{Modulo{R}}) = Modulo{promote_type(T,R)}

convert{R}(::Type{Modulo{R}}, x::Modulo) = modulo(R(x.val), R(x.modulus))

+(a::Modulo, b::Modulo) = +(promote(a,b)...)
+{R}(x, a::Modulo{R}) = +(quot(covert(R,x),a)...)
+(a::Modulo, x) = +(x,a)


-{T}(a::Union(T, Modulo{T}), b::Union(T, Modulo{T})) = -(quot(a,b)...)
*{T}(a::Union(T, Modulo{T}), b::Union(T, Modulo{T})) = *(quot(a,b)...)
=={T}(a::Union(T, Modulo{T}), b::Union(T, Modulo{T})) = ==(quot(a,b)...)

end
