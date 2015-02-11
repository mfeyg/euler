module PolyRings

export X_mod, modulus

require("/Users/Mendel/Desktop/euler/Polynomials.jl/Polynomials.jl")
using Polynomials

abstract PolyRing{T<:Number} <: Number

function X_mod{T<:Number}(m::Polynomial{T})
  Ring = symbol("PolyRing", gensym())
  eval(quote
         immutable $Ring <: PolyRing{$T}
           v :: Polynomial{$T}
         end

         Base.promote_rule(::Type{$T}, ::Type{$Ring}) = $Ring
         Base.convert(::Type{$Ring}, x::$T) = $Ring(x)

         modulus(::Type{$Ring}) = $m
         modulus(::$Ring) = $m

         ## FIXME
         function Base.convert{R <: PolyRing{$T}}(::Type{R}, x::$Ring)
           (d,r) = divrem(modulus(R), modulus(x))
           if r != 0
             error("Incompatible moduli")
           else
             R(d*x.v)
           end
         end

         +(a::$Ring, b::$Ring) = $Ring((a.v+b.v)%$m)
         -(a::$Ring, b::$Ring) = $Ring((a.v-b.v)%$m)
         -(a::$Ring) = $Ring((-a.v)%$m)
         *(a::$Ring, b::$Ring) = $Ring((a.v*b.v)%$m)
         ==(a::$Ring, b::$Ring) = (a.v-b.v)%$m == 0

         return $Ring(X)
       end)
end

end