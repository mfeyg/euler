import Base.convert
import Base.promote_rule

immutable Mod{T<:Integer} <: Number
  n::T
  m::T
  Mod(n::T,m::T) = new(m==0?n:mod(n,m),m)
end
Mod{T<:Integer}(n::T, m::T) = Mod{T}(n,m)
Mod(n::Integer, m::Integer) = Mod(promote(n,m)...)

convert{T<:Integer}(::Type{Mod{T}}, n::T) = Mod(n, 0)
promote_rule{T<:Integer}(::Type{T}, ::Type{Mod{T}}) = Mod{T}

zero(a::Mod) = Mod(0, a.m)
one(a::Mod) = Mod(1, a.m)

function +(a::Mod, b::Mod)
  Mod(a.n + b.n, gcd(a.m, b.m))
end

-(a::Mod) = Mod(-a.n, a.m)
-(a::Number,b::Number) = a+(-b)

function *(a::Mod, b::Mod)
  a += zero(b)
  b += zero(a)
  a.n >= b.n || ((a,b) = (b,a))
  b.n > 0 || return zero(a)
  d,r = b.n >> 1, b.n & 1
  x = d*a
  (r == 1 ? a : 0) + x + x
end

function ^(a::Mod, b::Integer)
  b > 0 || return one(a)
  d,r = b>>1, b&1
  x = a^d
  (r == 1 ? a : 1) * x * x
end
