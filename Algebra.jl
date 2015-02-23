module Algebra

export @algebra

abstract Alg <: Number

macro algebra(types, with, fml)
  # ex: @algebra B{args} = R[[I]] with B(a) * B(b) = expr
  @assert types.head == :(=)
  A = types.args[1]
  if isa(A,Symbol)
    B = A
    args = []
  elseif isa(A,Expr) && A.head == :curly
    B = A.args[1]
    args = A.args[2:end]
  else
    error("Invalid type")
  end
  t = types.args[2]
  @assert t.head == :ref
  R = t.args[1]
  @assert t.args[2].head == :vcat
  I = t.args[2].args[1]
  @assert with == :with
  @assert fml.head == :(=)
  lhs,expr = fml.args
  @assert lhs.head == :call && lhs.args[1] == :*
  x,y = lhs.args[2:3]
  @assert x.head == y.head == :call && x.args[1] == y.args[1] == B
  a,b = x.args[2], y.args[2]

  A = esc(A)
  I = esc(I)
  R = esc(R)
  B = esc(B)
  mul = esc(:(($a,$b) -> $expr))
  * = Expr(:curly, :(Base.(:*)), args...)

  return quote
    immutable $A <: Alg
      coeffs :: Dict{$I,$R}
      $A(coeffs::Dict) = new(filter((i,a)->a!=0, coeffs))
    end

    $A(i::$I) = $A(Dict(i => one($R)))

    $(*)(λ::$R, α::$A) = $A([i => λ*a for (i,a) in α.coeffs])
    $(*)(α::$A, λ::$R) = λ*α
    $(*)(α::$A, β::$A) = reduce(+, zero($A), [a*b*mul(i,j) for (i,a) in α.coeffs, (j,b) in β.coeffs])
  end
end

# macro algebra(alg, mul)
#   if alg.head != :curly || length(alg.args) != 3
#     error("Invalid syntax")
#   end
#   A,R,I = alg.args
#   return quote
#     R,I = $(esc(R)), $(esc(I))
#     mul = $(esc(mul))

#     immutable $(esc(A)) <: Alg
#       coeffs :: Dict{I,R}
#       $(esc(A))(coeffs::Dict) = new(filter((i,a)->a!=0, coeffs))
#     end
#     A = $(esc(A))

#     $(esc(A))(i::I) = A(Dict(i => one(R)))

#     Base.(:*)(λ::R, α::A) = A([i => λ*a for (i,a) in α.coeffs])
#     Base.(:*)(α::A, λ::R) = λ*α
#     Base.(:*)(α::A, β::A) = reduce(+, zero(A), [a*b*mul(i,j) for (i,a) in α.coeffs, (j,b) in β.coeffs])
#   end
# end

+{Exp <: Alg}(α::Exp, β::Exp) = Exp([i => get(α.coeffs, i, 0) + get(β.coeffs, i, 0)
                                     for i in union(keys(α.coeffs), keys(β.coeffs))])

-{Exp <: Alg}(α::Exp) = Exp([i => -α.coeffs[i] for i in keys(α.coeffs)])

-{Exp <: Alg}(α::Exp, β::Exp) = α+(-β)

=={Exp <: Alg}(α::Exp, β::Exp) = α.coeffs == β.coeffs

zero{Exp <: Alg}(::Type{Exp}) = Exp(Dict())

end
