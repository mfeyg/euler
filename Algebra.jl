module Algebra

export @algebra

abstract Alg <: Number

macro algebra(alg, mul)
  if alg.head != :curly || length(alg.args) != 3
    error("Invalid syntax")
  end
  A,R,I = alg.args
  return quote
    R,I = $(esc(R)), $(esc(I))
    mul = $(esc(mul))

    immutable $(esc(A)) <: Alg
      coeffs :: Dict{I,R}
      $(esc(A))(coeffs::Dict) = new(filter((i,a)->a!=0, coeffs))
    end
    A = $(esc(A))

    $(esc(A))(i::I) = A(Dict(i => one(R)))

    Base.(:*)(λ::R, α::A) = A([i => λ*a for (i,a) in α.coeffs])
    Base.(:*)(α::A, λ::R) = λ*α
    Base.(:*)(α::A, β::A) = reduce(+, zero(A), [a*b*mul(i,j) for (i,a) in α.coeffs, (j,b) in β.coeffs])
  end
end

+{Exp <: Alg}(α::Exp, β::Exp) = Exp([i => get(α.coeffs, i, 0) + get(β.coeffs, i, 0)
                                     for i in union(keys(α.coeffs), keys(β.coeffs))])

-{Exp <: Alg}(α::Exp) = Exp([i => -α.coeffs[i] for i in keys(α.coeffs)])

-{Exp <: Alg}(α::Exp, β::Exp) = α+(-β)

=={Exp <: Alg}(α::Exp, β::Exp) = α.coeffs == β.coeffs

zero{Exp <: Alg}(::Type{Exp}) = Exp(Dict())

end
