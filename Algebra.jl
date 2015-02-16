module Algebra

export fAlgebra

abstract Alg <: Number

function fAlgebra{R,I}(f::Function, ::Type{R}, ::Type{I})
  Exp = symbol("Algebra", gensym())
  Exp = eval(quote
               immutable $Exp <: Alg
                 coeffs :: Dict{$I,$R}
                 $Exp(coeffs::Dict) = new(filter((i,a)->a!=0, coeffs))
               end
               $Exp
             end)
  Exp(i::I) = Exp(Dict(i => one(R)))

  global *
  *(λ::R, α::Exp) = Exp([i => λ*a for (i,a) in α.coeffs])
  *(α::Exp, λ::R) = λ*α
  *(α::Exp, β::Exp) = reduce(+, zero(Exp), [a*b*f(i,j) for (i,a) in α.coeffs, (j,b) in β.coeffs])

  return Exp
end

fAlgebra{R,I}(::Type{R}, ::Type{I}, f::Function) = fAlgebra(f,R,I)

+{Exp <: Alg}(α::Exp, β::Exp) = Exp([i => get(α.coeffs, i, 0) + get(β.coeffs, i, 0)
                                     for i in union(keys(α.coeffs), keys(β.coeffs))])

-{Exp <: Alg}(α::Exp) = Exp([i => -α.coeffs[i] for i in keys(α.coeffs)])

-{Exp <: Alg}(α::Exp, β::Exp) = α+(-β)

=={Exp <: Alg}(α::Exp, β::Exp) = α.coeffs == β.coeffs

zero{Exp <: Alg}(Exp) = Exp(Dict())

end