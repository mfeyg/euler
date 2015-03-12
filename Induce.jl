module Induce
export induce, @induce

function induce{T}(::Type{T}, f, start=1)
  A = Array{T}(1024)
  grow() = A = [A;A]
  n = start
  res = Nullable{T}()
  while isnull(res)
    A[n] = f(n, i->if !(1<=i<n) error("Induction error") else A[i] end, x->(res=Nullable(x)).value)
    n+=1
    if n>length(A) grow() end
  end
  return res.value
end

induce(f, init=1) = induce(Int, f, init)

macro induce(T, fun)
  @assert isa(fun,Expr)
  @assert fun.head == :(=)
  @assert length(fun.args) == 2
  sig = fun.args[1]
  def = fun.args[2]
  @assert isa(sig,Expr)
  @assert sig.head == :call
  @assert length(sig.args) == 2
  name = sig.args[1]
  @assert isa(name, Symbol)
  arg = sig.args[2]
  @assert isa(arg,Expr)
  @assert arg.head == :comparison
  @assert length(arg.args) == 3
  @assert arg.args[2] == :>=
  var = arg.args[1]
  @assert isa(var, Symbol)
  start = arg.args[3]

  return (name, T, var, start, def)

  A = Array{T}(1024)
  grow() = A = [A;A]
end

end