module Induce
export induce

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

end