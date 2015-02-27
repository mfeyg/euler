require("divisors.jl")
require("Iterate.jl")

N=10^6

a = Array(Int,N)

using Memoize
@memoize ami(n) = sum(divisors(n)[1:end-1])

function findloop(itr)
  t = Iterator(itr)
  h = Iterator(itr)
  while !h.done
    for _=1:2
      h.next()
      if h.done || h.val > N || a[h.val] != 0 return Nullable{eltype(itr)}() end
    end
    t.next()
    if h.val == t.val return Nullable{eltype(itr)}(h.val) end
  end
  return Nullable{eltype(itr)}()
end

function chains()
  for n = 1:N
    if a[n] == 0
      v = findloop(until(x->x==1, iterate(ami,n)))
      # mark visited values
      while n<=N && n > 1 && a[n] == 0
        a[n] = -1
        n = ami(n)
      end
      if !isnull(v)
        n = v.value
        println(n)
        loop = until(x->x==n, iterate(ami,ami(n)))
        len = count(_->true, loop)
        for x in loop
          a[x] = len
        end
      end
    end
  end
end