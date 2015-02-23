twos(n::Integer) = if isodd(n) return (0,n) else (s,d) = twos(n>>1); (s+1,d) end

powermod(x,p,m) = if p==0 1 else y = mod(if isodd(p) x else 1 end * powermod(x,p>>1,m)^2, m) end

function millerrabin(n::Integer, tests::Vector)
  if n<2 return false end
  (s,d) = twos(n-1)
  all([powermod(a,d,n)==1 || any([powermod(a,2^r*d,n)==n-1 for r=0:s-1]) for a=tests])
end

isprime(n,k=20) =
  millerrabin(n, if log2(n)<40.96 filter(a->a<n,[2,3,5,7,11]) else [rand(2:n-1) for _=1:k] end)