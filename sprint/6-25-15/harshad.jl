twos(n) = if mod(n,2) == 1 0 else 1+twos(div(n,2)) end

test(n,a) = begin
  s = twos(n-1)
  d = div(n-1,2^s)
  powermod(a,d,n) != 1 && all(r -> powermod(a,2^r*d,n) != n-1, 0:s-1)
end

isprime(n) = n!=1 && !any(a-> test(n,a), filter(a->a!=n, [2, 3, 5, 7, 11, 13, 17, 19, 23]))

isharshad(n) = mod(n,sum(digits(n))) == 0

function harshad(n)
  if n > 10^14 return [] end
  if n >= 100 && isprime(n) && isprime(div(div(n,10), sum(digits(div(n,10))))) return [n] end
  if isharshad(n) return vcat(map(harshad, [10*n+i for i=0:9])...) end
  return []
end

solve() = sum(vcat(map(harshad,1:9)...))