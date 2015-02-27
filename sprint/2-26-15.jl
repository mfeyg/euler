function sieve(A)
  N = maximum(A)
  delete!(A,1)
  p = 1
  while true
    while !in(p,A)
      p += 1
      if p > N return A end
    end
    for a = 2:div(N,p)
      delete!(A,p*a)
    end
    p += 1
  end
end

primes = IntSet(1:10^8)
sieve(primes)

B = collect(filter(p->in(div(p+3,2),primes),primes))

function divisors(n)
  divs = Array(Int,0)
  s = isqrt(n)
  for i=1:s
    if mod(n,i)==0
      push!(divs, i)
      if i^2!=n push!(divs,div(n,i)) end
    end
  end
  return divs
end

sqrt(36)

function test(n)
  for d in divisors(n)
    if d <= isqrt(n)
      if !in(d+div(n,d), primes) return false end
    end
  end
  return true
end

isqrt(3)

divisors(3)

sum([BigInt(p-1) for p in filter(p->test(p-1), B)])


# amicable numbers



d(n) = sum(divisors(n))-n

isamicable(n) = d(d(n)) == n && d(n) != n

isamicable(220)
d(220)
d(221)
isamicable(221)
isamicable(284)

collect(filter(isamicable, 2:10000-1))

sum(collect(filter(isamicable, 2:10000-1)))

d(3)

divisors(8128)
sum(divisors(8128))-8128


collect(primes)[10001]
