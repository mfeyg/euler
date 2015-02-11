function iscircularprime(x)
  isprime(x) || return false
  e = int(floor(log10(x)))
  for i = 1:e
    x = apply(divrem(x,10^e)) do a,b 10*b+a end
    isprime(x) || return false
  end
  true
end
