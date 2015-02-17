function ϕ(n)
  f = [x[1] for x = factor(n)]
  function go(f)
    if isempty(f) 0
    else
      a = f[1]
      r=go(f[2:end])
      r+div(n-r,a)
    end
  end
  go(f)
end

ϕ(15)
cop(n) = count(m->gcd(m,n)==1,1:n)
cop(15)

test(n) = all([x==ϕ(x)+cop(x) for x=1:n])

test(618)

ϕ(1)+cop(1)


print(sum([d-ϕ(d) for d=1:10^6])-1)