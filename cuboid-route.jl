M = 1818

gens = filter(p->gcd(p...)==1, [(x,x+y) for x=1:isqrt(2M),y=1:2:isqrt(2M)])
gens=[(m^2-n^2,2*m*n) for (n,m) = gens]

pairs = vcat([[(k*min(p...),k*max(p...)) for k = 1:div(2M,max(p...))] for p=gens]...)

splits(a,b) = [[(a-c,c,b) for c=1:div(a,2)]; [(a,c,b-c) for c=b-a:div(b,2)]]
ways(a,b) = div(a,2)+max(div(2a-b,2)+1,0)

filter(t->max(t...)<=M, vcat(map(p->splits(p...),pairs)...))