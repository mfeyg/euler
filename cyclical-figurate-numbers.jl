fig = Array(Function, 6)
afig = Array(Function, 6)

fig[1]=n->n*(n+1)>>1
fig[2]=n->n^2
fig[3]=n->n*(3n-1)>>1
fig[4]=n->n*(2n-1)
fig[5]=n->n*(5n-3)>>1
fig[6]=n->n*(3n-2)

afig[1]=m->sqrt(2m+1//4)-1//2
afig[2]=m->sqrt(m)
afig[3]=m->sqrt(2//3*m+1//36)+1//6
afig[4]=m->sqrt(m//2+1//16)+1//4
afig[5]=m->sqrt(2//5*m+9//100)+3//10
afig[6]=m->sqrt(m//3+1//9)+1//3

range(i, n, N = 100*int(floor(n/100)+1)) =
  filter(x->mod(x,100)>9, map(fig[i], int(ceil(afig[i](n))):int(floor(afig[i](N)))))

function loop(seq,left)
       if isempty(left) return seq end
       for t in left
       delete!(left,t)
       for m in (isempty(seq) ? range(t,1000,10000)
                              : range(t,100*mod(seq[end],100)))
       r = loop([seq,m],left)
       if r != Nothing() && mod(r[end],100) == floor(r[1]/100) return r end
       end
       push!(left,t)
       end
       end

println(sum(loop([], IntSet(1:6))))
