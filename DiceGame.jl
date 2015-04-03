D(P,S,k,n) = if k==1 max(0,min(1,P(n))) else sum([D(P,S,k-1,n-i) for i=S])/length(S) end
println(sum([D(x->1/6*(1<=x<=6),1:6,6,n)*D(n->1-n/4,1:4,9,n) for n=1:36]))
