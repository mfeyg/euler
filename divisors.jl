divisors(n) = begin go(facs) = if isempty(facs); [1] else [x*facs[1][1]^a for a=0:facs[1][2], x=go(facs[2:end])] end; collect(go(collect(factor(n)))) end