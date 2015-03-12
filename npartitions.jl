# adapted from combinatorics.jl:
#
# let _npartitions = Array(Moduli.Modulo{Int}, 10^6)
#   for i=1:10^6 _npartitions[i] = modulo(0,10^6) end
#     global npartitions
#     function npartitions(n::Int)
#         if n < 0
#             0
#         elseif n < 2
#             1
#         elseif (np = get(_npartitions, n, 0)) != modulo(0,10^6)
#             np
#         else
#             np = modulo(0,10^6)
#             sgn = 1
#             for k = 1:n
#                 np += sgn * (npartitions(n-k*(3k-1)>>1) + npartitions(n-k*(3k+1)>>1))
#                 sgn = -sgn
#             end
#             _npartitions[n] = np
#         end
#     end
# end

LIM=10^5
m=10^6
Parts = Array{Int}(LIM)
parts(n::Int) = if n < 0 0 elseif n < 2 1 else Parts[n] end
for n=1:LIM
  np = 0
  sgn = 1
  for k=1:div(5 + isqrt(24n+1), 6)
    np = mod(np + sgn * (parts(n-k*(3k-1)>>1) + parts(n-k*(3k+1)>>1)), m)
    sgn = -sgn
  end
  Parts[n] = np
end
