cd("/Users/Mendel/Desktop/euler")

using SquareRoot

intpart(a) = floor(Int, float64(a))

function period(n)
  if isinteger(sqrt(n)) return 0 end
  a = Sqrt(n)
  s = Array(Float64,0)
  x = float64(a)
  while !(x in s)
    push!(s, x)
    a = 1/(a-intpart(a))
    x = float64(a)
  end
  length(s) - findlast(s, x) + 1
end