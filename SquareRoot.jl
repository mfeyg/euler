module SquareRoot

immutable Sqrt
  n::Integer
end

immutable Product
  v :: Vector
end

isunity(n::Integer) = n == 1
isunity(a::Sqrt) = isunity(a.n)

sqrt(n::Integer) = ((a,b) -> product(a,Sqrt(b)))(map(prod,zip([(x^div(y,2), x^isodd(y)) for (x,y) in factor(n)]...))...)

*(a::Sqrt, b::Sqrt) = sqrt(a.n*b.n)

*(a::Integer, b::Sqrt) = Product([a,b])
*(a::Sqrt, b::Integer) = b*a

*(a::Product, b::Product) = reduce((a,x) -> a*x, a, b.v)

*(x, p::Product) = p*x
function *(p::Product, x)
  for i = 1:length(p.v)
    try
      return product([p.v[1:i-1], p.v[i]*x, p.v[i+1:end]]...)
    end
  end
  Product([p.v,x])
end

product() = 1
product(x) = x
function product(x,y)
  if isunity(x) y
  elseif isunity(y) x
  else
    try
      x*y
    catch
      Product([x,y])
    end
  end
end
product(xs...) = reduce(product, xs)

immutable Sum
  v :: Vector
end

function +(x::Product, y::Product)
  i = intersect(x.v, y.v)
  a = setdiff(x.v, i)
  b = setdiff(y.v, i)
  if length(a)>1 || length(b)>1
    Sum([x,y])
  else
    Product([i])*(product(a...)+product(b...))
  end
end

+(x::Product, a) = x + Product([a])
+(a, x::Product) = Product([a]) + x

+(a::Sqrt,b::Sqrt) = if a.n==b.n 2*a else Sum([a,b]) end

end
