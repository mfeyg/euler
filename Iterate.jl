immutable Iterate{T}
  x::T
  f::Function
end

Base.eltype{T}(::Iterate{T}) = T
Base.start(it::Iterate) = it.x
Base.next(it::Iterate, x) = (x,it.f(x))
Base.done(it::Iterate, x) = false

iterate(f,x) = Iterate(x,f)

trampoline(f,iter) = begin go(f,s) = if !done(iter,s) x,s2 = next(iter,s); f(x, g->go(g,s2)) end; go(f,start(iter)) end

nth(n, iter) = begin f(n)=(x,g)->if n<=1 x else g(f(n-1)) end; trampoline(f(n), iter) end

take(n, iter) = begin
  a = Array(eltype(iter), 0)
  f(n) = (x,g) -> begin push!(a,x); if n>1 g(f(n-1)) end end
  trampoline(f(n), iter)
  a
end

immutable Drop{It}
  it::It
  n::Integer
end

Base.eltype(it::Drop) = eltype(it.it)
Base.start(it::Drop) = nth(it.n+1, iterate(s -> next(it.it,s)[2], start(it.it)))
Base.next(it::Drop, s) = Base.next(it.it, s)
Base.done(it::Drop, s) = Base.done(it.it, s)

drop(n,it) = Drop(it,n)

immutable Until{It}
  iter::It
  done::Function
end

Base.eltype(u::Until) = eltype(u.iter)
Base.start(u::Until) = (start(u.iter),false)
Base.next(u::Until, s) = begin x,n = next(u.iter, s[1]) ; (x,(n,u.done(x))) end
Base.done(u::Until, s) = s[2] || done(u.iter, s[1])

until(f,it) = Until(it,f)

immutable Single{T}
  x::T
end

Base.eltype{T}(s::Single{T}) = T
Base.start(s::Single) = false
Base.next(s::Single, _) = (s.x,true)
Base.done(s::Single, d) = d

immutable Concat{I,J}
  i::I
  j::J
end

Base.start(c::Concat) = (:i, start(c.i))
Base.next(c::Concat, s) = if s[1] == :i && done(c.i, s[2]) x,z = next(c.j, start(c.j)); (x,(:j,z)) else x,z = next(c.(s[1]), s[2]); (x,(s[1],z)) end
Base.done(c::Concat, s) = s[1] == :j && done(c.j, s[2])

concat(a) = a
concat(a,b) = Concat(a,b)
concat(a...) = concat(a[1], concat(a[2:end]...))

type Iterator{It}
  it::It
  state
  val::Union(eltype(It),Void)
  next::Function
  done::Bool
end

function Iterator(it)
  s = start(it)
  dn = done(it,s)
  i = Iterator{typeof(it)}(it, s, nothing, ()->(), dn)
  i.next = ()->advance_iter(i)
  return i
end

function advance_iter(i)
  if i.done error("Can't advance iterator") end
  x,s = next(i.it, i.state)
  i.val = x
  i.state = s
  i.done = done(i.it, i.state)
  return i.val
end
