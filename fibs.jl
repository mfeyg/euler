import Base.start, Base.next, Base.done

type Fibs
  limit
end

start(::Fibs) = (1,1)

function next(::Fibs, i)
  a,b = i
  a, (b,a+b)
end

done(f::Fibs, i) = i[1] > f.limit
