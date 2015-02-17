module Convergents
export convergents

type State{T}
  cont :: Function
  rem :: T
end

immutable Conv{T}
  val::T
end

convergents(x) = Conv(x)

Base.start(c::Conv) = State(x -> x, c.val)

function Base.next(c::Conv, s::State)
  n = floor(Integer, float(s.rem))
  (s.cont(n), State(v -> s.cont(n + 1//v), if n==s.rem 0 else 1/(s.rem-n) end))
end

Base.done(c::Conv, s::State) = s.rem == 0

end