die = 1 : try parseint(ARGS[1]) catch 4 end   # die sides
B = 40                                        # board size
jail = 10                                     # jail square

# We will triplicate our state space to count consecutive doubles,
# so that states 0..39 have had no doubles, 40..79 have had ones double roll,
# and 80..119 have had two doubles

S = 0 : 3B-1   # the full state space
doubles(s) = div(s,B)

# the results of a dice roll:
Dice = sum([ Int64[ a != b && to == mod(from + a + b, B) ||
                    a == b && to == mod(from + a + b, B) + B * (doubles(from) + 1) ||
                    a == b && doubles(from) == 2 && to == jail
               for to = S, from = S ]
             for a = die, b = die]) / length(die)^2

# For the individual squares' rules, we forget about doubles, and focus on a single board
board = 0 : B-1

# these are the squares with special rules:
g2j = [30]       # go to jail
cc = [2,17,33]   # community chest
ch = [7,22,36]   # chance

# to find the next railroad and utility:
nextRR(b) = 5 + 10 * mod(div(b + 5, 10), 4)
nextUtil(b) = 12 <= b < 28 ? 28 : 12

# since our board rules ignore doubles, they just repeat along the diagonal:
diagonal(M) =
  let O = zero(M)
    [M O O;
     O M O;
     O O M]
  end

Board = diagonal(Float64[
  if in(from, g2j)
    to == jail
  elseif in(from, cc)
    (14 * (to == from) + (to == 0) + (to == jail)) / 16
  elseif in(from, ch)
    (sum(to .== [0,10,11,24,39,5])
     + 2 * (to == nextRR(from))
     + (to == nextUtil(from))
     + (to == from - 3)
     + 6 * (to == from)
     ) / 16
  else
    to == from
  end

  for to = board, from = board ])

# the full results of a single move: first roll the dice, then follow the rules
Move = Board * Dice

# compute the limiting distribution
π = nullspace(one(Move) - Move)

# discard doubles and normalize
odds = (π[1 : B] + π[B+1 : 2B] + π[2B+1 : 3B]) / sum(π)

# sort squares by odds
top = sort([0:B-1;], by = b -> odds[b + 1], rev = true)

# print the modal string
@printf("Modal string: %02d%02d%02d\n", top[1:3]...)

# print the top probabilities
print("Top squares:\n")
for b in top[1:9]
  @printf("  %02d (%.3f%%)\n", b, odds[b + 1] * 100)
end
