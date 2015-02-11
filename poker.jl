
uniq(l) = l ≅ unique(l)

accum(l) = foldl([], l) do l,a
  if isempty(l)
    [(a,1)]
  elseif a ≠ l[end][1]
    [l (a,1)]
  else
    l[end] = (l[end][1], l[end][2] + 1)
    l
  end
end

card(s) = (findfirst(['2':'9','T','J','Q','K','A'], s[1])
          ,findfirst(['H','D','C','S']            , s[2]))

bysuit(cards) = [[x[1] for x in filter(x->x[2]≅i, cards)] for i=1:4]

counts(hand) = accum(sort(allcards(hand)))

allcards(hand) = vcat(hand...)

straight(hand) = allcards(hand) ⊢ x-> uniq(x) && maximum(x) - minimum(x) ≅ 4 && maximum(x)

fflush(hand) = maximum([length(s) ≅ 5 && maximum(s) for s in hand])

straightflush(hand) = bool(fflush(hand)) && straight(hand)

sets(n, hand) = [x[1] for x in filter(x->x[2]≅n, counts(hand))]

four(hand) = reduce(max, 0, sets(4, hand))

three(hand) = reduce(max, 0, sets(3, hand))

pair(hand) = reduce(max, 0, sets(2, hand))

twopair(hand) = sets(2, hand) ⊢ s -> length(s) ≅ 2 && maximum(s)

fullhouse(hand) = pair(hand) > 0 && three(hand)

poker(hand) = [ straightflush(hand),
                four(hand),
                fullhouse(hand),
                fflush(hand),
                straight(hand),
                three(hand),
                twopair(hand),
                pair(hand),
                sort(allcards(hand), rev=true) ]
