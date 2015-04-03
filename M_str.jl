macro M_str(str)
  transpose(hcat(map(l->map(int,split(l)), split(str,"\n"))...))
end
