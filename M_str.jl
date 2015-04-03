macro M_str(str)
  parse("["*replace(str,'\n',';')*"]")
end
