function ismagic(digs)
  outside = digs[1:5]
  inside = digs[6:10]
  outside[1]+inside[1]+inside[2] == outside[2]+inside[2]+inside[3] == outside[3]+inside[3]+inside[4] == outside[4]+inside[4]+inside[5] == outside[5]+inside[5]+inside[1]
end

