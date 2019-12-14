#removing correlated columns
repeat {
  vif.result=data.frame(vif(temp.table))
  vif.result$names=rownames(vif.result)
  vif.result=vif.result[order(vif.result$vif.temp.table, decreasing=TRUE), ]
  if (vif.result[1, "vif.temp.table."] > 5){
    temp.table[, vif.result[1, "names"]]=NULL
  } else {
    break
  }
}
