# Flow duration curve
# 2018-10-22

FDC = function(q.data){
  n = length(q.data)
  q.sort = q.data[order(q.data)]
  R = seq(1, length(q.data))
  Ep = R/(n+1)
  out = as.data.frame(cbind(q.sort, Ep))
  out
}