# MK_SS()
# This function takes a time series input and does:
# 1.Mann-Kendall trend test, and
# 2.Sen's slope test.
# output: a 3-element list of: MK Tau, MK p-value, and Sen's slope

library(mice)
library(Kendall)
library(trend)

########################################################################################
MK_SS = function(ts.data  # vector of values, with row names as years (or other time step)
                 ){
  TS = ts(data = as.numeric(ts.data), start = min(as.numeric(names(ts.data))), end = max(as.numeric(names(ts.data))), frequency = 1)
  MK = MannKendall(TS)
  SS = sens.slope(TS)
  
  out = c(round(MK$tau[1], 3), round(MK$sl[1],3), round(SS$estimates,3))
  names(out) = c("Tau", "p", "Sen_slope")
  return(out)
}
########################################################################################