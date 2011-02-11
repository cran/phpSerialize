# phpSerialize Web example
library(phpSerialize)

# htest object
Delta=as.numeric(Sys.getenv("DELTA")) # How to get information from php
if (is.na(Delta)) Delta=3
wc = wilcox.test(rnorm(10),rnorm(10)+Delta)
# Note: we must cat and add a "\n" here. Don't know
# adding the "\n" in phpSerialize blocked the pipe under Windows
cat(phpSerialize(wc),"\n")

#lm summary
group = gl(2,10,20, labels=c("Ctl","Trt"))
weight = c(rnorm(10)+10, rnorm(10)+6)
sumlm = summary(lm(weight ~ group))
cat(phpSerialize(sumlm),"\n")
