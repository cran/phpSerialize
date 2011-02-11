# Extended Test example for phpSerialize
library(nlme)
library(phpSerialize)

group = gl(2,10,20, labels=c("Ctl","Trt"))
weight = c(rnorm(10)+10, rnorm(10)+6)
lm1 = lm(weight ~ group)
sumlm1 = summary(lm1)
nlm1 = nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
sumnlm1= summary(nlm1)
fm1 = lme(distance ~ age, data = Orthodont) # random is ~ age
sumfm1 = summary(fm1)
x = c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
wc = wilcox.test(x, y, paired = TRUE, alternative = "greater")

# output string
phpSerialize(weight)
# output to file
# use:
#   php.exe -f testserialize.php >testserialize.txt
# to display the array tree.
phpSerialize(weight,file="testserialize.php",phpTestCode=T)
phpSerialize(wc,file="testserialize.php",phpTestCode=T,append=T)
phpSerialize(lm1,file="testserialize.php",phpTestCode=T,append=F,simplifyMono=T)
phpSerialize(sumnlm1,file="testserialize.php",phpTestCode=T,append=T)
phpSerialize(nlm1,file="testserialize.php",phpTestCode=T,append=T)
phpSerialize(sumfm1,file="testserialize.php",phpTestCode=T,append=T)

# Serialize all. Only "sumfm1" and "sumnlm1" are serialized
phpSerializeAll(include="sum",exclude=c("sumlm"),
  file="testserializeAll.php",phpTestCode=TRUE)
