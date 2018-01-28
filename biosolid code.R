#setwd("~/Desktop/STAT 450/Stat450-Biosolid-Case")
biosolid <- read.csv("soil_blocks.csv", header = T)
controldata <- subset(biosolid, biosolid$Treatment == "Control")
str(controldata2)
### you can drop levels of treatment from dployR 
#(sample mean takes away a df, need 3 to know the other)
(anova <- aov(y.avg ~ Species, data = controldata))
summary(anova)
#at least 1 population group mean is differnt than the others.

pairwise <- pairwise.t.test(controldata$y.avg, controldata$Species, p.adj= "bonferroni")
summary(pairwise)
### if p.adj = "none", then same for lm)

###if you onlt want just one itself, use y ~ x-1!!! very important, SE are all the same, 
### so the pool sd , can set it to False, pool.sd)

TukeyHSD(anova)

linearM <- lm(y.avg ~ Species, data = controldata)
summary(linearM)
### (baseline is ASAG, H0 is referenced against ASAG, ie muASAG = muHECO, muASAG = KOMA....) )
#lm does not correct
heco <- subset(controldata, controldata$Species == "HECO")
litt <- subset(controldata, controldata$Species == "LITT")
twosample <- (t.test(heco$y.avg, litt$y.avg))
twosample

combined <- c(heco, litt)
mod <- lm(y.avg ~ Species, data = combined)

linearM <- lm(y.avg ~ Species, data = controldata,)

