rm(list=ls())

tb= read.csv("Table1DePaepe06.csv")

m1 = lm( tb$DecayRate ~ log10(tb$BurstSize) )
summary(m1)
plot( tb$DecayRate ~ log10(tb$BurstSize), xlab="Log(Burst Size)", ylab="Mortality Rate")
abline( m1, col="red")

