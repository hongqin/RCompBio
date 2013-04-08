#2013 April 6
# analysis of metrics, scientific literacy and attitude survey
# Hong Qin

rm(list=ls())
list.files()
tb.ori = read.csv("response20130401.csv")
str(tb.ori)
tb = tb.ori
#rename the columns for convenience 
names(tb) = c("time","gender", "age", "degree", "country", "light", "shaq", "fossil", "kilo", "mm", 
        "food","inseam", "weather","electronCharge","earlyHuman", 
        "laser", "continents", "antibiotics", "electronSize","earthCenter",
        "religiousView","dailyLife","SciOnLife", "SciEffect")

#visual check of the renaming 
cbind (names(tb), substr(names(tb.ori), 1, 20))

tb = tb[ (! is.na(tb$age)), ]

head(tb)
tb$gender[tb$gender=='']='Do not wish to answer'
table(tb$gender)

table(tb$age)
tb$age[tb$age=="Option 5"] = NA
table(tb$age, tb$gender)

for( i in 5:length(tb[, 1])) {
  for( j in 5:length(tb[1, ])) {
    if ( is.na(tb[i, j]) ) {
      # tb[i,j] = NA #do nothing
    } else if (tb[i,j]=='') {
      tb[i,j] = NA
    } 
  }
}

summary(tb)

table(tb$weather)
tbGenWeather = table(tb$gender, tb$weather)
tbGenWeather
fisher.test( tbGenWeather)


head(tb)
names(tb)
metrics = c("shaq", "kilo", "mm", "inseam", "weather")
sciLiteracy = c("light", "fossil", "food", "electronCharge", 
                "earlyHuman", "laser", "continents", "antibiotics", "electronSize", "earthCenter")
sciAttitude = c("religiousView", "dailyLife", "SciOnLife", "SciEffect")

##### create a second table, convert factors to numerics
tb2 = tb[,c(2,4,5)]  #this is the score table
tb2$age = 20
tb2$age[grep("23-30", tb$age)] = 23/2 + 30/2
tb2$age[grep("31-40", tb$age)] = 31/2 + 40/2
tb2$age[grep("41-50", tb$age)] = 41/2 + 50/2
tb2$age[grep("51-55", tb$age)] = 51/2 + 55/2
tb2$age[grep("60", tb$age)] = 65
table(tb$age)
table(tb2$age)


###country 
tb2$country = 0
tb2$country[tb$country=='United States'] = 1
table( tb2$country )
table( tb$country )

########calculate the metric scores
tb2$shaq = 0.5
tb2$shaq[ tb$shaq=='Yes' ] = 1
tb2$shaq[ tb$shaq=='No' ] = 0
table(tb2$shaq)

tb2$kilo = 0
tb2$kilo[ tb$kilo=='1000 x' ] = 1
table(tb2$kilo)

tb2$mm=0
tb2$mm[ tb$mm==0.145 ] = 1
table(tb2$mm)
table(tb$mm)

tb2$inseam = 0.5
tb2$inseam[tb$inseam=="This person is short"] = 1
tb2$inseam[tb$inseam=="This person is tall"] = 0
table(tb2$inseam)

tb2$weather = 0.5
tb2$weather[tb$weather=="A Short sleeve shirt"] = 1
tb2$weather[tb$weather=="A winter coat"] = 0
table(tb$weather)
table(tb2$weather)

#testing the grep function
#tb$weather[ grep("shirt", tb$weather) ]

#######calcualte the science attitude scores
#sciAttitude = c("religiousView", "dailyLife", "SciOnLife", "SciEffect")

tb2$religiousView = 0.5
tb2$religiousView[grep("No", tb$religiousView)] = 1
tb2$religiousView[grep("Yes", tb$religiousView)] = 0
table(tb2$religiousView)
table(tb$religiousView)

tb2$dailyLife = 0.5
tb2$dailyLife[ tb$dailyLife=='TRUE' ] = 0
tb2$dailyLife[ tb$dailyLife=='FALSE' ] = 1
table(tb2$dailyLife)

tb2$SciOnLife = 0.5
tb2$SciOnLife[ tb$SciOnLife=='TRUE' ] = 1
tb2$SciOnLife[ tb$SciOnLife=='FALSE' ] = 0
table(tb2$SciOnLife)

tb2$SciEffect = 0.5
tb2$SciEffect[ tb$SciEffect=='TRUE' ] = 1
tb2$SciEffect[ tb$SciEffect=='FALSE' ] = 0
table( tb2$SciEffect )

###########calculate scientific literacy
#sciLiteracy = c("light", "fossil", "food", "electronCharge", 
#                "earlyHuman", "laser", "continents", "antibiotics", "electronSize", "earthCenter")
tb2$light = 0.5
tb2$light[ tb$light=='TRUE' ] =1
tb2$light[ tb$light=='Wrong' ] =0
table(tb$light)
table(tb2$light)

tb2$fossil = 0.5
tb2$fossil[ tb$fossil=='6 million and 5 years old' ] = 0
tb2$fossil[grep('Still', tb$fossil)] = 1;
table(tb$fossil)
table(tb2$fossil)

tb2$food = 0.5
tb2$food[ tb$food=='Dis-agree' ] = 1
tb2$food[grep('Agree', tb$food)] = 0; 
table(tb$food)
table(tb2$food)

tb2$electronCharge = 0
tb2$electronCharge[grep('Positive', tb$electronCharge)] = 1; 
table(tb$electronCharge)
table(tb2$electronCharge)

tb2$earlyHuman = 0.5
tb2$earlyHuman[grep('TRUE', tb$earlyHuman)] = 0; 
tb2$earlyHuman[grep('FALSE', tb$earlyHuman)] = 1; 
table(tb$earlyHuman)
table(tb2$earlyHuman)

tb2$earlyHuman = 0.5
tb2$earlyHuman[grep('TRUE', tb$earlyHuman)] = 0; 
tb2$earlyHuman[grep('FALSE', tb$earlyHuman)] = 1; 
table(tb$earlyHuman)
table(tb2$earlyHuman)

tb2$laser = 0.5
tb2$laser[grep('TRUE', tb$laser)] = 0; 
tb2$laser[grep('FALSE', tb$laser)] = 1; 
table(tb$laser)
table(tb2$laser)

tb2$continents = 0.5
tb2$continents[grep('TRUE', tb$continents)] = 1; 
tb2$continents[grep('FALSE', tb$continents)] = 0; 
table(tb$continents)
table(tb2$continents)

tb2$antibiotics = 0.5
tb2$antibiotics[grep('TRUE', tb$antibiotics)] = 0; 
tb2$antibiotics[grep('FALSE', tb$antibiotics)] = 1; 
table(tb$antibiotics)
table(tb2$antibiotics)

tb2$electronSize = 0.5
tb2$electronSize[grep('True', tb$electronSize)] = 1; 
tb2$electronSize[grep('FALSE', tb$electronSize)] = 0; 
table(tb$electronSize)
table(tb2$electronSize)

tb2$earthCenter = 0.5
tb2$earthCenter[grep('TRUE', tb$earthCenter)] = 1; 
tb2$earthCenter[grep('FALSE', tb$earthCenter)] = 0; 
table(tb$earthCenter)
table(tb2$earthCenter)

#sciLiteracy = c("light", "fossil", "food", "electronCharge", 
#                "earlyHuman", "laser", "continents", "antibiotics", "electronSize", "earthCenter")

tb2$SciLitScore = apply( tb2[, sciLiteracy], MARGIN=1, FUN=sum ) #by row
hist(tb2$SciLitScore, br=20)

#sciAttitude = c("religiousView", "dailyLife", "SciOnLife", "SciEffect")
#Attitude total score
tb2$SciAttitude = apply( tb2[, sciAttitude], MARGIN=1, FUN=sum)
hist(tb2$SciAttitude, br=20)

#metrics = c("shaq", "kilo", "mm", "inseam", "weather")
#metric total score
tb2$metric = apply( tb2[, metrics], MARGIN=1, FUN=sum )
hist(tb2$metric, br=20)

summary(tb)
summary(tb2)
str(tb2); 
str(tb)

pairs(tb2[, c("metric", "SciLitScore", "SciAttitude")])
summary(lm(tb2$SciLitScore ~ tb2$metric )) #significant
summary(lm(tb2$SciAttitude ~ tb2$metric )) #significant
summary(lm(tb2$SciAttitude ~ tb2$SciLitScore + tb2$metric )) #significant
## metric -> SciAttitude and SciLitScore

summary(lm(tb2$SciLitScore ~ tb2$country)) #not significant
summary(lm(tb2$SciLitScore ~ tb2$metric + tb2$age + tb2$gender + tb2$country  )) #only metric is significant


plot( tb2$SciLitScore ~ jitter(tb2$metric), xlab='Metric Proficiency', ylab='Scientific Literacy', ylim=c(2,10) )
m1 = lm(tb2$SciLitScore ~ tb2$metric )
abline(m1, col='red')
summary(m1)
text(2, 2.5, "SciLit ~ Metric, R2=0.28, p=1.5E-15", col="red", pos=4)
#abline(m2, col='blue')
summary(m2)
m2 = lm(tb2$SciLitScore ~ tb2$metric + tb2$age)
anova(m1, m2)
m3 = lm(tb2$SciLitScore ~ tb2$metric + tb2$age + tb2$gender)
summary(m3)
anova(m2,m3)
m4 = lm(tb2$SciLitScore ~ tb2$metric + tb2$age + tb2$country)
anova(m2, m4)
#text(2, 2, "SciLit ~ Metric + Age, R2=0.29, p=2.8E-14", col="blue", pos=4)

plot( tb2$SciAttitude ~ jitter(tb2$metric), ylim=c(0.5,4), xlab='Metric Proficiency', ylab='Attitude toward Science' )
m1 = lm( tb2$SciAttitude ~ tb2$metric )
m2 = lm( tb2$SciAttitude ~ tb2$metric + tb2$age )
abline(m1, col='red')
abline(m2, col='blue')
summary(m1)
summary(m2)
anova(m1, m2)
text(2, 0.9, "SciAttitude ~ Metric , R2=0.18, p=1.0E-9", col="red", pos=4)
text(2, 0.7, "SciAttitude ~ Metric + Age, R2=0.24, p=4.7E-12", col="blue", pos=4)

plot( tb2$SciAttitude ~ jitter(tb2$age), ylab='Attitude toward Science', xlab='Age')
m2 = lm( tb2$SciAttitude ~ tb2$age + tb2$metric)
abline(m2, col='blue')
text(30, 1.7, "SciAttitude ~ Metric + Age, R2=0.24, p=4.7E-12", col="blue", pos=4)

summary(lm(tb2$SciAttitude ~ tb2$metric + tb2$age + tb2$gender + tb2$country  )) #age is signicant!!!
#but this might be a bias in the sample
# 1) there is many faculty
# 2) people took the sample may be interested in the metric and science at the first place?!

summary(lm(tb2$SciAttitude ~ tb2$metric + tb2$age + tb2$gender + tb2$country + tb2$degree  )) #age is signicant!!!

summary(lm(tb2$SciAttitude ~ tb2$SciLitScore))
summary(lm(tb2$SciAttitude ~ tb2$SciLitScore + tb2$metric))
###########
# remove phD from the samples
#
summary(tb2[, 1:5])
tb3 = tb2[ - grep('Ph.D.', tb2$degree)  , ]
summary(tb3)
summary(lm(tb3$SciAttitude ~ tb3$metric + tb3$age + tb3$gender + tb3$country + tb3$degree  )) 
#age is still signicant after PhD are removed from the sample


########test 
testTwoFactorTb2 = function( fac1, fac2) {
  tbTwo = table( tb2[,fac1], tb2[,fac2] )
  print(tbTwo)
  f = fisher.test(tbTwo)
}

#metrics = c("shaq", "kilo", "mm", "inseam", "weather")
#sciLiteracy = c("light", "fossil", "food", "electronCharge", 
#                "earlyHuman", "laser", "continents", "antibiotics", "electronSize", "earthCenter")
#sciAttitude = c("religiousView", "dailyLife", "SciOnLife", "SciEffect")

f = testTwoFactorTb2( "shaq", "religiousView"); f
f = testTwoFactorTb2( "shaq", "dailyLife"); f
f = testTwoFactorTb2( "shaq", "SciOnLife"); f

f = testTwoFactorTb2( "shaq", "SciEffect"); f #significant effect!!!!
f = testTwoFactorTb2( "kilo", "SciEffect"); f #significant effect!!!
f = testTwoFactorTb2( "mm", "SciEffect"); f #significant effect!!!
f = testTwoFactorTb2( "inseam", "SciEffect"); f #significant effect!!!
f = testTwoFactorTb2( "weather", "SciEffect"); f #p=0.078
f = testTwoFactorTb2( "country", "SciEffect"); f #p=0.24

summary(lm(tb2$SciEffect ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #significant kilo 
summary(lm(tb2$SciOnLife ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #no effect
summary(lm(tb2$religiousView ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #age effect
summary(lm(tb2$dailyLife ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #gender

summary(lm(tb2$religiousView ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #significant age, gender 
summary(lm(tb2$SciOnLife ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #no effect
summary(lm(tb2$dailyLife ~ tb2$kilo + tb2$country + tb2$gender + tb2$age + tb2$degree )) #gender effect, education

summary(lm(tb2$SciEffect ~ tb2$mm + tb2$country + tb2$gender + tb2$age + tb2$degree )) #no effect
summary(lm(tb2$SciEffect ~ tb2$inseam + tb2$country + tb2$gender + tb2$age + tb2$degree )) #random
summary(lm(tb2$SciEffect ~ tb2$shaq + tb2$country + tb2$gender + tb2$age + tb2$degree )) #p=0.066 shaq
summary(lm(tb2$SciEffect ~ tb2$weather + tb2$country + tb2$gender + tb2$age + tb2$degree )) #no effect


f = testTwoFactorTb2("country", "shaq")
f

f = testTwoFactorTb2( "country", "shaq")
f


