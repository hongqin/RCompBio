
##############################
# example of salary data

setwd("~/Dropbox/courses.student.research.dp/bio386,2012Fall/lectures/1.R.an.overview/1")

# input the data into a dataframe 'salary'
sal = read.table( "salary.dat",header=T);

# look at the first six rows of data
# You can also see the columns in this table
head(sal)

#salary is a dataframe, we can look the structure of this dataframe
str(sal)

#There 11 columns in this dataframe, we can pick individual column 
# The table name should be before $ sign, and the column name should
# be after the $ sign. 
sal$gender; #Gender is an array (vector) of F & M values
head(sal$gender); #this gives the first 6 values; parenthesis are for functions
sal$gender[10:15]; #This list 10th-15th values; square brakets are for vectors
sal$gender[c(11, 101, 3005, 19000)]
sal[2:4, ] #what does do? 
sal[10:15, 2:4]; #list values from 10-15 rows and 2-4 columns
sal[2,3]; #List the value at the 2nd row, the 3rd column
sal[c(101, 341), c(6,9,10)]

#how many Females and Males in this data set? 
table( sal$gender );

# Q: how many F and M in each rank? 
# ... ... figure it out

#
hist( sal$salary );
# What is a histogram? 

# Question: How long did it take to get this job after degree?
sal$interval = sal$startyr - sal$yrdeg #this gives an extra colum in 'sal'

hist( sal$interval );
# Apparently, some people started working before they have
# got their degrees. 


#######################################
###Question: Is there a gender bias in salary? 
#######################################
#first, we can look at a plot of salary by gender
boxplot( sal$salary ~ sal$gender );
# what does boxplot mean? 

# Partition the salary by gender
fs = sal$salary[sal$gender=='F'] #females' salary
ms = sal$salary[sal$gender=='M'] #males' salary
mean(fs)
mean(ms)
median(fs)
median(ms)


# try t-test
t.test( ms, fs )
t.test( log(ms), log(fs)); #t-test on log-transformed data
# we need to compare H0 and H1 here
# what are H0 and H1? why do we need them?
# H1 is alternative H
# H1 is alway our theories or opnions (our theisis, your preject, idea)
# H0 is the world without our theories or idea, etc.

#using linear regression to examine the association
m = lm( sal$salary ~ sal$gender );
#look at the sumamry report of the regression results
summary(m);

# OR, we do this in a different way 
s = sal$salary
g = sal$gender
m = lm( s ~ g );
summary(m); # R^2 = 0.01804

#What is R^2?
ss_tot = sum( (s - mean(s))^2 );
fitted_values = predict( m );
ss_reg = sum( (fitted_values - mean(fitted_values))^2 ) 
ss_err = sum( (salary - fitted_values)^2)
ss_reg / ss_tot; #This should the same as R^2

# work on your own, examine the effect of rank on salary
# ... ... 
# ... ...


# So, rank is much better associated with salary than gender.
# Well, what's going about salary, rank, and gender? 
table( sal$gender, sal$rank ); #what does this mean? 

tb = table( sal$gender, sal$rank );
#this is the rank ~ gender table, the observation

#chisq.test to see whether gender -> rank
chisq.test( tb ); 
# what does this mean, pay attention to p-value

# Let's calcuate the chisq manually
#manual calculation of chisq test
fp = sum(tb[1,])/sum(tb) #femal fraction in the sample
mp = sum(tb[2,])/sum(tb) # male fraction in the sample
atp = sum(tb[,1])/sum(tb) #assitant fraction
asp = sum(tb[,2])/sum(tb) #associate fraction
flp = sum(tb[,3])/sum(tb) #full professor fraction
col1= sum(tb) * atp * c(fp, mp); #expectation, assistant prof
col2= sum(tb) * asp * c(fp, mp); #expectation, associate prof
col3= sum(tb) * flp * c(fp, mp); # expectation, full prof
E = matrix( c(col1, col2, col3), nrow=2) #This is the expectation

#In later sesseions, you will know tricks to do the above
# calculation more efficiently using 'apply'.

O = tb; #rename 'tb' as the Observations
c = sum( (O-E)^2/E ) #this is the chisquare value
1 - pchisq( c, df= 2); #this is the p value
#this should be the same as chisq.test


#Q, what is the difference between  t-test and chisq test? 
## ... ... 

# another example of chisq test
x = matrix ( c( 890, 310, 900, 300), nrow=2);
chisq.test( x )


#fisher.test ()
fisher.test(tb); #run out of memory. 

#What is/are our new hypothesis or hypothyses?
# H0: salary is indepedent of rank, gender
# H1a: gender -> salary <- rank
# H1b: gender -> rank -> salary

# multiple regression
summary( lm( sal$salary ~ sal$gender + sal$rank ))
summary( lm( sal$rank   ~ sal$gender ))

# A more sophisticated way to compare the likelihood
# of each models. We discuss the likelihood methods in 
# in late parts of this course. 

