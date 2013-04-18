set.seed(999) #use a fixed random number generator

#set three varialbes
# x -> y -> z 
x = rnorm(1E4)
y = 0.8*x + rnorm(1000)
z = 0.8*y  + rnorm(1000)

#pairwise correlations are significants
summary(lm(z ~ x))
summary(lm(z ~ y))

# when y is controlled, z and x are not correlated.
summary(lm(z ~ x + y))

### Now, look at the partial correlation from another view

#first set up a dataframe
tb = data.frame(cbind(x,y,z))

#second take a thin slice of data with 'constant' y
sub = tb[tb$y<0.03 & tb$y>-0.03, ]
str(sub)
hist(sub$y)

#third, examine z~x when for -0.03<y<0.03
summary(lm(sub$z ~ sub$x))

