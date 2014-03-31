# The '#' sign indicates comments. Computer will ignore this line. 

###############
#simple addition
2+2


#square 
3^5 
pi
pi^2

5-2
5/2
5*3

#Q: 5 to the 3rd power? 
5^3

# * means times
5*5*5

#natural log, No 'ln'
log(pi)

#exponetial function
exp(pi)
exp(0)

#square-root
sqrt(pi)
sqrt(100)

#Q, 100 to the 1/2 power
100^0.5


#these are arrays (vectors)
#x = seq(0,10, 0.1)
#x;
x = 1:10;  # = means assignment, x will stay in memory
1:15 #no assignment, no results stay in memory
z = 1:15

#spelman = 100:106

x = 3:10; 
x; 
length(x)  #length() is an function in R
#Q what does length() do? 

#look for helps
?seq
help(seq)
?length

x
x; 
y;

x
x+1 #no assignment
x = x + 1; # what happens to x?
#The difference bw theese two lines is an important computing concept
# x =x+1, assign a new value from righthandside to the lefthandside.
x;

#self exercises on self-assignment
# .... ... 

y = x+4
#simple plot
plot( y ~ x );
plot( x ~ y )

#If in R, try recording, page-down and up to see plot histories
#Rstudio save plots by default
plot( y ~ x, main="first plot" );
plot( x ~ y, main="second plot" )
?plot

#exercise
# modify plot( y ~ x ) to line plot
# by adding type into the command
# ... ... 
plot( y ~ x, main="line-point plot", type='b', pch=19 )


#this is another way of specifying an array
x = c( 0.1, 0.3, 1, 3, 5, 10, 0.001, 0.913 );
x
x[4:6]
x[2]
x[c(1,5,2)]

y = log(x);
plot( y ~ x, pch=19 );

