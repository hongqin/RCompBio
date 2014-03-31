
#####
# How does the regression look when two samples are independent? 
x = rnorm( 1000 );
#y = rnorm( 1000 );
y=x 
m = lm( y ~ x ); 
summary( m );
plot( y ~ x);
abline( m, col="red");

