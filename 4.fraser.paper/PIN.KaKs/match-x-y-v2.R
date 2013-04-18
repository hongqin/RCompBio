x = c("red", 'green', 'sky', 'shoe', 'orange', 'purple',
      'pink', 'dog', 'penguin', 'apple');

y = c('purple', "spelmanblue", 'dog',  'ocean', 'shoe', 'orange', 
      'pink', 'pear', 'shoe'  );

match( x, y )
match( y, x )

### more complicated example
tb1 = data.frame(x)
tb1$degree = 1:length(x)

tb2 = data.frame(y)

# We want to match tb1$degree to tb2
tb2$degree = tb1$degree[ match( tb2$y , tb1$x )]

# self exercise, matchh the values back to tb1
# tb1$degree2 = tb2$degree[ match( ?  , ?   )  ]

mxy = match( x, y );
myx = match( y , x);

cbind( x, mxy)

