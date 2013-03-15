tb = read.csv("4cities.csv", row.names=1);
d = as.dist(tb);
d
hclust(d)
?hclust
plot(hclust(d, method="average") )

quit("yes");






















tb = read.csv("classroom.csv", row.names=1);
d = as.dist(tb);
plot(hclust(d, method="average") )

x = c(23, 19, 1, 5, 7, 10, 15);
names(x) = x;

d = dist(x );
plot( hclust(d) );

x = c(1,2,3, 7,8, 10, 11, 12);
names(x) = x;
d = dist(x );
plot( hclust(d, method="single") );


