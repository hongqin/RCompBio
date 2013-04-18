 
##### a simple plot
 tb = read.csv("Carlson_Yeast.csv");

 # a simple plot
 plot( tb$Amount ~ tb$Hours );

 # a better plot
 plot( Amount ~ Hours, data=tb );

 plot( Amount ~ Hours, data=tb, type="b",
    main="Carlson growth curve" );

##### the second data set, multiple plots
 tb2 = read.csv( "Carlson2.csv" );

 ##first try, not pretty
 plot( tb2$Amount ~ tb2$Hours, ylab="Amount",xlab="Hours",type="l", 
    main="growth comparison");
 lines( tb2$Amount2 ~ tb2$Hours );

 ##now, let's add colors
 plot( tb2$Amount ~ tb2$Hours, ylab="Amount",xlab="Hours",type="l", 
    main="growth comparison", col="blue");
 lines( tb2$Amount2 ~ tb2$Hours, col="red" );

 ##add legends
 legend(2,500,c("wt","mutant"), col=c("blue","red"), lwd=c(1,1) )


##### plot sunflower seed data
 sunflower = read.csv("SunflowerSeedData.csv");

 plot( sunflower$Stripes ~ sunflower$Seeds, ylab="Stripes"
       , xlab="Seeds", main="sunflower data", type="l"
       , col="red"
       )
 
 
 x= seq(1:30)
 y= x^2;
 write.csv(x, "x.csv", row.names=F)
 write.csv(y, "y.csv", row.names=F)
 output = cbind(x, y)
 write.csv(output, "xy.csv", row.names=F)
  
 plot( y ~ x, pch=x )
 
 