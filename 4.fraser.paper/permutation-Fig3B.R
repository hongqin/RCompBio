rm(list=ls())
#input evolutionary distances
Kdata = read.csv( "Sce.Spa.KaKs.csv");

#input protein interaction pairs
pairs = read.csv("pairs.csv");
pairs$ORF1 = as.character( pairs$ORF1 );
pairs$ORF2 = as.character( pairs$ORF2 );

#input growth fitness
fit = read.csv("growth.fitness.hom.csv");
fit$orf = as.character( fit$orf ); 

#calculate connecting degrees
ids = c(pairs$ORF1, pairs$ORF2);
degree = table( ids );

# summarize the results in a dataframe 'net'
net = data.frame(degree);
str(net);
net$id = as.character( net$id);

#double check
intersect( net$id, Kdata$orfname);

#match data and net
Kdata$degree = net$Freq[match(Kdata$orfname, net$id )]

#visual examination to double the merged results
head(Kdata)
net[net$id=='YAL005C', ]
net[net$id=='YAL012W', ]

# find out correlation bw degree and Omega, Ka, Ks
summary(lm(Kdata$Ka ~ Kdata$degree))

#match data and fitness
Kdata$YPD = fit$YPD[match(Kdata$orfname, fit$orf)] 

#visual check the merge results
fit[fit$orf=='YAL007C',]


summary(lm(Kdata$Ka ~ Kdata$YPD))
summary(lm(Kdata$degree ~ Kdata$YPD))
summary(lm( log(Kdata$degree) ~ Kdata$YPD))#normalization

#multiple regression
summary(lm(Kdata$Ka ~ Kdata$YPD + Kdata$degree ))

#####################################
#### use permutation to calculate p-value of Ka difference
#### in interacting pairs;
#### This is equivalent to Fig 3A in Fraser 02, Science

# First, define a function to calculate Ka difference in pairs of proteins
diff.K = function( inpairs ) {
  inpairs$K1= Kdata$Ka[match(inpairs$ORF1, Kdata$orfname)];
  inpairs$K2= Kdata$Ka[match(inpairs$ORF2, Kdata$orfname)];
  ret = mean( abs( inpairs$K1 - inpairs$K2 ), na.rm=T );
} 

# calculate the observed difference in Ka
diff.K.obs = diff.K ( pairs );

#I want to use 'sample' to generate permutations. 
# Because 'sample' only take a single vector, 
# so I need to merge the pairs of interacting genes in two columns
# into a single column. 

#permutation of pairs, and their difference in Ka
Nsims = 100; #number of permutations
permutated.diff.K = numeric( Nsims ); #empty vector to store calculations

#merge two columns to a single columns for 'sample' function
ids.orig = c( pairs$ORF1, pairs$ORF2 ); 
len = length( ids.orig );

# Now do N simulations
#i =2; 
for( i in 1:Nsims ) {
  newids = sample( ids.orig ); #permutation is done here. 
  # newids is a single column vector
  
  #now, reformat newids to two columns (random pairs)
  ORF1 = newids[1: (len/2)];   # split the long to two columns, 1st half
  ORF2 = newids[ (len/2+1) : len]; # the second hard
  
  #Convert them back spreadsheet, (the random network in pairs)
  new.pairs = data.frame( cbind(ORF1, ORF2) ); 
  
  #calculate delta.K for one random network
  permutated.diff.K[i] = diff.K( new.pairs ); 
}

head(permutated.diff.K)
hist(permutated.diff.K)

#Note that this simple permutation does not exclude 
# self-pairing. We skip this detail in this excercise, but
# remember that this has to be taken care of 
# in actual research work. 

# calulate p-value
sub = permutated.diff.K[ permutated.diff.K < diff.K.obs ]
p.K = length(sub) / length(permutated.diff.K);

# generate a figure
hist( permutated.diff.K, xlim=c(0.025, 0.035), br=20 );
arrows( diff.K.obs, 5, diff.K.obs, 2, col="red" );
text( diff.K.obs, 5.5, "obs"); 

###End of permutaion on diff.K  
#####################################

### Assignment, Figure 3B. 
### use permutation to find out p-value of the
### difference in fitness (YPD) of interacting pairs
### This is equivalent to Figure 3B in Fraser 02, Science. 


 # First, define a function to calculate Ka difference in pairs of proteins
 diff.YPD = function( inpairs ) {
   inpairs$YPD1= fit$YPD[match(inpairs$ORF1, fit$orf)];
   inpairs$YPD2= fit$YPD[match(inpairs$ORF2, fit$orf)];
   ret = mean( abs( inpairs$YPD1 - inpairs$YPD2 ), na.rm=T );
 } 
 
 # calculate the observed difference in Ka
 diff.YPD.obs = diff.YPD ( pairs );
 
 #permutation of pairs, and their difference in Ka
 N = 1000; #number of permutations
 permutated.diff.YPD = numeric( N ); #story for simulation
 ids.orig = c( pairs$ORF1, pairs$ORF2 );
 len = length( ids.orig );
 for( i in 1:N ) {
   newids = sample( ids.orig );
   ORF1 = newids[1: (len/2)];
   ORF2 = newids[ (len/2+1) : len];
   new.pairs = data.frame( cbind(ORF1, ORF2) );
   permutated.diff.YPD[i] = diff.YPD( new.pairs );
 }
 #Note that this simple permutation does not exclude 
 # self-pairing. We skip this detail in this excercise, but
 # remember that this has to be taken care of 
 # in actual research work. 

 # calulate p-value
  sub2 = permutated.diff.YPD[ permutated.diff.YPD < diff.YPD.obs ]
  p.YPD = length(sub2) / length(permutated.diff.YPD);

 # generate a figure
 hist( permutated.diff.YPD, xlim=c(0, 0.3)) 
 arrows( diff.YPD.obs, 50, diff.YPD.obs, 2, col="red" );
 text( diff.YPD.obs, 50.5, "obs"); 

