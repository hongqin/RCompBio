
#input evolutionary distances
data = read.csv( "Sce.Spa.KaKs.csv");

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
intersect( net$id, data$orfname);

#match data and net
data$degree = net$Freq[match(data$orfname, net$id )]

#visual examination to double the merged results
head(data)
net[net$id=='YAL005C', ]
net[net$id=='YAL012W', ]
 
# find out correlation bw degree and Omega, Ka, Ks
summary(lm(data$Ka ~ data$degree))

#match data and fitness
data$YPD = fit$YPD[match(data$orfname, fit$orf)] 

#visual check the merge results
fit[fit$orf=='YAL007C',]


summary(lm(data$Ka ~ data$YPD))
summary(lm(data$degree ~ data$YPD))
summary(lm( log(data$degree) ~ data$YPD))#normalization

#multiple regression
summary(lm(data$Ka ~ data$YPD + data$degree ))

#####################################
#### use permutation to calculate p-value of Ka difference
#### in interacting pairs;
#### This is equivalent to Fig 3A in Fraser 02, Science

 # First, define a function to calculate Ka difference in pairs of proteins
 diff.K = function( inpairs ) {
   inpairs$K1= data$Ka[match(inpairs$ORF1, data$orfname)];
   inpairs$K2= data$Ka[match(inpairs$ORF2, data$orfname)];
   ret = mean( abs( inpairs$K1 - inpairs$K2 ), na.rm=T );
 } 
 
 # calculate the observed difference in Ka
 diff.K.obs = diff.K ( pairs );
 
 #permutation of pairs, and their difference in Ka
 N = 100; #number of permutations
 permutated.diff.K = numeric( N );
 ids.orig = c( pairs$ORF1, pairs$ORF2 );
 len = length( ids.orig );
 for( i in 1:N ) {
   newids = sample( ids.orig );
   ORF1 = newids[1: (len/2)];
   ORF2 = newids[ (len/2+1) : len];
   new.pairs = data.frame( cbind(ORF1, ORF2) );
   permutated.diff.K[i] = diff.K( new.pairs );
 }
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

### Assignment 2, continued
### use permutation to find out p-value of the
### difference in fitness (YPD) of interacting pairs
### This is equivalent to Figure 3B in Fraser 02, Science. 
