# There are three key dataset in Fraser paper
# evolutionary distance, protein interaction, and fitness
# First, we need load the 3 data sets into R.
# Second, we need to do some 'informatics' to merge the three
# datasets. 

#evolutionary rates
Kdata = read.csv( "Sce.Spa.KaKs.csv");

# protein interaction network (PIN) in gene
pairs = read.csv("pairs.csv");

#now study the data tables with 'head' and 'str'
str(pairs)

#make sure gene names are treated as letters, not fake numbers (factors)
pairs$ORF1 = as.character( pairs$ORF1 ); 
pairs$ORF2 = as.character( pairs$ORF2 );
# How do we know that ORF1 and 2 are now letters? 
# ... ... 

Kdata$orfname = as.character(Kdata$orfname)

# this is the growth fitness data
fitness = read.csv("growth.fitness.hom.csv");
fitness$orf = as.character( fitness$orf ); 

#####calculate the degree/connectivity (number of interactor per protein) 
ids = c(pairs$ORF1, pairs$ORF2);
degree = table( ids );
sum(degree); #check the counting result, the length of ids
length(ids)

# Now, we want to regenerate Figure 1 in Frasher.
# we have degree and Ka, Ks in 'data', and we want 
# run lm() between degree and Ka
# 
# if we try: 
# lm ( degree ~ data$Ka) #but this won't work? why?
# 


#now, put degree into a spreadsheet
net = data.frame(degree);
str(net);
net$id = as.character( net$ids); #make sure gene names are "characters"
net = net[, -1]; #remove the first column of unecessary data
#ids are orfs in net

# By now, we have interaction degree in 'net' and 
# evolutionary rates in 'data'. 
# We need to merge the two dataframes (spreadsheets)
# based on ORFs (like SSNs for genes)

str(net)
str(Kdata)

#check network gene names match to gene names in the evolutionary data
intersect( net$id, Kdata$orfname);
# we 2676 matching ORFs

# try to do linear regression between evolutionary rate
# and number of interactions
 lm( Kdata$Ka ~ net$Freq) #won't work
#the above command does not work, beceause the data are in two spreadsheets
# and they are not matched. 

#### Now, do informatics work to merge the dataset
#The evolutionary rate and degree are in separate spreadsheet. 
# We need to put them together by 'match'' data and net
# This match 'net$Freq' into 'data'
Kdata$degree = net$Freq[match(Kdata$orfname, net$id )]
orfnamePositionsInNet = match(Kdata$orfname, net$id )

#let try to match 'Kdata$Ka' into 'net'
#basically flitp the previous matching procedure
# ... ... 

#You can also check the matches in a different way
positions =  Kdata$orfname %in% net$id ;  # many true or false, 

#Now try to put Omega into 'net'
# ... ... 

#Now try to put Ks into 'net'
# ... ... 

#visual examination to prevent errors
head(Kdata)
net[net$id=='YAL010C', ]
net[net$id=='YAL012W', ]


### Comment: 
### Merge heterogenous datasets is an important bioinformatics
###           and practical skill. 
#### end of merging

###Now the real work 
# Figure 1
# find out correlation bw degree and Omega, Ka, Ks
m = lm(Kdata$Ka ~ Kdata$degree);
summary(m)

plot( Kdata$Ka ~ Kdata$degree, xlab='interactions per protein', ylab='evolutionary rate'); #this is figure 1
#this looks like a non-linear negative correlation
abline(m, col="red" );

### Using Omega is principly more correct than Ka
### lm between omega and degree
###
m2 = lm(Kdata$Omega ~ Kdata$degree )
summary(m2)
plot( Kdata$Omega ~ Kdata$degree, main="Omega ~ degree" )
abline( m2, col="red")

### Now We will work on Figure 2. 
# math data and fitness
Kdata$YPD = fit$YPD[match(Kdata$orfname, fit$orf)] 

#visual check two entries
fit[fit$orf=='YAL007C',]

# Now, for figure 2
summary(lm(Kdata$Ka ~ Kdata$YPD)) #evolutionary rate = ka ypd= growth in rich media)
# allows us to compare overall fitness
summary(lm(Kdata$degree ~ Kdata$YPD))
summary(lm( log(Kdata$degree) ~ Kdata$YPD))#normalization
summary(lm(Kdata$Ka ~ Kdata$degree)) #evolutionary rate compared to frequency of interactions

summary(lm(Kdata$Ka ~ Kdata$YPD + Kdata$degree ))

# try to see conditional correlation between degree ~ omega
#  ... ... 
# summary(lm(Kdata$Omega ~ ????  ))
#
