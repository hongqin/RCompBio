#2012 Nov 14

rm(list=ls())

source("http://bioconductor.org/biocLite.R")
biocLite("GEOquery")

require(GEOquery)

#http://www.ncbi.nlm.nih.gov/geo/browse/
#GEO2R 
myGSE = "GSE3821" # qin, demo 

gset <- getGEO(myGSE, GSEMatrix =TRUE)
if (length(gset) > 1) idx <- grep("GPL90", attr(gset, "names")) else idx <- 1
gset <- gset[[idx]]

ex <- exprs(gset) #This is the expression matrix

#########
# Find out probes and ORFs
dictionary = gset@featureData@data[, c('ID', 'ORF')]  #This is a lookup table for probe ID and ORF 

ORFs = unique(as.character(dictionary$ORF))
yORFs = ORFs[grep( "Y\\w{2}\\d{3}.*", ORFs)]  #these are yeast ORFs
str(yORFs)
setdiff(ORFs, yORFs)
ORFs = yORFs

#########
# A simple approach to create an expression matrix with ORFs as row names
# This approach takes only one probe for each ORFs, which is often true for cDNA arrays

ex2 = ex[match(ORFs, dictionary$ORF), ]   
rownames(ex2) = ORFs
head(ex2) #Now, expression matrix is named by ORFs

##########
#Another approach is to calculate the average sigals for all the probes in the same ORFs
multipleProbedORFs = NA;
ex3 = ex2 #This is just a template
# orf = 'YLR331C'
for (orf in ORFs) {
  myrows = as.character( dictionary$ID[dictionary$ORF==orf] )
  if (length(myrows) > 1) {
    print (orf)
    multipleProbedORFs = c(multipleProbedORFs, orf)
    ex[myrows, ] = apply(ex[myrows,], 2, mean) 
  }else {
    ex3[orf, ] = ex[myrows[1], ]
  }
}

######
#normalization of expression data between experiments
# each colum is a seprate experiment
colSums = apply(ex3, 2, sum)  #get total intensity of each experiment
colSums/1E6
ex3norm = ex3  #just generate a template
for( col in 1:length(ex3[1,])) { #go over each column
  ex3norm[,col] = ex3[,col] * max(colSums) / sum(ex3[,col])
  #    individual signals * maxExperimentTotal / Total Signal of the current experiment
}
apply(ex3norm, 2, sum) / max(colSums)
ex3 = ex3norm 

#########
# now, have a look at the signals
# we often to log2 transform microarray signals
hist(ex3[,1], br=100)
ex4 = log2(ex3)
hist(ex4[,3])
ex4[ex4<0] = NA #remove backgrounds

#############
#calculate coefficient of variation
myVar = apply( ex4, 1, FUN=function(x){var(x, na.rm=T)})
myStddev = sqrt(myVar)
myMean = apply( ex4, 1, FUN=function(x){mean(x, na.rm=T)})
myCV = myStddev / myMean
myarray= data.frame(cbind( myStddev, myMean, myCV))
myarray$ORF = ORFs
myarray = myarray[, c(4, 1:3)]
summary(myarray)

outfilename = paste(myGSE, "_log2CV.csv", sep='')
write.csv(myarray, outfilename, row.names=F)
test = read.csv( outfilename, colClasses = c('character', NA, NA, NA))
str(test)
hist(test$myCV, br=100)
hist(test$myStddev, br=100)
hist(test$myMean, br=100)
