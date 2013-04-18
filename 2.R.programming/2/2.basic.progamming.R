

#### loops

 for ( i in 1:3) print(i);

 for ( i in c(0.5, 1, -2, 5) ) print(i);


 i=2;
 while( i<=10 ) { 
  print(i); 
  i = i + 2; 
 }


#### conditionals

x = 1.5;
#if ((x<1) & (x>0)) { 
if ((x<0) | (x>1)) { 
   print("X is not between 0 and 1");
} else {
   print("X is between 0 and 1 ");
}

x =0.5;
if ( ( ! x>1) & ( ! x<0 ) ) {
   print("x is between 0 and 1");
}

if ( x == 0.5 ) {
   print("50%");
}

x= -1;
if ( ! x<0 ) {
  print("non-negative");
} else {
  print("negative");
}

y = ifelse( x<0, -x, x ); # assign abosulte value of x to y


#### a function
x = c( 1,2,3,4); #this is a global variable
#x = c(5, 4, 3, 6, 10, 12)

take_even = function( x ) {
  y = c(); # a locale copy inside of loop
  for( i in 1:length(x)) {
    if ( (x[i]%% 2 )== 0 ) { # x mod 2
        y = c(y, x[i] );  # add a new x[i] to y
    }
  }
  y; #the last line, return y to the main program
}

y = take_even(x); # c(2,4)
z = take_even(x); # c(2,4);
w = take_even(1:10); #c (2,4,6,8,10)
u = -5:10;
v = take_even(u); # -4 -2  0  2  4  6  8 10
take_even(c(5,4,10,11, 100, -3, 9, 0, 3333, 5524234234))

#### a function calls another function
even_total = function( x ) {
  x = take_even(x);
  sum(x);
 }

x = c( 1,2,3,4 );
u = even_total( x );

# x will be changed if the following line is run.
x = take_even(x)
 
 
 #### scope of variable 
 x = c( 1,2,3,4); #this is a global variable
 y = c(-1, -2, -3, -4)# this is also a global one
 y.outside = y; 
 take_uphalf = function( x ) {
   y = c(); # a local copy inside of loop
   xbar = mean(x); 
   for( i in 1:length(x)) {
     if ( x[i] > xbar ) { 
       y = c(y, x[i] );
     }
   }
   y.inside = y; 
   y; #the last line, return y to the main program
 }
 
 w = take_uphalf(x);
 
 
 
#### packages
 #install packages
 require("ape");
 require("seqinr");

 #load packages
 library(ape);
 library(seqinr);

 #what's in the package?
 help(package=ape);
 help(package=seqinr);
  #you should also read the manual of the packages

#### a simple example
 library(seqinr);

 tablecode(); #the genetic code

 # read in some bacterial 16s rDNA sequences
 seqs = read.fasta( "http://www.bioinformatics.org/ctls/download/data/16srDNA.fasta",seqtype="DNA");

 # look at the first sequence
 seq1 = seqs[[1]]
 table( seq1 ); #nucleotide composition
 GC(seq1);  # GC content

 # a loop for all sequences 
 num = 1:length(seqs);
 gc  = 1:length(seqs);
 out = data.frame( cbind( num, gc ) );
 for( i in 1:length(seqs) ) {
   out$gc[i] = GC( seqs[[i]] );
 }
 out;

 write.csv(out, "gc.csv", row.names=F) # output the results


#### save and load images
 save.image("Test.RData");
 rm(list=ls());
 ls(); #nothing
 load("Test.RData");
 ls(); #everthing is back

