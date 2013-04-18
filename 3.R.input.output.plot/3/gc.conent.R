#### a simple example
 library(seqinr);

 # read in some bacterial 16s rDNA sequences
 seqs = read.fasta(   "http://www.bioinformatics.org/ctls/download/data/16srDNA.fasta",
  seqtype="DNA");

 str(seqs);

 # look at the first sequence
 seq1 = seqs[[1]]
 table( seq1 ); #nucleotide composition
 GC(seq1);  # GC content

 # a loop for all sequences 
 num = 1:length(seqs); #these are storages for later use
 gc  = 1:length(seqs); # gc = num; 
 out = data.frame( cbind( num, gc ) );
 out2 = cbind(num, gc)
 
 for( i in 1:length(seqs) ) {
   out$gc[i] = GC( seqs[[i]] );
 }
 out;

 write.csv(out, "gc.csv", row.names=F) # output the results

 ###Now, let's try some chanllenging features
 ###This is optional
 # a loop for all sequences 
 num = 1:length(seqs);
 out2 = data.frame( cbind( num) );
 for( i in 1:length(seqs) ) {
   out2$name[i] = attributes( seqs[[i]] )$name
   out2$gc[i] = GC( seqs[[i]] );
 }
 out2;

 write.csv(out2, "gc2.csv", row.names=F) # output the results


#### now, calculate the length of each gene
### hint using length()

 num = 1:length(seqs);
 out3 = data.frame( cbind( num) );
 for( i in 1:length(seqs) ) {
   out3$name[i] = attributes( seqs[[i]] )$name
   out3$len[i] = length( seqs[[i]] );  ### remove this 
   out3$gc[i] = GC( seqs[[i]] );
 }
 out3;

 write.csv(out3, "gc3.csv", row.names=F) # output the results





