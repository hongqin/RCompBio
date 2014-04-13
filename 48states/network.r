#2014 April 9, Hong Qin hqin@spelman.edu

#20140408 old ms02_singlerun() did not check id1-id2 versus id2-id1. 
# So, I wrote v2 and wrapp the previous function to v2 function call. 
ms02_singlerun = function( inpairs,  ncycles=10, indebug=0 ) { # Renamed, 2014 Feb 12
  return( ms02_singlerun_v2( inpairs,  ncycles=ncycles, indebug=indebug ))
}


#20140409, v2 has trouble with small networks
ms02_singlerun_v2 = function( inpairs,  ncycles=10, indebug=0 ) { 
  if (ncycles >= 1 ) {
    if(indebug>0) {
      print(paste('ncycles=', ncycles))
    }
    longids = c(as.character(inpairs[,1]), as.character(inpairs[,2]) )
    longids = sample(longids)
    len = length(inpairs[,1])
    newpairs2 = data.frame( cbind( longids[1:len], longids[(len+1): (2*len)]) )
    newpairs2 = t(apply(newpairs2, 1, sort))
    newpairs2 = data.frame(newpairs2)
    names(newpairs2) = c('id1', 'id2')
    newpairs2$id1 = as.character( newpairs2$id1)
    newpairs2$id2 = as.character( newpairs2$id2)    
    
    newpairs2$tag =  paste(newpairs2[,1], newpairs2[,2], sep="_")
    counts = table( newpairs2$tag )
    newpairs2$tag_counts = counts[newpairs2$tag]
    
    newpairs2$selfpairs = ifelse( newpairs2$id1 == newpairs2$id2, 1, 0 )
    
    redo.tb = newpairs2[ newpairs2$selfpairs==1 | newpairs2$tag_counts>1, ]
    rest.tb = newpairs2[ newpairs2$selfpairs==0 & newpairs2$tag_counts==1, ]
    if(indebug>0) {
      print(paste("===redopairs===="),NULL);      print(redo.tb);
      #print(paste("===restpairs===="),NULL);      print(rest.tb);
      print(paste("================="),NULL)
    }
    if( length(redo.tb[,1])>=1 ) {
      if ( ncycles == 0) { 
        #return (c(NA,NA, NA) );
        print(paste("ncycles reached zero, ncycles"),ncycles)
        print(paste("Abort!"),NULL)
        stop; 
      } else {
        ncycles = ncycles - 1
        splitPos = round( length(redo.tb[,1]) * sqrt(ncycles) ) + 5
        splitPos = min( splitPos, (length(rest.tb[,1])-1 ) )
        selectedpairs = rbind(redo.tb,  rest.tb[1: splitPos, ] )   #20140408, potential bug. always take initial section
        unchangedpairs = rest.tb[ (splitPos + 1): length(rest.tb[,1]), ] #20140408, potential bug. 
        return( rbind(unchangedpairs, ms02_singlerun_v2(selectedpairs, ncycles)))  #2014 Feb 12
      }
    } else {  
      return (newpairs2 )
    }
  } else {
    return( c(NA,NA,NA )) 
  }
}#end of ms02 v2








single_network_failure = function(lambda, p, pairs, runningORFs) {
  # single network failure simulation
  # lambda: exponential constant failure rate for edges
  # pairs: network in pairwide format
  # runningORFs: GooddEssentialORFsPPI  
  
  inpairs = pairs[,1:2] #bookkeeping  
  names(inpairs) = c('id1','id2')
  
  #stochasticity into pairs   
  inpairs$active = runif(length(inpairs[,1]))  #uniform
  # tmp = pairs$active > 1-p
  # table(tmp) / length(tmp)  ; #double-check, very good. 
  
  inpairs$age = rexp( length(inpairs[,1]), rate=lambda )  #exponential ages for pairs
  inpairs$age = ifelse(inpairs$active > (1-p), inpairs$age, NA ) #if not active, intxn is excluded. 
  #pairs$age = ifelse(pairs$active > (1-p), pairs$age, 0 )  # in what situations, can non-ative intxn be treat as 0-age?
  
  ModuleTb = data.frame(runningORFs) #buffer for module ages    
  #loop every essential genes to identify the module age
  for (i in 1:length(runningORFs)) {
    myORF = runningORFs[i]
    pos1 = grep(myORF, inpairs$id1)
    pos2 = grep(myORF, inpairs$id2)  #id1,2 to ORF1,2 is a really bad choice. 
    if( length( c(pos1,pos2))>=1 ) {
      ModuleTb$age.m[i] = max( inpairs$age[c(pos1,pos2)], na.rm=T )   #maximal intxn age -> module age
    } else {
      ModuleTb$age.m[i] = NA; 
    }
  }
  #head(ModuleTb); 
  summary(ModuleTb)
  ModuleTb$age.m[ ModuleTb$age.m== -Inf] = 0; #dead births occur when links are not active
  currentNetworkAge = min(ModuleTb$age.m)
}


