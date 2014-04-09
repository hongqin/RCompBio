#continental US demo, permutation of pariwse interaction network, igraph usage

require(RCurl)
#URL = "https://docs.google.com/spreadsheet/pub?key=0ArLJZixvTlU7dDI5c3dnTzdRX1dndzBORUk1UC0wYlE&single=true&gid=0&output=csv"
#states = read.csv(textConnection(getURL(URL)), colClass = c("character", "character"))
states = read.csv("48states.csv")

require(igraph)
g = graph.data.frame(states, directed=F)
g.degree = degree(g)
g.degree [g.degree == max(g.degree)] #TN and MO have 8 bordering states

g.shortestpath.m = shortest.paths(g)
str(g.shortestpath.m)
sorted.names = sort( rownames(g.shortestpath.m) )
gsm = g.shortestpath.m[, sorted.names]
gsm = gsm[sorted.names, ]

URL2 = "https://docs.google.com/spreadsheet/pub?key=0ArLJZixvTlU7dC1rTlRhTkw5UlY0eWZGT1JOVUVCWGc&single=true&gid=0&output=csv"
state.year.tb = read.csv(textConnection(getURL(URL2)), colClass = c("character", NA))


#recursive call, return pairs with a selfpair flag
# An 'error' will be returned when permutation failed. 
permute.pairs.wo.selfpairs = function( inpairs,  ncycles=10, debug=1, error=c(NA,NA, NA) ) {
  if (ncycles >= 1 ) {
    if(debug) {
      print(paste('ncycles=', ncycles))
    }
    longids = c(as.character(inpairs[,1]), as.character(inpairs[,2]) )
    longids = sample(longids)
    len = length(inpairs[,1])
    newpairs = data.frame( cbind( longids[1:len], longids[(len+1): (2*len)]) )
    names(newpairs) = c('id1', 'id2')
    newpairs$id1 = as.character( newpairs$id1)
    newpairs$id2 = as.character( newpairs$id2)    
    newpairs$selfpairs = ifelse( newpairs$id1 == newpairs$id2, 1, 0 )
    self.tb = newpairs[ newpairs$selfpairs==1, ]
    nonself.tb = newpairs[newpairs$selfpairs==0, ]
    if(debug) {
      print(self.tb)
    }
    if( length(self.tb[,1])>0 ) {
      if ( ncycles == 0) { return ( error ) 
      } else {
        ncycles = ncycles - 1
        selectedpairs = rbind(self.tb,  nonself.tb[1: (length(self.tb[,1])*2), ] )
        restpairs = nonself.tb[ (length(self.tb[,1])*2+1): length(nonself.tb[,1]), ]
        return( rbind(restpairs, permute.pairs.wo.selfpairs(selectedpairs, ncycles)))
      }
    } else {  
      return (newpairs)
    }
  } else {
    return( error ) 
  }
}

#test 
x = permute.pairs.wo.selfpairs(states)
x2 = permute.pairs.wo.selfpairs(states)

