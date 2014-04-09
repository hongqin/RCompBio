rm(list=ls())

source("network.r")

net1 = read.table("repeat.tab")
net1 = read.table("pair.tb")
net1 = read.csv("48states.csv")

x = ms02_singlerun_v2(net1, indebug=1 )
y = ms02_singlerun_v2(x, indebug=1 )
z = ms02_singlerun_v2(y, indebug=1 )

tx = table(c( x[,1], x[,2]) )
ty = table(c( y[,1], y[,2]) )

table( tx==ty )


#double-check


as.matrix(net1)
net2 = t(apply(net1, 1, sort))  #order ids
net2 = data.frame(net2)
net2$tag =  paste(net2[,1], net2[,2], sep="_")
counts = table( net2$tag )
net2$tag_counts = counts[net2$tag]
net2


x = ms02_singlerun_v2( inpairs, indebug=1 )

#continental US demo, permutation of pariwse interaction network, igraph usage

require(RCurl)
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

state.year.tb = read.csv(textConnection(getURL(URL2)), colClass = c("character", NA))


#small2big = function( IN ){
#  if (IN[1] > IN[2]){ return (t((IN[2], IN[1]) )
#  } else { return(IN); 
#  }
#  x = small2big(c('b','a'))
  


