# source("http://bioconductor.org/biocLite.R"); 
# biocLite("Heatplus");


 require(Heatplus); #load Heatplus package into R
 require(adimpro)
 require(pixmap)

 g = read.pnm("gsmall.pbm")
 m = matrix(g@grey, g@size[1], g@size[2])
 image(m, col=gray(0:10/10), main="original")
       
  
 m2 = m[sample(1:g@size[1], g@size[1]), ] 
 image(m2, col=gray(0:10/10), main="shuffled")
 
   
 h = hclust(dist(m2))
 m3 = m2[h$order, ]
 image(m3, col=gray(0:10/10), main="hclust result")
 
   
heatmap_2(m2, legend = 1, col = gray(0:20/20), do.dendro=c(F,F), Rowv=NA, Colv=NA ) #customerize the color
heatmap_2(m2, legend = 1, col = gray(0:20/20), do.dendro=c(T,F),  Colv=NA ) #cluster by rows
heatmap_2(m2, legend = 1, col = gray(0:20/20), do.dendro=c(F,T),  Rowv=NA ) #cluster by columns
heatmap_2(m2, legend = 1, col = gray(0:20/20), do.dendro=c(T,T) ) #cluster by columns
heatmap_2(m, legend = 1, col = gray(0:20/20), do.dendro=c(F,F), Rowv=NA, Colv=NA ) #customerize the color

heatmap_2(m2); #A simple heat map
heatmap_2(m2, legend = 1 ) #Add a figure legend


############
############ END
############

