## Creating a social ecological network 
## Created: November 2023

# set working directory 

setwd("/Users/jstoll/tmp/SMS500_SES") 

# for this activity, you will need the following package(s)
library(igraph)

# Create a matrix with diet matrix data from Gulf of Maine (Data source: Energy Modeling and Analysis eXercise (Link et al. 2006)). 

emax <- read.delim("EMAX.txt", header = TRUE, row.names = 1) # read in data
emax_matrix <- as.matrix(emax) # convert to data matrix
gomnet <- graph.adjacency(emax_matrix,mode="undirected", weighted=TRUE, diag=FALSE) # convert to adjacency matrix 


emax_2 = read.delim("EMAX.txt", header = TRUE) # read in data w/out row name 
ncol(emax_2$Species.ID)
V(gomnet)$Type=as.character(emax_2$Type[match(V(gomnet)$name,emax_2$Species.ID)])
V(gomnet)$Type
V(gomnet)$color = V(gomnet)$Type #assign the "Type" attribute as the vertex color
V(gomnet)$color = gsub("E","green",V(gomnet)$color) #Ecological will be green
V(gomnet)$color = gsub("S","orange",V(gomnet)$color) #Social will be orange
plot.igraph(gomnet,vertex.label=V(gomnet)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(gomnet)$weight/10)

#Create a matrix with diet matrix data from Gulf of Maine - using 46 groups in typology (only 33 w/ lobster)

emax = read.delim("Link et al EMAXm.txt", header = TRUE, row.names = 1)
emax = (emax[,-(1:1)])
emax = as.matrix(emax)
gomnet = graph.adjacency(emax,mode="undirected",weighted=TRUE,diag=FALSE) 

a = read.delim("Link et al EMAXm.txt", header = TRUE)
V(gomnet)$Type=as.character(a$Type[match(V(gomnet)$name,a$Species.ID)])
V(gomnet)$Type
V(gomnet)$color = V(gomnet)$Type #assign the "Type" attribute as the vertex color
V(gomnet)$color = gsub("E","dark green",V(gomnet)$color) #Ecological will be green
V(gomnet)$color = gsub("S","orange",V(gomnet)$color) #Social will be orange
plot.igraph(gomnet,vertex.label=V(gomnet)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(gomnet)$weight/10)

#Create a matrix with diet matrix data from Gulf of Maine - (lobster aggregated)

emax = read.delim("Link et al EMAXo.txt", header = TRUE, row.names = 1)
emax = (emax[,-(1:1)])
emax = as.matrix(emax)
gomnet = graph.adjacency(emax,mode="undirected",weighted=TRUE,diag=FALSE) 

a = read.delim("Link et al EMAXo.txt", header = TRUE)
V(gomnet)$Type=as.character(a$Type[match(V(gomnet)$name,a$Species.ID)])
V(gomnet)$Type
V(gomnet)$color = V(gomnet)$Type #assign the "Type" attribute as the vertex color
V(gomnet)$color = gsub("E","dark green",V(gomnet)$color) #Ecological will be green
V(gomnet)$color = gsub("S","orange",V(gomnet)$color) #Social will be orange
plot.igraph(gomnet,vertex.label=V(gomnet)$name, vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(gomnet)$weight/10)


### Subsampled networks

# Poly
emax_poly = read.delim("Sub_Poly.txt", header = TRUE, row.names = 1)
emax_poly = (emax_poly[,-(1:1)])
emax_poly = as.matrix(emax_poly)
poly = graph.adjacency(emax_poly,mode="undirected",weighted=TRUE,diag=FALSE) 

a = read.delim("Sub_Poly.txt", header = TRUE)
V(poly)$Type=as.character(a$Type[match(V(poly)$name,a$Species.ID)])
V(poly)$Type
V(poly)$color = V(poly)$Type #assign the "Type" attribute as the vertex color
V(poly)$color = gsub("E","green",V(poly)$color) #Ecological will be red
V(poly)$color = gsub("S","orange",V(poly)$color) #Social will be blue
plot.igraph(poly,vertex.label=V(poly)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(poly)$weight/10)

# Moll
emax_moll = read.delim("Sub_Moll.txt", header = TRUE, row.names = 1)
emax_moll = (emax_moll[,-(1:1)])
emax_moll = as.matrix(emax_moll)
moll = graph.adjacency(emax_moll,mode="undirected",weighted=TRUE,diag=FALSE) 

a = read.delim("Sub_Moll.txt", header = TRUE)
V(moll)$Type=as.character(a$Type[match(V(moll)$name,a$Species.ID)])
V(moll)$Type
V(moll)$color = V(moll)$Type #assign the "Type" attribute as the vertex color
V(moll)$color = gsub("E","green",V(moll)$color) #Ecological will be red
V(moll)$color = gsub("S","orange",V(moll)$color) #Social will be blue
plot.igraph(moll,vertex.label=V(moll)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(moll)$weight/10)

# MaOt
emax_maot = read.delim("Sub_MaOt.txt", header = TRUE, row.names = 1)
emax_maot = (emax_maot[,-(1:1)])
emax_maot = as.matrix(emax_maot)
maot = graph.adjacency(emax_maot,mode="undirected",weighted=TRUE,diag=FALSE) 

a = read.delim("Sub_MaOt.txt", header = TRUE)
V(maot)$Type=as.character(a$Type[match(V(maot)$name,a$Species.ID)])
V(maot)$Type
V(maot)$color = V(maot)$Type #assign the "Type" attribute as the vertex color
V(maot)$color = gsub("E","green",V(maot)$color) #Ecological will be red
V(maot)$color = gsub("S","orange",V(maot)$color) #Social will be blue
plot.igraph(maot,vertex.label=V(maot)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(maot)$weight/10)

# MbFl
emax_mbfl = read.delim("Sub_MbFl.txt", header = TRUE, row.names = 1)
emax_mbfl = (emax_mbfl[,-(1:1)])
emax_mbfl = as.matrix(emax_mbfl)
mbfl = graph.adjacency(emax_mbfl,mode="undirected",weighted=TRUE,diag=FALSE) 

a = read.delim("Sub_MbFl.txt", header = TRUE)
V(mbfl)$Type=as.character(a$Type[match(V(mbfl)$name,a$Species.ID)])
V(mbfl)$Type
V(mbfl)$color = V(mbfl)$Type #assign the "Type" attribute as the vertex color
V(mbfl)$color = gsub("E","green",V(mbfl)$color) #Ecological will be red
V(mbfl)$color = gsub("S","orange",V(mbfl)$color) #Social will be blue
plot.igraph(mbfl,vertex.label=V(mbfl)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(mbfl)$weight/10)

# MbOt
emax_mbot = read.delim("Sub_MbOt.txt", header = TRUE, row.names = 1)
emax_mbot = (emax_mbot[,-(1:1)])
emax_mbot = as.matrix(emax_mbot)
mbot = graph.adjacency(emax_mbot,mode="undirected",weighted=TRUE,diag=FALSE) 

a = read.delim("Sub_MbOt.txt", header = TRUE)
V(mbot)$Type=as.character(a$Type[match(V(mbot)$name,a$Species.ID)])
V(mbot)$Type
V(mbot)$color = V(mbot)$Type #assign the "Type" attribute as the vertex color
V(mbot)$color = gsub("E","green",V(mbot)$color) #Ecological will be red
V(mbot)$color = gsub("S","orange",V(mbot)$color) #Social will be blue
plot.igraph(mbot,vertex.label=V(mbot)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(mbot)$weight/10)

# Shri
emax_shri = read.delim("Sub_Shri.txt", header = TRUE, row.names = 1)
emax_shri = (emax_shri[,-(1:1)])
emax_shri = as.matrix(emax_shri)
shri = graph.adjacency(emax_shri,mode="undirected",weighted=TRUE,diag=FALSE) 

a = read.delim("Sub_Shri.txt", header = TRUE)
V(shri)$Type=as.character(a$Type[match(V(shri)$name,a$Species.ID)])
V(shri)$Type
V(shri)$color = V(shri)$Type #assign the "Type" attribute as the vertex color
V(shri)$color = gsub("E","green",V(shri)$color) #Ecological will be red
V(shri)$color = gsub("S","orange",V(shri)$color) #Social will be blue
plot.igraph(shri,vertex.label=V(shri)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(shri)$weight/10)

par(mfrow=c(2,3), par(new=F), mar=c(1,1,1,1))
plot.igraph(poly,vertex.label=V(poly)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(poly)$weight/10)
plot.igraph(moll,vertex.label=V(moll)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(moll)$weight/10)
plot.igraph(maot,vertex.label=V(maot)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(maot)$weight/10)
plot.igraph(mbfl,vertex.label=V(mbfl)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(mbfl)$weight/10)
plot.igraph(mbot,vertex.label=V(mbot)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(mbot)$weight/10)
plot.igraph(shri,vertex.label=V(shri)$name,vertex.label.color='black',vertex.label.cex=.75,vertex.size=12,layout=layout.fruchterman.reingold,edge.curved=FALSE,edge.color="black",edge.width=E(shri)$weight/10)

