## Creating social ecological networks to explore Maine's fisheries
## Created: November 2023

# Set working directory 

setwd("/Users/jstoll/tmp/SMS500_SES") # you'll need to set this 

# You will need the following package(s): 

library(igraph) # If you don't already have them, you'll need to install them 
library(tibble)
library(lubridate)
library(eeptools)
library(dplyr)

# Let's start by uploading the data into R Studio. You can access the data from GitHub: 

lic_2018_2022 <- read.delim("me_all_lics_2018_2022_tidy.txt") # upload data
lic_key <- read.csv("license_key.csv", stringsAsFactors = FALSE)

# Now let's tidy our data so we can start to use it! 

## To start, let's convert date of birth (dob) to age. You don't need this for the initial network analysis, but it may be of interest to you for your intersectional analysis.  

lic_2018_2022$dob <- substr(lic_2018_2022$dob, 1, nchar(lic_2018_2022$dob)-10) # Configure DOB column
lic_2018_2022$dob <- as.Date(lic_2018_2022$dob, format =  "%Y-%m-%d") # Convert to data 

lic_2018_2022 <- lic_2018_2022 %>% add_column(Age = NA) # Add "Age" column
lic_2018_2022$Age <- as.numeric(difftime(Sys.Date(),lic_2018_2022$dob, units = "weeks"))/52.25 # Calculate age

## For some reason, there are also some duplicates in the data, so let's get rid of those

lic_2018_2022 <- distinct(lic_2018_2022, landings_number, license_type, license_year, .keep_all= TRUE) 


## You might also find it helpful to have descriptions that tell us about the different license types. To do this, let's use the lic_key dataframe to populate our license_description column in the main dataset.

lic_key$license_type <- tolower(lic_key$license_type) 

for(i in 1:nrow(lic_2018_2022)) {
  if(length(which(lic_key$license_type == lic_2018_2022$license_type[i])) > 0 ) {
 lic_2018_2022$license_description[i] <- lic_key$license_description[which(lic_key$license_type == lic_2018_2022$license_type[i])]
 }
}

## For simplicity, let's also remove some of the irrelevant licenses (like the lobster non-commercial license) and those without descriptions (obviously we wouldn't do this if this was really a research project!) 

lic_2018_2022 <- lic_2018_2022[which(lic_2018_2022$license_type != "lnc"),]
lic_2018_2022 <- lic_2018_2022[which(lic_2018_2022$license_type != "dl"),]

lic_2018_2022 <- lic_2018_2022[which(lic_2018_2022$license_description != "NA"),]


## Now let's do some grouping of licenses based on target species 

# To do this, we first need to create a new column to put these groupings

lic_2018_2022 <- lic_2018_2022 %>% add_column(fishery = NA) # Add "fishery" column

## We can start with grouping lobster licenses 

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'lcu' | 
     lic_2018_2022$license_type[i] == 'lc2o'|
     lic_2018_2022$license_type[i] == 'lc3'|
     lic_2018_2022$license_type[i] == 'lc2'|
     lic_2018_2022$license_type[i] == 'la'|
     lic_2018_2022$license_type[i] == 'lcs'|
     lic_2018_2022$license_type[i] == 'lc1'|
     lic_2018_2022$license_type[i] == 'lco'|
     lic_2018_2022$license_type[i] == 'lao'|
     lic_2018_2022$license_type[i] == 'lc3o'|
     lic_2018_2022$license_type[i] == 'plc3'|
     lic_2018_2022$license_type[i] == 'lau'|
     lic_2018_2022$license_type[i] == 'nlc1'|
     lic_2018_2022$license_type[i] == 'milc1'
     ) lic_2018_2022$fishery[i] <- 'lobster' 
}

## Next we can group the others

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'al' 
  ) lic_2018_2022$fishery[i] <- 'aquaculture' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'cfs' |
     lic_2018_2022$license_type[i] == 'cfc'
  ) lic_2018_2022$fishery[i] <- 'commercial' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'cps' |
     lic_2018_2022$license_type[i] == 'cpc'
  ) lic_2018_2022$fishery[i] <- 'pelagicc and anadromous crew'
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'e1c' |
     lic_2018_2022$license_type[i] == 'e0' |
     lic_2018_2022$license_type[i] == 'e1' |
     lic_2018_2022$license_type[i] == 'e0c' |
     lic_2018_2022$license_type[i] == 'e2c' |
     lic_2018_2022$license_type[i] == 'e6c' |
     lic_2018_2022$license_type[i] == 'e6' |
     lic_2018_2022$license_type[i] == 'ep' |
     lic_2018_2022$license_type[i] == 'e2'
  ) lic_2018_2022$fishery[i] <- 'elver' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'gc' 
  ) lic_2018_2022$fishery[i] <- 'green crab' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'menc' |
     lic_2018_2022$license_type[i] == 'menr'
  ) lic_2018_2022$fishery[i] <- 'menhaden' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'mh' |
     lic_2018_2022$license_type[i] == 'md'
  ) lic_2018_2022$fishery[i] <- 'mussel' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'qm' 
  ) lic_2018_2022$fishery[i] <- 'quahog' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'sd' |
     lic_2018_2022$license_type[i] == 'sdt' |
     lic_2018_2022$license_type[i] == 'sdi'
  ) lic_2018_2022$fishery[i] <- 'scallop' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'scd' 
  ) lic_2018_2022$fishery[i] <- 'sea cucumber' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'cs' |
     lic_2018_2022$license_type[i] == 'csu'|
     lic_2018_2022$license_type[i] == 'cso'
  ) lic_2018_2022$fishery[i] <- 'shellfish' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'surf' 
  ) lic_2018_2022$fishery[i] <- 'surf clam' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'suh' |
     lic_2018_2022$license_type[i] == 'suwt' |
     lic_2018_2022$license_type[i] == 'ten' |
     lic_2018_2022$license_type[i] == 'sur' |
     lic_2018_2022$license_type[i] == 'sub'
  ) lic_2018_2022$fishery[i] <- 'urchin' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'mwd' 
  ) lic_2018_2022$fishery[i] <- 'worms' 
}

for(i in 1:nrow(lic_2018_2022)) {
  if(lic_2018_2022$license_type[i] == 'sw' |
     lic_2018_2022$license_type[i] == 'sws'
  ) lic_2018_2022$fishery[i] <- 'seaweed' 
}


## Select Year (in this case we'll go with 2022)

lic_2022 <- lic_2018_2022[which(lic_2018_2022$license_year == "2022"),]

# Create a matrix

l22 <- (lic_2022[,-(2:12)]) # Select harvester data and information about their licenses 
l22 <- as.matrix( table(l22) ) # Create matrix 
l22col <- t(l22) %*% l22 

# Now let's take a look at the matrix to look at the connections between fisheries

l22col # This table shows us all of the people who participate in specific fisheries based on the pairs of licenses that they hold. E.g. There are 168 aquaculture licenses. Of those, 35 hold lobster licenses.  

# Our last step is to convert our matrix into an igraph

il22col <- graph.adjacency(l22col,mode="undirected", weighted=TRUE, diag=FALSE)   

# Let's create our first network! 

plot.igraph(il22col,vertex.label=V(il22col)$name,layout=layout.kamada.kawai, # If you want to try other layouts, check out: https://networkx.org/documentation/stable/reference/drawing.html#module-networkx.drawing.layout
            edge.color="black", vertex.color="gray50")  

# This plot shows us ALL the different fisheries and which fisheries are connected to each other by the people who hold licenses to them It's a cool first stab at created a network, but there is a lot more to learn! 

## Let's build off of this can add information about how many people hold specific combinations of licenses. We do this by calculating edge WEIGHT

E(il22col)$weight # calculate edge weight  

plot.igraph(il22col,vertex.label=V(il22col)$name,layout=layout.kamada.kawai, # If you want to try other layouts, check out: https://networkx.org/documentation/stable/reference/drawing.html#module-networkx.drawing.layout
            edge.color="black", vertex.color="gray50",edge.width=E(il22col)$weight/50) # Try playing around with the weighting to see how it changes the vitual representation of your network. By default, I set it to 50. 

# What jumps out from this is that not all licenses are connected in the same way. Why for example is lobster and menhaden so connected? 

## Now let's calculate total DEGREE (Degree centrality is a count of how many edges each node has. The more degree central an actor has the more important it is in a network.      

deg <- strength(il22col, mode="all")
plot.igraph(il22col, vertex.size=deg/50, layout=layout.kamada.kawai,
            edge.color="black", vertex.color="gray50",edge.width=E(il22col)$weight/40)

deg # the actual weighted degree values can be viewed here. 

# We can explore this further by creating a degree curve to see the distribution of degree centrality. The more curved to the right, the less evenly distributed the network is.   

deg.dist <- degree_distribution(il22col, cumulative=T, mode="all")
degree_fishery <- degree(il22col, mode="all")

plot( x=0:max(degree_fishery), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      
      xlab="Degree", ylab="Cumulative Frequency")


## Now that we've gotten some of the basics down, your team's task is to spend some time exploring the data with an eye towards investigating questions related to INTERSECTIONALITY. What can we say about how different groups of people interact with SES? 


## End 

