
library(FactoMineR)


###############################################
###### guidelines to do clustering ############
################################################

## 0. (optional) contruct a partitioning of the data if we have TOO MANY individuals 
# and we are not interested in the bottom of the tree
# we can do this partition directly in the HCPC() function by adding 
# kk=100 (for first partitioning the data into 100 classes)

## 1. Run the principal component method choosing the number of dimensions to retain
# put ncp=Inf

## 2. Construct the hierarchical tree

## 3. cut the hierarchical tree to define the clusters

## 4. make it more robust using the K-means algorithm
# we can do this using consol=TRUE inside the HCPC() call

## 5. Characterize the clusters with individuals and variables






link="C:/Users/iosu/Dropbox/Anglet/teaching/lectures/CLUSTERING/"
### Importation des donnees
load(paste0(link,"temperature.Rdata"))

            
### DO PCA. If you dont remember how to do this. Go back to the previous lectures!!
res <- PCA(temperature, quanti.sup=13:16,quali.sup=17,ncp=Inf)

### Do the clustering
res.hcpc <- HCPC(res, kk=Inf,min=3 , max=10, consol=T) 
# kk=Inf means that we do not want to partition the data into clasess
# min=3 and max=10 we expect to have between 3 and 10 groups of individuals
# consol=T makes more robust the partition after selecting the number of groups

## here plots one by one
plot(res.hcpc,choice="tree")
plot(res.hcpc,choice="3D.map")
plot(res.hcpc,choice="map")

### here we see a lot of information
res.hcpc$call$t
res.hcpc$data.clust ## adds the cluster in which we have includedeach individual



##### lets understand the clusters
res.hcpc$desc.var$test.chi2 ## test categorical supplementary variables
res.hcpc$desc.var$quanti.var ## test which variables are the most important to characterize classes
res.hcpc$desc.var$quanti





################################
#### you do it with decathlon ##
################################

data(decathlon)

res <- PCA(decathlon, quanti.sup=11:12,quali.sup=13,ncp=Inf)
### you continue
res.hcpc <- HCPC()



#################################
####  MCA data, choose one #######
#################################








