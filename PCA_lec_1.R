
library(FactoMineR)
library(GGally)

################################################################################
##################### First contact ######################################
##############################################################################
link="~/Bureau/lectures/ACP/DATA/"

### load data and visualize scatterplots and cors
orange  <- read.csv(paste0(link,"orange.csv"), sep=",")
row.names(orange)=orange[,1]
orange=orange[,-1]
orange
ggpairs(orange[1:7])

#### compute PCA with 8:15 supplementary quantitative variables and 16:17 qualitative ones
orangeres.pca <- PCA(orange,quanti.sup=8:15,quali.sup=16:17)
summary(orangeres.pca)
plot(orangeres.pca,choix="ind",axes = 1:2,habillage = 16)
plot(orangeres.pca,choix="var",axes = 1:2,habillage = 16)
round(orangeres.pca$var$coord[,1:2],2)
round(orangeres.pca$eig,2)
round(orangeres.pca$ind$dist,2)
round(orangeres.pca$ind$cos2,2)
round(orangeres.pca$ind$contrib[,1:2],2)
### variables
round(orangeres.pca$var$cor,2)
round(orangeres.pca$var$cos2,2)
round(orangeres.pca$var$contrib[,1:2],2)

dimdesc(orangeres.pca,proba = .05)




################################################################################
##################### Second example ######################################
##############################################################################

data("decathlon")

decathlon
ggpairs(decathlon[,1:10])


pca.deca = PCA(decathlon,quanti.sup = 11:12,quali.sup = 13)

pca.deca$eig

plot(pca.deca,choix="ind",axes = 1:2,habillage = 13)
plot(pca.deca,choix="ind",axes = 3:4,habillage = 13)

plot(pca.deca,choix="var",axes = 1:2)
plot(pca.deca,choix="var",axes = 3:4)
dimdesc(pca.deca,proba=.1)

plotellipses(pca.deca,cex=.8)

##########################################################################################
### the first cloud opposes relatively high performance vs relatively low performance
# check rank vs points and what does it mean to have low values in 100m, 400m etc?? 
plot(pca.deca,choix="var",axes = 1:2,habillage = 13)
# see karpov vs Bourguignon
plot(pca.deca,choix="ind",axes = 1:2,habillage = 13)
round(scale(decathlon[,1:10]),2)
cor(decathlon[,1:10]) ### correlated variables >0.5


##########################################################################################
### which are the most relevant variables in components 1 and 2?
plot(pca.deca,choix="var",axes = 1:2,habillage = 13)
dimdesc(pca.deca,proba=.05)

##########################################################################################
### the second component opposes 400m and 1500m vs shot put and discus, endurance vs power
plot(pca.deca,choix="var",axes = 1:2,habillage = 13)
dimdesc(pca.deca,proba=.05)$Dim.2

##########################################################################################
#### once we have understood what the first two components mean, we can explain some differences in individuals
## for example, what could you tell me between Casarsa and Lorenzo
plot(pca.deca,choix="ind",axes = 1:2,habillage = 13)

#########################################################################################
###### what about the third component?
plot(pca.deca,choix="var",axes = 3:4)
dimdesc(pca.deca,proba=.05)$Dim.3

plot(pca.deca,choix="ind",axes = 3:4,habillage = 13)


nb.dim <- estim_ncp(decathlon[,1:10],scale=TRUE)

################################################################################
##################### Third example ######################################
##############################################################################

load(paste0(link,"temperature.Rdata"))

ggpairs(temperature[,1:16])


###### include supplementary quanti and quali
res <- PCA(temperature,  quanti.sup=13:16, quali.sup=17)
###### plot individuals results according to area
plot.PCA(res, choix="ind", habillage=17)
###### plot variables results 
plot.PCA(res, choix="var",axes=1:2)
#
### describe principal co,ponents based on the variables
dimdesc(res)
res$eig
res$ind
res$ind.sup
res$var
res$quanti.sup
res$quali.sup
scale(temperature[,1:16])


concat.data <- cbind.data.frame(temperature[,17],res$ind$coord)
ellipse.coord <- coord.ellipse(concat.data,bary=TRUE)
plot.PCA(res, habillage=17, ellipse=ellipse.coord, cex=0.8)






#############################################################################################"
################## Random dataset without shared variability across variables ################
##############################################################################################

n=100
var1 = rnorm(n)
var2 = rnorm(n)
var3 = rnorm(n)

rand_data=data.frame(var1,var2,var3)
rownames(rand_data)=paste0("ind",1:nrow(rand_data))


ggpairs(rand_data)


###### include supplementary quanti and quali
res_rand <- PCA(rand_data)
summary(res_rand)

res_rand$eig
res_rand$var
res_rand$ind


#################################################################################
################## Could you try the fertility dataset? #########################
#################################################################################




