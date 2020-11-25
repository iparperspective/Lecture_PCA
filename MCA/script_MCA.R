
library(FactoMineR)

############################################
############### Hobbiers ####################
############################################
data("hobbies")

# Transform the TV variable as factor
hobbies[,"TV"] = as.factor(hobbies[,"TV"])

# MCA with the graphs given by default
res.mca <- MCA(hobbies,quali.sup=19:22,quanti.sup=23)

summary(res.mca)

# Graph of the eigenvalues
barplot(res.mca$eig[,2],main="Eigenvalues", names.arg=1:nrow(res.mca$eig))
lines(x=c(0,1000),y=c(1/18,1/18)*100) ## J=18. Those components/axis/synthethic variables that are below the line cannot be interpreted

# Graphs of the individuals
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals") 
plot(res.mca,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Gardening") 

# Graphs of the categories
plot(res.mca,invis=c("ind","quali.sup"),col.var=c(rep(c("black","red"),17),"black",rep("red",4)),title="Graph of the active categories")
plot(res.mca,invisible=c("ind","var"),hab="quali", palette=palette(c("blue","maroon","darkgreen","black","red")), title="Graph of the supplementary categories")

# Graphs of the variables
plot(res.mca,choix="var",title="Graph of the variables")
plot(res.mca,choix="quanti.sup",title="Graph of the continuous variables")

# Confidence ellipses around the categories for the first 4 variables
plotellipses(res.mca, cex=0.2, magnify=12, keepvar=1:4)

dimdesc(res.mca)

names(res.mca)




################################################
#### Genetically Modified Organisms ######
##############################################

library(FactoMineR)
gmo <- read.table("http://factominer.free.fr/book/gmo.csv",header=TRUE,sep=";")
summary(gmo[,1:16])
### only one observation for Very favourable in Position Al.H. Not enough data to draw conclusions
### basically the same happens with Position.Culture

## put them as Favourable
levels(gmo$Position.Al.H)[4] <- levels(gmo$Position.Al.H)[1] 
levels(gmo$Position.Culture)
levels(gmo$Position.Culture) <- c("Favourable","Somewhat Against","Totally opposed","Favourable")
levels(gmo$Position.Culture) 

### questions regarding the aim of the survey
# 1- Are you implicated with the debate on GMOs?
# 2- What is your view on cultivating GMO in France?
# 3- What do you think about the inclusion of Genetically modified raw materials for human consumption?
# 4- What do you think about the inclusion of Genetically modified raw materials for animals consumption?
# 5- Have you ever protested against GMO?
# 6- Do you think there is enough debate on GMOs in the media?
# 7- have you expent significant time reading about GMOs?
# 8- Do you think GMOs are good to reduce the use of fungicides?
# 9- Do you think GMOs are good to reduce hunger in the world?
# 10- Do you think GMOs are good to imporove farmers life?
# 11- Do you think GMOsmight lead to scientific advances?
# 12- Do you think GMOs are dangerous to our health?
# 13- Do you think GMOs are dangerous to the environment?
# 14- Do you think GMOs represent a financial risk to farmers?
# 15- Do you think GMOs are a useless scientific practice?
# 16-Do you think our grandparents generation had a healthier diet?
summary(gmo[,1:16])
### sociodemographic questions of survey takers
summary(gmo[,17:21])


### run MCA
res.gmo <- MCA(gmo, ncp=5, quali.sup=17:21, graph = FALSE)
### ncp=5 keep first 5 components information only
summary(res.gmo)

### select number of dimensions
barplot(res.gmo$eig[,2],main="Eigenvalues", names.arg=1:nrow(res.gmo$eig))
lines(x=c(0,1000),y=c(1/16,1/16)*100) ## J=16. Those components/axis/synthethic variables that are below the line cannot be interpreted
### we can only interpret first three dimensions


### plot individuals cloud according to first two components
plot.MCA(res.gmo,invisible=c("var","quali.sup"))
### we see a guttman effect. This illustrates a cloud of individuals that is highly structured according to the first principla component.
## the second component usually separates extreme positions from moderate positions

### plot categories cloud according to first two components
plot.MCA(res.gmo,invisible=c("ind","quali.sup"))
### here we  see two extreme positions related to GMO according to the first component. Favourable on the left and
## opposed on the right. This explains the structure behind the guttman effect
## the second component separates extreme positions on the view over GMOs from more moderate ones.

# plot square correlations with first two components of the variables
plot(res.gmo,choix="var",title="Graph of the variables")

## lets visualize the three main variables 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Position.Culture") 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Position.Al.H") 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Position.Al.A") 


### Now lets look at other variables that are less correlated to the first two components but might be important
## here we use an arbitrary way of selecting the most "important" variables by selecting 
# those square correlations that are bigger than the mean square correlations of the vairables with each component

### first axis
idx.1=which(res.gmo$var$eta2[,1]>median(res.gmo$var$eta2[,1])) ## select which square correlations are bigger thanthe median squared correlations with the first component
barplot(res.gmo$var$eta2[idx.1,1])

plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Protest") 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Hunger") 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Danger") 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Threat") 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="implicated") 

### second axis
idx.2=which(res.gmo$var$eta2[,2]>median(res.gmo$var$eta2[,2])) ## select which square correlations are bigger thanthe median squared correlations with the first component
barplot(res.gmo$var$eta2[idx.2,2])

plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Grandparents") 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Finan.risk") 
plot(res.gmo,invisible=c("var","quali.sup"),cex=.5,label="none",title="Graph of the individuals", habillage="Info.Active") 


########################################################
##### Overall conclusion over the active variables

### There is a clear pattern from the left of the graph (in favour) to the right of the graph (agains)
## This is substained by a number of variables 
# clearly Position.Culture, Position.Al.H, Position.Al.A
# less clear Protest, Hunger danger

### there is a clear pattern from top of the graph ("radicals") to the bottom of the graph ("moderates")
## this can be seen in the previous plots as well


#### NOW! can we link these things that we have interpreted with the sociodemography of the survey?
### lets see if we can link the first two components with sociodemographic variables
plot.MCA(res.gmo, col.quali.sup="brown", invisible=c("quanti.sup","ind","var"))

## I see at least two things:
# 1- strong structure for both variables profession and identification with a political movement
# 2- No particular structure with the variables age and sex

# and maybe men seem more radical than women?


###########################################
######## tea #####################
####################################

tea <- read.table("http://factominer.free.fr/book/tea.csv",header=TRUE,sep=";")
summary(tea)
res.mca<-MCA(tea, quanti.sup=22, quali.sup=c(19:21,23:36))


###########################################
######## Perfume #####################
####################################
perfume <- read.table("http://factominer.free.fr/book/perfume.csv",header=TRUE,sep=";",row.names=1)
res.perfume <- MCA(perfume)
