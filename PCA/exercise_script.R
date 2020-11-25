
library(FactoMineR)
library(GGally)

link="~/teaching_PAU/Exam/"
load(paste0(link,"mtcars.Rdata"))

# 1 Choose active variables
# 'vs' and 'am' are qualitative supplementary variables as well as "country"
# The rest are active variables because they provide technical details about the cars and we
# want to use all this characteristics to find similarities and dissimilarities between cars

# 2 Rescale (or not) the variables
# YES we re-scale because the scales of the variables are not comparable

# 3 Perform PCA taking into account points 1 and 2
cars.pca <- PCA(mtcars , quali.sup = c(8,9,12), scale.unit = TRUE)

# 4 Choose the number of dimensions to interpret
barplot(cars.pca$eig[,2])
# Based on the percentage of inertia explained I would choose two variables
# Nevertheless, the most important reason to choose these two is that both are easy to interpret

### Question. You are trying to predict Y using a regression model based on a set of 
### very correlated variables variables. You decide to perform PCA on these highly correlated variables
### and you are able to interpret PC2 but not PC1. What would you do?


# 5 Joint analysis of the cloud of individuals and the cloud of variables
# 6 Use indicators to enrich interpretation

# cloud of variables
plot(cars.pca,choix="var",axes = 1:2)
dimdesc(cars.pca)
# cloud of individuals
plot(cars.pca,choix="ind",axes = 1:2,habillage = 8)
plot(cars.pca,choix="ind",axes = 1:2,habillage = 9)
plot(cars.pca,choix="ind",axes = 1:2,habillage = 12)
plotellipses(cars.pca)
###### FIRST AXIS
## the first axis is mainly explained by = cyl, disp, mpg, wt, hp and drat. 
## we could say that the first axis separates powerfull and heavy cars that consume a lot from 
## lighter cars that consume less. Remember high MPG means low consumption!!!

## we see that the first axis separates well american cars from japanese and european cars
## we could say that american cars are heavier with bigger engines and more powerfull that consume a lot
## japanese and european cars are quite mixed although european cars are somehow more dispersed, there is more variaility in them

## we see that the first axis separates well "v" shaped engines from normal engines
## american cars are more prone to have "v" shaped cars

## we also see that the first axis separates quiet well automatic and manual cars
## american cars tend to be automatic while japanese and european cars a bit more manual 9with some exceptions)


###### SECOND AXIS
## the second axis is mainly explained by = gear, qsec and carb
## cars with more gears and more carburators seem to have faster acceleration (small qsec)
## we could say that the second axis opposes ~sportive cars vs more normal cars

## we see that the second axis does not separate well cars from different countries nor "v" shape engine vs normal shape
## however sportive cars seem to more often have manual transmission (1)

## I would also say that japansese and european cars tend to have manual transmission and normal shape engines
## while american cars the opposite



# 7 Go back to raw data for interpretation
## I let you do that to confirm my conclusions and interpretation. 
## Correct me if I'm wrong and I will mark you very high!

