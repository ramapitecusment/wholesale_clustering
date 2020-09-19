library(ggplot2)
library(reshape)
library(corrplot)

setwd("")
Team = read.csv("wholesale_clustering.csv")

str(Team)
summary(Team)

plot(Team$Grocery, Team$Detergents_Paper)# xlim = c(50,100), ylim = c(50,100))
points(Team$Grocery[Team$Channel == 1], Team$Detergents_Paper[Team$Channel == 1], col = "green", pch = 19)
points(Team$Grocery[Team$Channel == 2], Team$Detergents_Paper[Team$Channel == 2], col = "red", pch = 19)

plot(Team$Grocery, Team$Detergents_Paper)# xlim = c(50,100), ylim = c(50,100))
points(Team$Grocery[Team$Region == 1], Team$Detergents_Paper[Team$Region == 1], col = "green", pch = 19)
points(Team$Grocery[Team$Region == 2], Team$Detergents_Paper[Team$Region == 2], col = "red", pch = 19)
points(Team$Grocery[Team$Region == 3], Team$Detergents_Paper[Team$Region == 3], col = "blue", pch = 19)


for (i in 3:8){
  Team[,i] = (Team[,i] - min(Team[,i]))/
    (max(Team[,i]) - min(Team[,i]))
}

z <- cor(Team[3:8])
corrplot(z, method="number")

distances = dist(Team[3:8], method = "euclidean")

# Hierarchical clustering
clusterTeam = hclust(distances, method = "ward.D")

plot(clusterTeam)

clusterGroups = cutree(clusterTeam, k = 3)

plot(Team$Grocery, Team$Fresh)# xlim = c(50,100), ylim = c(50,100))
points(Team$Grocery[clusterGroups == 1], Team$Fresh[clusterGroups == 1], col = "green", pch = 19)
points(Team$Grocery[clusterGroups == 2], Team$Fresh[clusterGroups == 2], col = "red", pch = 19)
points(Team$Grocery[clusterGroups == 3], Team$Fresh[clusterGroups == 3], col = "blue", pch = 19)

spl = split(Team[3:8], clusterGroups)
lapply(spl, colMeans)

table(clusterGroups, Team$Channel)
table(clusterGroups, Team$Region)
