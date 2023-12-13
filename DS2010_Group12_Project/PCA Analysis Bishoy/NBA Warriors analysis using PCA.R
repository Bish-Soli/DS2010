# Loading the database
data <- DS2010_Warriors_Data

# Saving the important columns into variable data
data <- data[, -c(1, 3, 5, 25:41)]


# Making win(1) or loss(0) ---- 1 or 0
data$WL <- ifelse(data$WL == "W", 1, 0)


# Making X column "HomeAway" into 1 or 0 ---- home(1) / away(0)
colnames(data)[2] <- "HomeAway"
data$HomeAway <- ifelse(data$HomeAway == "@", 0, 1)

library('corrr')
library(ggplot2)
library(ggcorrplot)
library("FactoMineR")

# Normalizing the data
numerical_data <- data[,5:21]
head(numerical_data)


data_normalized <- scale(numerical_data)
head(data_normalized)

#Compute the correlation matrix

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)


# Applying PCA

data.pca <- princomp(corr_matrix)

#Cumulative Proportion
summary(data.pca)

# Visualization of the principal components

library(factoextra)
library(vctrs)

data.pca$loadings[, 1:17]

# Scree Plot
fviz_eig(data.pca, addlabels = TRUE)

# Biplot of the attributes
fviz_pca_var(data.pca, col.var = "black")

# Biplot combined with cos2 
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

# Select the top 9 contributing variable
fviz_pca_var(data.pca, select.var = list(contrib = 9))


# Regression Tree Part

library(rpart)
library(rpart.plot)
fit.tree = rpart(Tm ~ FG+AST+X3PPercentage+X3P+FGPercentage+FT+FTA+FGA+ORB, data=data, method="anova", control = list(cp = 0, xval = 9))
#summary(fit.tree)
fit.tree

rpart.plot(fit.tree)

plotcp(fit.tree)



