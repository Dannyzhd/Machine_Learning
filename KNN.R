# Author: Handing Zhang
# let's get started!

# Packages ####

pacman::p_load(ggplot2, dplyr, class, gridExtra, gmodels)
devtools::install_github('cttobin/ggthemr')  
# This package is just for setting the colour palette, optional
library(ggthemr)



# Data ####

iris.data <- iris
str(iris.data)
# 

# Data Visualization

ggthemr("light")

# scatter plot visualizing petal width and length grouped by species.
(scatter <- ggplot(iris.data, aes(x = Petal.Width, y = Petal.Length, color = Species)) +
  geom_point(size = 3, alpha = 0.6) + 
    theme_classic() + 
    theme(legend.position = c(0.2, 0.8))
  
  
)


# Boxplot visualizing variation in petal width between species.

(boxplot <- ggplot(iris.data, aes(x = Species, y = Petal.Width, fill = Species)) + 
    geom_boxplot() + 
    theme_classic() +
    theme(legend.position = c(0.2, 0.8)))



# Building a normalisation function
normalise <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# This is a min-max scaling, "- min(x)" makes sure vector starts at zero.
# " /drnom" makes sure max is at one.

iris.norm <- as.data.frame(lapply(iris[1:4], normalise))

# Randomly generate training set and testing set.
# Use sample function.

# Generating seed
set.seed(1234)
# Randomly generating our training and testing samples with respective ratio of 2/3 and 1/3

datasample <- sample(2, nrow(iris.norm), replace = TRUE, prob = c(0.67, 0.33))
# generate index consisting of 1 and 2 with corresponding proportion we want.

# Training set
iris.training <- iris.norm[datasample == 1, 1:4]

# Testing set
iris.testing <- iris.norm[datasample == 2, 1:4]

# Generate Training labels
iris_training_labels <- iris[datasample == 1, 5]

# Generate Testing labels
iris_testing_labels <- iris[datasample == 2, 5]

# Build KNN classifier ####
# Use class package

# Parameters: ####

# Training Dataset
# Testing Dataset
# Training Labels
# K

iris.knn <- knn(train = iris.training,
                test = iris.testing,
                cl = iris.training_labels,
                k = 3)


# Assess our model
# find out if the classes our algorithm predicts based on the training data accurately predict the species classes in our original iris dataset

# create a dataframe from known TRUE test labels
test.labels <- data.frame(iris_testing_labels)

# Combine it with our prediction
class.comparison <- data.frame(iris.knn, test.labels)

# proper column names.
names(class.comparison) <- c("Predicted Species", "Observed Species")

# Look at a contingency table
CrossTable(x = iris_testing_labels, y = iris.knn, prop.chisq = F)



