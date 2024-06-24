# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }

library(NbClust)
library(class)

# Q3- Load the dataset of breast wbcd. Do the preliminary analysis and implement a KNN (K-
# nearest neighbors) model for this dataset and don’t forget that whenever it is required you should
# use: set.seed(12345). For designing the model, use following command:
# knn(train = <training features>, test = <testing features>, cl=<training labels>, k = custom value)
wbcd <- read.csv('wisc_bc_data.csv')

str(wbcd)
summary(wbcd)

wbcd_proc <- wbcd[-1]
wbcd_proc$diagnosis <- factor(wbcd_proc$diagnosis, labels = c("Benign","Malignant"), levels = c("B", "M"))

round(prop.table(table(wbcd_proc$diagnosis)) * 100, 2)

# Scales the data from -2 to 4. Data is skewed but still works well
df2 <- as.data.frame(scale(wbcd_proc[-1]))

# Data is mostly normal. Data is now between 0 - 1. Distribution shape has not changed
# Data does not need to be normalized
# df3 <- as.data.frame(lapply(wbcd_proc[-1], normalize))

# Preference to use scaling instead

# Starting the analysis
set.seed(12345)
wbcd.nc <- NbClust(df2, min.nc = 2, max.nc = 15, method = 'kmeans')
print(table(wbcd.nc$Best.nc[1,]))

barplot(table(wbcd.nc$Best.n[1,]), xlab = "Number of Clusters", ylab = "Number of Criteria", main =
          "Number of Clusters Chosen by 26 Criteria")

# Based on results from both the NbCluster and barplot we can assume that k = 2.
# This is supported by the indices, where a majority (11) proposed k = 2 as the 
# best number of clusters and accordingly the majority rule favors k = 2.

# Create 80/20 split and randomly select. Sample is not large enough for straight access
wbcd.index <- createDataPartition(wbcd_proc[,1], p = 0.8, list = FALSE)

# Remove the diagnosis variable
wbcd.train <- wbcd_proc[wbcd.index, -1]
wbcd.test <- wbcd_proc[-wbcd.index, -1]

# Use diagnosis variable as labels
wbcd.train.labels <- wbcd_proc[wbcd.index, 1]
wbcd.test.labels <- wbcd_proc[-wbcd.index, 1]

# Check proportions
prop.table(table(wbcd.train.labels) * 100)
prop.table(table(wbcd.test.labels) * 100)
# Proportions are similar across both training and testing datasets

# Create KNN model using k = 2
wbcd.knn <- knn(train = wbcd.train, test = wbcd.test, cl = wbcd.train.labels, k = 2)

# Evaluation
(wbcd_tbl <- table(wbcd.knn, wbcd.test.labels))
(Accuracy_3 <- sum(diag(wbcd_tbl))/sum(wbcd_tbl) * 100)
