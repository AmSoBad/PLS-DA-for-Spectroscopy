library(mixOmics)
#Turn Data into dataframe
data1 <- data.frame(KeyData)
#Select Key data for design matrix
Key <- data.frame((data1[1:nrow(data1), 2:ncol(data1)]))
#Create response matrix
classes <- data1$Adulteration
FactorClasses <- as.factor(classes)
#Turn key data into actual matrix
Key2 <- data.matrix(Key, rownames.force = classes)
#Create unique rows for indetity
rownames(Key2) <- make.unique(classes)
Saf.splsda <- splsda(Key2, FactorClasses, ncomp=4, keepX = c(10, 100))
plotIndiv(Saf.splsda, group = FactorClasses, ind.names = FALSE, legend = TRUE, title = "SplsDA Spectroscopy")

set.seed(25)
train <- sample(1:nrow(Key2), 140)
test <- setdiff(1:nrow(Key2), train)
X.train <- Key2[train, ]
X.test <- Key2[test,]
Y.train <- FactorClasses[train]
Y.test <- FactorClasses[test]
train.splsda.Saf <- splsda(X.train, Y.train, ncomp = 4, keepX = c(7, 100, 100, 8))
predict.splsda.Saf <- predict(train.splsda.Saf, X.test, dist = "mahalanobis.dist")
predict.comp2 <- predict.splsda.Saf$class$mahalanobis.dist[,4]
table(factor(predict.comp2, levels = levels(FactorClasses)), Y.test)
