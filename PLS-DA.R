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
DataPlsda <- plsda(Key2, FactorClasses, ncomp = 2)
plotIndiv(DataPlsda , comp = 1:2,
          group = FactorClasses, ind.names = FALSE, 
          ellipse = TRUE, legend = TRUE, title = 'PLSDA on Spectrescopy')
