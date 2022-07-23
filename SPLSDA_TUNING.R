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
Saf.splsda <- splsda(Key2, FactorClasses, ncomp=4, keepX = c(10, 100, 100,60))
plotIndiv(Saf.splsda, group = FactorClasses, ind.names = FALSE, legend = TRUE, title = "SplsDA Spectroscopy")
list.keepX <- c(5:10,  seq(20, 100, 10))
set.seed(30) # for reproducbility in this vignette, otherwise increase nrepeat
tune.splsda.Saf <- tune.splsda(Key2, FactorClasses, ncomp = 4,
                                 validation = 'Mfold',
                                 folds = 6, dist = 'max.dist', progressBar = FALSE,
                                 measure = "BER", test.keepX = list.keepX,
                                 nrepeat = 50)