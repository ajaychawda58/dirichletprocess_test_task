library(datasets)
library(dirichletprocess)
library(dplyr)

data <- faithful

#Transform data for zero mean and unit variance
data$waiting <- (data$waiting - mean(data$waiting))/sd(data$waiting)
data$eruptions <- (data$eruptions - mean(data$eruptions))/sd(data$eruptions)

dp <- DirichletProcessGaussian(data$eruptions )
dp <- Fit(dp, 1000)

plot(dp)

xGrid <- seq(-3, 3, by=0.01)
postSamples <- data.frame(replicate(100, PosteriorFunction(dp)(xGrid)))
postFrame <- data.frame(x=xGrid, y=rowMeans(postSamples))

library(ggplot2)

ggplot() + geom_histogram(data=data.frame(x=data$eruptions), 
                          aes(x=x, y=..density..), binwidth = 0.25)+
  geom_line(data=postFrame, aes(x=x,y=y), colour='red')


iris_data <- iris

#correlation of features

df <- dplyr::select_if(iris_data, is.numeric)
r <- cor(df) 
round(r,2)


iris_data <- iris_data %>% select(Sepal.Length, Petal.Length) %>% scale

dp2 <- DirichletProcessMvnormal(iris_data, numInitialClusters = nrow(iris_data))
dp2 <- Fit(dp2, 1000)
plot(dp2)

gridVals <- expand.grid(seq(-2, 2, by=0.01), 
                        seq(-2, 2, by=0.01))
prob <- PosteriorFunction(dp2)(gridVals)

plotFrame <- data.frame(gridVals, Probability = prob)

ggplot(plotFrame, aes(x=Var1, y=Var2, colour=Probability, fill=Probability)) + 
  geom_tile() + 
  ggtitle("Posterior Values") + 
  xlab("Sepal.Length") + 
  ylab("Petal.Length") + 
  theme(legend.position = "none") + 
  scale_fill_distiller() + 
  scale_color_distiller()
