# Classifier_SVM_Total data set_150814 - 4 PChem(water)
OSCube_KJB  
2015년 8월 21일  


```r
require(RCurl)
```

```
## Loading required package: RCurl
## Loading required package: bitops
```

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
mycsv <- getURL("https://docs.google.com/spreadsheets/d/1zda4Ylxc7vxnqLyWET2iLqbw-gqIv6rwwPXgrSYAE6w/pub?gid=1991801738&single=true&output=csv")

raw_data <- read.csv(textConnection(mycsv))

nor_data <- data.frame(apply(raw_data,MARGIN=2,FUN=function(X)(X - min(X))/diff(range(X))))

nor_data.NT <- nor_data[nor_data[,"y02"]==1,]
nor_data.T <- nor_data[nor_data[,"y02"]==0,]

sampling_nor_data.NT <- sample(nrow(nor_data.NT),round(nrow(nor_data.NT)*0.6))
sampling_nor_data.T <- sample(nrow(nor_data.T),round(nrow(nor_data.T)*0.6))

training_data <- rbind(nor_data.NT[sampling_nor_data.NT,],nor_data.T[sampling_nor_data.T,])
validation_data <- rbind(nor_data.NT[-sampling_nor_data.NT,],nor_data.T[-sampling_nor_data.T,])

variable <- c(1:(ncol(raw_data)-4))

Logistic_regression.model <- c("sensitivity","specificity","accuracy","all variables","model.coef","model.p-value")
for (i in c(1:(ncol(raw_data)-4)))
{
variable.combination <- combn(variable,i)
variable.combination.count <- ncol(variable.combination)
  for (j in c(1:variable.combination.count))
  {training_data.subset <- training_data[,c(variable.combination[,j],match("y02",names(training_data)))]
   validation_data.subset <- validation_data[,c(variable.combination[,j],match("y02",names(validation_data)))]
   Logistic_regression.training <- glm(y02~.,training_data.subset,family=binomial("logit"))
   Logistic_regression.validation <- round(predict(Logistic_regression.training,newdata=validation_data.subset,type="response"))
   Logistic_regression.AIC <- toString(extractAIC(Logistic_regression.training)[2])
   Logistic_regression.coefficient <- toString(Logistic_regression.training$coef)
   Logistic_regression.p_value <- toString(summary(Logistic_regression.training)$coefficients[,4])
   Logistic_regression.performance <- confusionMatrix(Logistic_regression.validation, validation_data.subset$y02)
   Logistic_regression.sensitivity <- Logistic_regression.performance$byClass[1]
   Logistic_regression.specificity <- Logistic_regression.performance$byClass[2]
   Logistic_regression.accuracy <- Logistic_regression.performance$overall[1]
   colnames <- toString(colnames(training_data.subset))
   Logistic_regression.model <- rbind(Logistic_regression.model,c(Logistic_regression.sensitivity,Logistic_regression.specificity,Logistic_regression.accuracy,colnames,Logistic_regression.coefficient,Logistic_regression.p_value))
  }
}
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```r
Logistic_regression.model
```

```
##                           Sensitivity          Specificity        
## Logistic_regression.model "sensitivity"        "specificity"      
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.0967741935483871" "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.993150684931507"
##                           "0.0967741935483871" "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.986301369863014"
##                           "0.0967741935483871" "0.993150684931507"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "0.979452054794521"
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.986301369863014"
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.986301369863014"
##                           "0.032258064516129"  "0.979452054794521"
##                           "0"                  "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           Accuracy           
## Logistic_regression.model "accuracy"         
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.84180790960452" 
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.84180790960452" 
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.84180790960452" 
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.84180790960452" 
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.84180790960452" 
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.813559322033898"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.813559322033898"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.813559322033898"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.807909604519774"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.807909604519774"
##                           "0.819209039548023"
##                                                                             
## Logistic_regression.model "all variables"                                   
##                           "x01, y02"                                        
##                           "x02, y02"                                        
##                           "x03, y02"                                        
##                           "x04, y02"                                        
##                           "x05, y02"                                        
##                           "x06, y02"                                        
##                           "x07, y02"                                        
##                           "x08, y02"                                        
##                           "x09, y02"                                        
##                           "x01, x02, y02"                                   
##                           "x01, x03, y02"                                   
##                           "x01, x04, y02"                                   
##                           "x01, x05, y02"                                   
##                           "x01, x06, y02"                                   
##                           "x01, x07, y02"                                   
##                           "x01, x08, y02"                                   
##                           "x01, x09, y02"                                   
##                           "x02, x03, y02"                                   
##                           "x02, x04, y02"                                   
##                           "x02, x05, y02"                                   
##                           "x02, x06, y02"                                   
##                           "x02, x07, y02"                                   
##                           "x02, x08, y02"                                   
##                           "x02, x09, y02"                                   
##                           "x03, x04, y02"                                   
##                           "x03, x05, y02"                                   
##                           "x03, x06, y02"                                   
##                           "x03, x07, y02"                                   
##                           "x03, x08, y02"                                   
##                           "x03, x09, y02"                                   
##                           "x04, x05, y02"                                   
##                           "x04, x06, y02"                                   
##                           "x04, x07, y02"                                   
##                           "x04, x08, y02"                                   
##                           "x04, x09, y02"                                   
##                           "x05, x06, y02"                                   
##                           "x05, x07, y02"                                   
##                           "x05, x08, y02"                                   
##                           "x05, x09, y02"                                   
##                           "x06, x07, y02"                                   
##                           "x06, x08, y02"                                   
##                           "x06, x09, y02"                                   
##                           "x07, x08, y02"                                   
##                           "x07, x09, y02"                                   
##                           "x08, x09, y02"                                   
##                           "x01, x02, x03, y02"                              
##                           "x01, x02, x04, y02"                              
##                           "x01, x02, x05, y02"                              
##                           "x01, x02, x06, y02"                              
##                           "x01, x02, x07, y02"                              
##                           "x01, x02, x08, y02"                              
##                           "x01, x02, x09, y02"                              
##                           "x01, x03, x04, y02"                              
##                           "x01, x03, x05, y02"                              
##                           "x01, x03, x06, y02"                              
##                           "x01, x03, x07, y02"                              
##                           "x01, x03, x08, y02"                              
##                           "x01, x03, x09, y02"                              
##                           "x01, x04, x05, y02"                              
##                           "x01, x04, x06, y02"                              
##                           "x01, x04, x07, y02"                              
##                           "x01, x04, x08, y02"                              
##                           "x01, x04, x09, y02"                              
##                           "x01, x05, x06, y02"                              
##                           "x01, x05, x07, y02"                              
##                           "x01, x05, x08, y02"                              
##                           "x01, x05, x09, y02"                              
##                           "x01, x06, x07, y02"                              
##                           "x01, x06, x08, y02"                              
##                           "x01, x06, x09, y02"                              
##                           "x01, x07, x08, y02"                              
##                           "x01, x07, x09, y02"                              
##                           "x01, x08, x09, y02"                              
##                           "x02, x03, x04, y02"                              
##                           "x02, x03, x05, y02"                              
##                           "x02, x03, x06, y02"                              
##                           "x02, x03, x07, y02"                              
##                           "x02, x03, x08, y02"                              
##                           "x02, x03, x09, y02"                              
##                           "x02, x04, x05, y02"                              
##                           "x02, x04, x06, y02"                              
##                           "x02, x04, x07, y02"                              
##                           "x02, x04, x08, y02"                              
##                           "x02, x04, x09, y02"                              
##                           "x02, x05, x06, y02"                              
##                           "x02, x05, x07, y02"                              
##                           "x02, x05, x08, y02"                              
##                           "x02, x05, x09, y02"                              
##                           "x02, x06, x07, y02"                              
##                           "x02, x06, x08, y02"                              
##                           "x02, x06, x09, y02"                              
##                           "x02, x07, x08, y02"                              
##                           "x02, x07, x09, y02"                              
##                           "x02, x08, x09, y02"                              
##                           "x03, x04, x05, y02"                              
##                           "x03, x04, x06, y02"                              
##                           "x03, x04, x07, y02"                              
##                           "x03, x04, x08, y02"                              
##                           "x03, x04, x09, y02"                              
##                           "x03, x05, x06, y02"                              
##                           "x03, x05, x07, y02"                              
##                           "x03, x05, x08, y02"                              
##                           "x03, x05, x09, y02"                              
##                           "x03, x06, x07, y02"                              
##                           "x03, x06, x08, y02"                              
##                           "x03, x06, x09, y02"                              
##                           "x03, x07, x08, y02"                              
##                           "x03, x07, x09, y02"                              
##                           "x03, x08, x09, y02"                              
##                           "x04, x05, x06, y02"                              
##                           "x04, x05, x07, y02"                              
##                           "x04, x05, x08, y02"                              
##                           "x04, x05, x09, y02"                              
##                           "x04, x06, x07, y02"                              
##                           "x04, x06, x08, y02"                              
##                           "x04, x06, x09, y02"                              
##                           "x04, x07, x08, y02"                              
##                           "x04, x07, x09, y02"                              
##                           "x04, x08, x09, y02"                              
##                           "x05, x06, x07, y02"                              
##                           "x05, x06, x08, y02"                              
##                           "x05, x06, x09, y02"                              
##                           "x05, x07, x08, y02"                              
##                           "x05, x07, x09, y02"                              
##                           "x05, x08, x09, y02"                              
##                           "x06, x07, x08, y02"                              
##                           "x06, x07, x09, y02"                              
##                           "x06, x08, x09, y02"                              
##                           "x07, x08, x09, y02"                              
##                           "x01, x02, x03, x04, y02"                         
##                           "x01, x02, x03, x05, y02"                         
##                           "x01, x02, x03, x06, y02"                         
##                           "x01, x02, x03, x07, y02"                         
##                           "x01, x02, x03, x08, y02"                         
##                           "x01, x02, x03, x09, y02"                         
##                           "x01, x02, x04, x05, y02"                         
##                           "x01, x02, x04, x06, y02"                         
##                           "x01, x02, x04, x07, y02"                         
##                           "x01, x02, x04, x08, y02"                         
##                           "x01, x02, x04, x09, y02"                         
##                           "x01, x02, x05, x06, y02"                         
##                           "x01, x02, x05, x07, y02"                         
##                           "x01, x02, x05, x08, y02"                         
##                           "x01, x02, x05, x09, y02"                         
##                           "x01, x02, x06, x07, y02"                         
##                           "x01, x02, x06, x08, y02"                         
##                           "x01, x02, x06, x09, y02"                         
##                           "x01, x02, x07, x08, y02"                         
##                           "x01, x02, x07, x09, y02"                         
##                           "x01, x02, x08, x09, y02"                         
##                           "x01, x03, x04, x05, y02"                         
##                           "x01, x03, x04, x06, y02"                         
##                           "x01, x03, x04, x07, y02"                         
##                           "x01, x03, x04, x08, y02"                         
##                           "x01, x03, x04, x09, y02"                         
##                           "x01, x03, x05, x06, y02"                         
##                           "x01, x03, x05, x07, y02"                         
##                           "x01, x03, x05, x08, y02"                         
##                           "x01, x03, x05, x09, y02"                         
##                           "x01, x03, x06, x07, y02"                         
##                           "x01, x03, x06, x08, y02"                         
##                           "x01, x03, x06, x09, y02"                         
##                           "x01, x03, x07, x08, y02"                         
##                           "x01, x03, x07, x09, y02"                         
##                           "x01, x03, x08, x09, y02"                         
##                           "x01, x04, x05, x06, y02"                         
##                           "x01, x04, x05, x07, y02"                         
##                           "x01, x04, x05, x08, y02"                         
##                           "x01, x04, x05, x09, y02"                         
##                           "x01, x04, x06, x07, y02"                         
##                           "x01, x04, x06, x08, y02"                         
##                           "x01, x04, x06, x09, y02"                         
##                           "x01, x04, x07, x08, y02"                         
##                           "x01, x04, x07, x09, y02"                         
##                           "x01, x04, x08, x09, y02"                         
##                           "x01, x05, x06, x07, y02"                         
##                           "x01, x05, x06, x08, y02"                         
##                           "x01, x05, x06, x09, y02"                         
##                           "x01, x05, x07, x08, y02"                         
##                           "x01, x05, x07, x09, y02"                         
##                           "x01, x05, x08, x09, y02"                         
##                           "x01, x06, x07, x08, y02"                         
##                           "x01, x06, x07, x09, y02"                         
##                           "x01, x06, x08, x09, y02"                         
##                           "x01, x07, x08, x09, y02"                         
##                           "x02, x03, x04, x05, y02"                         
##                           "x02, x03, x04, x06, y02"                         
##                           "x02, x03, x04, x07, y02"                         
##                           "x02, x03, x04, x08, y02"                         
##                           "x02, x03, x04, x09, y02"                         
##                           "x02, x03, x05, x06, y02"                         
##                           "x02, x03, x05, x07, y02"                         
##                           "x02, x03, x05, x08, y02"                         
##                           "x02, x03, x05, x09, y02"                         
##                           "x02, x03, x06, x07, y02"                         
##                           "x02, x03, x06, x08, y02"                         
##                           "x02, x03, x06, x09, y02"                         
##                           "x02, x03, x07, x08, y02"                         
##                           "x02, x03, x07, x09, y02"                         
##                           "x02, x03, x08, x09, y02"                         
##                           "x02, x04, x05, x06, y02"                         
##                           "x02, x04, x05, x07, y02"                         
##                           "x02, x04, x05, x08, y02"                         
##                           "x02, x04, x05, x09, y02"                         
##                           "x02, x04, x06, x07, y02"                         
##                           "x02, x04, x06, x08, y02"                         
##                           "x02, x04, x06, x09, y02"                         
##                           "x02, x04, x07, x08, y02"                         
##                           "x02, x04, x07, x09, y02"                         
##                           "x02, x04, x08, x09, y02"                         
##                           "x02, x05, x06, x07, y02"                         
##                           "x02, x05, x06, x08, y02"                         
##                           "x02, x05, x06, x09, y02"                         
##                           "x02, x05, x07, x08, y02"                         
##                           "x02, x05, x07, x09, y02"                         
##                           "x02, x05, x08, x09, y02"                         
##                           "x02, x06, x07, x08, y02"                         
##                           "x02, x06, x07, x09, y02"                         
##                           "x02, x06, x08, x09, y02"                         
##                           "x02, x07, x08, x09, y02"                         
##                           "x03, x04, x05, x06, y02"                         
##                           "x03, x04, x05, x07, y02"                         
##                           "x03, x04, x05, x08, y02"                         
##                           "x03, x04, x05, x09, y02"                         
##                           "x03, x04, x06, x07, y02"                         
##                           "x03, x04, x06, x08, y02"                         
##                           "x03, x04, x06, x09, y02"                         
##                           "x03, x04, x07, x08, y02"                         
##                           "x03, x04, x07, x09, y02"                         
##                           "x03, x04, x08, x09, y02"                         
##                           "x03, x05, x06, x07, y02"                         
##                           "x03, x05, x06, x08, y02"                         
##                           "x03, x05, x06, x09, y02"                         
##                           "x03, x05, x07, x08, y02"                         
##                           "x03, x05, x07, x09, y02"                         
##                           "x03, x05, x08, x09, y02"                         
##                           "x03, x06, x07, x08, y02"                         
##                           "x03, x06, x07, x09, y02"                         
##                           "x03, x06, x08, x09, y02"                         
##                           "x03, x07, x08, x09, y02"                         
##                           "x04, x05, x06, x07, y02"                         
##                           "x04, x05, x06, x08, y02"                         
##                           "x04, x05, x06, x09, y02"                         
##                           "x04, x05, x07, x08, y02"                         
##                           "x04, x05, x07, x09, y02"                         
##                           "x04, x05, x08, x09, y02"                         
##                           "x04, x06, x07, x08, y02"                         
##                           "x04, x06, x07, x09, y02"                         
##                           "x04, x06, x08, x09, y02"                         
##                           "x04, x07, x08, x09, y02"                         
##                           "x05, x06, x07, x08, y02"                         
##                           "x05, x06, x07, x09, y02"                         
##                           "x05, x06, x08, x09, y02"                         
##                           "x05, x07, x08, x09, y02"                         
##                           "x06, x07, x08, x09, y02"                         
##                           "x01, x02, x03, x04, x05, y02"                    
##                           "x01, x02, x03, x04, x06, y02"                    
##                           "x01, x02, x03, x04, x07, y02"                    
##                           "x01, x02, x03, x04, x08, y02"                    
##                           "x01, x02, x03, x04, x09, y02"                    
##                           "x01, x02, x03, x05, x06, y02"                    
##                           "x01, x02, x03, x05, x07, y02"                    
##                           "x01, x02, x03, x05, x08, y02"                    
##                           "x01, x02, x03, x05, x09, y02"                    
##                           "x01, x02, x03, x06, x07, y02"                    
##                           "x01, x02, x03, x06, x08, y02"                    
##                           "x01, x02, x03, x06, x09, y02"                    
##                           "x01, x02, x03, x07, x08, y02"                    
##                           "x01, x02, x03, x07, x09, y02"                    
##                           "x01, x02, x03, x08, x09, y02"                    
##                           "x01, x02, x04, x05, x06, y02"                    
##                           "x01, x02, x04, x05, x07, y02"                    
##                           "x01, x02, x04, x05, x08, y02"                    
##                           "x01, x02, x04, x05, x09, y02"                    
##                           "x01, x02, x04, x06, x07, y02"                    
##                           "x01, x02, x04, x06, x08, y02"                    
##                           "x01, x02, x04, x06, x09, y02"                    
##                           "x01, x02, x04, x07, x08, y02"                    
##                           "x01, x02, x04, x07, x09, y02"                    
##                           "x01, x02, x04, x08, x09, y02"                    
##                           "x01, x02, x05, x06, x07, y02"                    
##                           "x01, x02, x05, x06, x08, y02"                    
##                           "x01, x02, x05, x06, x09, y02"                    
##                           "x01, x02, x05, x07, x08, y02"                    
##                           "x01, x02, x05, x07, x09, y02"                    
##                           "x01, x02, x05, x08, x09, y02"                    
##                           "x01, x02, x06, x07, x08, y02"                    
##                           "x01, x02, x06, x07, x09, y02"                    
##                           "x01, x02, x06, x08, x09, y02"                    
##                           "x01, x02, x07, x08, x09, y02"                    
##                           "x01, x03, x04, x05, x06, y02"                    
##                           "x01, x03, x04, x05, x07, y02"                    
##                           "x01, x03, x04, x05, x08, y02"                    
##                           "x01, x03, x04, x05, x09, y02"                    
##                           "x01, x03, x04, x06, x07, y02"                    
##                           "x01, x03, x04, x06, x08, y02"                    
##                           "x01, x03, x04, x06, x09, y02"                    
##                           "x01, x03, x04, x07, x08, y02"                    
##                           "x01, x03, x04, x07, x09, y02"                    
##                           "x01, x03, x04, x08, x09, y02"                    
##                           "x01, x03, x05, x06, x07, y02"                    
##                           "x01, x03, x05, x06, x08, y02"                    
##                           "x01, x03, x05, x06, x09, y02"                    
##                           "x01, x03, x05, x07, x08, y02"                    
##                           "x01, x03, x05, x07, x09, y02"                    
##                           "x01, x03, x05, x08, x09, y02"                    
##                           "x01, x03, x06, x07, x08, y02"                    
##                           "x01, x03, x06, x07, x09, y02"                    
##                           "x01, x03, x06, x08, x09, y02"                    
##                           "x01, x03, x07, x08, x09, y02"                    
##                           "x01, x04, x05, x06, x07, y02"                    
##                           "x01, x04, x05, x06, x08, y02"                    
##                           "x01, x04, x05, x06, x09, y02"                    
##                           "x01, x04, x05, x07, x08, y02"                    
##                           "x01, x04, x05, x07, x09, y02"                    
##                           "x01, x04, x05, x08, x09, y02"                    
##                           "x01, x04, x06, x07, x08, y02"                    
##                           "x01, x04, x06, x07, x09, y02"                    
##                           "x01, x04, x06, x08, x09, y02"                    
##                           "x01, x04, x07, x08, x09, y02"                    
##                           "x01, x05, x06, x07, x08, y02"                    
##                           "x01, x05, x06, x07, x09, y02"                    
##                           "x01, x05, x06, x08, x09, y02"                    
##                           "x01, x05, x07, x08, x09, y02"                    
##                           "x01, x06, x07, x08, x09, y02"                    
##                           "x02, x03, x04, x05, x06, y02"                    
##                           "x02, x03, x04, x05, x07, y02"                    
##                           "x02, x03, x04, x05, x08, y02"                    
##                           "x02, x03, x04, x05, x09, y02"                    
##                           "x02, x03, x04, x06, x07, y02"                    
##                           "x02, x03, x04, x06, x08, y02"                    
##                           "x02, x03, x04, x06, x09, y02"                    
##                           "x02, x03, x04, x07, x08, y02"                    
##                           "x02, x03, x04, x07, x09, y02"                    
##                           "x02, x03, x04, x08, x09, y02"                    
##                           "x02, x03, x05, x06, x07, y02"                    
##                           "x02, x03, x05, x06, x08, y02"                    
##                           "x02, x03, x05, x06, x09, y02"                    
##                           "x02, x03, x05, x07, x08, y02"                    
##                           "x02, x03, x05, x07, x09, y02"                    
##                           "x02, x03, x05, x08, x09, y02"                    
##                           "x02, x03, x06, x07, x08, y02"                    
##                           "x02, x03, x06, x07, x09, y02"                    
##                           "x02, x03, x06, x08, x09, y02"                    
##                           "x02, x03, x07, x08, x09, y02"                    
##                           "x02, x04, x05, x06, x07, y02"                    
##                           "x02, x04, x05, x06, x08, y02"                    
##                           "x02, x04, x05, x06, x09, y02"                    
##                           "x02, x04, x05, x07, x08, y02"                    
##                           "x02, x04, x05, x07, x09, y02"                    
##                           "x02, x04, x05, x08, x09, y02"                    
##                           "x02, x04, x06, x07, x08, y02"                    
##                           "x02, x04, x06, x07, x09, y02"                    
##                           "x02, x04, x06, x08, x09, y02"                    
##                           "x02, x04, x07, x08, x09, y02"                    
##                           "x02, x05, x06, x07, x08, y02"                    
##                           "x02, x05, x06, x07, x09, y02"                    
##                           "x02, x05, x06, x08, x09, y02"                    
##                           "x02, x05, x07, x08, x09, y02"                    
##                           "x02, x06, x07, x08, x09, y02"                    
##                           "x03, x04, x05, x06, x07, y02"                    
##                           "x03, x04, x05, x06, x08, y02"                    
##                           "x03, x04, x05, x06, x09, y02"                    
##                           "x03, x04, x05, x07, x08, y02"                    
##                           "x03, x04, x05, x07, x09, y02"                    
##                           "x03, x04, x05, x08, x09, y02"                    
##                           "x03, x04, x06, x07, x08, y02"                    
##                           "x03, x04, x06, x07, x09, y02"                    
##                           "x03, x04, x06, x08, x09, y02"                    
##                           "x03, x04, x07, x08, x09, y02"                    
##                           "x03, x05, x06, x07, x08, y02"                    
##                           "x03, x05, x06, x07, x09, y02"                    
##                           "x03, x05, x06, x08, x09, y02"                    
##                           "x03, x05, x07, x08, x09, y02"                    
##                           "x03, x06, x07, x08, x09, y02"                    
##                           "x04, x05, x06, x07, x08, y02"                    
##                           "x04, x05, x06, x07, x09, y02"                    
##                           "x04, x05, x06, x08, x09, y02"                    
##                           "x04, x05, x07, x08, x09, y02"                    
##                           "x04, x06, x07, x08, x09, y02"                    
##                           "x05, x06, x07, x08, x09, y02"                    
##                           "x01, x02, x03, x04, x05, x06, y02"               
##                           "x01, x02, x03, x04, x05, x07, y02"               
##                           "x01, x02, x03, x04, x05, x08, y02"               
##                           "x01, x02, x03, x04, x05, x09, y02"               
##                           "x01, x02, x03, x04, x06, x07, y02"               
##                           "x01, x02, x03, x04, x06, x08, y02"               
##                           "x01, x02, x03, x04, x06, x09, y02"               
##                           "x01, x02, x03, x04, x07, x08, y02"               
##                           "x01, x02, x03, x04, x07, x09, y02"               
##                           "x01, x02, x03, x04, x08, x09, y02"               
##                           "x01, x02, x03, x05, x06, x07, y02"               
##                           "x01, x02, x03, x05, x06, x08, y02"               
##                           "x01, x02, x03, x05, x06, x09, y02"               
##                           "x01, x02, x03, x05, x07, x08, y02"               
##                           "x01, x02, x03, x05, x07, x09, y02"               
##                           "x01, x02, x03, x05, x08, x09, y02"               
##                           "x01, x02, x03, x06, x07, x08, y02"               
##                           "x01, x02, x03, x06, x07, x09, y02"               
##                           "x01, x02, x03, x06, x08, x09, y02"               
##                           "x01, x02, x03, x07, x08, x09, y02"               
##                           "x01, x02, x04, x05, x06, x07, y02"               
##                           "x01, x02, x04, x05, x06, x08, y02"               
##                           "x01, x02, x04, x05, x06, x09, y02"               
##                           "x01, x02, x04, x05, x07, x08, y02"               
##                           "x01, x02, x04, x05, x07, x09, y02"               
##                           "x01, x02, x04, x05, x08, x09, y02"               
##                           "x01, x02, x04, x06, x07, x08, y02"               
##                           "x01, x02, x04, x06, x07, x09, y02"               
##                           "x01, x02, x04, x06, x08, x09, y02"               
##                           "x01, x02, x04, x07, x08, x09, y02"               
##                           "x01, x02, x05, x06, x07, x08, y02"               
##                           "x01, x02, x05, x06, x07, x09, y02"               
##                           "x01, x02, x05, x06, x08, x09, y02"               
##                           "x01, x02, x05, x07, x08, x09, y02"               
##                           "x01, x02, x06, x07, x08, x09, y02"               
##                           "x01, x03, x04, x05, x06, x07, y02"               
##                           "x01, x03, x04, x05, x06, x08, y02"               
##                           "x01, x03, x04, x05, x06, x09, y02"               
##                           "x01, x03, x04, x05, x07, x08, y02"               
##                           "x01, x03, x04, x05, x07, x09, y02"               
##                           "x01, x03, x04, x05, x08, x09, y02"               
##                           "x01, x03, x04, x06, x07, x08, y02"               
##                           "x01, x03, x04, x06, x07, x09, y02"               
##                           "x01, x03, x04, x06, x08, x09, y02"               
##                           "x01, x03, x04, x07, x08, x09, y02"               
##                           "x01, x03, x05, x06, x07, x08, y02"               
##                           "x01, x03, x05, x06, x07, x09, y02"               
##                           "x01, x03, x05, x06, x08, x09, y02"               
##                           "x01, x03, x05, x07, x08, x09, y02"               
##                           "x01, x03, x06, x07, x08, x09, y02"               
##                           "x01, x04, x05, x06, x07, x08, y02"               
##                           "x01, x04, x05, x06, x07, x09, y02"               
##                           "x01, x04, x05, x06, x08, x09, y02"               
##                           "x01, x04, x05, x07, x08, x09, y02"               
##                           "x01, x04, x06, x07, x08, x09, y02"               
##                           "x01, x05, x06, x07, x08, x09, y02"               
##                           "x02, x03, x04, x05, x06, x07, y02"               
##                           "x02, x03, x04, x05, x06, x08, y02"               
##                           "x02, x03, x04, x05, x06, x09, y02"               
##                           "x02, x03, x04, x05, x07, x08, y02"               
##                           "x02, x03, x04, x05, x07, x09, y02"               
##                           "x02, x03, x04, x05, x08, x09, y02"               
##                           "x02, x03, x04, x06, x07, x08, y02"               
##                           "x02, x03, x04, x06, x07, x09, y02"               
##                           "x02, x03, x04, x06, x08, x09, y02"               
##                           "x02, x03, x04, x07, x08, x09, y02"               
##                           "x02, x03, x05, x06, x07, x08, y02"               
##                           "x02, x03, x05, x06, x07, x09, y02"               
##                           "x02, x03, x05, x06, x08, x09, y02"               
##                           "x02, x03, x05, x07, x08, x09, y02"               
##                           "x02, x03, x06, x07, x08, x09, y02"               
##                           "x02, x04, x05, x06, x07, x08, y02"               
##                           "x02, x04, x05, x06, x07, x09, y02"               
##                           "x02, x04, x05, x06, x08, x09, y02"               
##                           "x02, x04, x05, x07, x08, x09, y02"               
##                           "x02, x04, x06, x07, x08, x09, y02"               
##                           "x02, x05, x06, x07, x08, x09, y02"               
##                           "x03, x04, x05, x06, x07, x08, y02"               
##                           "x03, x04, x05, x06, x07, x09, y02"               
##                           "x03, x04, x05, x06, x08, x09, y02"               
##                           "x03, x04, x05, x07, x08, x09, y02"               
##                           "x03, x04, x06, x07, x08, x09, y02"               
##                           "x03, x05, x06, x07, x08, x09, y02"               
##                           "x04, x05, x06, x07, x08, x09, y02"               
##                           "x01, x02, x03, x04, x05, x06, x07, y02"          
##                           "x01, x02, x03, x04, x05, x06, x08, y02"          
##                           "x01, x02, x03, x04, x05, x06, x09, y02"          
##                           "x01, x02, x03, x04, x05, x07, x08, y02"          
##                           "x01, x02, x03, x04, x05, x07, x09, y02"          
##                           "x01, x02, x03, x04, x05, x08, x09, y02"          
##                           "x01, x02, x03, x04, x06, x07, x08, y02"          
##                           "x01, x02, x03, x04, x06, x07, x09, y02"          
##                           "x01, x02, x03, x04, x06, x08, x09, y02"          
##                           "x01, x02, x03, x04, x07, x08, x09, y02"          
##                           "x01, x02, x03, x05, x06, x07, x08, y02"          
##                           "x01, x02, x03, x05, x06, x07, x09, y02"          
##                           "x01, x02, x03, x05, x06, x08, x09, y02"          
##                           "x01, x02, x03, x05, x07, x08, x09, y02"          
##                           "x01, x02, x03, x06, x07, x08, x09, y02"          
##                           "x01, x02, x04, x05, x06, x07, x08, y02"          
##                           "x01, x02, x04, x05, x06, x07, x09, y02"          
##                           "x01, x02, x04, x05, x06, x08, x09, y02"          
##                           "x01, x02, x04, x05, x07, x08, x09, y02"          
##                           "x01, x02, x04, x06, x07, x08, x09, y02"          
##                           "x01, x02, x05, x06, x07, x08, x09, y02"          
##                           "x01, x03, x04, x05, x06, x07, x08, y02"          
##                           "x01, x03, x04, x05, x06, x07, x09, y02"          
##                           "x01, x03, x04, x05, x06, x08, x09, y02"          
##                           "x01, x03, x04, x05, x07, x08, x09, y02"          
##                           "x01, x03, x04, x06, x07, x08, x09, y02"          
##                           "x01, x03, x05, x06, x07, x08, x09, y02"          
##                           "x01, x04, x05, x06, x07, x08, x09, y02"          
##                           "x02, x03, x04, x05, x06, x07, x08, y02"          
##                           "x02, x03, x04, x05, x06, x07, x09, y02"          
##                           "x02, x03, x04, x05, x06, x08, x09, y02"          
##                           "x02, x03, x04, x05, x07, x08, x09, y02"          
##                           "x02, x03, x04, x06, x07, x08, x09, y02"          
##                           "x02, x03, x05, x06, x07, x08, x09, y02"          
##                           "x02, x04, x05, x06, x07, x08, x09, y02"          
##                           "x03, x04, x05, x06, x07, x08, x09, y02"          
##                           "x01, x02, x03, x04, x05, x06, x07, x08, y02"     
##                           "x01, x02, x03, x04, x05, x06, x07, x09, y02"     
##                           "x01, x02, x03, x04, x05, x06, x08, x09, y02"     
##                           "x01, x02, x03, x04, x05, x07, x08, x09, y02"     
##                           "x01, x02, x03, x04, x06, x07, x08, x09, y02"     
##                           "x01, x02, x03, x05, x06, x07, x08, x09, y02"     
##                           "x01, x02, x04, x05, x06, x07, x08, x09, y02"     
##                           "x01, x03, x04, x05, x06, x07, x08, x09, y02"     
##                           "x02, x03, x04, x05, x06, x07, x08, x09, y02"     
##                           "x01, x02, x03, x04, x05, x06, x07, x08, x09, y02"
##                                                                                                                                                                                                                            
## Logistic_regression.model "model.coef"                                                                                                                                                                                     
##                           "0.793760021363255, 14.1386499174865"                                                                                                                                                            
##                           "1.3282427950252, 2.18222539543672"                                                                                                                                                              
##                           "1.98425891192183, -1.70156728068281"                                                                                                                                                            
##                           "1.81986510291523, -0.616403923031077"                                                                                                                                                           
##                           "1.97525587015576, -0.847067761783654"                                                                                                                                                           
##                           "1.97713269649061, -0.978656014456487"                                                                                                                                                           
##                           "2.14139855609569, -1.7249330740534"                                                                                                                                                             
##                           "1.52344929611214, 0.94427694691179"                                                                                                                                                             
##                           "1.52322632404406, 0.955432627380585"                                                                                                                                                            
##                           "0.780733375466834, 13.4575899729405, 0.47633635898569"                                                                                                                                          
##                           "1.15318480155281, 12.8678765575207, -1.08276148504988"                                                                                                                                          
##                           "-0.461223644802486, 22.5595855925815, 1.79107754927057"                                                                                                                                         
##                           "1.19879880380496, 14.3521915051357, -0.794632041737831"                                                                                                                                         
##                           "1.16378857079863, 13.7026204031736, -0.769329277333745"                                                                                                                                         
##                           "1.3558950373934, 14.5630128138227, -1.6387538590783"                                                                                                                                            
##                           "0.887092794928173, 14.9408039022322, -10.5505725302515"                                                                                                                                         
##                           "0.55687551377981, 16.6499972314604, 7.39588169112468"                                                                                                                                           
##                           "1.76422413997825, 2.3894693352685, -1.7761789553553"                                                                                                                                            
##                           "1.66228970693859, 2.33000738027516, -0.770169126452252"                                                                                                                                         
##                           "1.80411216468819, 2.58453652208278, -1.00462649401601"                                                                                                                                          
##                           "1.74577903548052, 2.01440893215386, -0.896464702097138"                                                                                                                                         
##                           "2.09533144259663, 3.44768182472149, -2.51329859392399"                                                                                                                                          
##                           "1.32945543590327, 2.37588246113202, -1.27332241126274"                                                                                                                                          
##                           "1.29203119529126, 2.27108047534478, 1.71205548975903"                                                                                                                                           
##                           "2.12180351450701, -1.63260428601775, -0.334458249326251"                                                                                                                                        
##                           "2.1110794574365, -1.48914901560225, -0.356503146624598"                                                                                                                                         
##                           "2.21680765925797, -1.47966282757382, -0.653923409795708"                                                                                                                                        
##                           "2.3547038829542, -1.45331332529101, -1.2427570591831"                                                                                                                                           
##                           "1.97240752800717, -1.69109017645362, 0.550050635344889"                                                                                                                                         
##                           "1.97070981268938, -1.69345274911002, 0.681133311734449"                                                                                                                                         
##                           "2.24086579089602, -0.593980137187467, -0.83517549879721"                                                                                                                                        
##                           "2.32601273175127, -0.706399644600488, -1.03552835244658"                                                                                                                                        
##                           "2.26998061632755, -0.402059815214422, -1.57099318297109"                                                                                                                                        
##                           "1.80168673440774, -0.605532167239042, 0.816300820780257"                                                                                                                                        
##                           "1.84225617762904, -0.763538881790056, 2.7959884920554"                                                                                                                                          
##                           "2.15059231611679, -0.578483689938937, -0.702570764216362"                                                                                                                                       
##                           "2.42247065600713, -0.690970283336678, -1.51251038372275"                                                                                                                                        
##                           "1.95889315367162, -0.846413420134504, 0.968372018616288"                                                                                                                                        
##                           "1.96833993009515, -0.840924373971275, 0.227217991872163"                                                                                                                                        
##                           "2.47410027471047, -0.875211103100199, -1.54978780265789"                                                                                                                                        
##                           "1.96670540666348, -0.967449565376285, 0.330524530243476"                                                                                                                                        
##                           "1.97038138920632, -0.972292794039073, 0.236495126724772"                                                                                                                                        
##                           "2.12525448422638, -1.70854332282598, 0.627932786885564"                                                                                                                                         
##                           "2.13188106153725, -1.7141000598604, 0.345175931054858"                                                                                                                                          
##                           "1.51026003396135, 0.87086704615792, 0.872762393699203"                                                                                                                                          
##                           "1.16060237862306, 11.3689973929965, 0.952228363711635, -1.18759186592649"                                                                                                                       
##                           "-0.960752585044564, 29.3344331165268, -1.95042068854821, 2.55862990519639"                                                                                                                      
##                           "1.2112834289834, 13.0084587702755, 0.92740461892807, -0.86792585487296"                                                                                                                         
##                           "1.14930668725959, 13.1239449017587, 0.402191844913108, -0.761496328068529"                                                                                                                      
##                           "1.4563818150696, 12.4867418660748, 1.5973968948743, -2.06366906909789"                                                                                                                          
##                           "0.866667693932141, 14.0344888153765, 0.719409374727707, -10.9530748215571"                                                                                                                      
##                           "0.542148072518359, 15.835818704738, 0.542679231732466, 7.46451296072"                                                                                                                           
##                           "-0.104213205356785, 20.9770950461999, -1.01340041137532, 1.76573943585714"                                                                                                                      
##                           "1.31965882887351, 13.3817276240651, -0.741497654623861, -0.547185408386219"                                                                                                                     
##                           "1.37032422027898, 12.7266489939035, -0.893630580799544, -0.58118893891461"                                                                                                                      
##                           "1.52464405551392, 13.5268264313612, -0.784649517932691, -1.3648011787624"                                                                                                                       
##                           "1.25981521948379, 14.0302678313813, -1.16449260546973, -10.8990282446782"                                                                                                                       
##                           "0.904639508162254, 15.13033732452, -0.93146840245892, 6.14408415032917"                                                                                                                         
##                           "-0.0320824093306765, 22.0107632423342, 1.69281363463245, -0.695041482213034"                                                                                                                    
##                           "-0.106079213935078, 21.3560297282967, 1.6197902582771, -0.479912041996164"                                                                                                                      
##                           "0.00463348764123553, 25.5923443740115, 2.30489764087605, -2.39875494383637"                                                                                                                     
##                           "-0.543531111638566, 25.406546077377, 2.13559266610517, -17.2162522480033"                                                                                                                       
##                           "-0.444830529597483, 23.0319688172533, 1.57774516562239, 3.81362314262031"                                                                                                                       
##                           "1.32518876185489, 13.9729331938144, -0.608273654617123, -0.456150666355003"                                                                                                                     
##                           "1.58276020633149, 14.6503031158805, -0.608969353360525, -1.39292949054314"                                                                                                                      
##                           "1.30946147554139, 15.4186859483607, -0.833197748358152, -11.0506899824024"                                                                                                                      
##                           "0.934861580705872, 16.236530924828, -0.639982653687453, 5.79499117914513"                                                                                                                       
##                           "1.60847918771416, 14.1178832072419, -0.628060097117412, -1.48698068237319"                                                                                                                      
##                           "1.33595339921372, 15.0693358708787, -0.943976267739782, -12.3753404649637"                                                                                                                      
##                           "0.886322603731095, 15.7199497424248, -0.57196104069499, 5.69490680602726"                                                                                                                       
##                           "1.52024515035997, 15.9347196330665, -1.83885332874203, -12.4015098906707"                                                                                                                       
##                           "1.11549008656095, 16.322317941998, -1.43467223617075, 5.17156482896424"                                                                                                                         
##                           "0.32508362760301, 25.0379187495878, -22.6142715319151, 17.2259319347988"                                                                                                                        
##                           "1.97931881520122, 2.51764543350535, -1.68977550264484, -0.536958989961958"                                                                                                                      
##                           "1.94182475954788, 2.58180033516873, -1.468017250412, -0.541937070028526"                                                                                                                        
##                           "1.96007713660736, 2.27071543349658, -1.59707430354881, -0.518677935841596"                                                                                                                      
##                           "2.30135922733032, 3.36252976431771, -1.43311675871634, -2.03359359285918"                                                                                                                       
##                           "1.77501510586016, 2.68332345524473, -1.8093994503628, -2.01949123306904"                                                                                                                        
##                           "1.72885836154471, 2.45975779025595, -1.76138529070902, 1.47685148560184"                                                                                                                        
##                           "2.1923486172873, 2.83083171119089, -0.842653208327878, -1.05528146378402"                                                                                                                       
##                           "2.15468682269107, 2.19407339902476, -0.864413027978899, -0.969227061263004"                                                                                                                     
##                           "2.28933092832841, 3.51072311730451, -0.574159405942852, -2.34456777443725"                                                                                                                      
##                           "1.67233137980057, 2.51647575878743, -0.780289401508807, -1.58449946546682"                                                                                                                      
##                           "1.69366826660928, 2.70657095993571, -1.12370698906283, 6.08168185843056"                                                                                                                        
##                           "1.93332267525608, 2.40006106626276, -0.809764010499924, -0.474408076795921"                                                                                                                     
##                           "2.46889253909646, 3.86058084716543, -0.893857719017859, -2.37836705803746"                                                                                                                      
##                           "1.81378551399147, 2.8326248968538, -1.01908490749456, -1.69726192040466"                                                                                                                        
##                           "1.77687693355734, 2.61773242428185, -0.984268338743708, 0.830361183932313"                                                                                                                      
##                           "2.35644474591921, 3.19152003315024, -0.689807588874603, -2.30685874809718"                                                                                                                      
##                           "1.76149783838702, 2.27720361201446, -0.926854642472672, -1.77028122010455"                                                                                                                      
##                           "1.715739371258, 2.06163555735493, -0.870776408095898, 0.854190770976218"                                                                                                                        
##                           "2.16018570943407, 4.00300830271592, -2.70196992146349, -3.268415311572"                                                                                                                         
##                           "2.06739573931406, 3.49109336020002, -2.48958467765603, 0.941855296051048"                                                                                                                       
##                           "1.28621054897227, 2.5429727368908, -1.57796385869902, 1.99960297691638"                                                                                                                         
##                           "2.26750998303767, -1.39943307929281, -0.359813492221297, -0.381140895549294"                                                                                                                    
##                           "2.41940307266338, -1.36858456382971, -0.437401428662935, -0.716736490858174"                                                                                                                    
##                           "2.41502025974822, -1.42159054442209, -0.19619938795882, -1.18006313418197"                                                                                                                      
##                           "2.10873885076424, -1.62471023760774, -0.328038765586592, 0.4856178256563"                                                                                                                       
##                           "2.1277782837281, -1.59626810327851, -0.42271526129208, 1.53542693549843"                                                                                                                        
##                           "2.25043903790895, -1.40844781262494, -0.152389291192933, -0.596243655052347"                                                                                                                    
##                           "2.4402139437159, -1.28857395483715, -0.283355757316348, -1.19748463199587"                                                                                                                      
##                           "2.09972493395212, -1.47463119939607, -0.361392910327998, 0.605999454283979"                                                                                                                     
##                           "2.09888031912843, -1.49137138144323, -0.344545087598547, 0.396252121126077"                                                                                                                     
##                           "2.54581256953613, -1.26093458398403, -0.602928888593838, -1.16532138772193"                                                                                                                     
##                           "2.21043994030644, -1.47820536819947, -0.647535526920694, 0.190399652018195"                                                                                                                     
##                           "2.21084689337265, -1.47902106478422, -0.648355420720762, 0.196957324330513"                                                                                                                     
##                           "2.34386053007849, -1.44730279943424, -1.23477100532033, 0.387818848627685"                                                                                                                      
##                           "2.34737056922332, -1.45156232857881, -1.23502184495948, 0.246314097365686"                                                                                                                      
##                           "1.96069224344954, -1.68427988661441, 0.506502886292427, 0.634085562284435"                                                                                                                      
##                           "2.47206543532788, -0.673206771319826, -0.538202345661714, -0.777284233459201"                                                                                                                   
##                           "2.54214611915355, -0.388020858885749, -0.686625159637868, -1.35731606166159"                                                                                                                    
##                           "2.22123409229112, -0.581899095176925, -0.834570172837599, 0.842571129350724"                                                                                                                    
##                           "2.22978303345488, -0.6736330657363, -0.792286560074608, 1.55407970087444"                                                                                                                       
##                           "2.65433151938731, -0.499518857163371, -0.924793689565114, -1.35117625124996"                                                                                                                    
##                           "2.32231066685632, -0.704756419634221, -1.0323359734677, 0.0935056251863684"                                                                                                                     
##                           "2.3187466531264, -0.782885237580996, -0.99789106738847, 1.5479816766768"                                                                                                                        
##                           "2.25373305035922, -0.395161428533517, -1.55970187516093, 0.557763506982818"                                                                                                                     
##                           "2.25973994955238, -0.469803042952551, -1.50747129128174, 1.13395850254692"                                                                                                                      
##                           "1.82789574783446, -0.747930584227948, 0.564852508417114, 2.66623908051695"                                                                                                                      
##                           "2.58857673988478, -0.466069196278684, -0.666624629729378, -1.46063526248217"                                                                                                                    
##                           "2.13658804559227, -0.586631324019207, -0.680989599973101, 0.518558187945255"                                                                                                                    
##                           "2.15241276088817, -0.57957169274294, -0.703437411598123, -0.0529862927346619"                                                                                                                   
##                           "2.40546966572468, -0.692743758660602, -1.49424287945234, 0.691879658375762"                                                                                                                     
##                           "2.42752912524074, -0.694408467590233, -1.51552061584098, -0.13477859180929"                                                                                                                     
##                           "1.95494410988621, -0.842761147437994, 0.956890164766729, 0.136260387860781"                                                                                                                     
##                           "2.46945247472884, -0.871017663882981, -1.54745316914415, 0.11684458462505"                                                                                                                      
##                           "2.4791373588713, -0.878749153581329, -1.55317328238539, -0.136220371857181"                                                                                                                     
##                           "1.9608680187104, -0.961978764041284, 0.318905976800571, 0.216635559557046"                                                                                                                      
##                           "2.11758052881192, -1.69972651788211, 0.606387446027634, 0.297494209825298"                                                                                                                      
##                           "-0.496814690916894, 25.7545435643709, -1.32263037809864, -0.862763718324139, 2.29665241393326"                                                                                                  
##                           "1.34987902976203, 11.6121514374429, 1.13805994979035, -0.831752143377635, -0.609652192788838"                                                                                                   
##                           "1.36129974744782, 11.4302600238463, 0.822571086041684, -0.998135896700802, -0.538546527837417"                                                                                                  
##                           "1.66266921486769, 10.9431341862791, 1.82546112834359, -0.89830922505486, -1.80812162293903"                                                                                                     
##                           "1.25533957254746, 12.4527294118354, 1.23048390056913, -1.29470804912117, -11.1942228024865"                                                                                                     
##                           "0.917040946768701, 13.5173421671551, 0.959392960497034, -1.03883472436758, 6.11757299344578"                                                                                                    
##                           "-0.466108220263317, 26.8902114916089, -1.38316256875115, 2.26520264599561, -0.572555116457858"                                                                                                  
##                           "-0.63323442073518, 27.8363122822856, -1.80341782715347, 2.36512186686994, -0.395373324901953"                                                                                                   
##                           "-0.263872314839382, 28.6778282992978, -0.976017448615789, 2.64822550102052, -2.25390390612355"                                                                                                  
##                           "-0.980509115625666, 31.4531288899624, -1.73233841235762, 2.79580958633659, -16.9243800318067"                                                                                                   
##                           "-0.899868809418681, 29.044389101356, -1.73129340223998, 2.32432100992548, 2.65408119981379"                                                                                                     
##                           "1.31775558218681, 12.8699832629327, 0.793607448740909, -0.698751690929875, -0.389023022937527"                                                                                                  
##                           "1.76016246917221, 12.0078297769915, 1.96450438067165, -0.725612383336807, -1.886935562862"                                                                                                      
##                           "1.32427658758937, 13.9347528727322, 1.21010112452906, -0.933823207905913, -11.6904756938038"                                                                                                    
##                           "0.954173049640532, 14.8155988014458, 0.910237391603474, -0.715370067478068, 5.72665293541195"                                                                                                   
##                           "1.67323708273699, 12.2656076041886, 1.43727998402568, -0.562658481228042, -1.88369634384935"                                                                                                    
##                           "1.30782117881137, 14.2854159954371, 0.632182325452945, -0.930654532966341, -12.4667558314982"                                                                                                   
##                           "0.865853957952876, 15.0325588918773, 0.473555102688031, -0.558963683315931, 5.79677966254439"                                                                                                   
##                           "1.64909386021049, 13.8668182476628, 2.0750365870687, -2.42866962855242, -13.4336453932683"                                                                                                      
##                           "1.23012362064676, 14.0994378528481, 1.54209460382538, -1.85534716675702, 4.85078883300114"                                                                                                      
##                           "0.300686823514165, 23.7251016858308, 0.852477494636403, -22.3039158742407, 17.0396891503058"                                                                                                    
##                           "0.0738792843775395, 21.0237666397759, -0.747590180261089, 1.71150345671799, -0.440580925722784"                                                                                                 
##                           "0.0616057157475054, 20.4356842056783, -0.934360382898407, 1.67462609309167, -0.261799404110711"                                                                                                 
##                           "0.16434706873744, 24.2586099572763, -0.58014245208323, 2.23422978765489, -2.14495547018365"                                                                                                     
##                           "-0.178089594063961, 24.2343278046295, -1.1590572859791, 2.17384253497457, -18.4965295283805"                                                                                                    
##                           "-0.118476745633764, 21.4723643987373, -0.945082456934687, 1.61036294391889, 2.82492299359244"                                                                                                   
##                           "1.429880139435, 13.0926836233305, -0.705404197755018, -0.386602503098495, -0.421897412737492"                                                                                                   
##                           "1.63308809434529, 13.9249800421674, -0.519655243114998, -0.448495250495663, -1.2711034497798"                                                                                                   
##                           "1.43411072414057, 14.5943899045334, -0.821290031896055, -0.558621816975121, -11.168135533329"                                                                                                   
##                           "1.05978795556599, 15.2356120170184, -0.68859040095626, -0.414893091603505, 5.42109076128088"                                                                                                    
##                           "1.69256861445996, 13.3478876351099, -0.635630048304703, -0.504617928193387, -1.28692457231046"                                                                                                  
##                           "1.5353049459395, 14.3928085118304, -0.92503681872341, -0.752794950740712, -12.1126166763811"                                                                                                    
##                           "1.10117558857145, 14.6239050714559, -0.820551856671243, -0.413107370260328, 5.06881599703446"                                                                                                   
##                           "1.68625844074977, 15.1523917973022, -0.837480073222688, -1.54729827270074, -12.2549126111258"                                                                                                   
##                           "1.28747944245211, 15.2174224953804, -0.6968540273087, -1.20624780646021, 4.62810971778126"                                                                                                      
##                           "0.634349009951958, 23.8429882633948, -0.862223754248305, -21.6930514779604, 15.6853732151383"                                                                                                   
##                           "0.0317143267632491, 21.7066478194725, 1.65293761886017, -0.645371780812723, -0.124630465480186"                                                                                                 
##                           "0.218880201648684, 24.9990124504667, 2.21198974382619, -0.423226065836836, -2.19050379557461"                                                                                                   
##                           "-0.108609863036645, 25.2215124773893, 2.0595186577065, -0.742948875668057, -17.558881334038"                                                                                                    
##                           "-0.0678924549717581, 22.4232270769538, 1.55805953808306, -0.624565815729525, 2.67222611961609"                                                                                                  
##                           "0.0939296833241448, 25.1743291823348, 2.24666843636219, -0.138595348946553, -2.34035867170622"                                                                                                  
##                           "-0.0690407041905435, 24.1017450503147, 1.92421729878339, -0.655724263692532, -18.4723693344399"                                                                                                 
##                           "-0.16163213344171, 21.98869927155, 1.47933273132308, -0.390334339629479, 3.13895025893697"                                                                                                      
##                           "-0.00321693156573082, 30.8270983497613, 2.91663646117909, -3.03358728256902, -23.6556448565708"                                                                                                 
##                           "-0.0142012699588023, 25.763366867001, 2.21018872645162, -2.31099208112633, 1.44766515843179"                                                                                                    
##                           "-0.713791920378835, 31.9038887893678, 1.73767001864411, -27.8801261062484, 13.6918608860018"                                                                                                    
##                           "1.69479991174654, 14.2853569526946, -0.457404510228433, -0.40948653455335, -1.36092097911029"                                                                                                   
##                           "1.48784649825904, 15.2873109506192, -0.567893531933794, -0.652891903219071, -12.2016760618374"                                                                                                  
##                           "1.05267765484257, 15.7498248418444, -0.52539922528553, -0.319716867011101, 5.12171091781138"                                                                                                    
##                           "1.75016058945053, 16.2139430819208, -0.626434116920584, -1.58730177442973, -12.5587211408967"                                                                                                   
##                           "1.34707254032694, 16.0612541534674, -0.504827175502199, -1.26777460311178, 4.22916429890084"                                                                                                    
##                           "0.635387844859566, 24.5436839944098, -0.516959898148866, -22.0429705775427, 15.7555965644263"                                                                                                   
##                           "1.85370288298843, 16.0776803647702, -0.824787791957469, -1.6799454229307, -13.7200040408924"                                                                                                    
##                           "1.36761912612953, 15.5674450244225, -0.492051909167078, -1.36269573546688, 3.98039928297771"                                                                                                    
##                           "0.640986594237171, 24.1156690045264, -0.548763340359631, -22.04746595548, 15.2037679048917"                                                                                                     
##                           "0.88757146987841, 25.2500224076431, -1.49328511492336, -22.9737493084889, 15.1798249833019"                                                                                                     
##                           "2.22215924836958, 2.77904860139464, -1.32039697614738, -0.628720481338976, -0.628343624407918"                                                                                                  
##                           "2.24100843237709, 2.40568727330738, -1.46780193408382, -0.621599321007312, -0.603425135117499"                                                                                                  
##                           "2.43626398408666, 3.43318689319378, -1.37735127370768, -0.408935823115456, -1.93943034660303"                                                                                                   
##                           "2.00210015325435, 2.80019556467543, -1.72520468336337, -0.552840150796611, -2.25907764447548"                                                                                                   
##                           "1.98975582691528, 2.7791481530999, -1.61002178658341, -0.804346230333485, 4.23015860909815"                                                                                                     
##                           "2.02850737579904, 2.4523035450805, -1.42239793958091, -0.414453626687599, -0.339707338520041"                                                                                                   
##                           "2.46999935620851, 3.6105672122138, -1.12226623834499, -0.533874124974404, -2.03287005814148"                                                                                                    
##                           "1.95731143216277, 2.89180761843845, -1.49776132468879, -0.552386311946563, -2.14137997069392"                                                                                                   
##                           "1.90816208770693, 2.62028380545414, -1.47274994063136, -0.514485454168065, 1.02016066763529"                                                                                                    
##                           "2.41640341999712, 3.22872786855827, -1.31668414132773, -0.366247089026897, -1.94665247563801"                                                                                                   
##                           "1.98434348146397, 2.59066153252191, -1.62318396852753, -0.55168781836348, -2.22943763192608"                                                                                                    
##                           "1.92468964947947, 2.32313410091528, -1.5975032009033, -0.486882470749287, 0.964059223222423"                                                                                                    
##                           "2.37531479721071, 3.95217959255238, -1.46020813979462, -2.23069615563377, -3.49918029837679"                                                                                                    
##                           "2.27474117652168, 3.39605283365766, -1.42671159258346, -2.01174681208864, 0.829657198991654"                                                                                                    
##                           "1.72986319838431, 2.82785568642787, -1.79507686762716, -2.26350077590671, 1.88077040294598"                                                                                                     
##                           "2.35502680237026, 2.63134956381883, -0.881016232796195, -0.834049501137332, -0.53639881602692"                                                                                                  
##                           "2.69111557867367, 3.96533798310797, -0.626609867807275, -0.925699117167098, -2.17854719811382"                                                                                                  
##                           "2.21893622902898, 3.07421925245484, -0.859795663268371, -1.07600489355264, -2.13747351897512"                                                                                                   
##                           "2.17302853709072, 3.07750236882574, -1.09145287435849, -0.976390391193531, 4.42687696086535"                                                                                                    
##                           "2.60448399971419, 3.25483277478992, -0.655653574411747, -0.756133506067213, -2.09838312104149"                                                                                                  
##                           "2.19435468286276, 2.45211557018172, -0.889657434193542, -1.0130732089488, -2.2469686726436"                                                                                                     
##                           "2.12113814003416, 2.45610564950483, -1.08625479998037, -0.866319586534559, 4.02110396076145"                                                                                                    
##                           "2.35996654460048, 4.01912549594253, -0.579341753858533, -2.52819409654262, -3.41661897143743"                                                                                                   
##                           "2.26589304988353, 3.6455428106691, -0.751878773277292, -2.21388110736626, 2.79741520703316"                                                                                                     
##                           "1.71993060300525, 3.14579480996598, -1.22752679007606, -2.75228216915833, 7.4374892624241"                                                                                                      
##                           "2.53773573895898, 3.69701222813299, -0.791216161438357, -0.288092241491729, -2.3136940017233"                                                                                                   
##                           "1.95070466882861, 2.66845698296069, -0.814264636375553, -0.501631940073832, -1.87077282957051"                                                                                                  
##                           "1.91209567341142, 2.42628484317296, -0.801063657309993, -0.462685678984136, 0.549302599008139"                                                                                                  
##                           "2.55387534953191, 4.47806125750379, -0.9187638895615, -2.58669503365563, -3.67968570334827"                                                                                                     
##                           "2.45660693730054, 3.87080356844778, -0.88623297214735, -2.37205590894527, 0.311812530603733"                                                                                                    
##                           "1.77707628411043, 2.91142110893137, -0.993124420360161, -1.85233742860141, 1.12453656839794"                                                                                                    
##                           "2.44164103169866, 3.77541707298156, -0.732047999708875, -2.49835059566496, -3.47129422323328"                                                                                                   
##                           "2.33671801563054, 3.21832003116359, -0.675026775169904, -2.29949872787588, 0.482107011770449"                                                                                                   
##                           "1.72257480443701, 2.36990769789663, -0.895297233255174, -1.90821523180439, 1.10978846307218"                                                                                                    
##                           "2.12266526236167, 4.13306986202305, -2.68445491409427, -3.4784193740003, 1.39203121081612"                                                                                                      
##                           "2.45697178785824, -1.29117041265698, -0.440979898795582, -0.162781916378208, -0.655965539096714"                                                                                                
##                           "2.51028828365988, -1.24574215060634, -0.215962223913068, -0.297622833428166, -1.12432849194083"                                                                                                 
##                           "2.25428310268898, -1.38838137487304, -0.352833109996143, -0.384781532506663, 0.541427866451296"                                                                                                 
##                           "2.25944092841571, -1.38981657822154, -0.424595034615909, -0.350157212078421, 1.18384301483078"                                                                                                  
##                           "2.65413228273502, -1.19804689183812, -0.299377097502298, -0.650549948286712, -1.06582719663107"                                                                                                 
##                           "2.41730399304328, -1.36842562884718, -0.436455973053585, -0.714961712845043, 0.0502583564587489"                                                                                                
##                           "2.41378683493683, -1.35241800919827, -0.489037509816657, -0.696078550963915, 0.96349227175481"                                                                                                  
##                           "2.40384205947886, -1.41678474328078, -0.191872274390835, -1.1743278512323, 0.355334568687506"                                                                                                   
##                           "2.40835971088138, -1.41092486267884, -0.236012711727822, -1.14693541873694, 0.629659049767674"                                                                                                  
##                           "2.11740523574241, -1.59172630674874, -0.413812305922049, 0.366042111702247, 1.47302283647781"                                                                                                   
##                           "2.56947813371079, -1.20587389379917, -0.115752732333954, -0.562964269868583, -1.15611989398702"                                                                                                 
##                           "2.24341903290793, -1.40389622789582, -0.158051062572276, -0.58581513329982, 0.247099964272067"                                                                                                  
##                           "2.24620177617369, -1.40932489402525, -0.14961680223137, -0.593937039409082, 0.120169443587966"                                                                                                  
##                           "2.42932170652641, -1.27904741567455, -0.287952219988627, -1.18784453876779, 0.441224471316759"                                                                                                  
##                           "2.43813782618345, -1.28908502381137, -0.281783634792702, -1.19607881581031, 0.0541718319758256"                                                                                                 
##                           "2.08980138908329, -1.47702671513181, -0.351076558120693, 0.580867153083608, 0.337753395854923"                                                                                                  
##                           "2.54312411910626, -1.26066134191714, -0.600527948151218, -1.16416231549421, 0.0683626306993502"                                                                                                 
##                           "2.54976074706227, -1.26086156061621, -0.605744408551501, -1.16805716994113, -0.10374174219711"                                                                                                  
##                           "2.20510210969408, -1.47766574447568, -0.642561604236202, 0.181747897535525, 0.185645696326564"                                                                                                  
##                           "2.33778337181356, -1.44595854411495, -1.22824194311325, 0.374188946447417, 0.216540984894411"                                                                                                   
##                           "2.7499181917069, -0.471395656910128, -0.439225018400374, -0.725451020533945, -1.27222180225154"                                                                                                 
##                           "2.46181414103973, -0.667716305322384, -0.542707410896623, -0.765265695946786, 0.285795872918592"                                                                                                
##                           "2.45947100428075, -0.728834544280441, -0.512605268010802, -0.762962553741818, 1.12253757197358"                                                                                                 
##                           "2.52452649338241, -0.379899560542362, -0.688202961254982, -1.34472859313255, 0.626056721775495"                                                                                                 
##                           "2.5321433825085, -0.417986253637296, -0.673539564181988, -1.33324442186195, 0.509035660617772"                                                                                                  
##                           "2.21383533963607, -0.655561928179657, -0.795575792892598, 0.704031689126271, 1.41646703231928"                                                                                                  
##                           "2.65561729514748, -0.499999569609818, -0.925835910894866, -1.35152983577734, -0.0287211374845188"                                                                                               
##                           "2.64397673863997, -0.531770784641884, -0.912470474836385, -1.32294699000009, 0.57541851877606"                                                                                                  
##                           "2.31928768975372, -0.783231786659658, -0.998318065479363, -0.0135134516793203, 1.54981665013505"                                                                                                
##                           "2.24651501238885, -0.459751498005644, -1.50174288887069, 0.466809410651764, 1.07077789712053"                                                                                                   
##                           "2.57872267139339, -0.470879015736399, -0.65454866135544, -1.4541225396541, 0.276595757904044"                                                                                                   
##                           "2.60327413919392, -0.473107039081573, -0.672456635862043, -1.46804528815467, -0.353422018851842"                                                                                                
##                           "2.13957254259507, -0.588572614378089, -0.682275689311635, 0.524324074847503, -0.0912279753487385"                                                                                               
##                           "2.41237674199287, -0.69767842421255, -1.49822440595727, 0.706345549201962, -0.193399533673108"                                                                                                  
##                           "2.47448480922284, -0.874504379062149, -1.55087894549405, 0.123455554135168, -0.143061075923104"                                                                                                 
##                           "-0.282439073231909, 24.9636473429677, -1.09253315435846, -0.665855769680858, 2.16310504125644, -0.367359012843468"                                                                              
##                           "-0.338084850386188, 25.1366647691001, -1.28498938889396, -0.796329059587604, 2.20154054079815, -0.234246525030625"                                                                              
##                           "-0.0323827621889487, 26.4494460830891, -0.653166407108159, -0.522817700616722, 2.47302438557631, -2.07131660691535"                                                                             
##                           "-0.447136333385819, 27.5845840693015, -0.974167766669595, -1.05060016886613, 2.54221216376079, -18.5396630662316"                                                                               
##                           "-0.464288921791094, 25.6429829301263, -1.15890206059045, -0.823921378201983, 2.10710426364465, 2.25402681940065"                                                                                
##                           "1.43227500907661, 11.5736162502573, 1.01453235687219, -0.792981476831307, -0.478962305653805, -0.327037051788991"                                                                               
##                           "1.81857954225247, 11.0905766450345, 2.02263906191587, -0.585734835221309, -0.549181269802907, -1.75635652856831"                                                                                
##                           "1.45960991631512, 12.7978098558663, 1.43522948108119, -0.926270925375026, -0.644799925902553, -11.6001275045505"                                                                                
##                           "1.09989501335515, 13.3751525832261, 1.10831812533521, -0.777115622349346, -0.480507020465799, 5.2768449819961"                                                                                  
##                           "1.78114335769903, 10.9948929869596, 1.67793991964567, -0.773460034994541, -0.390105065001805, -1.70981994471728"                                                                                
##                           "1.50833705441146, 12.9876620937511, 1.06013707417761, -1.0533471465339, -0.695458005568959, -12.1159309635844"                                                                                  
##                           "1.08825549063677, 13.2259746536906, 0.869301026258018, -0.931170758625799, -0.362706634271164, 5.18019554617058"                                                                                
##                           "1.84413042818891, 12.7280549392349, 2.30117367027549, -0.964985000262496, -2.1487200908594, -13.0629639666257"                                                                                  
##                           "1.44556612018723, 12.4605087073818, 1.75717472395958, -0.813126963407808, -1.64589168655539, 4.16858299930491"                                                                                  
##                           "0.644454927153853, 21.7196738855363, 1.23762056043759, -0.992182927877585, -21.051545224256, 15.1828639831654"                                                                                  
##                           "-0.402621078702754, 26.5926528341576, -1.3851235072434, 2.22607200941685, -0.520333408506819, -0.127799513863475"                                                                               
##                           "0.0286596427185521, 26.9470125013438, -0.596585669261602, 2.4337964268546, -0.37537199942247, -2.12179268902512"                                                                                
##                           "-0.457552462814058, 29.2411949408378, -1.14086249549356, 2.5158511873395, -0.64552993495674, -17.4385615113966"                                                                                 
##                           "-0.458324163384131, 26.8291703980821, -1.25049060246939, 2.10334611826894, -0.524459129450733, 2.11070226432365"                                                                                
##                           "-0.183815559074707, 28.2574605287497, -0.952116797443303, 2.59222460131656, -0.114834337305949, -2.2087276728063"                                                                               
##                           "-0.492298427326345, 29.3630846828286, -1.49328924659284, 2.51857167741381, -0.581183106166229, -18.3791893052985"                                                                               
##                           "-0.631706323113361, 27.8253232746696, -1.64009712313917, 2.19636072078662, -0.336742769072498, 2.26759805787767"                                                                                
##                           "-0.124268472419613, 32.2113418227772, -0.456024348587247, 3.06626489468206, -2.95886614750442, -23.5529983441099"                                                                               
##                           "-0.25833238735998, 28.5934410803778, -0.891527709037875, 2.53608602575696, -2.19034535560102, 1.26437211546402"                                                                                 
##                           "-0.886277857175243, 34.3647318964325, -0.873824018763459, 2.06930906924039, -28.7185028065286, 13.113992402202"                                                                                 
##                           "1.81452702359285, 11.9420083764084, 1.84848958283002, -0.63496848332049, -0.232973264059639, -1.83759209959143"                                                                                 
##                           "1.47250059227853, 14.0713420827685, 1.01558890781661, -0.688366169226237, -0.564218761777236, -12.3650828887899"                                                                                
##                           "1.04334618797925, 14.5698687911759, 0.825734922285508, -0.620199400668845, -0.246406923097463, 5.21345516535168"                                                                                
##                           "1.97078210612793, 13.7304242489582, 2.4811804922949, -0.781448387056852, -2.25077335070564, -13.7752941214837"                                                                                  
##                           "1.54971852590848, 13.2535895927107, 1.87399395271085, -0.628689797430565, -1.75301784023263, 3.62791743551435"                                                                                  
##                           "0.663923886043324, 22.6313100909943, 1.17930944530505, -0.622341425550791, -21.5216346000484, 15.2288569627364"                                                                                 
##                           "1.93119656205857, 14.1856408958013, 1.86321591516825, -0.743043298529458, -2.22056350143969, -14.2184988532875"                                                                                 
##                           "1.43951879010426, 13.608535381933, 1.43234753860536, -0.427815347397435, -1.76068992289765, 3.87688342207073"                                                                                   
##                           "0.602705157549412, 22.9654445096427, 0.767912111922159, -0.520106246542779, -21.7756199690173, 15.1496384379479"                                                                                
##                           "1.03951124146734, 22.4367809597816, 1.99516852111615, -2.06549913937419, -22.5137021166857, 14.2125893165464"                                                                                   
##                           "0.113547862554525, 20.8430286819093, -0.741485296931573, 1.68626237736468, -0.410749151516493, -0.0801607561493908"                                                                             
##                           "0.265553611773495, 24.2243101709232, -0.412875761202037, 2.18987026675695, -0.291760142440839, -2.07182048760925"                                                                               
##                           "-0.00686525078876945, 24.3739079215876, -0.899688798337362, 2.12257495765117, -0.43330075302587, -18.4200884058678"                                                                             
##                           "0.0365951612898077, 21.4462574510016, -0.722759903607609, 1.58969142430904, -0.383615239202145, 2.38789055133589"                                                                               
##                           "0.174320209052307, 24.2155612893172, -0.575790396890863, 2.22738116078081, -0.0173891842222862, -2.13931995996596"                                                                              
##                           "0.0747642331654179, 23.6583306469606, -1.034250489896, 2.03992538244063, -0.419886461605573, -18.8996294811516"                                                                                 
##                           "0.00441624112029298, 21.0353316864627, -0.89123470575938, 1.55749255758818, -0.194342459029635, 2.58129033203819"                                                                               
##                           "0.155538442007138, 29.6318031448982, -0.675266653600955, 2.87046952631251, -2.7502516784042, -23.6886809422861"                                                                                 
##                           "0.141159547105744, 24.4653489143244, -0.555920043839149, 2.154872597728, -2.07875314410473, 1.25739230148881"                                                                                   
##                           "-0.435170264034814, 30.7741462074344, -0.947468079161716, 1.84264864595187, -27.0520724433131, 11.8146564156195"                                                                                
##                           "1.73195474462283, 13.6431208003794, -0.484162304656539, -0.317425335361973, -0.382866211590435, -1.24490219004563"                                                                              
##                           "1.58921923605011, 14.615864117584, -0.771021039853612, -0.322390128109651, -0.619537839378893, -12.0821165527683"                                                                               
##                           "1.16268011628886, 14.8342840823399, -0.668402265072886, -0.317316046829529, -0.29097045321316, 4.83090494431314"                                                                                
##                           "1.79711040663147, 15.5794824367377, -0.577463817463626, -0.446770789643597, -1.45297940488023, -12.4312015081411"                                                                               
##                           "1.39863053046343, 15.3411476086653, -0.496917030774851, -0.352271840694386, -1.15192542100411, 4.12794980460287"                                                                                
##                           "0.745968196609059, 23.7769600765064, -0.700819261050193, -0.281055292152038, -21.5392357760267, 15.1459132104181"                                                                               
##                           "1.92043819044725, 15.4947368935919, -0.627825782598451, -0.699718170046149, -1.4735431820319, -13.3379605143072"                                                                                
##                           "1.45558771756579, 14.7827279568042, -0.597253140220196, -0.38033715448376, -1.17808482614834, 3.79655169882475"                                                                                 
##                           "0.827691037094502, 23.3034785525017, -0.756888053332869, -0.400736734743607, -21.3806565874804, 14.3681694365164"                                                                               
##                           "1.02882917315219, 24.3621382784745, -0.612672254872611, -1.28243152428029, -22.2588889974275, 14.3587423939156"                                                                                 
##                           "0.174572485914352, 25.24527650467, 2.24433745683938, -0.453662284840301, 0.0901296867817627, -2.21226142141934"                                                                                 
##                           "0.0510061553160957, 24.5483244589079, 1.96525399517875, -0.615768329644697, -0.316034740237635, -18.117040968661"                                                                               
##                           "-0.0279643103589774, 22.2273769248349, 1.5372392570504, -0.595269757197959, -0.0772978569332381, 2.60401494481747"                                                                              
##                           "0.193409679582717, 30.4594637534202, 2.83381457481694, -0.420447356072867, -2.82192963771552, -23.3888114380132"                                                                                
##                           "0.189131826986931, 25.1756756400525, 2.14595335193872, -0.39408395674803, -2.13737568215993, 1.12620877503642"                                                                                  
##                           "-0.405159178335095, 31.2116752284524, 1.73914785825435, -0.511683243326599, -27.0813511751879, 12.2488442649358"                                                                                
##                           "0.204879518987451, 30.0677623461749, 2.79293987666139, -0.331383763793957, -2.91366575407566, -24.04035279807"                                                                                  
##                           "0.0529458272669886, 25.445631080863, 2.17120228322711, -0.103086360112864, -2.27162156182612, 1.3879768262995"                                                                                  
##                           "-0.452534452166219, 30.9330147619789, 1.66926883376824, -0.38039005935868, -27.2616508959611, 12.4508592854405"                                                                                 
##                           "-0.227393656926248, 35.8916934635516, 2.65977863091857, -2.7773529606313, -31.2397091852574, 8.64554823771933"                                                                                  
##                           "1.92571342737999, 16.1702243998987, -0.389070273439422, -0.638016233947108, -1.56717708094049, -13.5493666776259"                                                                               
##                           "1.4606975570404, 15.6061492271325, -0.400541136477711, -0.310512977675004, -1.26048346670946, 3.67593893375636"                                                                                 
##                           "0.767219453304138, 24.0120831508652, -0.384438575609156, -0.364521421314382, -21.776381051939, 14.7497038725825"                                                                                
##                           "1.06161423438902, 24.9366688885351, -0.36925908821318, -1.36942337817542, -22.586875299671, 14.3001430132908"                                                                                   
##                           "1.14850926221374, 24.4767564665727, -0.489290238408113, -1.44171133233795, -22.5215227450317, 13.4974147846769"                                                                                 
##                           "2.34005333114628, 2.63501017230555, -1.25848841675748, -0.664697470468347, -0.480591226528166, -0.401680385607174"                                                                              
##                           "2.64851264607223, 3.72711308960388, -1.02524539090328, -0.487256793532801, -0.593294851324768, -1.91390371986924"                                                                               
##                           "2.25447263462419, 3.08000818370849, -1.35063557522811, -0.648361836036052, -0.643831475092207, -2.46049515556799"                                                                               
##                           "2.20658291013847, 2.98217503654392, -1.28520508187427, -0.846023488645455, -0.570275002076908, 3.61401958657573"                                                                                
##                           "2.59677851250165, 3.2869520255411, -1.22898113643091, -0.473970594278528, -0.439930865466133, -1.82237644560547"                                                                                
##                           "2.28863411493196, 2.71853249040173, -1.49266219657701, -0.650147643431033, -0.647972353044112, -2.57367317431987"                                                                               
##                           "2.21153448708607, 2.62032277815475, -1.43372138553885, -0.813767900857961, -0.521569849499996, 3.23569847711204"                                                                                
##                           "2.51743555231049, 3.99224722841539, -1.4062986043358, -0.41848384532443, -2.1311477764502, -3.63339280966531"                                                                                   
##                           "2.41564438730364, 3.53598057584788, -1.34505814717983, -0.550845972991187, -1.84590514934233, 2.10338267050802"                                                                                 
##                           "2.02671739755634, 3.2719339099823, -1.6354102185604, -0.918071945967054, -3.08342354311906, 5.59931634223773"                                                                                   
##                           "2.50861991123747, 3.51839287052458, -1.09856646207936, -0.478157844736932, -0.173976167410059, -1.99588272263258"                                                                               
##                           "2.05243639723804, 2.7697101676339, -1.45010226175235, -0.412801765531873, -0.371792577413562, -2.24775907512592"                                                                                
##                           "1.99715327243498, 2.48966565733642, -1.42835459179791, -0.40054357786537, -0.319966218899055, 0.794862200300541"                                                                                
##                           "2.55503854593608, 4.23400365469894, -1.13926092901116, -0.553858630280148, -2.2402777352107, -3.6834490436369"                                                                                  
##                           "2.45127416722942, 3.62350365420674, -1.12570772890128, -0.521240323803678, -2.02142213158596, 0.464514911410602"                                                                                
##                           "1.91116467829418, 2.98580987681945, -1.50684170862243, -0.515838647008879, -2.31679489412539, 1.40676054906891"                                                                                 
##                           "2.50413903873306, 3.82515804834289, -1.33253750924669, -0.404394145831987, -2.14103918732444, -3.58026497528355"                                                                                
##                           "2.39201745172883, 3.25883547423376, -1.31792192336758, -0.347327059507864, -1.93643329804781, 0.577628118577213"                                                                                
##                           "1.93642163513204, 2.69731093933493, -1.62599768751951, -0.509746458388717, -2.37789171141187, 1.30880843722042"                                                                                 
##                           "2.33637494727937, 4.06052606987626, -1.45085431832531, -2.21074494820888, -3.66953105596629, 1.30746017073829"                                                                                  
##                           "2.78384395801427, 3.77399085127461, -0.656026737601888, -0.800072987248041, -0.347530430823387, -2.09376200480141"                                                                              
##                           "2.39819303910454, 2.89587015336083, -0.906993415448443, -0.840780098979549, -0.576596727637457, -2.38700044762456"                                                                              
##                           "2.31532154953909, 2.85802173220953, -1.07880401486297, -0.797670572280196, -0.463275638337384, 3.62440394199195"                                                                                
##                           "2.78618878624339, 4.53877378732959, -0.636532218005667, -0.953988913545654, -2.38017346105306, -3.88926899339753"                                                                               
##                           "2.65981056582606, 4.03793460977993, -0.741284879601696, -0.890103499286471, -2.0988912877968, 1.85250664309579"                                                                                 
##                           "2.20975769509308, 3.53709189547216, -1.20340949704062, -0.987274672969614, -3.03008592571332, 5.8605652920385"                                                                                  
##                           "2.70742841287401, 3.79469435590892, -0.675408980872008, -0.809217810880416, -2.28235170808785, -3.7135533763148"                                                                                
##                           "2.56707452319501, 3.36726506694735, -0.774902205233494, -0.70666508870396, -2.02399393290225, 1.9780201386264"                                                                                  
##                           "2.16445687438595, 2.89983027563264, -1.19265847789074, -0.893063912918967, -2.96132180041248, 5.27125889209976"                                                                                 
##                           "2.34183479863521, 4.33902115373731, -0.838753558779072, -2.38182311504657, -4.00162380150545, 3.98498742276378"                                                                                 
##                           "2.63289248058506, 4.31143726027367, -0.804530620206897, -0.324370679342163, -2.51899822108316, -3.71855174020184"                                                                               
##                           "2.52942166583345, 3.70536168316984, -0.788077771098423, -0.284062620110883, -2.31083656534796, 0.186720725552479"                                                                               
##                           "1.91923593268564, 2.73062688450365, -0.80190869642393, -0.485033946697664, -1.97254045187499, 0.819430089238319"                                                                                
##                           "2.52586834784224, 4.53853780721312, -0.900843056064385, -2.58036005959694, -3.78430641288995, 0.76817655202293"                                                                                 
##                           "2.40750632207866, 3.86284991918612, -0.70497635467944, -2.49482436110548, -3.58734639978929, 0.880214197373443"                                                                                 
##                           "2.67905245049773, -1.14061689293836, -0.301337318621665, -0.120372731327821, -0.609112240233134, -1.05465299838578"                                                                             
##                           "2.45291086086753, -1.2896990795352, -0.438947938297583, -0.16514010305578, -0.65119793001673, 0.110044432213305"                                                                                
##                           "2.44746663875525, -1.28522027927945, -0.487111979591884, -0.144083952831539, -0.644458111853589, 0.874491028100255"                                                                             
##                           "2.49872289979545, -1.23793414812683, -0.211209219092899, -0.30144861085062, -1.11725905159761, 0.408762725034255"                                                                               
##                           "2.50229420007643, -1.24469767632698, -0.241973395268266, -0.287038142269346, -1.10406070144936, 0.427224084081143"                                                                              
##                           "2.24883051219414, -1.38122925411854, -0.414187372214622, -0.355240387360343, 0.445799143971493, 1.10712435369504"                                                                               
##                           "2.65500140657334, -1.19805562803888, -0.299727023798938, -0.651261985917305, -1.06602469815699, -0.0191500134398968"                                                                            
##                           "2.64850193128751, -1.19440622691772, -0.318634479567653, -0.644563644238058, -1.05043795658329, 0.325207357450739"                                                                              
##                           "2.41427622809211, -1.35243010595473, -0.489343929325637, -0.696468066843019, -0.011734679399436, 0.964893465121915"                                                                             
##                           "2.39890838202318, -1.40734503733472, -0.229633093534645, -1.14400259781714, 0.311015338022456, 0.591643297641124"                                                                               
##                           "2.56564536015608, -1.20420967334541, -0.118320701288261, -0.558190827046995, -1.15402791560041, 0.110682454039921"                                                                              
##                           "2.57636130620611, -1.2040032371805, -0.119493414596025, -0.566011556985879, -1.16005943662842, -0.16115232975268"                                                                               
##                           "2.23998387250336, -1.40473773692925, -0.15558189123951, -0.584084056959752, 0.241364551008121, 0.101966794298751"                                                                               
##                           "2.42876360972398, -1.27921082556433, -0.287501920699533, -1.18747080488282, 0.440168873590998, 0.0152411129424537"                                                                              
##                           "2.54704965338233, -1.26056701049985, -0.603296714079884, -1.16692913663452, 0.0729644104541823, -0.107846888528733"                                                                             
##                           "2.74437298563088, -0.468928197354263, -0.441630023132393, -0.719450624513796, -1.27015136616006, 0.134864095878923"                                                                             
##                           "2.74301280225851, -0.488480861575811, -0.432490715472356, -0.722231085639216, -1.25863457459011, 0.300345453734115"                                                                             
##                           "2.45256324449743, -0.723390762113049, -0.516445604172806, -0.75488490142446, 0.197800082594472, 1.09357373817366"                                                                               
##                           "2.51702743657967, -0.405520213699678, -0.677051940559345, -1.32517451486013, 0.58812699830937, 0.431448587083655"                                                                               
##                           "2.64682049576183, -0.533263869647698, -0.914731102037544, -1.32342626404808, -0.0656372940561941, 0.581777303141525"                                                                            
##                           "2.59351052092487, -0.47860680058335, -0.659873901866063, -1.4614796945445, 0.297238247729301, -0.372855261867697"                                                                               
##                           "-0.239823811986836, 24.7828517009787, -1.09883611382891, -0.65901438804385, 2.13737499789988, -0.332181394337003, -0.0913317883357462"                                                          
##                           "0.112170363066012, 25.8054138344233, -0.470739175057786, -0.389502700323652, 2.36718088980806, -0.260554404955792, -2.02495913410243"                                                           
##                           "-0.239045226889314, 26.9994975670154, -0.762829415699799, -0.842720845507707, 2.42078777621736, -0.385209606067531, -18.484787651094"                                                           
##                           "-0.278093737172298, 24.9403831340316, -0.971069154907133, -0.653867274240601, 2.01032426991633, -0.325020713650984, 2.00494061150276"                                                           
##                           "-0.0242271736190147, 26.4129456856598, -0.65252352704995, -0.519396214968443, 2.46734657161816, -0.0139264446807122, -2.06687177524285"                                                         
##                           "-0.169407364247479, 26.6365198422566, -0.901979611736926, -0.943775785780333, 2.38200131378754, -0.40601381591241, -19.2533717829806"                                                           
##                           "-0.344268853264698, 25.1807072605979, -1.14465095295783, -0.774162360211002, 2.04980418512947, -0.183295599660431, 2.07179396549474"                                                            
##                           "0.133987712488845, 29.8697218831965, -0.0803963691750088, -0.668902776306681, 2.89707831523938, -2.73979466500817, -23.7041669869526"                                                           
##                           "-0.0341369121482722, 26.4335369398455, -0.585590625698636, -0.506022374694297, 2.3757728080439, -2.01894210764716, 1.1528863078905"                                                             
##                           "-0.535597176336513, 32.0914329675475, -0.454828311578465, -0.910264312655807, 2.01301236437603, -27.5760059305463, 11.5999435257323"                                                            
##                           "1.8581253622746, 11.0802476251384, 1.92734361130394, -0.563674192220016, -0.48363796276145, -0.184982447074538, -1.71929168570645"                                                              
##                           "1.57818644363408, 13.048988847028, 1.24280987223318, -0.867700425792087, -0.444570319655363, -0.497661892324934, -12.1224616982502"                                                             
##                           "1.16459910431821, 13.231330763698, 1.03746886806953, -0.757681236662527, -0.412737295734157, -0.190367356280053, 4.90122995667948"                                                              
##                           "2.01547479710753, 12.9979696620255, 2.52642525757697, -0.637784410687866, -0.585701071519327, -2.10162682063219, -13.4304814492031"                                                             
##                           "1.6117915895377, 12.3277943983415, 1.9332724984821, -0.562757653115879, -0.461204728521877, -1.63025081842268, 3.49037247593213"                                                                
##                           "0.794712531639088, 21.4205527629868, 1.36162690555121, -0.79308996478443, -0.373997923035657, -20.800552133401, 14.4252721401331"                                                               
##                           "2.01343300416789, 13.1987552910328, 2.08817156020686, -0.782113111015567, -0.560867144118488, -2.02946543794097, -13.6854825463666"                                                             
##                           "1.55410876948258, 12.3085839368071, 1.66437758684802, -0.73583795695305, -0.268906381781523, -1.60041564858004, 3.63980433202821"                                                               
##                           "0.799515264795441, 21.4298219132319, 1.14555749361139, -0.897116180281481, -0.322830169009619, -20.8304780211167, 14.1639261181053"                                                             
##                           "1.22075979065729, 21.0404086885375, 2.16569968157499, -0.734941808000752, -1.85679643362638, -21.5165190194928, 13.0989445842021"                                                               
##                           "-0.00672688855003753, 27.1274412839539, -0.586021441167131, 2.45812562172983, -0.403489483910417, 0.0796742529654528, -2.14209201060039"                                                        
##                           "-0.292643718412133, 28.4796088644113, -1.12426880365548, 2.41543194057879, -0.519814527214078, -0.31165320174933, -18.0954835222176"                                                            
##                           "-0.413853522694839, 26.6256500448548, -1.25720741358623, 2.08113491772463, -0.488137321680025, -0.0917585730986444, 2.04651544388529"                                                           
##                           "0.16913230260985, 30.706962335494, -0.0806306713906334, 2.86169092258766, -0.414297655408271, -2.81144537837894, -23.3838576481301"                                                             
##                           "0.0152670509951498, 26.9747255316079, -0.549280151473621, 2.35461876740246, -0.351465571125965, -2.07837521845181, 1.05109883248579"                                                            
##                           "-0.540586985067934, 32.8091699294721, -0.547113035534252, 1.9517947197452, -0.469660347032705, -27.6615400626592, 11.9834220182503"                                                             
##                           "0.101249183474958, 31.202564617878, -0.383437601021852, 2.92123696844903, -0.3222010879346, -2.8545995237407, -24.0458432564091"                                                                
##                           "-0.198984017524106, 28.2838943541526, -0.877005509966956, 2.49868943106768, -0.0855072186021423, -2.15922352557412, 1.2201634069004"                                                            
##                           "-0.625016724549197, 33.3976342540231, -0.873429445045576, 2.00135826019159, -0.379881886139454, -28.1665660319472, 11.8908634761019"                                                            
##                           "-0.239292367119925, 36.029425000459, -0.0558124383742105, 2.67817832389286, -2.76988849819506, -31.2748597978526, 8.6178999729035"                                                              
##                           "2.06542075168978, 13.9059226162491, 2.26769787913912, -0.616132412829158, -0.421236083074113, -2.17470839534996, -14.1187125467864"                                                             
##                           "1.59469180713479, 13.1430641801131, 1.80821340333492, -0.577580308327898, -0.142887669734633, -1.73112724059097, 3.41170568380457"                                                              
##                           "0.75359078727982, 22.3996971089163, 1.08133091752681, -0.521936582722776, -0.252619271394174, -21.352220150184, 14.566285879184"                                                                
##                           "1.31490530981143, 21.5745775406802, 2.27496284941685, -0.528325299755825, -1.97948824509847, -21.9068142731731, 12.8104388116826"                                                               
##                           "1.24025370732527, 21.9624471377493, 1.8818259437607, -0.393923419588195, -1.98991385230737, -22.1561381514062, 12.9347593987749"                                                                
##                           "0.210702935887486, 24.5182502305653, -0.420859396462165, 2.23049054805184, -0.327857539703627, 0.114689454151742, -2.09836068656159"                                                            
##                           "0.120511877500085, 23.9184647569408, -0.881747392748899, 2.04448264855143, -0.328201436218553, -0.27438034230532, -18.7288197566456"                                                            
##                           "0.0550898370268839, 21.3591838269706, -0.720164548149724, 1.57975991800927, -0.370486902127847, -0.0369378635527383, 2.35866492995233"                                                          
##                           "0.236651140876133, 29.6507481217449, -0.536380160392473, 2.83116532614638, -0.24363069165329, -2.68304581135639, -23.5385599114864"                                                             
##                           "0.236071047140777, 24.4085362489431, -0.40682301223228, 2.1258289110201, -0.265411750590603, -2.02172335582947, 1.09165131077688"                                                               
##                           "-0.333550879495943, 30.5914555451939, -0.816661837819043, 1.82949738265834, -0.231372613578783, -26.7897419022007, 11.3940958182229"                                                            
##                           "0.262339618022232, 29.3105660240339, -0.624607607143354, 2.80125544060441, -0.195387190382703, -2.69730059959922, -23.8355150813787"                                                            
##                           "0.13452182941679, 24.4944627762214, -0.558679359063762, 2.15896117105514, 0.0114036900142036, -2.08209237239348, 1.26305438963998"                                                              
##                           "-0.323842163884449, 30.3597124317517, -0.898262341819277, 1.80431602513167, -0.182853504089156, -26.7891644994157, 11.3013871508405"                                                            
##                           "-0.101504411475266, 34.8445884148463, -0.547205189228641, 2.64674444679539, -2.55977908290468, -30.462428064875, 7.95482917643217"                                                              
##                           "1.95267448875786, 15.6501461789897, -0.516969215603935, -0.238614083992228, -0.607648707671038, -1.43935689880694, -13.3100602418224"                                                           
##                           "1.49838643956885, 14.9675363946256, -0.472643504223704, -0.264059671778154, -0.284529289950116, -1.14757460197347, 3.6324312958312"                                                             
##                           "0.863569392856449, 23.3333346216138, -0.678752361191233, -0.166234806295342, -0.336697960550118, -21.3256513290677, 14.246046867911"                                                            
##                           "1.1022252193697, 24.3405300638411, -0.494244305328151, -0.214164817303451, -1.24983963246334, -22.1641884149299, 13.9962738741298"                                                              
##                           "1.21254294711149, 23.8790396099723, -0.510329409793865, -0.391055819407184, -1.27237252682019, -22.0027713002085, 13.1332619423934"                                                             
##                           "0.262246671788903, 30.1507169504433, 2.78895054830525, -0.371064254821769, -0.142798392115423, -2.79674763351424, -23.5976190782176"                                                            
##                           "0.135446778340222, 25.4743318302156, 2.18228547835677, -0.429748170079251, 0.10776728563152, -2.16149349883829, 1.16021104242932"                                                               
##                           "-0.336663986358969, 30.8861383935793, 1.71082184104984, -0.456420465891885, -0.145506186282086, -26.9138181828481, 11.9171721393172"                                                            
##                           "-0.0930872847963748, 35.4135058732849, 2.62645910996018, -0.260319714244041, -2.66509429051309, -30.6995172373869, 8.1170581626681"                                                             
##                           "-0.124327236448906, 35.4601372530759, 2.61763904117453, -0.164857776024718, -2.73338999305147, -30.9989755582501, 8.2375028337231"                                                              
##                           "1.20540185506875, 24.4113789072622, -0.238892863561182, -0.38041527664811, -1.37587346777862, -22.3438579215309, 13.2700511796848"                                                              
##                           "2.70959528211638, 3.60846463274007, -0.989203073962758, -0.511938059997811, -0.519290647284696, -0.234353817702428, -1.86024047640064"                                                          
##                           "2.38815362272242, 2.94467075409565, -1.28474744501731, -0.692481891136994, -0.481006879734262, -0.444567423210105, -2.6223441253309"                                                            
##                           "2.30609541872313, 2.8320615755553, -1.23713371743154, -0.845155411330292, -0.454268854879011, -0.335644295294083, 3.10489838789481"                                                             
##                           "2.74404288793375, 4.31853929484576, -1.04347188220447, -0.500049070712243, -0.615912791001045, -2.11477920495581, -3.87097488148549"                                                            
##                           "2.62091526076687, 3.79545094329257, -1.01695174975234, -0.596054964656225, -0.563086553533416, -1.84144719423109, 1.69160319950032"                                                             
##                           "2.24742687168395, 3.47652741301853, -1.30998967992326, -0.960757568412479, -0.574773547986864, -3.18645782773159, 4.99485694589114"                                                             
##                           "2.70064389662462, 3.85277456874629, -1.24313044747134, -0.495488563288644, -0.48777642794295, -2.00869706850182, -3.77161596198345"                                                             
##                           "2.56484559401563, 3.38542375309133, -1.21506070092363, -0.584039491407591, -0.399290035988969, -1.75815937847429, 1.73168831195551"                                                             
##                           "2.26052193803994, 3.10564877706004, -1.45403957631933, -0.925058519979588, -0.545144302486327, -3.17023244133182, 4.4585652189996"                                                              
##                           "2.49880936142443, 4.24261780104757, -1.36237584271839, -0.637025124476086, -2.01857540678583, -4.07390695497289, 3.1457961609157"                                                               
##                           "2.60196621278814, 4.13301680171643, -1.11068176830328, -0.487940759222082, -0.208230029100886, -2.19858043488587, -3.70380273512854"                                                            
##                           "2.49113312711251, 3.53396842299957, -1.10259939114513, -0.470734909798936, -0.16468670055914, -1.98847014729335, 0.382580884664501"                                                             
##                           "2.00752413771081, 2.85497947640026, -1.46043774350514, -0.393418620404671, -0.344128302201306, -2.37900764767448, 1.1444462334651"                                                              
##                           "2.51964950292859, 4.30000275310184, -1.14603154970386, -0.529421472527596, -2.22750274558648, -3.7987611090876, 0.936593361671936"                                                              
##                           "2.46325653212394, 3.91913524582923, -1.33525647138735, -0.370719007917037, -2.13376206159576, -3.70242787605563, 1.0126605816779"                                                               
##                           "2.89582425870195, 4.34361744574995, -0.674172339250537, -0.813426554498106, -0.394381608304206, -2.29019295285014, -3.96404614777651"                                                           
##                           "2.74833036266787, 3.8527692217565, -0.752858850048799, -0.778618824613767, -0.318599544795637, -2.03220327322947, 1.61021853964359"                                                             
##                           "2.36019668327121, 3.30934568597578, -1.18910478637117, -0.800659811608744, -0.485739731181552, -3.07793139649697, 4.94386611908388"                                                             
##                           "2.75155865698577, 4.75354368166807, -0.82522911903103, -0.905378005356352, -2.28109155260097, -4.30606150803243, 2.94150381270984"                                                              
##                           "2.66485573866112, 4.04802142814397, -0.856654767663781, -0.741158652318801, -2.19827277046728, -4.10812564740417, 2.90052976307068"                                                             
##                           "2.60707147884852, 4.36617508892616, -0.794984973746709, -0.310615048114309, -2.51667621176992, -3.79779516109817, 0.613538134145387"                                                            
##                           "2.67804781746129, -1.14034301957601, -0.300894754111572, -0.120922756797102, -0.60800754974856, -1.05434774646993, 0.0246216240430728"                                                          
##                           "2.67326753160482, -1.14040391801135, -0.316823748397219, -0.114592203028643, -0.606278085221113, -1.04269089297954, 0.264051597187278"                                                          
##                           "2.44580028179829, -1.28464070153205, -0.48588924689372, -0.145223236415116, -0.642510834080967, 0.0463366089701392, 0.868281394560148"                                                          
##                           "2.49245220991046, -1.23755577758448, -0.234454917376196, -0.291820117204312, -1.09988777119319, 0.378409148541816, 0.378485419747833"                                                           
##                           "2.65022060982499, -1.1943809589, -0.31958505730618, -0.645955556360367, -1.05065877975778, -0.0391569642762707, 0.328953843398917"                                                              
##                           "2.57257506279233, -1.20211805687042, -0.12244311726822, -0.561025076423274, -1.15800330415897, 0.119496707004986, -0.169334957672845"                                                           
##                           "2.73861353065506, -0.485662202210524, -0.434807113015323, -0.717292797521307, -1.25746417472507, 0.113583576499404, 0.287937231170456"                                                          
##                           "0.0684972959647539, 26.010479654056, -0.452893363302492, -0.397356207549203, 2.39705529941891, -0.294960851644245, 0.104160653467502, -2.0507186983055"                                         
##                           "-0.101661286979059, 26.4679748605098, -0.771481575183619, -0.828380119982562, 2.34041663470976, -0.273088943696682, -0.285654538286557, -18.9958558537483"                                      
##                           "-0.252913713523609, 24.836498037545, -0.978018870274543, -0.649748201999599, 1.99823512703304, -0.304403643409763, -0.0553510645302306, 1.96671920313707"                                       
##                           "0.25632293544055, 29.4505000912927, 0.0673716165528429, -0.539264392539731, 2.80803102199735, -0.2479266387396, -2.6907973449463, -23.5232096225229"                                            
##                           "0.0985586742386887, 25.8400099503268, -0.425032987726358, -0.385900235546663, 2.2893374020517, -0.23828309884732, -1.98264439299605, 1.03453079302412"                                          
##                           "-0.423008792985153, 31.6427284140531, -0.356227684320199, -0.800385314834128, 1.96550687022958, -0.208325961630809, -27.2204840494062, 11.2619857311561"                                        
##                           "0.240685713967486, 29.5506700364064, -0.0833813657710328, -0.61819474943978, 2.82862863557368, -0.195646311106553, -2.6864057206921, -23.8643375077226"                                         
##                           "-0.0411575952127438, 26.4647216788889, -0.585755183973948, -0.508907769589203, 2.38012670298274, 0.0120084556429516, -2.02246522383959, 1.15859141320774"                                       
##                           "-0.423797279423144, 31.7365881102351, -0.483328978599356, -0.855982364484785, 1.9831778326131, -0.193888409966234, -27.3413094762009, 11.0443781703904"                                         
##                           "-0.0666789147718588, 34.4486011879698, 0.153146906093676, -0.556659700893414, 2.59583223069886, -2.57675204501473, -30.3368333587503, 8.01157504267925"                                         
##                           "2.09088273349167, 13.2245741401889, 2.33389093487462, -0.590504358681733, -0.455597317767888, -0.366213083095051, -2.04014796324479, -13.7433786200137"                                         
##                           "1.64003260627661, 12.2757954158122, 1.88734952742974, -0.552055135771131, -0.429928773056744, -0.0960809016931839, -1.61639281401193, 3.35118055545518"                                         
##                           "0.859869595557234, 21.2880698037959, 1.28109216251877, -0.774016684282928, -0.30290385862307, -0.193690499725379, -20.7000833648269, 13.9445283832932"                                          
##                           "1.36151485387971, 20.7886526456639, 2.31209501165286, -0.54433524738279, -0.36125098783384, -1.84922810479082, -21.3384670096178, 12.4074068360978"                                             
##                           "1.32588183889199, 20.8751997950465, 2.07674685743873, -0.665558392892016, -0.242620184554023, -1.82561774705459, -21.38152163661, 12.4076869759207"                                             
##                           "0.234690075364605, 30.434865936528, -0.0953591555171792, 2.82130915516641, -0.363142536944587, -0.144415343167024, -2.78417099538255, -23.6045982811929"                                        
##                           "-0.0277529913836588, 27.1913054750953, -0.534411001969659, 2.3813313330022, -0.385052358694186, 0.096747636108413, -2.10157895568731, 1.08194210589589"                                         
##                           "-0.470367516367653, 32.5656855200042, -0.595985079096333, 1.93629050574884, -0.398775990763097, -0.175234232515406, -27.5399470199851, 11.5737121020403"                                        
##                           "-0.0556936070729167, 35.0293323085178, 0.147352805868319, 2.57611459496777, -0.271141539744169, -2.68066542794853, -30.5850308475903, 8.16808405125268"                                         
##                           "-0.138573637190814, 35.628628485132, -0.06909274842582, 2.64018715335783, -0.165602934523206, -2.72388906498431, -31.0441933621803, 8.2028234045207"                                            
##                           "1.36659097537274, 21.4532590289638, 2.19784353056081, -0.470870501515841, -0.156930722238475, -1.96038483244003, -21.8055185741535, 12.4299328980164"                                           
##                           "0.287160878226596, 29.4506927643814, -0.528082766759606, 2.79674554262445, -0.207575263304421, -0.111913164621486, -2.66367029304156, -23.6553671733625"                                        
##                           "0.172214621150225, 24.751619449276, -0.415798255133894, 2.16966072433453, -0.306046490852511, 0.131688999786879, -2.04994834966424, 1.13222715608695"                                           
##                           "-0.286863340755017, 30.3811067396874, -0.809786389108298, 1.80960591933766, -0.195147335755247, -0.101885103802649, -26.6793349908451, 11.1709162258634"                                        
##                           "-0.0629746800053662, 34.7605171138251, -0.493329104395408, 2.63516622379199, -0.0987510214638047, -2.53783352486314, -30.3341646295569, 7.82095830650506"                                       
##                           "-0.0734915766553622, 34.7376410063132, -0.534726100916763, 2.63417287582253, -0.0496479397285878, -2.55090698287193, -30.4050505675059, 7.84712242706799"                                       
##                           "1.23115942268819, 23.9026471707126, -0.463562458022728, -0.102602504704715, -0.353490397615538, -1.2592301132327, -21.9720998931295, 13.0659750008184"                                          
##                           "-0.0711134213855168, 35.3129641460543, 2.61608091170862, -0.244277745639435, -0.0476993433512693, -2.65964663952462, -30.6614386954037, 8.02952452857978"                                       
##                           "2.81935409212099, 4.19015926905042, -1.00025352230393, -0.532356069789864, -0.529400055451505, -0.278266979479174, -2.05446243268265, -3.91780895351409"                                        
##                           "2.67665082217793, 3.68523164814012, -0.98585155246566, -0.608134176022651, -0.500102389632348, -0.20600330281319, -1.80105028508889, 1.54688923568658"                                          
##                           "2.35448748306805, 3.31568441791329, -1.2601117918613, -0.957214691277207, -0.451299795664869, -0.358183012730767, -3.2163537786747, 4.36935741695917"                                           
##                           "2.71302959402956, 4.51554445055645, -1.02996352640333, -0.680897054299398, -0.574356860750287, -2.02220435051751, -4.24132680500178, 2.70457925813729"                                          
##                           "2.6627642737678, 4.08098030253722, -1.22417125237498, -0.669078312252227, -0.428814197449809, -1.93200931875463, -4.12280906361941, 2.63540936757909"                                           
##                           "2.56577145910301, 4.20148314946061, -1.11938826429264, -0.472923184583985, -0.187203631628532, -2.19144632770901, -3.8024331443525, 0.830043509511662"                                          
##                           "2.85168770493789, 4.55331081832721, -0.83484430342245, -0.785095535134428, -0.348953814450613, -2.21451594852781, -4.3144955040366, 2.57136009539767"                                           
##                           "2.67302716176034, -1.14033487986778, -0.31667004054995, -0.11474712000453, -0.606004642493452, -1.04264498177661, 0.00623870823160242, 0.26337250116773"                                        
##                           "0.299344203222421, 29.3231333967167, 0.0444042105158202, -0.529972315364514, 2.7819656057367, -0.210923184699698, -0.110513234276068, -2.6690001114583, -23.6399433397591"                      
##                           "0.0476658501512836, 26.0755166170964, -0.401712791483155, -0.395019505102727, 2.32052849369981, -0.277646147739112, 0.121234321145145, -2.01076641741853, 1.07339381476123"                     
##                           "-0.375926379121112, 31.5070998988967, -0.396986954248107, -0.790423972039527, 1.95651266091676, -0.160791977075987, -0.125256762265476, -27.1433458795474, 10.9766104245952"                    
##                           "-0.00778876657583029, 34.1874534592982, 0.216669469695606, -0.498762471883399, 2.56122585605974, -0.11329753753961, -2.55890547380252, -30.1398086953291, 7.88225418444644"                     
##                           "-0.0431061589500597, 34.3739732559345, 0.144152008849339, -0.544667794528104, 2.5873398741006, -0.0454236549251158, -2.56766286100109, -30.2906521797149, 7.90913151239849"                     
##                           "1.39549988534031, 20.7284476989144, 2.25701865749804, -0.532337189128482, -0.325005082168134, -0.108850156983521, -1.8372660329096, -21.2852390963896, 12.1566114513733"                        
##                           "-0.0406803245760816, 34.9809175860907, 0.133209685913276, 2.57219177528549, -0.256567595079556, -0.0403603716449202, -2.67456715700567, -30.5621726457978, 8.08820597536343"                    
##                           "-0.056891647775998, 34.7342931487974, -0.492200232186635, 2.6321931904052, -0.0945570152907632, -0.0135927181587981, -2.53639040289729, -30.3237300596079, 7.79695105991264"                    
##                           "2.77808917634083, 4.39143311297902, -0.99457495102868, -0.692793673791255, -0.504170546342503, -0.233319844394991, -1.98015825376423, -4.24748206777618, 2.47893190869678"                      
##                           "-0.0077276807574823, 34.187292175099, 0.216602699260313, -0.49874601917577, 2.56121064023986, -0.113239606114017, -0.000174343175925813, -2.55888053867932, -30.1397262260793, 7.88192247433973"
##                                                                                                                                                                                                                                 
## Logistic_regression.model "model.p-value"                                                                                                                                                                                       
##                           "0.00102399623402558, 0.00119092206397558"                                                                                                                                                            
##                           "3.5714498810098e-12, 0.0868250330901933"                                                                                                                                                             
##                           "6.31698151472606e-17, 0.00469832548785189"                                                                                                                                                           
##                           "3.96728632876185e-10, 0.226282456842189"                                                                                                                                                             
##                           "2.25103425373783e-10, 0.0816169054142014"                                                                                                                                                            
##                           "3.54201811001891e-11, 0.0627413915402057"                                                                                                                                                            
##                           "2.63327656769797e-10, 0.0321031266685503"                                                                                                                                                            
##                           "2.6986852241661e-20, 0.711902950984314"                                                                                                                                                              
##                           "8.63201619048354e-20, 0.752968886280556"                                                                                                                                                             
##                           "0.00124306372495206, 0.00324315746060587, 0.690946010331386"                                                                                                                                         
##                           "0.000469028154306938, 0.00428218242651175, 0.0894845653128617"                                                                                                                                       
##                           "0.447072535525869, 0.000319117710930431, 0.0257797536733749"                                                                                                                                         
##                           "0.000861539219912762, 0.00156672677366052, 0.107510263063775"                                                                                                                                        
##                           "0.00135088301312054, 0.00196170676757618, 0.15624208869742"                                                                                                                                          
##                           "0.000419993017563614, 0.00153546032113713, 0.0473743106044862"                                                                                                                                       
##                           "0.000177350784755004, 0.000674316049674497, 0.104093596396305"                                                                                                                                       
##                           "0.0590923498979763, 0.000784095567959226, 0.214556281321247"                                                                                                                                         
##                           "2.83924848757834e-12, 0.0575725294257908, 0.00310364357257112"                                                                                                                                       
##                           "4.35824629029844e-08, 0.0586905103353916, 0.138453963552027"                                                                                                                                         
##                           "9.72614642829814e-09, 0.0485412850227937, 0.0369367428634273"                                                                                                                                        
##                           "6.60954739299149e-08, 0.113005243413716, 0.0909041252565996"                                                                                                                                         
##                           "2.50726527224861e-09, 0.0252403786030389, 0.005732598287941"                                                                                                                                         
##                           "2.15609665206254e-12, 0.0823415405604954, 0.651944607434603"                                                                                                                                         
##                           "1.75861560450354e-10, 0.0799521142314846, 0.625225960025726"                                                                                                                                         
##                           "1.61344266781176e-10, 0.00818562776600271, 0.535272441329136"                                                                                                                                        
##                           "1.35651013810992e-11, 0.0280473787369052, 0.511618632148305"                                                                                                                                         
##                           "2.73987090905106e-12, 0.0181719820191351, 0.237719474401514"                                                                                                                                         
##                           "4.45438212296949e-11, 0.0204128951554058, 0.140135723839532"                                                                                                                                         
##                           "3.79296625284972e-16, 0.00506019181208335, 0.819909742937687"                                                                                                                                        
##                           "6.37781243104203e-16, 0.00492596373005632, 0.822768285987829"                                                                                                                                        
##                           "2.08754985850014e-08, 0.253448158983985, 0.0896524403400986"                                                                                                                                         
##                           "1.54814516642032e-08, 0.182246220015888, 0.052164700810423"                                                                                                                                          
##                           "2.68091913677176e-09, 0.448404772675419, 0.0561442761621983"                                                                                                                                         
##                           "1.06623882079819e-09, 0.23582279094525, 0.760165865213458"                                                                                                                                           
##                           "3.501021901295e-10, 0.167689818372193, 0.540973324025835"                                                                                                                                            
##                           "3.73263646727651e-10, 0.276778627183749, 0.218729279211829"                                                                                                                                          
##                           "1.71803860990296e-09, 0.161768537338721, 0.0637014095216127"                                                                                                                                         
##                           "3.95211698398453e-10, 0.0811150748606819, 0.705169476714607"                                                                                                                                         
##                           "1.06473005156731e-09, 0.0874608441617718, 0.936914867566367"                                                                                                                                         
##                           "2.08327734210254e-09, 0.110361488076757, 0.0559533193925838"                                                                                                                                         
##                           "1.4956242600824e-10, 0.0687458533302723, 0.889918458619767"                                                                                                                                          
##                           "1.59217188800681e-10, 0.066957631625558, 0.930681344902516"                                                                                                                                          
##                           "5.94512766748489e-10, 0.0340279534353601, 0.793746003785046"                                                                                                                                         
##                           "7.41970649715981e-10, 0.0340795468118989, 0.896775338949087"                                                                                                                                         
##                           "7.61811637893665e-19, 0.726167442002934, 0.772602201452755"                                                                                                                                          
##                           "0.000354367703239382, 0.0149501973649927, 0.442451768693324, 0.0654583279827258"                                                                                                                     
##                           "0.203677796010054, 0.00124615320257044, 0.209092949722818, 0.0146213654109051"                                                                                                                       
##                           "0.000683243861113477, 0.0051764690395528, 0.45527582798841, 0.081787503409782"                                                                                                                       
##                           "0.00161445994408322, 0.004729147067353, 0.736884613147706, 0.16114568584939"                                                                                                                         
##                           "0.000235962953129022, 0.00689185173190853, 0.263209060600422, 0.0250524937009067"                                                                                                                    
##                           "0.000162844702039747, 0.00164634764466137, 0.550316151895454, 0.0678734799850533"                                                                                                                    
##                           "0.0650654119648993, 0.00203389350225161, 0.654317079752447, 0.21009423062324"                                                                                                                        
##                           "0.872254446155808, 0.000866935553304797, 0.102752846441275, 0.0273438103009302"                                                                                                                      
##                           "0.000470451512424688, 0.00388908530356029, 0.301482941179765, 0.321277208244144"                                                                                                                     
##                           "0.000549330847115364, 0.00498283711970258, 0.175758818068321, 0.303604175098354"                                                                                                                     
##                           "0.000243824348524949, 0.00405585285453912, 0.243251824777157, 0.113952113691193"                                                                                                                     
##                           "4.62339524246542e-05, 0.00171079975231673, 0.0676262780372057, 0.0541597305516324"                                                                                                                   
##                           "0.0191813365914629, 0.0029145792966708, 0.148713126666547, 0.289771611097881"                                                                                                                        
##                           "0.962161990137071, 0.000431549467738409, 0.0344642687593544, 0.150889726301262"                                                                                                                      
##                           "0.885203150844611, 0.000812605621390588, 0.0500319427567988, 0.39143114152921"                                                                                                                       
##                           "0.994127530213122, 0.00015943127903989, 0.006389636265272, 0.0110851121729038"                                                                                                                       
##                           "0.371669978819485, 0.000170202278631177, 0.0103269666364846, 0.0377670447571496"                                                                                                                     
##                           "0.462788976810116, 0.000296977443628178, 0.0594367083409815, 0.466480451758366"                                                                                                                      
##                           "0.000849118088953138, 0.002108459436711, 0.268820397662962, 0.447948976980759"                                                                                                                       
##                           "0.000279330055480134, 0.00194514579651968, 0.22998251218526, 0.0997884485740677"                                                                                                                     
##                           "0.000250617871669913, 0.000874838614366251, 0.0964242188148372, 0.0893974369426033"                                                                                                                  
##                           "0.027519372478962, 0.00122846571562348, 0.204559408565133, 0.326019417437102"                                                                                                                        
##                           "0.000425176306048464, 0.00232505105999283, 0.266646501019133, 0.0763093310210088"                                                                                                                    
##                           "0.000119756105786216, 0.00072326830854339, 0.0846436455809436, 0.0261452503123938"                                                                                                                   
##                           "0.0455480492706676, 0.00166946934551685, 0.317310497980919, 0.342637212098171"                                                                                                                       
##                           "6.54642809957068e-05, 0.000654992408943713, 0.0293138072898365, 0.0397386487648199"                                                                                                                  
##                           "0.0128598568635196, 0.0012462788968693, 0.0916306416291708, 0.367569650449025"                                                                                                                       
##                           "0.310861222997208, 0.000362707091545467, 0.00192357373033328, 0.0297959360417594"                                                                                                                    
##                           "8.51162978141328e-09, 0.0423641953207414, 0.00578128095566181, 0.327605876683107"                                                                                                                    
##                           "7.73393088885442e-10, 0.0448047517870698, 0.0272999373176771, 0.308581588384915"                                                                                                                     
##                           "5.90731646086239e-09, 0.0722507432126091, 0.0109999141234361, 0.357476651027063"                                                                                                                     
##                           "4.25520327741205e-10, 0.0236252272242755, 0.0207803526475643, 0.0308419901648603"                                                                                                                    
##                           "1.70208887439668e-12, 0.0468937869012245, 0.00270114218546121, 0.447199432292275"                                                                                                                    
##                           "5.16958950807319e-11, 0.0540569357427282, 0.00334954320410555, 0.674981953887732"                                                                                                                    
##                           "1.36393487725679e-07, 0.0273498393806241, 0.118269851617225, 0.032474378651026"                                                                                                                      
##                           "4.39738143787067e-07, 0.0735885688127086, 0.109273761343949, 0.0723069360085944"                                                                                                                     
##                           "1.00857508536567e-08, 0.0193256657539202, 0.287448058194484, 0.0103410020719706"                                                                                                                     
##                           "4.18150120048403e-08, 0.0523818999260785, 0.133113726578246, 0.586824702893437"                                                                                                                      
##                           "3.16411539647604e-08, 0.0362869781377034, 0.0639452771488073, 0.314932385608488"                                                                                                                     
##                           "4.5362438010241e-08, 0.0687870598193838, 0.131812706869862, 0.416505415899932"                                                                                                                       
##                           "3.49330730015505e-09, 0.0151881304043993, 0.0651523037024618, 0.00979516266142656"                                                                                                                   
##                           "8.02824484374869e-09, 0.0432517775247587, 0.034755947325267, 0.546249683313601"                                                                                                                      
##                           "6.73065520638412e-08, 0.0475299495227428, 0.0428681193669178, 0.798097924141742"                                                                                                                     
##                           "2.57410766612149e-08, 0.0366618686802754, 0.223586397942404, 0.0120525521192563"                                                                                                                     
##                           "5.24883012450655e-08, 0.0946855859554798, 0.0821317825871644, 0.508195882319551"                                                                                                                     
##                           "4.0519018186995e-07, 0.109263412068601, 0.105097409635156, 0.780385028609419"                                                                                                                        
##                           "2.30101293849247e-09, 0.017221713576877, 0.00417052313076545, 0.244297090066479"                                                                                                                     
##                           "1.00311387197273e-08, 0.0244586349069764, 0.00636423937444429, 0.748262334235146"                                                                                                                    
##                           "1.94055647960829e-10, 0.0727920134716561, 0.563134530892204, 0.587241298600661"                                                                                                                      
##                           "1.344628937474e-08, 0.0449049186080924, 0.507874609214874, 0.486631045718732"                                                                                                                        
##                           "7.408194122812e-09, 0.0348985965210341, 0.43107829227579, 0.202080689390054"                                                                                                                         
##                           "1.26471897637705e-09, 0.0256515388318254, 0.72341205230071, 0.168387033239322"                                                                                                                       
##                           "4.01423427115639e-10, 0.00860748082602219, 0.543939756537094, 0.844480823747395"                                                                                                                     
##                           "1.52472693794683e-10, 0.0104251066226626, 0.463804409719972, 0.689724003725701"                                                                                                                      
##                           "5.52021036934673e-11, 0.0385153367885786, 0.792883588680071, 0.314106139396037"                                                                                                                      
##                           "8.04759924559274e-10, 0.0658366757153743, 0.606819111256661, 0.156899652773396"                                                                                                                      
##                           "2.53782211983643e-11, 0.0300283706095778, 0.505361360024552, 0.803145657488172"                                                                                                                      
##                           "9.34270105010258e-11, 0.0278438918188833, 0.530878217465963, 0.894144617978398"                                                                                                                      
##                           "5.11916168664767e-10, 0.0524621119110032, 0.290692192778105, 0.168907511963332"                                                                                                                      
##                           "1.20064721010266e-11, 0.0183284513353303, 0.246779585072415, 0.934912522556995"                                                                                                                      
##                           "1.54214739550361e-11, 0.0182158240652679, 0.246445815364795, 0.944026688964255"                                                                                                                      
##                           "1.00883921888494e-10, 0.0211053974027928, 0.143011722836183, 0.868309994570887"                                                                                                                      
##                           "1.45280239928489e-10, 0.0205812367059788, 0.144664496574162, 0.928555934796136"                                                                                                                      
##                           "2.38228673233292e-15, 0.00525730427646477, 0.831225394421095, 0.834460139717927"                                                                                                                     
##                           "2.26282268475492e-08, 0.207061972063296, 0.317811972532488, 0.180655510566577"                                                                                                                       
##                           "6.89141791611548e-09, 0.473679489016328, 0.168202631190346, 0.105570647152325"                                                                                                                       
##                           "3.4818946459851e-08, 0.264631386971481, 0.0891565702065596, 0.752396188716833"                                                                                                                       
##                           "2.27343939199977e-08, 0.224067917987055, 0.113577289086279, 0.697508435642675"                                                                                                                       
##                           "1.05614060699759e-08, 0.362978331445251, 0.0942695237838294, 0.103470083279134"                                                                                                                      
##                           "3.81543197459558e-08, 0.184673582538546, 0.0557885778353301, 0.969780881727298"                                                                                                                      
##                           "1.63493293943821e-08, 0.159774495794917, 0.0646370494732753, 0.683106519437389"                                                                                                                      
##                           "5.71082158523818e-09, 0.457056969564498, 0.0581309197651231, 0.821596706593798"                                                                                                                      
##                           "3.25049203597491e-09, 0.404059712073262, 0.0729561490438571, 0.734015154162315"                                                                                                                      
##                           "9.70617749301275e-10, 0.178419174791157, 0.81979127110443, 0.556690118208268"                                                                                                                        
##                           "2.72345725940227e-09, 0.379195176445467, 0.254818026484009, 0.0726615973049214"                                                                                                                      
##                           "8.74474464483998e-10, 0.27057367538961, 0.239350579969133, 0.831728759159192"                                                                                                                        
##                           "1.44823525119383e-09, 0.278567643708078, 0.219543249498225, 0.984361329959075"                                                                                                                       
##                           "2.95333707179004e-09, 0.159979901927052, 0.0673019213513975, 0.775563551368728"                                                                                                                      
##                           "4.67101710362162e-09, 0.163699357109429, 0.0638756981187319, 0.958796530440881"                                                                                                                      
##                           "1.506579219474e-09, 0.0860621684167277, 0.708307928692003, 0.961934787158002"                                                                                                                        
##                           "5.04285716875604e-09, 0.11596779033166, 0.0566628851123334, 0.959299057116632"                                                                                                                       
##                           "4.80495818304763e-09, 0.111505703075505, 0.0561719939078813, 0.956625319244126"                                                                                                                      
##                           "4.97976403667901e-10, 0.0725887538625664, 0.893289484356433, 0.936515730213115"                                                                                                                      
##                           "1.39053119004477e-09, 0.0357816895956167, 0.799342273340975, 0.910873749429654"                                                                                                                      
##                           "0.54760232527221, 0.00492072092685503, 0.413572526013337, 0.183395583816485, 0.0296806690504501"                                                                                                     
##                           "0.000308216043058894, 0.0144003500378476, 0.366766457640306, 0.243323801408504, 0.26628337320102"                                                                                                    
##                           "0.000541001246225017, 0.0155134879455774, 0.508328692907465, 0.137894132878065, 0.345062141558514"                                                                                                   
##                           "0.000107041091495185, 0.0206393508337057, 0.203268554635301, 0.176664670627587, 0.0560609385305091"                                                                                                  
##                           "2.48106740494697e-05, 0.00571031824395519, 0.322866902121982, 0.042962419076596, 0.0287901625478477"                                                                                                 
##                           "0.0158869647844233, 0.00971138159576983, 0.442985986562688, 0.111389303841135, 0.288900981547647"                                                                                                    
##                           "0.590827547280704, 0.00308469592775775, 0.397143317860871, 0.0349512887409556, 0.258006474254705"                                                                                                    
##                           "0.475894678602406, 0.00269514643579287, 0.249888645060325, 0.0291109824599047, 0.481211565654585"                                                                                                    
##                           "0.741726881745182, 0.00155115942847178, 0.568405772763846, 0.0122760751944437, 0.0210067118842826"                                                                                                   
##                           "0.191140595787128, 0.000795450034650968, 0.26048578120359, 0.00774700684244291, 0.0480247299214404"                                                                                                  
##                           "0.238168251241221, 0.00153525082765295, 0.278703612291239, 0.0350476688082223, 0.547681083124785"                                                                                                    
##                           "0.000844822620501606, 0.00586743294282351, 0.5266442489616, 0.215642222660271, 0.523686284435054"                                                                                                    
##                           "0.000127406246924146, 0.0108725650396522, 0.184810611772709, 0.150205675309754, 0.0437369925394114"                                                                                                  
##                           "0.000133553589259188, 0.00233827556941089, 0.332434573517044, 0.0649636347892046, 0.0415639835612854"                                                                                                
##                           "0.0230489235122055, 0.00390608923346586, 0.46679586431883, 0.16084018927021, 0.329428294490785"                                                                                                      
##                           "0.000299270966030872, 0.0086256873428825, 0.312354834969109, 0.327765103492676, 0.0442851071309911"                                                                                                  
##                           "0.000167570449461248, 0.00176452484520929, 0.597857590179215, 0.089322045072665, 0.0192530408047328"                                                                                                 
##                           "0.0515277524890653, 0.00364656580320997, 0.695560561838415, 0.329724145238963, 0.33526068693888"                                                                                                     
##                           "2.12732989236495e-05, 0.00241557247666934, 0.157096147969439, 0.0110841614960008, 0.0103746361205078"                                                                                                
##                           "0.00716108027557061, 0.00542159805494567, 0.280075699677341, 0.0496796200466005, 0.388203650757676"                                                                                                  
##                           "0.343284326272842, 0.000747537222234152, 0.486057086877943, 0.00160202845981692, 0.0285325187004785"                                                                                                 
##                           "0.914038760104517, 0.000845515048179763, 0.2832964239673, 0.0325270911525716, 0.414959360478274"                                                                                                     
##                           "0.934291063098121, 0.00139014720694785, 0.147420238856126, 0.0424715935984067, 0.655038169427116"                                                                                                    
##                           "0.802714367291853, 0.000419922749270813, 0.374763406308738, 0.0082292046931414, 0.0293834552929104"                                                                                                  
##                           "0.776448563911239, 0.000262444807089939, 0.0616578632067584, 0.00898557159323873, 0.0137219345126556"                                                                                                
##                           "0.854710876593252, 0.000794501568199505, 0.132071255186289, 0.0518899027533247, 0.542105070533152"                                                                                                   
##                           "0.00049622390401772, 0.00469947155981632, 0.325500492747304, 0.517933112089591, 0.488438731175348"                                                                                                   
##                           "0.000213689200046076, 0.00388033646490083, 0.485361379218803, 0.423191390981668, 0.14320323605384"                                                                                                   
##                           "8.41103700127852e-05, 0.00167876608086161, 0.252014145580389, 0.317658843551685, 0.0579136027705877"                                                                                                 
##                           "0.0168080998561276, 0.00292447121815043, 0.338043493939888, 0.458683656422526, 0.349669563120606"                                                                                                    
##                           "0.000271363048887807, 0.00470953855443491, 0.358007799625247, 0.38665796807332, 0.139345984369869"                                                                                                   
##                           "3.83321360305002e-05, 0.00159623512054331, 0.159338185892507, 0.185439514626694, 0.0243267202006706"                                                                                                 
##                           "0.0212597248725393, 0.00410405043107136, 0.215316381365934, 0.484925710887215, 0.384602155547506"                                                                                                    
##                           "2.79254092382821e-05, 0.00140823420220751, 0.213393057398917, 0.0786240839686864, 0.0308596472917435"                                                                                                
##                           "0.00751298774242018, 0.00322025063127494, 0.301576855250607, 0.171796357220863, 0.406703813014701"                                                                                                   
##                           "0.114181458694165, 0.000942959617591956, 0.179212624517904, 0.00337862247200036, 0.0471320369051667"                                                                                                 
##                           "0.966207488991597, 0.000726818693894666, 0.0451816969795495, 0.235857734872335, 0.842419692155727"                                                                                                   
##                           "0.747025607067417, 0.000233568646810509, 0.00916095766617046, 0.394256277296586, 0.0236304202247712"                                                                                                 
##                           "0.871766184210571, 0.000207333640093048, 0.0131953568823505, 0.130059805214526, 0.0309991263107054"                                                                                                  
##                           "0.919947904101489, 0.000407996962663967, 0.0590229311645359, 0.204659128896398, 0.567141051421746"                                                                                                   
##                           "0.899265121613148, 0.000327528277111119, 0.0109278006758716, 0.819022504970369, 0.0164754729392041"                                                                                                  
##                           "0.92307874987671, 0.00029178215840069, 0.0230524608072339, 0.243729108947544, 0.0152166432989294"                                                                                                    
##                           "0.826068751845433, 0.000721019844766487, 0.0797567850489249, 0.49552324711136, 0.521807686003735"                                                                                                    
##                           "0.995870525398794, 4.76031875999479e-05, 0.00113657124706778, 0.00303907291983812, 0.00508414762509749"                                                                                              
##                           "0.982011629129565, 0.000159567468818348, 0.0107817687394974, 0.0161027149883613, 0.655717617910635"                                                                                                  
##                           "0.225060146538578, 8.07608873633605e-05, 0.0378113320148626, 0.000745216719961657, 0.0871144911202596"                                                                                               
##                           "0.000323519525362075, 0.00253922611242837, 0.409239466307467, 0.505123924962803, 0.108471006016986"                                                                                                  
##                           "0.0001135150505805, 0.000845777567301838, 0.308283058159578, 0.282561890309735, 0.035608164548346"                                                                                                   
##                           "0.0283575282977389, 0.0018822547845631, 0.340441668649872, 0.606684191875343, 0.387268792577589"                                                                                                     
##                           "5.21475152075585e-05, 0.000863984493545019, 0.224654454326698, 0.0659998476050737, 0.0416700372815437"                                                                                               
##                           "0.00798999755814313, 0.00167816974211223, 0.326236993518048, 0.141337187489381, 0.445676322835224"                                                                                                   
##                           "0.156735664360388, 0.000583118741846329, 0.312014683514696, 0.00304151195017503, 0.0494651404323502"                                                                                                 
##                           "3.72703624143151e-05, 0.000735510840406468, 0.147525704064126, 0.0492678704678798, 0.0137558364377209"                                                                                               
##                           "0.0110612137431597, 0.00216792256971831, 0.401718333403445, 0.110419410687925, 0.467082331620657"                                                                                                    
##                           "0.167554598705513, 0.000684546107760998, 0.34156533665276, 0.00252180734129977, 0.0611887395040869"                                                                                                  
##                           "0.0575509184052732, 0.000567725015123545, 0.085853675151911, 0.00236606273371051, 0.0583046387933956"                                                                                                
##                           "9.03675375178587e-08, 0.0294640943961486, 0.0532716889531253, 0.262434351232969, 0.248888699020534"                                                                                                  
##                           "2.45480873394599e-07, 0.052414667955548, 0.0228980043377375, 0.269449040837782, 0.291875867112095"                                                                                                   
##                           "4.738304947852e-09, 0.0191653099577796, 0.0286113941386726, 0.465909081020121, 0.0398456132040506"                                                                                                   
##                           "8.04568460771333e-09, 0.033002841332335, 0.00497825786726021, 0.312750863324935, 0.405851708559539"                                                                                                  
##                           "7.91272987772529e-09, 0.0312876585717752, 0.00924686372522714, 0.204540528145801, 0.452345727701234"                                                                                                 
##                           "9.26931177322693e-09, 0.0594943196309204, 0.0336320595822139, 0.474986850248641, 0.576613057588584"                                                                                                  
##                           "2.12203331733102e-09, 0.0187586384592575, 0.10332104543815, 0.32133658414913, 0.0319478409240222"                                                                                                    
##                           "5.72912638588518e-10, 0.0359838996495022, 0.0246170026898479, 0.299763714040283, 0.423179466728823"                                                                                                  
##                           "8.2134515818951e-09, 0.0435018454869309, 0.0268035986750494, 0.338724680670698, 0.762327879468038"                                                                                                   
##                           "8.01367781848472e-09, 0.0305336412700625, 0.0418025877265861, 0.540099199341125, 0.0404807491171254"                                                                                                 
##                           "4.21213408833024e-09, 0.0557554448990234, 0.00989917912018341, 0.32958257423256, 0.390950704540266"                                                                                                  
##                           "5.67711191624366e-08, 0.0696544051522253, 0.0109514162960772, 0.395667964485502, 0.767297591282553"                                                                                                  
##                           "3.73491751312642e-10, 0.0153936611083397, 0.0187171213512467, 0.0220449817491323, 0.19628243650001"                                                                                                  
##                           "2.01264902204109e-09, 0.0230834323550811, 0.0213158181738992, 0.0333229909536419, 0.782736910604482"                                                                                                 
##                           "4.86486292858293e-11, 0.0425371052385252, 0.00288447581233692, 0.383506588745047, 0.616027172937998"                                                                                                 
##                           "2.57533627837757e-07, 0.0410203919324653, 0.107429389225854, 0.129704506785226, 0.366326033911283"                                                                                                   
##                           "1.00043955082934e-08, 0.0107575057682868, 0.260965870821099, 0.060747316547173, 0.018928978753397"                                                                                                   
##                           "1.33869179302841e-07, 0.0224979690875836, 0.110929567630245, 0.0299771612792356, 0.463523384627106"                                                                                                  
##                           "1.53132165754054e-07, 0.020648574450357, 0.076992204604495, 0.0501376461619999, 0.451409917461452"                                                                                                   
##                           "5.84391472266806e-08, 0.0279580187788812, 0.238030067307327, 0.186322456588237, 0.0233177305276776"                                                                                                  
##                           "4.18133976648977e-07, 0.0572275529609017, 0.100167498595779, 0.0625045813397213, 0.410271620939235"                                                                                                  
##                           "6.65428701527316e-07, 0.0555593468578172, 0.0744446340041043, 0.118433351868088, 0.481735955050786"                                                                                                  
##                           "9.09995135752306e-09, 0.0133256374121343, 0.282998610065285, 0.00758895123669427, 0.229699124750956"                                                                                                 
##                           "1.45406519761543e-08, 0.0161117913226613, 0.213877128282598, 0.0179091988015797, 0.555940184903341"                                                                                                  
##                           "2.78751832650005e-08, 0.0256162184771973, 0.0487913405722236, 0.300439471402266, 0.246856973070391"                                                                                                  
##                           "1.23486532328967e-08, 0.0218343296968709, 0.136476476334762, 0.636931221834534, 0.0125548309439598"                                                                                                  
##                           "3.55157952055469e-08, 0.0576970770811542, 0.130328526889378, 0.392324980710645, 0.492924435051187"                                                                                                   
##                           "2.56248764797908e-07, 0.0681363118796931, 0.136881237201125, 0.430915526783357, 0.857901422585506"                                                                                                   
##                           "3.08428910260236e-09, 0.00983506089249801, 0.0588866591818442, 0.00679494410564761, 0.194136553903498"                                                                                               
##                           "1.24764198368093e-08, 0.015180036682761, 0.0698539384633803, 0.0101236394543571, 0.911730231351905"                                                                                                  
##                           "6.96715507791941e-08, 0.0418838902501475, 0.0412231737621402, 0.503958102441324, 0.74232635519902"                                                                                                   
##                           "1.995777905775e-08, 0.0235874627881684, 0.199373611237002, 0.0085696808336225, 0.202039264690268"                                                                                                    
##                           "8.78387501289339e-08, 0.0364153170302921, 0.238375782635735, 0.0124250207849972, 0.86099474925527"                                                                                                   
##                           "3.95698348986788e-07, 0.0900645558370687, 0.0971389313014224, 0.470513466062052, 0.728398012895786"                                                                                                  
##                           "9.52398769106898e-09, 0.0162835543704984, 0.00452723009542263, 0.210057109538973, 0.657087086184437"                                                                                                 
##                           "2.55982287338841e-08, 0.0666321000376302, 0.427874381119992, 0.780497774385664, 0.273504131752154"                                                                                                   
##                           "1.04396019653541e-08, 0.0807091585570993, 0.699025706218221, 0.591032642510869, 0.192594485751959"                                                                                                   
##                           "2.15120738088381e-08, 0.0470032001159051, 0.516971998680109, 0.481950298806948, 0.828318344987146"                                                                                                   
##                           "1.44070913784505e-08, 0.0468877629650253, 0.460945881208472, 0.527805562302776, 0.749260505637887"                                                                                                   
##                           "9.74725243479081e-09, 0.072281702511519, 0.599610726501159, 0.260355126556904, 0.216159876870331"                                                                                                    
##                           "1.88791105726488e-08, 0.0349283216815997, 0.43354594386286, 0.208180457808884, 0.983204503876381"                                                                                                    
##                           "7.88339616557366e-09, 0.0378788581134725, 0.400971940317393, 0.21917183497459, 0.780491074826105"                                                                                                    
##                           "2.61725122157491e-09, 0.0262871411950797, 0.729649616094664, 0.170622051621432, 0.880987687804788"                                                                                                   
##                           "1.51241545627851e-09, 0.0273197637150291, 0.687553733065575, 0.188123603102656, 0.841452184072347"                                                                                                   
##                           "3.8172614672109e-10, 0.0106991833056613, 0.474809163363073, 0.878472277451507, 0.700509419898574"                                                                                                    
##                           "1.79237725760193e-09, 0.0871094189945294, 0.84140137643328, 0.349355723301125, 0.17244173766372"                                                                                                     
##                           "1.32382708190325e-10, 0.0394798492613467, 0.786074732211673, 0.328802724294538, 0.916406439062684"                                                                                                   
##                           "3.08973789870459e-10, 0.0384805387651271, 0.797725306475709, 0.31798242533967, 0.96584395309463"                                                                                                     
##                           "1.33311518513144e-09, 0.0683632029774353, 0.600818887752465, 0.160588778085691, 0.851414220194059"                                                                                                   
##                           "2.8628972601943e-09, 0.0659028715760094, 0.612394611735121, 0.158829083086766, 0.984138566652563"                                                                                                    
##                           "1.36804921353783e-10, 0.0298269050904494, 0.522872912345669, 0.809850485889135, 0.909443291814796"                                                                                                   
##                           "1.283266158183e-09, 0.0525281519315256, 0.297220257738547, 0.169714283322201, 0.976024942410966"                                                                                                     
##                           "1.4783669658311e-09, 0.0524964167325246, 0.292185273127104, 0.169331471766008, 0.968239719227605"                                                                                                    
##                           "4.91216373675011e-11, 0.018363129270609, 0.254619860353755, 0.93765162118793, 0.947271772081089"                                                                                                     
##                           "2.71202874021685e-10, 0.0212337297489232, 0.147047109431601, 0.872416777893544, 0.9371560445665"                                                                                                     
##                           "1.03314122931717e-08, 0.395121808294225, 0.412813200058028, 0.220827328461909, 0.12794401706429"                                                                                                     
##                           "4.57306830816558e-08, 0.212489395374168, 0.314656699554538, 0.194104544351742, 0.909761721554683"                                                                                                    
##                           "2.73748963972225e-08, 0.192782260823674, 0.345173871171736, 0.190417962852286, 0.757055932012486"                                                                                                    
##                           "1.19974470485586e-08, 0.483884439078244, 0.166502243354264, 0.109014769667324, 0.802087483524997"                                                                                                    
##                           "1.02126404077363e-08, 0.463846866704999, 0.181338748248917, 0.117145785907847, 0.871079531530501"                                                                                                    
##                           "3.57326814420727e-08, 0.238394673688027, 0.11140365988476, 0.7841999230698, 0.719302642159447"                                                                                                       
##                           "2.39487856093963e-08, 0.363764411549541, 0.0978432189931578, 0.103601673489258, 0.990255234517439"                                                                                                   
##                           "1.41964885681762e-08, 0.353543645443141, 0.100728878011444, 0.116085575410051, 0.849361502333235"                                                                                                    
##                           "3.77654461944075e-08, 0.162117403082756, 0.0671333852242838, 0.995472633046638, 0.68392128756178"                                                                                                    
##                           "6.33933975737087e-09, 0.415406131942914, 0.0738726602632531, 0.846457934408171, 0.747003029345374"                                                                                                   
##                           "5.50343230081822e-09, 0.375318663165747, 0.270024243088266, 0.0744351978345768, 0.905771706243602"                                                                                                   
##                           "6.48420210051077e-09, 0.374682724854334, 0.251950323573196, 0.0718742712515933, 0.88786879575385"                                                                                                    
##                           "2.73462375605367e-09, 0.271920421739441, 0.23948646692075, 0.830769882366224, 0.973077630472509"                                                                                                     
##                           "6.90154812563256e-09, 0.161097693047101, 0.0671931650130676, 0.773170679714151, 0.940823256548415"                                                                                                   
##                           "1.00361212336185e-08, 0.116805687064515, 0.0568223272594186, 0.957217018864341, 0.954497796745091"                                                                                                   
##                           "0.750118786660829, 0.00640615709362171, 0.510453158809286, 0.348709029010134, 0.0441826400210602, 0.507598583361611"                                                                                 
##                           "0.71243757716169, 0.00671492835135811, 0.427784061010169, 0.23354719970449, 0.0421660434524529, 0.688026000885478"                                                                                   
##                           "0.969726555367064, 0.00454695441105217, 0.709800755275493, 0.436712925126123, 0.0213985728299119, 0.0390827752286028"                                                                                
##                           "0.578004852066923, 0.00290504199976164, 0.541078439237014, 0.106385149576765, 0.0153641390022337, 0.0251518190855683"                                                                                
##                           "0.576236343155088, 0.00550488801430359, 0.483473107603758, 0.205985336151217, 0.0563437699822975, 0.589889097438706"                                                                                 
##                           "0.000429734804727676, 0.0149720076865281, 0.426917537367297, 0.268175875177382, 0.426649466143776, 0.598666518171413"                                                                                
##                           "8.75215215797e-05, 0.0210011052402556, 0.169577027264511, 0.422232759466561, 0.320305399716166, 0.0650595624932418"                                                                                  
##                           "3.72404727362322e-05, 0.00554506125972913, 0.257167303107776, 0.189683614744986, 0.245304784692913, 0.0265186186631104"                                                                              
##                           "0.0122150746036757, 0.0105131629239375, 0.382430988224896, 0.276890070171288, 0.388653611128637, 0.358037807823615"                                                                                  
##                           "0.000156696479093575, 0.0209139944771546, 0.244782781589314, 0.262886491361589, 0.515603891496514, 0.0740267877122326"                                                                               
##                           "4.12139882970968e-05, 0.0052201871661879, 0.39473408266771, 0.113962983221846, 0.225593000910542, 0.0168145071336245"                                                                                
##                           "0.02180924866467, 0.0115498699983195, 0.488719461424859, 0.167688125634705, 0.544430672896847, 0.374335987808165"                                                                                    
##                           "7.39256705747634e-06, 0.00647908617024196, 0.118448492783732, 0.14370513415738, 0.0284857552105139, 0.00968849194074434"                                                                             
##                           "0.00331600506186181, 0.0160441803751652, 0.221024429376928, 0.223936962885541, 0.0885010740887858, 0.437772360174376"                                                                                
##                           "0.101113653412442, 0.00250232935093318, 0.327792179456251, 0.124429387454045, 0.00299214710863978, 0.0478461116403207"                                                                               
##                           "0.662535161643426, 0.00387604099862443, 0.396568244098625, 0.0415134917180585, 0.359312345870466, 0.838634862555372"                                                                                 
##                           "0.974502628822433, 0.00348783636184202, 0.740519621293108, 0.026018652415798, 0.469209987397956, 0.0322107139182186"                                                                                 
##                           "0.591295180540665, 0.0018141023881377, 0.478887290236253, 0.0188654628490846, 0.206387204251022, 0.0381757385023426"                                                                                 
##                           "0.598135411019271, 0.0034310794443019, 0.452492171624171, 0.0591967408324566, 0.304999362341146, 0.612654120953662"                                                                                  
##                           "0.839233177581156, 0.00241601516002015, 0.578726879407143, 0.018259499819168, 0.849226361716601, 0.0276817669488424"                                                                                 
##                           "0.580165930317796, 0.00203888782272664, 0.337221810683645, 0.0200434159484698, 0.309415470455666, 0.0319727336030778"                                                                                
##                           "0.476845403545255, 0.00289826823875708, 0.305539739989264, 0.0494713256830186, 0.55427242813489, 0.585179151552775"                                                                                  
##                           "0.873896860387145, 0.000646317591174809, 0.789381613805349, 0.00403593208613393, 0.00528157043871798, 0.00674704090283593"                                                                           
##                           "0.74706879322234, 0.00169218383957618, 0.605604826941397, 0.0197600057802711, 0.0262997261216563, 0.685167994773493"                                                                                 
##                           "0.183475255971476, 0.000223196014248849, 0.550850428909924, 0.0429662771438823, 0.00110292706797508, 0.112499313635584"                                                                              
##                           "0.000182833929587079, 0.0114516479166661, 0.21929436610206, 0.257942905347849, 0.712895817496811, 0.0513751224275395"                                                                                
##                           "0.000104804925376081, 0.00229995064905881, 0.418497245978951, 0.228774059268823, 0.359183898760679, 0.0224771237325056"                                                                              
##                           "0.0287272676950844, 0.00475458766755834, 0.513922331149336, 0.272827009731305, 0.696272241806802, 0.379429106763218"                                                                                 
##                           "1.25475161263086e-05, 0.00373068400063934, 0.103380705842032, 0.127322837095669, 0.0201963052321332, 0.00910377418767568"                                                                            
##                           "0.00331496554066868, 0.00929599596983693, 0.205406586206183, 0.220843391557351, 0.0656670206524082, 0.489804587356063"                                                                               
##                           "0.133273688476341, 0.00143694198818688, 0.350676975336441, 0.229128428441039, 0.00253794571088397, 0.0512350015696577"                                                                               
##                           "2.15641690603558e-05, 0.00266704970495395, 0.200405255881741, 0.199759478441094, 0.0216894255507741, 0.00584448872654632"                                                                            
##                           "0.00778833130284654, 0.00742042879555954, 0.315145122392411, 0.47283877478376, 0.0639661700699133, 0.471214336924256"                                                                                
##                           "0.195281148593398, 0.00126318180096725, 0.530608565543653, 0.369791611081573, 0.00217041559535733, 0.0581382227475951"                                                                               
##                           "0.0296651843072193, 0.00171659136652449, 0.175502055111646, 0.0353074437277143, 0.00156841257425126, 0.0656895705110987"                                                                             
##                           "0.880175847960666, 0.0012339179402574, 0.288345972162591, 0.0409995859999382, 0.486325582109742, 0.899616053523119"                                                                                  
##                           "0.698194076076523, 0.000435201659008813, 0.568162507325253, 0.00984739750685202, 0.595194330658762, 0.0365955705468873"                                                                              
##                           "0.991788576074991, 0.000275343146160952, 0.19703872226187, 0.0108626200163705, 0.429368534160512, 0.0154119704450077"                                                                                
##                           "0.957359193836764, 0.000793221629604761, 0.301094436984779, 0.0536660229071384, 0.482833272433959, 0.589443464826011"                                                                                
##                           "0.816039442950591, 0.000591110863148632, 0.391577252907168, 0.0114295836572193, 0.977806670208937, 0.0332963847792751"                                                                               
##                           "0.915938951407528, 0.000321090349832028, 0.106977152580415, 0.0163836699545772, 0.472276738718407, 0.00756958438102923"                                                                              
##                           "0.995297393624566, 0.00125247178471976, 0.169155819383881, 0.0643389693105892, 0.74426474611976, 0.567263042644808"                                                                                  
##                           "0.80553929884947, 7.98633123626296e-05, 0.300125236804354, 0.00136419691318165, 0.00947612635710466, 0.00287530990899878"                                                                            
##                           "0.830398084672144, 0.000413173036450655, 0.396697872109673, 0.0128089895192181, 0.0370143857598944, 0.69352663010268"                                                                                
##                           "0.484164806812066, 0.0001776410666641, 0.129912542320051, 0.0288909754902479, 0.00108480884842887, 0.135636136636702"                                                                                
##                           "0.000250593411104896, 0.00461576516649871, 0.515939524411915, 0.595353637403851, 0.536498960066533, 0.152808580010162"                                                                               
##                           "4.74403034489781e-05, 0.00159746053185141, 0.280932817486281, 0.594798254421383, 0.313944380883045, 0.0282648004271614"                                                                              
##                           "0.0186296228290259, 0.00397083381845978, 0.352308547754367, 0.595869172694933, 0.643403832800953, 0.40570872980535"                                                                                  
##                           "3.17823908755417e-05, 0.00146278968493509, 0.438617264905167, 0.432367034876749, 0.100579494684277, 0.0340063074058536"                                                                              
##                           "0.00648912730927937, 0.00320655111422873, 0.504043416284905, 0.533410316653224, 0.192264905588551, 0.451781121670394"                                                                                
##                           "0.107760074714282, 0.00102966601222763, 0.328564084964498, 0.621395972399831, 0.00388876403765579, 0.0575540014025722"                                                                               
##                           "2.13062766659751e-05, 0.00135190221221077, 0.364305970066963, 0.233289408283619, 0.0968674830473519, 0.0154961032745026"                                                                             
##                           "0.00787208887184772, 0.00419780982597542, 0.387653965659944, 0.528165020815243, 0.182494527795586, 0.481035839040607"                                                                                
##                           "0.0952500404289145, 0.00130723193092495, 0.250685583350081, 0.501573596534362, 0.00384250121594489, 0.0755130038414504"                                                                              
##                           "0.0385055386034103, 0.00108478495232546, 0.363573595364674, 0.155865333955044, 0.00352872572132268, 0.0725817423823726"                                                                              
##                           "0.816160202496968, 0.000333400955861049, 0.0108326449037727, 0.404547348472456, 0.890971111034752, 0.0243060318361382"                                                                               
##                           "0.944858028686604, 0.000320305326057057, 0.0207831037122159, 0.26539745693067, 0.61803925386432, 0.0218948233570417"                                                                                 
##                           "0.97028472630854, 0.000669033518135729, 0.0679281988448773, 0.277077372154639, 0.902829222306093, 0.575038009466313"                                                                                 
##                           "0.771426554566231, 6.47938898495987e-05, 0.00163016809127212, 0.406492637195927, 0.00705470869175804, 0.00524617158483771"                                                                           
##                           "0.781448281972193, 0.000232072900952437, 0.0132474487186014, 0.431992700281352, 0.0288193463447746, 0.721913547939049"                                                                               
##                           "0.541564742690903, 0.000122601997053095, 0.0375261555810173, 0.308295497415567, 0.00122671942644321, 0.130208994078422"                                                                              
##                           "0.776178328944037, 7.32749412485234e-05, 0.00249392422360082, 0.585531436531541, 0.00523627896077292, 0.00293658765404912"                                                                           
##                           "0.943388080712704, 0.000317513379846253, 0.01545459805078, 0.865740048640792, 0.0212334986313826, 0.667840271139526"                                                                                 
##                           "0.524460878598977, 0.000165578126365324, 0.0479922758946604, 0.510793291176406, 0.000960232333789523, 0.126564052160111"                                                                             
##                           "0.712317640695282, 4.28468063530211e-05, 0.00441115047243219, 0.00888709400026155, 0.000504976390353068, 0.28415191107662"                                                                           
##                           "3.42125081085731e-05, 0.000849133645773457, 0.489907407349842, 0.305102916612437, 0.070150426128668, 0.0178222794993072"                                                                             
##                           "0.0085132064580322, 0.00239406157410854, 0.470748135330294, 0.622079723991998, 0.143226517944664, 0.494946699266158"                                                                                 
##                           "0.127314543764399, 0.000818089879054212, 0.492488768545517, 0.562871290187063, 0.0032644623502601, 0.0704492733055278"                                                                               
##                           "0.0457716461018595, 0.000773307907361298, 0.48039484278832, 0.121079067637276, 0.00324844188274101, 0.0770774489875964"                                                                              
##                           "0.0437647757257702, 0.000935947572227246, 0.409826804532408, 0.0987459901699271, 0.00289088764664244, 0.0998050663144984"                                                                            
##                           "2.91270298444053e-07, 0.0405386205872051, 0.0680582875236895, 0.241875152286116, 0.416239066664133, 0.513288664746523"                                                                               
##                           "1.59656534180419e-08, 0.0141960897961623, 0.144425175224707, 0.394583088291972, 0.278909678638959, 0.0438978963475022"                                                                               
##                           "8.6763363870931e-08, 0.0223203717439134, 0.0482212813003289, 0.247332482114336, 0.238582845548546, 0.371256304210416"                                                                                
##                           "1.00146843663028e-07, 0.0236475873850055, 0.0609913103636822, 0.183999655371132, 0.298521379202616, 0.512634390776527"                                                                               
##                           "5.82197894706412e-08, 0.0249211560066535, 0.0628832904461526, 0.408674731739462, 0.46707238931214, 0.0559335330431854"                                                                               
##                           "2.28640592064302e-07, 0.0381695506267071, 0.0207927602533393, 0.248826171477926, 0.260766910075307, 0.331226034578106"                                                                               
##                           "3.59368101451378e-07, 0.0423634942497786, 0.0268324853616701, 0.196509950062548, 0.374094796777432, 0.542755981002711"                                                                               
##                           "4.15004083056104e-09, 0.0124284053718478, 0.0255748694933991, 0.455050312527996, 0.0288886000604931, 0.184430894316075"                                                                              
##                           "6.64564000699272e-09, 0.0168268030186852, 0.0332521152276199, 0.37511422972284, 0.0545286569020788, 0.625960615491603"                                                                               
##                           "7.13525580846213e-09, 0.0205992114696827, 0.00829972143224789, 0.15999131371285, 0.236829997430462, 0.361911251290181"                                                                               
##                           "9.73384445042574e-09, 0.0245301323502962, 0.113525821748707, 0.406136042751118, 0.782375081454118, 0.0367440727202734"                                                                               
##                           "6.63744896317789e-09, 0.0461393383809262, 0.0305564524107304, 0.477514907325268, 0.543072617810892, 0.392430608580891"                                                                               
##                           "8.38235184942416e-08, 0.0581817503013486, 0.032992691530835, 0.4909315276383, 0.602138758796711, 0.806117124796213"                                                                                  
##                           "1.84190756365994e-09, 0.012008122067277, 0.0984869974863779, 0.304678470619591, 0.0224521695293039, 0.179192353107852"                                                                               
##                           "9.76419298493313e-09, 0.0186351160019971, 0.10237322249778, 0.337280931293762, 0.0334182298444642, 0.873404383656172"                                                                                
##                           "8.35211479671634e-09, 0.034243821750957, 0.0237715345502118, 0.337483010973119, 0.378599740551324, 0.695391070074793"                                                                                
##                           "6.08507420600562e-09, 0.0193734983243689, 0.039662334026287, 0.501168753171539, 0.0290995822582946, 0.18045050253499"                                                                                
##                           "3.78995143371749e-08, 0.0301933754123769, 0.0415665780937535, 0.565839592179455, 0.0418782199325589, 0.8423580814493"                                                                                
##                           "5.50420991367106e-08, 0.0528092236187636, 0.00974934399405298, 0.375905072980739, 0.354397462332486, 0.703883482160121"                                                                              
##                           "1.89000710036134e-09, 0.0147763100341198, 0.0193843667993504, 0.0237228916371058, 0.171681747278497, 0.68492797454862"                                                                               
##                           "2.84739342053172e-08, 0.0163457963020139, 0.244315363829887, 0.139932048883172, 0.573624997917318, 0.0251648404002818"                                                                               
##                           "2.44605369937549e-07, 0.0316520425256505, 0.098199929162875, 0.127854631423025, 0.334724296236118, 0.39381315317653"                                                                                 
##                           "4.14444099463013e-07, 0.0331293167098689, 0.0794026156418131, 0.147314878421622, 0.443744084252981, 0.522695430605741"                                                                               
##                           "9.00894155513147e-09, 0.00703251650750437, 0.253692454369068, 0.0544552080171209, 0.0133543433826069, 0.176619835675818"                                                                             
##                           "1.75341633977382e-08, 0.00974426689510153, 0.223992663995663, 0.0735821608364863, 0.026081284518506, 0.664888930492481"                                                                              
##                           "1.38560387872142e-07, 0.014462129873346, 0.0589297738726452, 0.0481900666316665, 0.267421441661284, 0.358322263918395"                                                                               
##                           "4.97719071982587e-08, 0.0179352880834988, 0.225657103294804, 0.160818928252561, 0.0168781840636319, 0.175831286981539"                                                                               
##                           "1.09193853992083e-07, 0.0247276817956904, 0.197458797739467, 0.222322306227402, 0.0306079788542684, 0.633525121352669"                                                                               
##                           "5.91851639117357e-07, 0.0376915605507752, 0.0583168711283374, 0.110143162439381, 0.257500984731088, 0.402815065176276"                                                                               
##                           "1.17844046174997e-08, 0.0100026777114284, 0.183521124026072, 0.0138428812592961, 0.147499479818372, 0.47871903644304"                                                                                
##                           "9.89930848495447e-09, 0.0137710047733919, 0.130702308764077, 0.596610094495188, 0.00870030742728591, 0.182491613431177"                                                                              
##                           "4.25973157387423e-08, 0.0220064678592663, 0.139288515691838, 0.643189420354272, 0.0127550479652267, 0.945961438841029"                                                                               
##                           "2.4640137339915e-07, 0.0568891051026399, 0.136795205175742, 0.410930878565612, 0.466503462048674, 0.79855503067586"                                                                                  
##                           "1.14603848194337e-08, 0.00984787749276889, 0.0658234253285756, 0.00699057233877477, 0.180385208380922, 0.797874486224788"                                                                            
##                           "7.38083485028069e-08, 0.0233001675948876, 0.221294367312243, 0.00875649464544218, 0.186154695752733, 0.761554965369464"                                                                              
##                           "2.21158862386097e-08, 0.113573343397888, 0.597745904826342, 0.8359794318123, 0.316827645309792, 0.221649126628206"                                                                                   
##                           "4.89727622751395e-08, 0.0671900298239124, 0.431425633294219, 0.778153757665711, 0.283956142247121, 0.963616776052969"                                                                                
##                           "2.99533353666263e-08, 0.0683344825773241, 0.402758413389385, 0.806275669054791, 0.283553803903189, 0.799556484833485"                                                                                
##                           "1.68126838972181e-08, 0.0830305165629427, 0.705739126625294, 0.586042746837881, 0.195554380411471, 0.864447405748909"                                                                                
##                           "1.46341787204677e-08, 0.081104009126212, 0.680806030375925, 0.607379203652109, 0.206904089002859, 0.890429692317716"                                                                                 
##                           "2.19718662706711e-08, 0.0485751872239821, 0.473386917357812, 0.521692246437686, 0.854801760964217, 0.763668625065079"                                                                                
##                           "2.22625541872645e-08, 0.0722844253040208, 0.600193935977485, 0.265098144904532, 0.216263499570644, 0.993402138883829"                                                                                
##                           "1.24429480801248e-08, 0.0735505438773275, 0.592798510721357, 0.266703183574551, 0.228673062526303, 0.912919769913116"                                                                                
##                           "1.90727529825e-08, 0.0378798484114944, 0.403278905786199, 0.223237748694763, 0.99600223221773, 0.780922526203025"                                                                                    
##                           "2.91175836885216e-09, 0.0277959576167603, 0.696278708858997, 0.189069399107733, 0.894645925093509, 0.850653430628584"                                                                                
##                           "3.42373751679388e-09, 0.0878754181564218, 0.838537877957292, 0.35962663412421, 0.173673155883811, 0.961521076167121"                                                                                 
##                           "5.56105313296568e-09, 0.0879148931687551, 0.837296613966577, 0.348380106528074, 0.172216306756045, 0.950806558794768"                                                                                
##                           "5.77960113329683e-10, 0.0394742079117797, 0.79066702851375, 0.331769671483238, 0.918288634066339, 0.971040426188816"                                                                                 
##                           "4.02798097677409e-09, 0.0685647847964928, 0.605202774747775, 0.162020016402715, 0.852162618158299, 0.995535331050015"                                                                                
##                           "3.11970427949011e-09, 0.052567341799147, 0.298301903357415, 0.170048954144977, 0.974512541446798, 0.967018364544036"                                                                                 
##                           "2.10321050717961e-08, 0.399006488684088, 0.41153927708773, 0.231756332619066, 0.128868403024128, 0.955150713130549"                                                                                  
##                           "1.50734312750535e-08, 0.398689589762627, 0.423249075986831, 0.223540696159292, 0.136862043665993, 0.919710289906008"                                                                                 
##                           "5.08124442339978e-08, 0.198984104736362, 0.343110440131964, 0.201266760739091, 0.935896405771569, 0.763254445408226"                                                                                 
##                           "1.60322705806023e-08, 0.478403648686866, 0.178495715988776, 0.119122441781858, 0.812506413876414, 0.889885788170774"                                                                                 
##                           "2.92939392113765e-08, 0.354341420676835, 0.103474066333396, 0.116065970201638, 0.977488672914594, 0.848261240733254"                                                                                 
##                           "1.13685217642801e-08, 0.37036560120366, 0.267092858666086, 0.0735647331022331, 0.900013340132809, 0.881887673646577"                                                                                 
##                           "0.797730180730052, 0.00734053744253923, 0.5080932173995, 0.354604202262411, 0.0499475062163562, 0.58376155269996, 0.885639173765371"                                                                 
##                           "0.90185803537704, 0.00591756401840423, 0.794727276973755, 0.59355537384562, 0.0310533468413945, 0.643754577634276, 0.0444789460743307"                                                               
##                           "0.780197573181109, 0.00347412826729942, 0.638757456417878, 0.237559729005743, 0.0228530413965231, 0.491210768867296, 0.0241372230290853"                                                             
##                           "0.754518802494865, 0.00692600827035035, 0.565225685386144, 0.358085008300107, 0.0712044250966866, 0.560543092938921, 0.623726353221115"                                                              
##                           "0.979172075718413, 0.00524929116874199, 0.710088114196386, 0.451172381551368, 0.0254825751171789, 0.982145594272546, 0.0433713593960457"                                                             
##                           "0.846231698508568, 0.00320092939943121, 0.564434525035585, 0.155170898596365, 0.0247158009949059, 0.489445472080797, 0.0157156372690116"                                                             
##                           "0.707466228386399, 0.00703531883080802, 0.488315207760957, 0.248106538992703, 0.0665009168259485, 0.756448801372946, 0.61112742158105"                                                               
##                           "0.863811255339509, 0.00100540986457341, 0.962105187598621, 0.315081297768709, 0.00621550072904747, 0.011380007207279, 0.00310790196492006"                                                           
##                           "0.968107898153963, 0.00478148913946816, 0.740542247535502, 0.452570111039304, 0.0313026429996957, 0.0459781809459288, 0.711406925549101"                                                             
##                           "0.449310653800851, 0.000627988209521743, 0.760295102162059, 0.153753463225305, 0.0483097502798885, 0.00137074290073763, 0.149056463569796"                                                           
##                           "0.000128469064673855, 0.021209268518984, 0.199363665893026, 0.442304100077297, 0.41865534902175, 0.773257863445132, 0.0731910262796775"                                                              
##                           "3.92373978766663e-05, 0.00527021017621925, 0.331499245290086, 0.221262872161439, 0.466808599801842, 0.42769072813217, 0.0184209266072183"                                                            
##                           "0.017348191917209, 0.0115727933560903, 0.420422692244289, 0.29084709381894, 0.493646405379925, 0.766907193716118, 0.398383049729393"                                                                 
##                           "7.48918494805675e-06, 0.00669755435057584, 0.0952353267421799, 0.378541364463512, 0.296562695493559, 0.0330357547897463, 0.00928565953543248"                                                        
##                           "0.00245971129991947, 0.0174298337418705, 0.188993497402699, 0.440781721808026, 0.410297473009049, 0.0921556104567556, 0.498985738036023"                                                             
##                           "0.0822647561445045, 0.00292987493675916, 0.288506213532938, 0.263283248215114, 0.509312318538595, 0.00349029479956102, 0.0619620367041391"                                                           
##                           "1.0088073399148e-05, 0.00608719256451151, 0.157132623052171, 0.25511008519889, 0.354149703355863, 0.040126044847697, 0.00727174675980452"                                                            
##                           "0.00472534444295697, 0.0172666702221618, 0.24952599476782, 0.287091464415804, 0.664175764531114, 0.0988193183741803, 0.488670772969792"                                                              
##                           "0.103758021140405, 0.00302188087069634, 0.368107330866746, 0.179836008696551, 0.594325343877719, 0.00341279755736064, 0.0723014444661581"                                                            
##                           "0.0165903187380362, 0.00388743271544697, 0.143403059953453, 0.268983051135764, 0.0644861005428874, 0.00267913120205437, 0.08741298995931"                                                            
##                           "0.994301337279934, 0.00368482849696159, 0.7453889256241, 0.026915478451913, 0.477530789444947, 0.903628393399343, 0.0332423812352381"                                                                
##                           "0.74950470938716, 0.00264564306240166, 0.484736541093448, 0.0266903975644745, 0.363858673144769, 0.625498633562074, 0.0327287673964098"                                                              
##                           "0.653843996054175, 0.00410507821330924, 0.449982176445083, 0.0642314809617223, 0.391324791404761, 0.884634360799096, 0.620252861049567"                                                              
##                           "0.843258408917924, 0.00110374136478136, 0.963671142268884, 0.00863440432274249, 0.429488191247141, 0.0087573303483051, 0.00553247974356397"                                                          
##                           "0.986432219499978, 0.00362365846476297, 0.761580397217833, 0.0349037266460223, 0.500782184030747, 0.0370968576400328, 0.734550666712344"                                                             
##                           "0.481295310566787, 0.000480558760443541, 0.718477055540196, 0.0590620070233232, 0.363336703024131, 0.00165647649459674, 0.146859612900339"                                                           
##                           "0.907515910466621, 0.000839862760805317, 0.819574866069862, 0.00744493002647765, 0.597794996811418, 0.00789275798076777, 0.00420957880034299"                                                        
##                           "0.826214829164447, 0.00250929219173505, 0.611918947433743, 0.0256713672597665, 0.888088701472521, 0.032323616240427, 0.695132780475226"                                                              
##                           "0.419940582403776, 0.00038289168265106, 0.54956533958772, 0.0512793803982288, 0.510112959072631, 0.00127316653485883, 0.155410008764952"                                                             
##                           "0.735125688404019, 0.000189257968569162, 0.972493500131617, 0.0128999761588648, 0.0105578704245557, 0.000558621612673312, 0.288535745307478"                                                         
##                           "1.43080478157149e-05, 0.0037038910536673, 0.140665772060786, 0.280640706947102, 0.510481908658768, 0.0253831788047282, 0.00694745077272142"                                                          
##                           "0.00471760485271569, 0.0100755814172988, 0.229295380065338, 0.305322879251724, 0.825276487732921, 0.0700774170371209, 0.51299314463344"                                                              
##                           "0.130474465760165, 0.00170708303511839, 0.399637186796903, 0.366176715543466, 0.694946047768129, 0.00278293606579499, 0.0677831025822925"                                                            
##                           "0.0178669126887311, 0.00291156675223491, 0.134297158513775, 0.312783129493307, 0.0450339131898781, 0.00233888248690797, 0.0986646169089747"                                                          
##                           "0.0306914313365316, 0.00235865336338083, 0.201843421147384, 0.516424634610876, 0.0437615279015707, 0.00196239006328651, 0.10305903314671"                                                            
##                           "0.779819543420122, 0.000557992952354922, 0.561574312954534, 0.0112557502907755, 0.576749431895897, 0.862525857012095, 0.0366855903051733"                                                            
##                           "0.867344756769078, 0.000340316912835957, 0.20604108331807, 0.0163085216595015, 0.584764850604349, 0.667268975195469, 0.010201241980624"                                                              
##                           "0.941911137395601, 0.00113706049951223, 0.303792792238479, 0.0604615320334601, 0.531610036653551, 0.954050590169757, 0.594277718722073"                                                              
##                           "0.720380300649404, 8.82900029040576e-05, 0.459101238573343, 0.00165631189517043, 0.664482275115005, 0.0120037778153672, 0.00334228431439707"                                                         
##                           "0.731571790312825, 0.000429962816237266, 0.574086366554728, 0.0140960756753584, 0.631179141846461, 0.043303593674929, 0.729264395705777"                                                             
##                           "0.618311270407374, 0.000195633372543725, 0.242582952597061, 0.0300578375177874, 0.678966136069441, 0.00129675284611959, 0.15376573858895"                                                            
##                           "0.71311000887182, 9.77961154205255e-05, 0.351668332305581, 0.00240417225988695, 0.754517910872525, 0.0118737619550965, 0.00219539348612762"                                                          
##                           "0.85832127956576, 0.000572747453199394, 0.406689033318541, 0.0158062779101962, 0.985515996543584, 0.0399855431590334, 0.69388825529513"                                                              
##                           "0.653952517765513, 0.000259110791567425, 0.164193498608765, 0.0343984979065147, 0.761159202559287, 0.00124242471768707, 0.161075138494601"                                                           
##                           "0.873522634978771, 8.35936668857325e-05, 0.402885282184714, 0.00459249873804677, 0.0192570634179159, 0.000650561964070584, 0.315663013099244"                                                        
##                           "2.33988088543969e-05, 0.00138785440177395, 0.487893147923431, 0.694250652483712, 0.332442431932415, 0.105449512585682, 0.0174430479641258"                                                           
##                           "0.00718341508270772, 0.00414397928636868, 0.525604617719296, 0.659041134132498, 0.654101007855083, 0.193954469723207, 0.497223058103843"                                                             
##                           "0.0933869349844081, 0.00133342822483793, 0.343821709519136, 0.7850408116453, 0.597561849686709, 0.00410749760980952, 0.0788554693274567"                                                             
##                           "0.0398151203281806, 0.00115513534770993, 0.506374560996571, 0.70997928576845, 0.167635733646193, 0.0039153838265714, 0.082300206575884"                                                              
##                           "0.0356236977904568, 0.0014376375298335, 0.460719019746834, 0.521530717626324, 0.160149664656354, 0.00389499148429237, 0.108012012873575"                                                             
##                           "0.721322907041312, 8.98251933083248e-05, 0.00252213931780294, 0.50471708105196, 0.830168126759963, 0.00789290521501956, 0.00436813238448206"                                                         
##                           "0.857774911364931, 0.000326852880532543, 0.0147002673994872, 0.432097698498173, 0.870154931627668, 0.029123380760237, 0.716428267735801"                                                             
##                           "0.644324451159373, 0.000179176698837696, 0.0430731728627242, 0.413230967151125, 0.82019826119428, 0.00130089927819748, 0.145936770917215"                                                            
##                           "0.889859103160792, 6.00456255083921e-05, 0.00493851094996469, 0.613127148487418, 0.0135136984269445, 0.000723669976235956, 0.31514455964352"                                                         
##                           "0.864151882938458, 6.8630546602864e-05, 0.00569777639259013, 0.78792280924032, 0.0107734681450288, 0.000549412493154477, 0.310698943444059"                                                          
##                           "0.0401138714941301, 0.00104888630962206, 0.673650802420051, 0.553977435357084, 0.119822388875758, 0.00342786534804014, 0.106592252269268"                                                            
##                           "5.97683255488819e-08, 0.0193138367708416, 0.163164475586672, 0.376093649034924, 0.374252472162059, 0.712220901392024, 0.0522279162771844"                                                            
##                           "2.6945634622029e-07, 0.0295585169057767, 0.0625557975792636, 0.223335981017795, 0.417191902657392, 0.472334144608275, 0.32995854459874"                                                              
##                           "4.41423821411936e-07, 0.033893832820919, 0.0736448663659973, 0.182285000593576, 0.441916364048431, 0.591477236945093, 0.560445838342097"                                                             
##                           "1.39250698270605e-08, 0.00901082857152459, 0.137266853186752, 0.38198513321926, 0.262644405854084, 0.0313774073899287, 0.164309477509956"                                                            
##                           "2.60069871968454e-08, 0.0129820534707396, 0.148009783456734, 0.339316991098219, 0.306651717545591, 0.0560737133187687, 0.68234073013837"                                                             
##                           "9.00720482807398e-08, 0.0156693336104933, 0.0563059132271998, 0.144578985091764, 0.295559551352278, 0.229429168269463, 0.414794024807342"                                                            
##                           "4.84904997466723e-08, 0.0156566982010498, 0.0600617003905078, 0.388787513447634, 0.42366351963548, 0.041057948967158, 0.162013679752082"                                                             
##                           "1.02385726370783e-07, 0.022477353719453, 0.06627756447362, 0.346686064790227, 0.51344256334313, 0.0680522073045264, 0.668608423957602"                                                               
##                           "3.19420825686316e-07, 0.0277055209404025, 0.0248618070282467, 0.156470823981827, 0.356875955219288, 0.220007531254796, 0.457586253402285"                                                            
##                           "5.36866020009638e-09, 0.0100824283298842, 0.0312273087465705, 0.32339964953464, 0.0420274071736244, 0.130939483715802, 0.539953007770757"                                                            
##                           "7.69519326837242e-09, 0.0154648955277955, 0.109945177611022, 0.397318333702096, 0.742026929131686, 0.0260685698257291, 0.173160336585742"                                                            
##                           "4.5198896696327e-08, 0.0244609057354261, 0.11250921556489, 0.415109470745382, 0.794952941479369, 0.0377815340253157, 0.894378095096842"                                                              
##                           "8.09920814823337e-08, 0.0445613349123045, 0.0295220089790182, 0.499063136466502, 0.577024363152554, 0.360381578162985, 0.738399003081029"                                                            
##                           "9.09087852264782e-09, 0.0118524446722431, 0.0965594523793492, 0.330239166694879, 0.0235234704745076, 0.163767136346869, 0.763667406841594"                                                           
##                           "3.25015282973168e-08, 0.0190230002341822, 0.0392023630640532, 0.542786281018593, 0.02996045120768, 0.164338189869719, 0.742529549169057"                                                             
##                           "2.46446700349687e-08, 0.0103561621401343, 0.232811727694574, 0.134651970992325, 0.525570479394062, 0.0178529401932123, 0.159705372633312"                                                            
##                           "5.6122598415588e-08, 0.0150484725706027, 0.215679532184166, 0.15189040294899, 0.608397929225637, 0.0317194692871578, 0.692552647110066"                                                              
##                           "3.6773613926039e-07, 0.0228641699319781, 0.06213770065925, 0.147006838094497, 0.425590068261763, 0.252126166697296, 0.433202442033366"                                                               
##                           "1.47304059017274e-08, 0.0059732205910195, 0.193552550857742, 0.069807532378824, 0.0195803369681431, 0.127332219126883, 0.569499051300806"                                                            
##                           "9.01561925889588e-08, 0.0148549646674333, 0.16848032309777, 0.205119892374907, 0.0230078520253724, 0.128631719701528, 0.555091843388581"                                                             
##                           "3.69031318057023e-08, 0.013975776439381, 0.135964187285158, 0.614164479538858, 0.00881880858549765, 0.173234163353094, 0.833379626038014"                                                            
##                           "4.11248415809723e-08, 0.113881132098777, 0.599262012198444, 0.835875825742461, 0.324723194978153, 0.222025472149169, 0.991591690848135"                                                              
##                           "3.06952424369202e-08, 0.113732256549645, 0.595207610542744, 0.844634872979018, 0.31973923135288, 0.232347674100863, 0.929272382731595"                                                               
##                           "5.34710760540666e-08, 0.0686846637097779, 0.406557284052158, 0.805674884172028, 0.291445754439877, 0.984414809191661, 0.801600068384058"                                                             
##                           "2.16582772119414e-08, 0.0832475070416398, 0.690859809372878, 0.601426605514064, 0.20840391779783, 0.873806200083726, 0.9026281847299"                                                                
##                           "2.62142606742591e-08, 0.0735676019480165, 0.593365528139238, 0.27044741557406, 0.228658694150901, 0.986449273827941, 0.912204343626755"                                                              
##                           "9.0707968014865e-09, 0.0887824751514093, 0.834047315155832, 0.35842899922425, 0.173329847621232, 0.958719831326363, 0.948387886808577"                                                               
##                           "2.75781719288382e-08, 0.403703691754343, 0.42247456576105, 0.233484013840251, 0.137321142016353, 0.96209563994038, 0.923187760173918"                                                                
##                           "0.942504466112421, 0.00598905783564175, 0.80291046946782, 0.58713034909304, 0.0312843654810875, 0.625403378294846, 0.875180745994849, 0.044861494797071"                                             
##                           "0.909468396033001, 0.00362356568152567, 0.630222655670231, 0.244411449504079, 0.0286632814797556, 0.657062682600622, 0.657272150475184, 0.0181064545020172"                                          
##                           "0.78706332375865, 0.0076628053351059, 0.562689680892184, 0.362090485039602, 0.0750568356913262, 0.616239854081256, 0.93108394774224, 0.629280133884886"                                              
##                           "0.757439444519546, 0.00121690964666351, 0.968994100268929, 0.45873008327204, 0.0091582017259015, 0.664855744939646, 0.0132668892297166, 0.00319600919786024"                                         
##                           "0.91383233680042, 0.00610455036861771, 0.815157052909012, 0.597139046118524, 0.0411039795481477, 0.673843606899355, 0.050562375699721, 0.739096073386239"                                            
##                           "0.584317179259946, 0.000808824838638813, 0.814749399756418, 0.255260101117713, 0.0563094282463373, 0.714210317266287, 0.00166766191138349, 0.163473620754145"                                        
##                           "0.774238149788941, 0.00100621749187624, 0.960223641935075, 0.365470424438745, 0.00855909602141188, 0.754228283584506, 0.0140620855299401, 0.00241144733362218"                                       
##                           "0.96465128011823, 0.00538559444096085, 0.740498300828182, 0.460962475232323, 0.0346014980467047, 0.984682879915812, 0.0492266671570081, 0.711561667695157"                                           
##                           "0.591148727724481, 0.000781478796053718, 0.745854303070171, 0.19413475135143, 0.0528282941665752, 0.746929422710823, 0.00152951978425693, 0.177470921553468"                                         
##                           "0.927819881849742, 0.000425505583480322, 0.925164657462043, 0.400131020317112, 0.0159106787960084, 0.0201369628615885, 0.000734408065733731, 0.312649032740028"                                      
##                           "9.3675048522511e-06, 0.00644781585519784, 0.128904059966002, 0.41800622239247, 0.453635454122861, 0.573013158943322, 0.0394639713980864, 0.00778804028569205"                                        
##                           "0.00371057600923404, 0.0179936015981214, 0.20869747968966, 0.451723278487787, 0.473465947896767, 0.883572209639146, 0.0962106417614349, 0.516497002380282"                                           
##                           "0.0907675099773373, 0.0032172296823787, 0.327338278542647, 0.276764622917702, 0.62314262745255, 0.767821269759224, 0.00369801738028642, 0.0770212982049889"                                          
##                           "0.0148316745294661, 0.00452585792194558, 0.12631563111992, 0.453784798125597, 0.527235514055382, 0.0658799377933209, 0.00307800239675473, 0.107639479089111"                                         
##                           "0.0224252161137455, 0.00433238160104616, 0.164125331400997, 0.333745739082386, 0.700903468114379, 0.0698647221568456, 0.00292913759158068, 0.114361903785895"                                        
##                           "0.793698893416041, 0.00116357546855321, 0.956682227154003, 0.010406019334677, 0.527961060426633, 0.828613092367092, 0.00979900153039504, 0.00470314864162154"                                        
##                           "0.976519489757008, 0.00376864024156832, 0.768397274463053, 0.035169520430795, 0.499224689449009, 0.883453306107921, 0.0376705981610754, 0.729433365687134"                                           
##                           "0.561504836566149, 0.000563810619863831, 0.695792219761396, 0.0613441833718937, 0.49091158013873, 0.785733308064167, 0.00169278309059606, 0.166357652747749"                                         
##                           "0.944089882411809, 0.000358242626187534, 0.929952106944209, 0.0183980004878481, 0.608260654666867, 0.0142210083520695, 0.00079224426972405, 0.31213565650504"                                        
##                           "0.862471302727425, 0.000259383506764086, 0.965856114991221, 0.0149551123031858, 0.786989995027796, 0.0128218665658888, 0.000609300575417505, 0.315465691124708"                                      
##                           "0.0223710185754875, 0.00318010089394046, 0.155839981381483, 0.415033814016946, 0.813687816518484, 0.0476911985304549, 0.00249108317503482, 0.116415618972388"                                        
##                           "0.691368781546642, 0.000108369054268419, 0.466707551950613, 0.0024432185015773, 0.729946382235876, 0.866817607341565, 0.0131021779879097, 0.00285759988053701"                                       
##                           "0.820451985385246, 0.000545467677237062, 0.566518889933225, 0.0152146840080425, 0.603737245363723, 0.842789350898224, 0.0428803364414873, 0.722503223816605"                                         
##                           "0.695447094531523, 0.000258897559781951, 0.247392877201349, 0.0338838985491656, 0.746971073439384, 0.875256982967587, 0.00137363295573989, 0.167549646717613"                                        
##                           "0.925687245769174, 8.92692008317316e-05, 0.495168631455491, 0.00484383249273553, 0.862040971131869, 0.0209420132329056, 0.00073214671842852, 0.32546310988424"                                       
##                           "0.919873786432697, 0.000105389521222785, 0.426933868252985, 0.00542199412591653, 0.937342142333786, 0.0202743555006984, 0.000681609436317889, 0.327688608797464"                                     
##                           "0.0363986838953035, 0.0014616848611782, 0.533749470381065, 0.866617514495538, 0.585231543201205, 0.16551610108666, 0.00406987303693563, 0.110520303988619"                                           
##                           "0.923332978948001, 7.70874509759946e-05, 0.00565759664570418, 0.663438824301868, 0.942866129309529, 0.013900272877759, 0.000729094157318047, 0.324324629722112"                                      
##                           "5.04886226970056e-08, 0.0120688797229035, 0.158482248964333, 0.358219122269853, 0.366234720817601, 0.663379216356057, 0.0378555386562782, 0.154539719946746"                                         
##                           "1.09762280650763e-07, 0.0178303551294133, 0.164790196951055, 0.329454526135252, 0.392922887026045, 0.747406572131325, 0.0630245467387296, 0.700433522330641"                                         
##                           "3.89373831554019e-07, 0.0224507622170868, 0.068525131796506, 0.145165651636162, 0.446135030069809, 0.5703897416902, 0.220422036045722, 0.469590140266214"                                            
##                           "2.14444666887124e-08, 0.00774685780508131, 0.142831582418066, 0.292580157221279, 0.298332829990509, 0.0425738878006131, 0.122635968319087, 0.584834464092249"                                        
##                           "8.37780892003724e-08, 0.0133138580248538, 0.0644631369832951, 0.296440945511389, 0.487096118713146, 0.0520036979259294, 0.122793896376901, 0.581004061766205"                                        
##                           "3.99249858245541e-08, 0.0153923489551394, 0.107480747873756, 0.41296753704496, 0.768952302901229, 0.0267953910941659, 0.160761755517398, 0.786437378952778"                                          
##                           "4.71901569809373e-08, 0.00908929352379716, 0.184961971837973, 0.149523561790849, 0.577922814187983, 0.0235610870781865, 0.121194902134768, 0.597932514120396"                                        
##                           "5.21567042028334e-08, 0.113981597620189, 0.597075406915329, 0.845170645544026, 0.326650077549802, 0.232452966072828, 0.997864362811733, 0.929702356197231"                                           
##                           "0.728584202557197, 0.00119765880059445, 0.979485310912104, 0.467238586503666, 0.0104284747219811, 0.731630882391653, 0.86885386107576, 0.014654732918517, 0.00283743669685979"                       
##                           "0.960023359623588, 0.00610623179018667, 0.825778197083412, 0.589522477888256, 0.0406160235858483, 0.646585202374313, 0.855361361080876, 0.050344636719444, 0.732534535112115"                        
##                           "0.643320793729279, 0.000886318804360231, 0.795627560167359, 0.262384601638925, 0.0577233018040924, 0.795760338658775, 0.848289815245926, 0.00172116384030607, 0.180649279090902"                     
##                           "0.992191964461454, 0.000526105442387803, 0.896605717459048, 0.490796487404251, 0.0188496175429834, 0.844712460454135, 0.0212765584212732, 0.00085211242580621, 0.321098939748445"                    
##                           "0.957371263050328, 0.00047088178983867, 0.929701317573417, 0.424718846901714, 0.0168942965172924, 0.942891184420338, 0.0213579772968126, 0.000765438126704415, 0.325163854084948"                    
##                           "0.0196395632928308, 0.00472438668655364, 0.144507784338975, 0.466136240254528, 0.59681911428962, 0.871853040147112, 0.0683203506985737, 0.00318676320220962, 0.122089295182617"                      
##                           "0.961035702575684, 0.000382045094116947, 0.937211053239771, 0.0187711362383233, 0.659375159362486, 0.952118412081139, 0.0148324870647264, 0.000799292133207911, 0.322181643385252"                   
##                           "0.938629543775539, 0.000106810473352315, 0.497418987422283, 0.00543396762933649, 0.875755241228639, 0.983831971893532, 0.0212728728467145, 0.000745497034962955, 0.331801537063618"                  
##                           "9.09624889990729e-08, 0.0106056925641156, 0.161118825376769, 0.282272843655263, 0.389997771236458, 0.717697589378421, 0.0477793343030483, 0.119192185423883, 0.6040316749212"                        
##                           "0.992577899288603, 0.000540043454771139, 0.897865690750236, 0.492502331250434, 0.0190262339365153, 0.855286506873317, 0.999795201588765, 0.0217750799440962, 0.000858592375023869, 0.327432556625719"
```
