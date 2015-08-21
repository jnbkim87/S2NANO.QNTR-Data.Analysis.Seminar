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

variable <- c(1:(ncol(raw_data)-3))

Logistic_regression.model <- c("sensitivity","specificity","accuracy","all variables","model.coef","model.p-value")
for (i in c(1:(ncol(raw_data)-3)))
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
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning in confusionMatrix.default(Logistic_regression.validation,
## validation_data.subset$y02): Levels are not in the same order for reference
## and data. Refactoring data to match.
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
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
##                           "0"                  "0.993150684931507"
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
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
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
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.193548387096774"  "0.986301369863014"
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
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
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
##                           "0"                  "0.986301369863014"
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
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
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
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.986301369863014"
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
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.193548387096774"  "0.958904109589041"
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.129032258064516"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
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
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
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
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "0.986301369863014"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.986301369863014"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
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
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.032258064516129"  "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "1"                
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
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "1"                
##                           "0.0645161290322581" "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0.032258064516129"  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.986301369863014"
##                           "0.0645161290322581" "0.945205479452055"
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
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.129032258064516"  "1"                
##                           "0.129032258064516"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.979452054794521"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
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
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
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
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
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
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "0.986301369863014"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.986301369863014"
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "1"                
##                           "0.0645161290322581" "0.993150684931507"
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
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.993150684931507"
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.993150684931507"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "0.993150684931507"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "1"                
##                           "0.0645161290322581" "1"                
##                           "0.032258064516129"  "0.993150684931507"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.952054794520548"
##                           "0.0645161290322581" "0.952054794520548"
##                           "0.0645161290322581" "0.965753424657534"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.945205479452055"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0.129032258064516"  "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.979452054794521"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.129032258064516"  "1"                
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
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
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
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
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.993150684931507"
##                           "0.032258064516129"  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "0.993150684931507"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0"                  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0967741935483871" "0.965753424657534"
##                           "0.0645161290322581" "0.993150684931507"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0.0967741935483871" "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0967741935483871" "1"                
##                           "0.0967741935483871" "1"                
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.0967741935483871" "0.972602739726027"
##                           "0.0967741935483871" "0.972602739726027"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.993150684931507"
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "0.993150684931507"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.945205479452055"
##                           "0"                  "0.993150684931507"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "1"                
##                           "0.0967741935483871" "1"                
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "0.986301369863014"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.129032258064516"  "0.979452054794521"
##                           "0.129032258064516"  "0.979452054794521"
##                           "0.0967741935483871" "0.979452054794521"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.161290322580645"  "0.965753424657534"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "0.993150684931507"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.0967741935483871" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "1"                
##                           "0.032258064516129"  "1"                
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.032258064516129"  "1"                
##                           "0.032258064516129"  "0.993150684931507"
##                           "0.0645161290322581" "0.965753424657534"
##                           "0.0967741935483871" "0.958904109589041"
##                           "0.0645161290322581" "0.972602739726027"
##                           "0"                  "1"                
##                           "0.129032258064516"  "0.952054794520548"
##                           "0.0967741935483871" "0.979452054794521"
##                           "0"                  "0.993150684931507"
##                           "0.0967741935483871" "0.972602739726027"
##                           "0.0967741935483871" "0.958904109589041"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "1"                
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0967741935483871" "0.972602739726027"
##                           "0.032258064516129"  "0.993150684931507"
##                           "0.0645161290322581" "0.958904109589041"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0967741935483871" "1"                
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.129032258064516"  "0.979452054794521"
##                           "0"                  "1"                
##                           "0"                  "1"                
##                           "0.0967741935483871" "0.972602739726027"
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.032258064516129"  "1"                
##                           "0.0645161290322581" "0.972602739726027"
##                           "0.0967741935483871" "0.979452054794521"
##                           "0.032258064516129"  "0.993150684931507"
##                           "0.0967741935483871" "0.958904109589041"
##                           "0.0967741935483871" "0.958904109589041"
##                           "0.0645161290322581" "0.979452054794521"
##                           "0.0645161290322581" "0.965753424657534"
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
##                           "0.819209039548023"
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
##                           "0.819209039548023"
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
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.847457627118644"
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
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
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
##                           "0.813559322033898"
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
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.830508474576271"
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
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.80225988700565" 
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
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.847457627118644"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
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
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
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
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
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
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
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
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.836158192090395"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.80225988700565" 
##                           "0.80225988700565" 
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.790960451977401"
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
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.847457627118644"
##                           "0.847457627118644"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
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
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
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
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.819209039548023"
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
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.836158192090395"
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
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.813559322033898"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.836158192090395"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.80225988700565" 
##                           "0.80225988700565" 
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.80225988700565" 
##                           "0.80225988700565" 
##                           "0.824858757062147"
##                           "0.796610169491525"
##                           "0.796610169491525"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.790960451977401"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.84180790960452" 
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.847457627118644"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
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
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.819209039548023"
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
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.813559322033898"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.836158192090395"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.84180790960452" 
##                           "0.84180790960452" 
##                           "0.830508474576271"
##                           "0.813559322033898"
##                           "0.80225988700565" 
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.836158192090395"
##                           "0.813559322033898"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.80225988700565" 
##                           "0.80225988700565" 
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.790960451977401"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.836158192090395"
##                           "0.84180790960452" 
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.813559322033898"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.819209039548023"
##                           "0.836158192090395"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.807909604519774"
##                           "0.819209039548023"
##                           "0.836158192090395"
##                           "0.80225988700565" 
##                           "0.819209039548023"
##                           "0.824858757062147"
##                           "0.80225988700565" 
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.84180790960452" 
##                           "0.819209039548023"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.819209039548023"
##                           "0.813559322033898"
##                           "0.819209039548023"
##                           "0.830508474576271"
##                           "0.813559322033898"
##                           "0.824858757062147"
##                           "0.824858757062147"
##                           "0.807909604519774"
##                           "0.807909604519774"
##                           "0.819209039548023"
##                           "0.807909604519774"
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
##                           "x10, y02"                                             
##                           "x01, x02, y02"                                        
##                           "x01, x03, y02"                                        
##                           "x01, x04, y02"                                        
##                           "x01, x05, y02"                                        
##                           "x01, x06, y02"                                        
##                           "x01, x07, y02"                                        
##                           "x01, x08, y02"                                        
##                           "x01, x09, y02"                                        
##                           "x01, x10, y02"                                        
##                           "x02, x03, y02"                                        
##                           "x02, x04, y02"                                        
##                           "x02, x05, y02"                                        
##                           "x02, x06, y02"                                        
##                           "x02, x07, y02"                                        
##                           "x02, x08, y02"                                        
##                           "x02, x09, y02"                                        
##                           "x02, x10, y02"                                        
##                           "x03, x04, y02"                                        
##                           "x03, x05, y02"                                        
##                           "x03, x06, y02"                                        
##                           "x03, x07, y02"                                        
##                           "x03, x08, y02"                                        
##                           "x03, x09, y02"                                        
##                           "x03, x10, y02"                                        
##                           "x04, x05, y02"                                        
##                           "x04, x06, y02"                                        
##                           "x04, x07, y02"                                        
##                           "x04, x08, y02"                                        
##                           "x04, x09, y02"                                        
##                           "x04, x10, y02"                                        
##                           "x05, x06, y02"                                        
##                           "x05, x07, y02"                                        
##                           "x05, x08, y02"                                        
##                           "x05, x09, y02"                                        
##                           "x05, x10, y02"                                        
##                           "x06, x07, y02"                                        
##                           "x06, x08, y02"                                        
##                           "x06, x09, y02"                                        
##                           "x06, x10, y02"                                        
##                           "x07, x08, y02"                                        
##                           "x07, x09, y02"                                        
##                           "x07, x10, y02"                                        
##                           "x08, x09, y02"                                        
##                           "x08, x10, y02"                                        
##                           "x09, x10, y02"                                        
##                           "x01, x02, x03, y02"                                   
##                           "x01, x02, x04, y02"                                   
##                           "x01, x02, x05, y02"                                   
##                           "x01, x02, x06, y02"                                   
##                           "x01, x02, x07, y02"                                   
##                           "x01, x02, x08, y02"                                   
##                           "x01, x02, x09, y02"                                   
##                           "x01, x02, x10, y02"                                   
##                           "x01, x03, x04, y02"                                   
##                           "x01, x03, x05, y02"                                   
##                           "x01, x03, x06, y02"                                   
##                           "x01, x03, x07, y02"                                   
##                           "x01, x03, x08, y02"                                   
##                           "x01, x03, x09, y02"                                   
##                           "x01, x03, x10, y02"                                   
##                           "x01, x04, x05, y02"                                   
##                           "x01, x04, x06, y02"                                   
##                           "x01, x04, x07, y02"                                   
##                           "x01, x04, x08, y02"                                   
##                           "x01, x04, x09, y02"                                   
##                           "x01, x04, x10, y02"                                   
##                           "x01, x05, x06, y02"                                   
##                           "x01, x05, x07, y02"                                   
##                           "x01, x05, x08, y02"                                   
##                           "x01, x05, x09, y02"                                   
##                           "x01, x05, x10, y02"                                   
##                           "x01, x06, x07, y02"                                   
##                           "x01, x06, x08, y02"                                   
##                           "x01, x06, x09, y02"                                   
##                           "x01, x06, x10, y02"                                   
##                           "x01, x07, x08, y02"                                   
##                           "x01, x07, x09, y02"                                   
##                           "x01, x07, x10, y02"                                   
##                           "x01, x08, x09, y02"                                   
##                           "x01, x08, x10, y02"                                   
##                           "x01, x09, x10, y02"                                   
##                           "x02, x03, x04, y02"                                   
##                           "x02, x03, x05, y02"                                   
##                           "x02, x03, x06, y02"                                   
##                           "x02, x03, x07, y02"                                   
##                           "x02, x03, x08, y02"                                   
##                           "x02, x03, x09, y02"                                   
##                           "x02, x03, x10, y02"                                   
##                           "x02, x04, x05, y02"                                   
##                           "x02, x04, x06, y02"                                   
##                           "x02, x04, x07, y02"                                   
##                           "x02, x04, x08, y02"                                   
##                           "x02, x04, x09, y02"                                   
##                           "x02, x04, x10, y02"                                   
##                           "x02, x05, x06, y02"                                   
##                           "x02, x05, x07, y02"                                   
##                           "x02, x05, x08, y02"                                   
##                           "x02, x05, x09, y02"                                   
##                           "x02, x05, x10, y02"                                   
##                           "x02, x06, x07, y02"                                   
##                           "x02, x06, x08, y02"                                   
##                           "x02, x06, x09, y02"                                   
##                           "x02, x06, x10, y02"                                   
##                           "x02, x07, x08, y02"                                   
##                           "x02, x07, x09, y02"                                   
##                           "x02, x07, x10, y02"                                   
##                           "x02, x08, x09, y02"                                   
##                           "x02, x08, x10, y02"                                   
##                           "x02, x09, x10, y02"                                   
##                           "x03, x04, x05, y02"                                   
##                           "x03, x04, x06, y02"                                   
##                           "x03, x04, x07, y02"                                   
##                           "x03, x04, x08, y02"                                   
##                           "x03, x04, x09, y02"                                   
##                           "x03, x04, x10, y02"                                   
##                           "x03, x05, x06, y02"                                   
##                           "x03, x05, x07, y02"                                   
##                           "x03, x05, x08, y02"                                   
##                           "x03, x05, x09, y02"                                   
##                           "x03, x05, x10, y02"                                   
##                           "x03, x06, x07, y02"                                   
##                           "x03, x06, x08, y02"                                   
##                           "x03, x06, x09, y02"                                   
##                           "x03, x06, x10, y02"                                   
##                           "x03, x07, x08, y02"                                   
##                           "x03, x07, x09, y02"                                   
##                           "x03, x07, x10, y02"                                   
##                           "x03, x08, x09, y02"                                   
##                           "x03, x08, x10, y02"                                   
##                           "x03, x09, x10, y02"                                   
##                           "x04, x05, x06, y02"                                   
##                           "x04, x05, x07, y02"                                   
##                           "x04, x05, x08, y02"                                   
##                           "x04, x05, x09, y02"                                   
##                           "x04, x05, x10, y02"                                   
##                           "x04, x06, x07, y02"                                   
##                           "x04, x06, x08, y02"                                   
##                           "x04, x06, x09, y02"                                   
##                           "x04, x06, x10, y02"                                   
##                           "x04, x07, x08, y02"                                   
##                           "x04, x07, x09, y02"                                   
##                           "x04, x07, x10, y02"                                   
##                           "x04, x08, x09, y02"                                   
##                           "x04, x08, x10, y02"                                   
##                           "x04, x09, x10, y02"                                   
##                           "x05, x06, x07, y02"                                   
##                           "x05, x06, x08, y02"                                   
##                           "x05, x06, x09, y02"                                   
##                           "x05, x06, x10, y02"                                   
##                           "x05, x07, x08, y02"                                   
##                           "x05, x07, x09, y02"                                   
##                           "x05, x07, x10, y02"                                   
##                           "x05, x08, x09, y02"                                   
##                           "x05, x08, x10, y02"                                   
##                           "x05, x09, x10, y02"                                   
##                           "x06, x07, x08, y02"                                   
##                           "x06, x07, x09, y02"                                   
##                           "x06, x07, x10, y02"                                   
##                           "x06, x08, x09, y02"                                   
##                           "x06, x08, x10, y02"                                   
##                           "x06, x09, x10, y02"                                   
##                           "x07, x08, x09, y02"                                   
##                           "x07, x08, x10, y02"                                   
##                           "x07, x09, x10, y02"                                   
##                           "x08, x09, x10, y02"                                   
##                           "x01, x02, x03, x04, y02"                              
##                           "x01, x02, x03, x05, y02"                              
##                           "x01, x02, x03, x06, y02"                              
##                           "x01, x02, x03, x07, y02"                              
##                           "x01, x02, x03, x08, y02"                              
##                           "x01, x02, x03, x09, y02"                              
##                           "x01, x02, x03, x10, y02"                              
##                           "x01, x02, x04, x05, y02"                              
##                           "x01, x02, x04, x06, y02"                              
##                           "x01, x02, x04, x07, y02"                              
##                           "x01, x02, x04, x08, y02"                              
##                           "x01, x02, x04, x09, y02"                              
##                           "x01, x02, x04, x10, y02"                              
##                           "x01, x02, x05, x06, y02"                              
##                           "x01, x02, x05, x07, y02"                              
##                           "x01, x02, x05, x08, y02"                              
##                           "x01, x02, x05, x09, y02"                              
##                           "x01, x02, x05, x10, y02"                              
##                           "x01, x02, x06, x07, y02"                              
##                           "x01, x02, x06, x08, y02"                              
##                           "x01, x02, x06, x09, y02"                              
##                           "x01, x02, x06, x10, y02"                              
##                           "x01, x02, x07, x08, y02"                              
##                           "x01, x02, x07, x09, y02"                              
##                           "x01, x02, x07, x10, y02"                              
##                           "x01, x02, x08, x09, y02"                              
##                           "x01, x02, x08, x10, y02"                              
##                           "x01, x02, x09, x10, y02"                              
##                           "x01, x03, x04, x05, y02"                              
##                           "x01, x03, x04, x06, y02"                              
##                           "x01, x03, x04, x07, y02"                              
##                           "x01, x03, x04, x08, y02"                              
##                           "x01, x03, x04, x09, y02"                              
##                           "x01, x03, x04, x10, y02"                              
##                           "x01, x03, x05, x06, y02"                              
##                           "x01, x03, x05, x07, y02"                              
##                           "x01, x03, x05, x08, y02"                              
##                           "x01, x03, x05, x09, y02"                              
##                           "x01, x03, x05, x10, y02"                              
##                           "x01, x03, x06, x07, y02"                              
##                           "x01, x03, x06, x08, y02"                              
##                           "x01, x03, x06, x09, y02"                              
##                           "x01, x03, x06, x10, y02"                              
##                           "x01, x03, x07, x08, y02"                              
##                           "x01, x03, x07, x09, y02"                              
##                           "x01, x03, x07, x10, y02"                              
##                           "x01, x03, x08, x09, y02"                              
##                           "x01, x03, x08, x10, y02"                              
##                           "x01, x03, x09, x10, y02"                              
##                           "x01, x04, x05, x06, y02"                              
##                           "x01, x04, x05, x07, y02"                              
##                           "x01, x04, x05, x08, y02"                              
##                           "x01, x04, x05, x09, y02"                              
##                           "x01, x04, x05, x10, y02"                              
##                           "x01, x04, x06, x07, y02"                              
##                           "x01, x04, x06, x08, y02"                              
##                           "x01, x04, x06, x09, y02"                              
##                           "x01, x04, x06, x10, y02"                              
##                           "x01, x04, x07, x08, y02"                              
##                           "x01, x04, x07, x09, y02"                              
##                           "x01, x04, x07, x10, y02"                              
##                           "x01, x04, x08, x09, y02"                              
##                           "x01, x04, x08, x10, y02"                              
##                           "x01, x04, x09, x10, y02"                              
##                           "x01, x05, x06, x07, y02"                              
##                           "x01, x05, x06, x08, y02"                              
##                           "x01, x05, x06, x09, y02"                              
##                           "x01, x05, x06, x10, y02"                              
##                           "x01, x05, x07, x08, y02"                              
##                           "x01, x05, x07, x09, y02"                              
##                           "x01, x05, x07, x10, y02"                              
##                           "x01, x05, x08, x09, y02"                              
##                           "x01, x05, x08, x10, y02"                              
##                           "x01, x05, x09, x10, y02"                              
##                           "x01, x06, x07, x08, y02"                              
##                           "x01, x06, x07, x09, y02"                              
##                           "x01, x06, x07, x10, y02"                              
##                           "x01, x06, x08, x09, y02"                              
##                           "x01, x06, x08, x10, y02"                              
##                           "x01, x06, x09, x10, y02"                              
##                           "x01, x07, x08, x09, y02"                              
##                           "x01, x07, x08, x10, y02"                              
##                           "x01, x07, x09, x10, y02"                              
##                           "x01, x08, x09, x10, y02"                              
##                           "x02, x03, x04, x05, y02"                              
##                           "x02, x03, x04, x06, y02"                              
##                           "x02, x03, x04, x07, y02"                              
##                           "x02, x03, x04, x08, y02"                              
##                           "x02, x03, x04, x09, y02"                              
##                           "x02, x03, x04, x10, y02"                              
##                           "x02, x03, x05, x06, y02"                              
##                           "x02, x03, x05, x07, y02"                              
##                           "x02, x03, x05, x08, y02"                              
##                           "x02, x03, x05, x09, y02"                              
##                           "x02, x03, x05, x10, y02"                              
##                           "x02, x03, x06, x07, y02"                              
##                           "x02, x03, x06, x08, y02"                              
##                           "x02, x03, x06, x09, y02"                              
##                           "x02, x03, x06, x10, y02"                              
##                           "x02, x03, x07, x08, y02"                              
##                           "x02, x03, x07, x09, y02"                              
##                           "x02, x03, x07, x10, y02"                              
##                           "x02, x03, x08, x09, y02"                              
##                           "x02, x03, x08, x10, y02"                              
##                           "x02, x03, x09, x10, y02"                              
##                           "x02, x04, x05, x06, y02"                              
##                           "x02, x04, x05, x07, y02"                              
##                           "x02, x04, x05, x08, y02"                              
##                           "x02, x04, x05, x09, y02"                              
##                           "x02, x04, x05, x10, y02"                              
##                           "x02, x04, x06, x07, y02"                              
##                           "x02, x04, x06, x08, y02"                              
##                           "x02, x04, x06, x09, y02"                              
##                           "x02, x04, x06, x10, y02"                              
##                           "x02, x04, x07, x08, y02"                              
##                           "x02, x04, x07, x09, y02"                              
##                           "x02, x04, x07, x10, y02"                              
##                           "x02, x04, x08, x09, y02"                              
##                           "x02, x04, x08, x10, y02"                              
##                           "x02, x04, x09, x10, y02"                              
##                           "x02, x05, x06, x07, y02"                              
##                           "x02, x05, x06, x08, y02"                              
##                           "x02, x05, x06, x09, y02"                              
##                           "x02, x05, x06, x10, y02"                              
##                           "x02, x05, x07, x08, y02"                              
##                           "x02, x05, x07, x09, y02"                              
##                           "x02, x05, x07, x10, y02"                              
##                           "x02, x05, x08, x09, y02"                              
##                           "x02, x05, x08, x10, y02"                              
##                           "x02, x05, x09, x10, y02"                              
##                           "x02, x06, x07, x08, y02"                              
##                           "x02, x06, x07, x09, y02"                              
##                           "x02, x06, x07, x10, y02"                              
##                           "x02, x06, x08, x09, y02"                              
##                           "x02, x06, x08, x10, y02"                              
##                           "x02, x06, x09, x10, y02"                              
##                           "x02, x07, x08, x09, y02"                              
##                           "x02, x07, x08, x10, y02"                              
##                           "x02, x07, x09, x10, y02"                              
##                           "x02, x08, x09, x10, y02"                              
##                           "x03, x04, x05, x06, y02"                              
##                           "x03, x04, x05, x07, y02"                              
##                           "x03, x04, x05, x08, y02"                              
##                           "x03, x04, x05, x09, y02"                              
##                           "x03, x04, x05, x10, y02"                              
##                           "x03, x04, x06, x07, y02"                              
##                           "x03, x04, x06, x08, y02"                              
##                           "x03, x04, x06, x09, y02"                              
##                           "x03, x04, x06, x10, y02"                              
##                           "x03, x04, x07, x08, y02"                              
##                           "x03, x04, x07, x09, y02"                              
##                           "x03, x04, x07, x10, y02"                              
##                           "x03, x04, x08, x09, y02"                              
##                           "x03, x04, x08, x10, y02"                              
##                           "x03, x04, x09, x10, y02"                              
##                           "x03, x05, x06, x07, y02"                              
##                           "x03, x05, x06, x08, y02"                              
##                           "x03, x05, x06, x09, y02"                              
##                           "x03, x05, x06, x10, y02"                              
##                           "x03, x05, x07, x08, y02"                              
##                           "x03, x05, x07, x09, y02"                              
##                           "x03, x05, x07, x10, y02"                              
##                           "x03, x05, x08, x09, y02"                              
##                           "x03, x05, x08, x10, y02"                              
##                           "x03, x05, x09, x10, y02"                              
##                           "x03, x06, x07, x08, y02"                              
##                           "x03, x06, x07, x09, y02"                              
##                           "x03, x06, x07, x10, y02"                              
##                           "x03, x06, x08, x09, y02"                              
##                           "x03, x06, x08, x10, y02"                              
##                           "x03, x06, x09, x10, y02"                              
##                           "x03, x07, x08, x09, y02"                              
##                           "x03, x07, x08, x10, y02"                              
##                           "x03, x07, x09, x10, y02"                              
##                           "x03, x08, x09, x10, y02"                              
##                           "x04, x05, x06, x07, y02"                              
##                           "x04, x05, x06, x08, y02"                              
##                           "x04, x05, x06, x09, y02"                              
##                           "x04, x05, x06, x10, y02"                              
##                           "x04, x05, x07, x08, y02"                              
##                           "x04, x05, x07, x09, y02"                              
##                           "x04, x05, x07, x10, y02"                              
##                           "x04, x05, x08, x09, y02"                              
##                           "x04, x05, x08, x10, y02"                              
##                           "x04, x05, x09, x10, y02"                              
##                           "x04, x06, x07, x08, y02"                              
##                           "x04, x06, x07, x09, y02"                              
##                           "x04, x06, x07, x10, y02"                              
##                           "x04, x06, x08, x09, y02"                              
##                           "x04, x06, x08, x10, y02"                              
##                           "x04, x06, x09, x10, y02"                              
##                           "x04, x07, x08, x09, y02"                              
##                           "x04, x07, x08, x10, y02"                              
##                           "x04, x07, x09, x10, y02"                              
##                           "x04, x08, x09, x10, y02"                              
##                           "x05, x06, x07, x08, y02"                              
##                           "x05, x06, x07, x09, y02"                              
##                           "x05, x06, x07, x10, y02"                              
##                           "x05, x06, x08, x09, y02"                              
##                           "x05, x06, x08, x10, y02"                              
##                           "x05, x06, x09, x10, y02"                              
##                           "x05, x07, x08, x09, y02"                              
##                           "x05, x07, x08, x10, y02"                              
##                           "x05, x07, x09, x10, y02"                              
##                           "x05, x08, x09, x10, y02"                              
##                           "x06, x07, x08, x09, y02"                              
##                           "x06, x07, x08, x10, y02"                              
##                           "x06, x07, x09, x10, y02"                              
##                           "x06, x08, x09, x10, y02"                              
##                           "x07, x08, x09, x10, y02"                              
##                           "x01, x02, x03, x04, x05, y02"                         
##                           "x01, x02, x03, x04, x06, y02"                         
##                           "x01, x02, x03, x04, x07, y02"                         
##                           "x01, x02, x03, x04, x08, y02"                         
##                           "x01, x02, x03, x04, x09, y02"                         
##                           "x01, x02, x03, x04, x10, y02"                         
##                           "x01, x02, x03, x05, x06, y02"                         
##                           "x01, x02, x03, x05, x07, y02"                         
##                           "x01, x02, x03, x05, x08, y02"                         
##                           "x01, x02, x03, x05, x09, y02"                         
##                           "x01, x02, x03, x05, x10, y02"                         
##                           "x01, x02, x03, x06, x07, y02"                         
##                           "x01, x02, x03, x06, x08, y02"                         
##                           "x01, x02, x03, x06, x09, y02"                         
##                           "x01, x02, x03, x06, x10, y02"                         
##                           "x01, x02, x03, x07, x08, y02"                         
##                           "x01, x02, x03, x07, x09, y02"                         
##                           "x01, x02, x03, x07, x10, y02"                         
##                           "x01, x02, x03, x08, x09, y02"                         
##                           "x01, x02, x03, x08, x10, y02"                         
##                           "x01, x02, x03, x09, x10, y02"                         
##                           "x01, x02, x04, x05, x06, y02"                         
##                           "x01, x02, x04, x05, x07, y02"                         
##                           "x01, x02, x04, x05, x08, y02"                         
##                           "x01, x02, x04, x05, x09, y02"                         
##                           "x01, x02, x04, x05, x10, y02"                         
##                           "x01, x02, x04, x06, x07, y02"                         
##                           "x01, x02, x04, x06, x08, y02"                         
##                           "x01, x02, x04, x06, x09, y02"                         
##                           "x01, x02, x04, x06, x10, y02"                         
##                           "x01, x02, x04, x07, x08, y02"                         
##                           "x01, x02, x04, x07, x09, y02"                         
##                           "x01, x02, x04, x07, x10, y02"                         
##                           "x01, x02, x04, x08, x09, y02"                         
##                           "x01, x02, x04, x08, x10, y02"                         
##                           "x01, x02, x04, x09, x10, y02"                         
##                           "x01, x02, x05, x06, x07, y02"                         
##                           "x01, x02, x05, x06, x08, y02"                         
##                           "x01, x02, x05, x06, x09, y02"                         
##                           "x01, x02, x05, x06, x10, y02"                         
##                           "x01, x02, x05, x07, x08, y02"                         
##                           "x01, x02, x05, x07, x09, y02"                         
##                           "x01, x02, x05, x07, x10, y02"                         
##                           "x01, x02, x05, x08, x09, y02"                         
##                           "x01, x02, x05, x08, x10, y02"                         
##                           "x01, x02, x05, x09, x10, y02"                         
##                           "x01, x02, x06, x07, x08, y02"                         
##                           "x01, x02, x06, x07, x09, y02"                         
##                           "x01, x02, x06, x07, x10, y02"                         
##                           "x01, x02, x06, x08, x09, y02"                         
##                           "x01, x02, x06, x08, x10, y02"                         
##                           "x01, x02, x06, x09, x10, y02"                         
##                           "x01, x02, x07, x08, x09, y02"                         
##                           "x01, x02, x07, x08, x10, y02"                         
##                           "x01, x02, x07, x09, x10, y02"                         
##                           "x01, x02, x08, x09, x10, y02"                         
##                           "x01, x03, x04, x05, x06, y02"                         
##                           "x01, x03, x04, x05, x07, y02"                         
##                           "x01, x03, x04, x05, x08, y02"                         
##                           "x01, x03, x04, x05, x09, y02"                         
##                           "x01, x03, x04, x05, x10, y02"                         
##                           "x01, x03, x04, x06, x07, y02"                         
##                           "x01, x03, x04, x06, x08, y02"                         
##                           "x01, x03, x04, x06, x09, y02"                         
##                           "x01, x03, x04, x06, x10, y02"                         
##                           "x01, x03, x04, x07, x08, y02"                         
##                           "x01, x03, x04, x07, x09, y02"                         
##                           "x01, x03, x04, x07, x10, y02"                         
##                           "x01, x03, x04, x08, x09, y02"                         
##                           "x01, x03, x04, x08, x10, y02"                         
##                           "x01, x03, x04, x09, x10, y02"                         
##                           "x01, x03, x05, x06, x07, y02"                         
##                           "x01, x03, x05, x06, x08, y02"                         
##                           "x01, x03, x05, x06, x09, y02"                         
##                           "x01, x03, x05, x06, x10, y02"                         
##                           "x01, x03, x05, x07, x08, y02"                         
##                           "x01, x03, x05, x07, x09, y02"                         
##                           "x01, x03, x05, x07, x10, y02"                         
##                           "x01, x03, x05, x08, x09, y02"                         
##                           "x01, x03, x05, x08, x10, y02"                         
##                           "x01, x03, x05, x09, x10, y02"                         
##                           "x01, x03, x06, x07, x08, y02"                         
##                           "x01, x03, x06, x07, x09, y02"                         
##                           "x01, x03, x06, x07, x10, y02"                         
##                           "x01, x03, x06, x08, x09, y02"                         
##                           "x01, x03, x06, x08, x10, y02"                         
##                           "x01, x03, x06, x09, x10, y02"                         
##                           "x01, x03, x07, x08, x09, y02"                         
##                           "x01, x03, x07, x08, x10, y02"                         
##                           "x01, x03, x07, x09, x10, y02"                         
##                           "x01, x03, x08, x09, x10, y02"                         
##                           "x01, x04, x05, x06, x07, y02"                         
##                           "x01, x04, x05, x06, x08, y02"                         
##                           "x01, x04, x05, x06, x09, y02"                         
##                           "x01, x04, x05, x06, x10, y02"                         
##                           "x01, x04, x05, x07, x08, y02"                         
##                           "x01, x04, x05, x07, x09, y02"                         
##                           "x01, x04, x05, x07, x10, y02"                         
##                           "x01, x04, x05, x08, x09, y02"                         
##                           "x01, x04, x05, x08, x10, y02"                         
##                           "x01, x04, x05, x09, x10, y02"                         
##                           "x01, x04, x06, x07, x08, y02"                         
##                           "x01, x04, x06, x07, x09, y02"                         
##                           "x01, x04, x06, x07, x10, y02"                         
##                           "x01, x04, x06, x08, x09, y02"                         
##                           "x01, x04, x06, x08, x10, y02"                         
##                           "x01, x04, x06, x09, x10, y02"                         
##                           "x01, x04, x07, x08, x09, y02"                         
##                           "x01, x04, x07, x08, x10, y02"                         
##                           "x01, x04, x07, x09, x10, y02"                         
##                           "x01, x04, x08, x09, x10, y02"                         
##                           "x01, x05, x06, x07, x08, y02"                         
##                           "x01, x05, x06, x07, x09, y02"                         
##                           "x01, x05, x06, x07, x10, y02"                         
##                           "x01, x05, x06, x08, x09, y02"                         
##                           "x01, x05, x06, x08, x10, y02"                         
##                           "x01, x05, x06, x09, x10, y02"                         
##                           "x01, x05, x07, x08, x09, y02"                         
##                           "x01, x05, x07, x08, x10, y02"                         
##                           "x01, x05, x07, x09, x10, y02"                         
##                           "x01, x05, x08, x09, x10, y02"                         
##                           "x01, x06, x07, x08, x09, y02"                         
##                           "x01, x06, x07, x08, x10, y02"                         
##                           "x01, x06, x07, x09, x10, y02"                         
##                           "x01, x06, x08, x09, x10, y02"                         
##                           "x01, x07, x08, x09, x10, y02"                         
##                           "x02, x03, x04, x05, x06, y02"                         
##                           "x02, x03, x04, x05, x07, y02"                         
##                           "x02, x03, x04, x05, x08, y02"                         
##                           "x02, x03, x04, x05, x09, y02"                         
##                           "x02, x03, x04, x05, x10, y02"                         
##                           "x02, x03, x04, x06, x07, y02"                         
##                           "x02, x03, x04, x06, x08, y02"                         
##                           "x02, x03, x04, x06, x09, y02"                         
##                           "x02, x03, x04, x06, x10, y02"                         
##                           "x02, x03, x04, x07, x08, y02"                         
##                           "x02, x03, x04, x07, x09, y02"                         
##                           "x02, x03, x04, x07, x10, y02"                         
##                           "x02, x03, x04, x08, x09, y02"                         
##                           "x02, x03, x04, x08, x10, y02"                         
##                           "x02, x03, x04, x09, x10, y02"                         
##                           "x02, x03, x05, x06, x07, y02"                         
##                           "x02, x03, x05, x06, x08, y02"                         
##                           "x02, x03, x05, x06, x09, y02"                         
##                           "x02, x03, x05, x06, x10, y02"                         
##                           "x02, x03, x05, x07, x08, y02"                         
##                           "x02, x03, x05, x07, x09, y02"                         
##                           "x02, x03, x05, x07, x10, y02"                         
##                           "x02, x03, x05, x08, x09, y02"                         
##                           "x02, x03, x05, x08, x10, y02"                         
##                           "x02, x03, x05, x09, x10, y02"                         
##                           "x02, x03, x06, x07, x08, y02"                         
##                           "x02, x03, x06, x07, x09, y02"                         
##                           "x02, x03, x06, x07, x10, y02"                         
##                           "x02, x03, x06, x08, x09, y02"                         
##                           "x02, x03, x06, x08, x10, y02"                         
##                           "x02, x03, x06, x09, x10, y02"                         
##                           "x02, x03, x07, x08, x09, y02"                         
##                           "x02, x03, x07, x08, x10, y02"                         
##                           "x02, x03, x07, x09, x10, y02"                         
##                           "x02, x03, x08, x09, x10, y02"                         
##                           "x02, x04, x05, x06, x07, y02"                         
##                           "x02, x04, x05, x06, x08, y02"                         
##                           "x02, x04, x05, x06, x09, y02"                         
##                           "x02, x04, x05, x06, x10, y02"                         
##                           "x02, x04, x05, x07, x08, y02"                         
##                           "x02, x04, x05, x07, x09, y02"                         
##                           "x02, x04, x05, x07, x10, y02"                         
##                           "x02, x04, x05, x08, x09, y02"                         
##                           "x02, x04, x05, x08, x10, y02"                         
##                           "x02, x04, x05, x09, x10, y02"                         
##                           "x02, x04, x06, x07, x08, y02"                         
##                           "x02, x04, x06, x07, x09, y02"                         
##                           "x02, x04, x06, x07, x10, y02"                         
##                           "x02, x04, x06, x08, x09, y02"                         
##                           "x02, x04, x06, x08, x10, y02"                         
##                           "x02, x04, x06, x09, x10, y02"                         
##                           "x02, x04, x07, x08, x09, y02"                         
##                           "x02, x04, x07, x08, x10, y02"                         
##                           "x02, x04, x07, x09, x10, y02"                         
##                           "x02, x04, x08, x09, x10, y02"                         
##                           "x02, x05, x06, x07, x08, y02"                         
##                           "x02, x05, x06, x07, x09, y02"                         
##                           "x02, x05, x06, x07, x10, y02"                         
##                           "x02, x05, x06, x08, x09, y02"                         
##                           "x02, x05, x06, x08, x10, y02"                         
##                           "x02, x05, x06, x09, x10, y02"                         
##                           "x02, x05, x07, x08, x09, y02"                         
##                           "x02, x05, x07, x08, x10, y02"                         
##                           "x02, x05, x07, x09, x10, y02"                         
##                           "x02, x05, x08, x09, x10, y02"                         
##                           "x02, x06, x07, x08, x09, y02"                         
##                           "x02, x06, x07, x08, x10, y02"                         
##                           "x02, x06, x07, x09, x10, y02"                         
##                           "x02, x06, x08, x09, x10, y02"                         
##                           "x02, x07, x08, x09, x10, y02"                         
##                           "x03, x04, x05, x06, x07, y02"                         
##                           "x03, x04, x05, x06, x08, y02"                         
##                           "x03, x04, x05, x06, x09, y02"                         
##                           "x03, x04, x05, x06, x10, y02"                         
##                           "x03, x04, x05, x07, x08, y02"                         
##                           "x03, x04, x05, x07, x09, y02"                         
##                           "x03, x04, x05, x07, x10, y02"                         
##                           "x03, x04, x05, x08, x09, y02"                         
##                           "x03, x04, x05, x08, x10, y02"                         
##                           "x03, x04, x05, x09, x10, y02"                         
##                           "x03, x04, x06, x07, x08, y02"                         
##                           "x03, x04, x06, x07, x09, y02"                         
##                           "x03, x04, x06, x07, x10, y02"                         
##                           "x03, x04, x06, x08, x09, y02"                         
##                           "x03, x04, x06, x08, x10, y02"                         
##                           "x03, x04, x06, x09, x10, y02"                         
##                           "x03, x04, x07, x08, x09, y02"                         
##                           "x03, x04, x07, x08, x10, y02"                         
##                           "x03, x04, x07, x09, x10, y02"                         
##                           "x03, x04, x08, x09, x10, y02"                         
##                           "x03, x05, x06, x07, x08, y02"                         
##                           "x03, x05, x06, x07, x09, y02"                         
##                           "x03, x05, x06, x07, x10, y02"                         
##                           "x03, x05, x06, x08, x09, y02"                         
##                           "x03, x05, x06, x08, x10, y02"                         
##                           "x03, x05, x06, x09, x10, y02"                         
##                           "x03, x05, x07, x08, x09, y02"                         
##                           "x03, x05, x07, x08, x10, y02"                         
##                           "x03, x05, x07, x09, x10, y02"                         
##                           "x03, x05, x08, x09, x10, y02"                         
##                           "x03, x06, x07, x08, x09, y02"                         
##                           "x03, x06, x07, x08, x10, y02"                         
##                           "x03, x06, x07, x09, x10, y02"                         
##                           "x03, x06, x08, x09, x10, y02"                         
##                           "x03, x07, x08, x09, x10, y02"                         
##                           "x04, x05, x06, x07, x08, y02"                         
##                           "x04, x05, x06, x07, x09, y02"                         
##                           "x04, x05, x06, x07, x10, y02"                         
##                           "x04, x05, x06, x08, x09, y02"                         
##                           "x04, x05, x06, x08, x10, y02"                         
##                           "x04, x05, x06, x09, x10, y02"                         
##                           "x04, x05, x07, x08, x09, y02"                         
##                           "x04, x05, x07, x08, x10, y02"                         
##                           "x04, x05, x07, x09, x10, y02"                         
##                           "x04, x05, x08, x09, x10, y02"                         
##                           "x04, x06, x07, x08, x09, y02"                         
##                           "x04, x06, x07, x08, x10, y02"                         
##                           "x04, x06, x07, x09, x10, y02"                         
##                           "x04, x06, x08, x09, x10, y02"                         
##                           "x04, x07, x08, x09, x10, y02"                         
##                           "x05, x06, x07, x08, x09, y02"                         
##                           "x05, x06, x07, x08, x10, y02"                         
##                           "x05, x06, x07, x09, x10, y02"                         
##                           "x05, x06, x08, x09, x10, y02"                         
##                           "x05, x07, x08, x09, x10, y02"                         
##                           "x06, x07, x08, x09, x10, y02"                         
##                           "x01, x02, x03, x04, x05, x06, y02"                    
##                           "x01, x02, x03, x04, x05, x07, y02"                    
##                           "x01, x02, x03, x04, x05, x08, y02"                    
##                           "x01, x02, x03, x04, x05, x09, y02"                    
##                           "x01, x02, x03, x04, x05, x10, y02"                    
##                           "x01, x02, x03, x04, x06, x07, y02"                    
##                           "x01, x02, x03, x04, x06, x08, y02"                    
##                           "x01, x02, x03, x04, x06, x09, y02"                    
##                           "x01, x02, x03, x04, x06, x10, y02"                    
##                           "x01, x02, x03, x04, x07, x08, y02"                    
##                           "x01, x02, x03, x04, x07, x09, y02"                    
##                           "x01, x02, x03, x04, x07, x10, y02"                    
##                           "x01, x02, x03, x04, x08, x09, y02"                    
##                           "x01, x02, x03, x04, x08, x10, y02"                    
##                           "x01, x02, x03, x04, x09, x10, y02"                    
##                           "x01, x02, x03, x05, x06, x07, y02"                    
##                           "x01, x02, x03, x05, x06, x08, y02"                    
##                           "x01, x02, x03, x05, x06, x09, y02"                    
##                           "x01, x02, x03, x05, x06, x10, y02"                    
##                           "x01, x02, x03, x05, x07, x08, y02"                    
##                           "x01, x02, x03, x05, x07, x09, y02"                    
##                           "x01, x02, x03, x05, x07, x10, y02"                    
##                           "x01, x02, x03, x05, x08, x09, y02"                    
##                           "x01, x02, x03, x05, x08, x10, y02"                    
##                           "x01, x02, x03, x05, x09, x10, y02"                    
##                           "x01, x02, x03, x06, x07, x08, y02"                    
##                           "x01, x02, x03, x06, x07, x09, y02"                    
##                           "x01, x02, x03, x06, x07, x10, y02"                    
##                           "x01, x02, x03, x06, x08, x09, y02"                    
##                           "x01, x02, x03, x06, x08, x10, y02"                    
##                           "x01, x02, x03, x06, x09, x10, y02"                    
##                           "x01, x02, x03, x07, x08, x09, y02"                    
##                           "x01, x02, x03, x07, x08, x10, y02"                    
##                           "x01, x02, x03, x07, x09, x10, y02"                    
##                           "x01, x02, x03, x08, x09, x10, y02"                    
##                           "x01, x02, x04, x05, x06, x07, y02"                    
##                           "x01, x02, x04, x05, x06, x08, y02"                    
##                           "x01, x02, x04, x05, x06, x09, y02"                    
##                           "x01, x02, x04, x05, x06, x10, y02"                    
##                           "x01, x02, x04, x05, x07, x08, y02"                    
##                           "x01, x02, x04, x05, x07, x09, y02"                    
##                           "x01, x02, x04, x05, x07, x10, y02"                    
##                           "x01, x02, x04, x05, x08, x09, y02"                    
##                           "x01, x02, x04, x05, x08, x10, y02"                    
##                           "x01, x02, x04, x05, x09, x10, y02"                    
##                           "x01, x02, x04, x06, x07, x08, y02"                    
##                           "x01, x02, x04, x06, x07, x09, y02"                    
##                           "x01, x02, x04, x06, x07, x10, y02"                    
##                           "x01, x02, x04, x06, x08, x09, y02"                    
##                           "x01, x02, x04, x06, x08, x10, y02"                    
##                           "x01, x02, x04, x06, x09, x10, y02"                    
##                           "x01, x02, x04, x07, x08, x09, y02"                    
##                           "x01, x02, x04, x07, x08, x10, y02"                    
##                           "x01, x02, x04, x07, x09, x10, y02"                    
##                           "x01, x02, x04, x08, x09, x10, y02"                    
##                           "x01, x02, x05, x06, x07, x08, y02"                    
##                           "x01, x02, x05, x06, x07, x09, y02"                    
##                           "x01, x02, x05, x06, x07, x10, y02"                    
##                           "x01, x02, x05, x06, x08, x09, y02"                    
##                           "x01, x02, x05, x06, x08, x10, y02"                    
##                           "x01, x02, x05, x06, x09, x10, y02"                    
##                           "x01, x02, x05, x07, x08, x09, y02"                    
##                           "x01, x02, x05, x07, x08, x10, y02"                    
##                           "x01, x02, x05, x07, x09, x10, y02"                    
##                           "x01, x02, x05, x08, x09, x10, y02"                    
##                           "x01, x02, x06, x07, x08, x09, y02"                    
##                           "x01, x02, x06, x07, x08, x10, y02"                    
##                           "x01, x02, x06, x07, x09, x10, y02"                    
##                           "x01, x02, x06, x08, x09, x10, y02"                    
##                           "x01, x02, x07, x08, x09, x10, y02"                    
##                           "x01, x03, x04, x05, x06, x07, y02"                    
##                           "x01, x03, x04, x05, x06, x08, y02"                    
##                           "x01, x03, x04, x05, x06, x09, y02"                    
##                           "x01, x03, x04, x05, x06, x10, y02"                    
##                           "x01, x03, x04, x05, x07, x08, y02"                    
##                           "x01, x03, x04, x05, x07, x09, y02"                    
##                           "x01, x03, x04, x05, x07, x10, y02"                    
##                           "x01, x03, x04, x05, x08, x09, y02"                    
##                           "x01, x03, x04, x05, x08, x10, y02"                    
##                           "x01, x03, x04, x05, x09, x10, y02"                    
##                           "x01, x03, x04, x06, x07, x08, y02"                    
##                           "x01, x03, x04, x06, x07, x09, y02"                    
##                           "x01, x03, x04, x06, x07, x10, y02"                    
##                           "x01, x03, x04, x06, x08, x09, y02"                    
##                           "x01, x03, x04, x06, x08, x10, y02"                    
##                           "x01, x03, x04, x06, x09, x10, y02"                    
##                           "x01, x03, x04, x07, x08, x09, y02"                    
##                           "x01, x03, x04, x07, x08, x10, y02"                    
##                           "x01, x03, x04, x07, x09, x10, y02"                    
##                           "x01, x03, x04, x08, x09, x10, y02"                    
##                           "x01, x03, x05, x06, x07, x08, y02"                    
##                           "x01, x03, x05, x06, x07, x09, y02"                    
##                           "x01, x03, x05, x06, x07, x10, y02"                    
##                           "x01, x03, x05, x06, x08, x09, y02"                    
##                           "x01, x03, x05, x06, x08, x10, y02"                    
##                           "x01, x03, x05, x06, x09, x10, y02"                    
##                           "x01, x03, x05, x07, x08, x09, y02"                    
##                           "x01, x03, x05, x07, x08, x10, y02"                    
##                           "x01, x03, x05, x07, x09, x10, y02"                    
##                           "x01, x03, x05, x08, x09, x10, y02"                    
##                           "x01, x03, x06, x07, x08, x09, y02"                    
##                           "x01, x03, x06, x07, x08, x10, y02"                    
##                           "x01, x03, x06, x07, x09, x10, y02"                    
##                           "x01, x03, x06, x08, x09, x10, y02"                    
##                           "x01, x03, x07, x08, x09, x10, y02"                    
##                           "x01, x04, x05, x06, x07, x08, y02"                    
##                           "x01, x04, x05, x06, x07, x09, y02"                    
##                           "x01, x04, x05, x06, x07, x10, y02"                    
##                           "x01, x04, x05, x06, x08, x09, y02"                    
##                           "x01, x04, x05, x06, x08, x10, y02"                    
##                           "x01, x04, x05, x06, x09, x10, y02"                    
##                           "x01, x04, x05, x07, x08, x09, y02"                    
##                           "x01, x04, x05, x07, x08, x10, y02"                    
##                           "x01, x04, x05, x07, x09, x10, y02"                    
##                           "x01, x04, x05, x08, x09, x10, y02"                    
##                           "x01, x04, x06, x07, x08, x09, y02"                    
##                           "x01, x04, x06, x07, x08, x10, y02"                    
##                           "x01, x04, x06, x07, x09, x10, y02"                    
##                           "x01, x04, x06, x08, x09, x10, y02"                    
##                           "x01, x04, x07, x08, x09, x10, y02"                    
##                           "x01, x05, x06, x07, x08, x09, y02"                    
##                           "x01, x05, x06, x07, x08, x10, y02"                    
##                           "x01, x05, x06, x07, x09, x10, y02"                    
##                           "x01, x05, x06, x08, x09, x10, y02"                    
##                           "x01, x05, x07, x08, x09, x10, y02"                    
##                           "x01, x06, x07, x08, x09, x10, y02"                    
##                           "x02, x03, x04, x05, x06, x07, y02"                    
##                           "x02, x03, x04, x05, x06, x08, y02"                    
##                           "x02, x03, x04, x05, x06, x09, y02"                    
##                           "x02, x03, x04, x05, x06, x10, y02"                    
##                           "x02, x03, x04, x05, x07, x08, y02"                    
##                           "x02, x03, x04, x05, x07, x09, y02"                    
##                           "x02, x03, x04, x05, x07, x10, y02"                    
##                           "x02, x03, x04, x05, x08, x09, y02"                    
##                           "x02, x03, x04, x05, x08, x10, y02"                    
##                           "x02, x03, x04, x05, x09, x10, y02"                    
##                           "x02, x03, x04, x06, x07, x08, y02"                    
##                           "x02, x03, x04, x06, x07, x09, y02"                    
##                           "x02, x03, x04, x06, x07, x10, y02"                    
##                           "x02, x03, x04, x06, x08, x09, y02"                    
##                           "x02, x03, x04, x06, x08, x10, y02"                    
##                           "x02, x03, x04, x06, x09, x10, y02"                    
##                           "x02, x03, x04, x07, x08, x09, y02"                    
##                           "x02, x03, x04, x07, x08, x10, y02"                    
##                           "x02, x03, x04, x07, x09, x10, y02"                    
##                           "x02, x03, x04, x08, x09, x10, y02"                    
##                           "x02, x03, x05, x06, x07, x08, y02"                    
##                           "x02, x03, x05, x06, x07, x09, y02"                    
##                           "x02, x03, x05, x06, x07, x10, y02"                    
##                           "x02, x03, x05, x06, x08, x09, y02"                    
##                           "x02, x03, x05, x06, x08, x10, y02"                    
##                           "x02, x03, x05, x06, x09, x10, y02"                    
##                           "x02, x03, x05, x07, x08, x09, y02"                    
##                           "x02, x03, x05, x07, x08, x10, y02"                    
##                           "x02, x03, x05, x07, x09, x10, y02"                    
##                           "x02, x03, x05, x08, x09, x10, y02"                    
##                           "x02, x03, x06, x07, x08, x09, y02"                    
##                           "x02, x03, x06, x07, x08, x10, y02"                    
##                           "x02, x03, x06, x07, x09, x10, y02"                    
##                           "x02, x03, x06, x08, x09, x10, y02"                    
##                           "x02, x03, x07, x08, x09, x10, y02"                    
##                           "x02, x04, x05, x06, x07, x08, y02"                    
##                           "x02, x04, x05, x06, x07, x09, y02"                    
##                           "x02, x04, x05, x06, x07, x10, y02"                    
##                           "x02, x04, x05, x06, x08, x09, y02"                    
##                           "x02, x04, x05, x06, x08, x10, y02"                    
##                           "x02, x04, x05, x06, x09, x10, y02"                    
##                           "x02, x04, x05, x07, x08, x09, y02"                    
##                           "x02, x04, x05, x07, x08, x10, y02"                    
##                           "x02, x04, x05, x07, x09, x10, y02"                    
##                           "x02, x04, x05, x08, x09, x10, y02"                    
##                           "x02, x04, x06, x07, x08, x09, y02"                    
##                           "x02, x04, x06, x07, x08, x10, y02"                    
##                           "x02, x04, x06, x07, x09, x10, y02"                    
##                           "x02, x04, x06, x08, x09, x10, y02"                    
##                           "x02, x04, x07, x08, x09, x10, y02"                    
##                           "x02, x05, x06, x07, x08, x09, y02"                    
##                           "x02, x05, x06, x07, x08, x10, y02"                    
##                           "x02, x05, x06, x07, x09, x10, y02"                    
##                           "x02, x05, x06, x08, x09, x10, y02"                    
##                           "x02, x05, x07, x08, x09, x10, y02"                    
##                           "x02, x06, x07, x08, x09, x10, y02"                    
##                           "x03, x04, x05, x06, x07, x08, y02"                    
##                           "x03, x04, x05, x06, x07, x09, y02"                    
##                           "x03, x04, x05, x06, x07, x10, y02"                    
##                           "x03, x04, x05, x06, x08, x09, y02"                    
##                           "x03, x04, x05, x06, x08, x10, y02"                    
##                           "x03, x04, x05, x06, x09, x10, y02"                    
##                           "x03, x04, x05, x07, x08, x09, y02"                    
##                           "x03, x04, x05, x07, x08, x10, y02"                    
##                           "x03, x04, x05, x07, x09, x10, y02"                    
##                           "x03, x04, x05, x08, x09, x10, y02"                    
##                           "x03, x04, x06, x07, x08, x09, y02"                    
##                           "x03, x04, x06, x07, x08, x10, y02"                    
##                           "x03, x04, x06, x07, x09, x10, y02"                    
##                           "x03, x04, x06, x08, x09, x10, y02"                    
##                           "x03, x04, x07, x08, x09, x10, y02"                    
##                           "x03, x05, x06, x07, x08, x09, y02"                    
##                           "x03, x05, x06, x07, x08, x10, y02"                    
##                           "x03, x05, x06, x07, x09, x10, y02"                    
##                           "x03, x05, x06, x08, x09, x10, y02"                    
##                           "x03, x05, x07, x08, x09, x10, y02"                    
##                           "x03, x06, x07, x08, x09, x10, y02"                    
##                           "x04, x05, x06, x07, x08, x09, y02"                    
##                           "x04, x05, x06, x07, x08, x10, y02"                    
##                           "x04, x05, x06, x07, x09, x10, y02"                    
##                           "x04, x05, x06, x08, x09, x10, y02"                    
##                           "x04, x05, x07, x08, x09, x10, y02"                    
##                           "x04, x06, x07, x08, x09, x10, y02"                    
##                           "x05, x06, x07, x08, x09, x10, y02"                    
##                           "x01, x02, x03, x04, x05, x06, x07, y02"               
##                           "x01, x02, x03, x04, x05, x06, x08, y02"               
##                           "x01, x02, x03, x04, x05, x06, x09, y02"               
##                           "x01, x02, x03, x04, x05, x06, x10, y02"               
##                           "x01, x02, x03, x04, x05, x07, x08, y02"               
##                           "x01, x02, x03, x04, x05, x07, x09, y02"               
##                           "x01, x02, x03, x04, x05, x07, x10, y02"               
##                           "x01, x02, x03, x04, x05, x08, x09, y02"               
##                           "x01, x02, x03, x04, x05, x08, x10, y02"               
##                           "x01, x02, x03, x04, x05, x09, x10, y02"               
##                           "x01, x02, x03, x04, x06, x07, x08, y02"               
##                           "x01, x02, x03, x04, x06, x07, x09, y02"               
##                           "x01, x02, x03, x04, x06, x07, x10, y02"               
##                           "x01, x02, x03, x04, x06, x08, x09, y02"               
##                           "x01, x02, x03, x04, x06, x08, x10, y02"               
##                           "x01, x02, x03, x04, x06, x09, x10, y02"               
##                           "x01, x02, x03, x04, x07, x08, x09, y02"               
##                           "x01, x02, x03, x04, x07, x08, x10, y02"               
##                           "x01, x02, x03, x04, x07, x09, x10, y02"               
##                           "x01, x02, x03, x04, x08, x09, x10, y02"               
##                           "x01, x02, x03, x05, x06, x07, x08, y02"               
##                           "x01, x02, x03, x05, x06, x07, x09, y02"               
##                           "x01, x02, x03, x05, x06, x07, x10, y02"               
##                           "x01, x02, x03, x05, x06, x08, x09, y02"               
##                           "x01, x02, x03, x05, x06, x08, x10, y02"               
##                           "x01, x02, x03, x05, x06, x09, x10, y02"               
##                           "x01, x02, x03, x05, x07, x08, x09, y02"               
##                           "x01, x02, x03, x05, x07, x08, x10, y02"               
##                           "x01, x02, x03, x05, x07, x09, x10, y02"               
##                           "x01, x02, x03, x05, x08, x09, x10, y02"               
##                           "x01, x02, x03, x06, x07, x08, x09, y02"               
##                           "x01, x02, x03, x06, x07, x08, x10, y02"               
##                           "x01, x02, x03, x06, x07, x09, x10, y02"               
##                           "x01, x02, x03, x06, x08, x09, x10, y02"               
##                           "x01, x02, x03, x07, x08, x09, x10, y02"               
##                           "x01, x02, x04, x05, x06, x07, x08, y02"               
##                           "x01, x02, x04, x05, x06, x07, x09, y02"               
##                           "x01, x02, x04, x05, x06, x07, x10, y02"               
##                           "x01, x02, x04, x05, x06, x08, x09, y02"               
##                           "x01, x02, x04, x05, x06, x08, x10, y02"               
##                           "x01, x02, x04, x05, x06, x09, x10, y02"               
##                           "x01, x02, x04, x05, x07, x08, x09, y02"               
##                           "x01, x02, x04, x05, x07, x08, x10, y02"               
##                           "x01, x02, x04, x05, x07, x09, x10, y02"               
##                           "x01, x02, x04, x05, x08, x09, x10, y02"               
##                           "x01, x02, x04, x06, x07, x08, x09, y02"               
##                           "x01, x02, x04, x06, x07, x08, x10, y02"               
##                           "x01, x02, x04, x06, x07, x09, x10, y02"               
##                           "x01, x02, x04, x06, x08, x09, x10, y02"               
##                           "x01, x02, x04, x07, x08, x09, x10, y02"               
##                           "x01, x02, x05, x06, x07, x08, x09, y02"               
##                           "x01, x02, x05, x06, x07, x08, x10, y02"               
##                           "x01, x02, x05, x06, x07, x09, x10, y02"               
##                           "x01, x02, x05, x06, x08, x09, x10, y02"               
##                           "x01, x02, x05, x07, x08, x09, x10, y02"               
##                           "x01, x02, x06, x07, x08, x09, x10, y02"               
##                           "x01, x03, x04, x05, x06, x07, x08, y02"               
##                           "x01, x03, x04, x05, x06, x07, x09, y02"               
##                           "x01, x03, x04, x05, x06, x07, x10, y02"               
##                           "x01, x03, x04, x05, x06, x08, x09, y02"               
##                           "x01, x03, x04, x05, x06, x08, x10, y02"               
##                           "x01, x03, x04, x05, x06, x09, x10, y02"               
##                           "x01, x03, x04, x05, x07, x08, x09, y02"               
##                           "x01, x03, x04, x05, x07, x08, x10, y02"               
##                           "x01, x03, x04, x05, x07, x09, x10, y02"               
##                           "x01, x03, x04, x05, x08, x09, x10, y02"               
##                           "x01, x03, x04, x06, x07, x08, x09, y02"               
##                           "x01, x03, x04, x06, x07, x08, x10, y02"               
##                           "x01, x03, x04, x06, x07, x09, x10, y02"               
##                           "x01, x03, x04, x06, x08, x09, x10, y02"               
##                           "x01, x03, x04, x07, x08, x09, x10, y02"               
##                           "x01, x03, x05, x06, x07, x08, x09, y02"               
##                           "x01, x03, x05, x06, x07, x08, x10, y02"               
##                           "x01, x03, x05, x06, x07, x09, x10, y02"               
##                           "x01, x03, x05, x06, x08, x09, x10, y02"               
##                           "x01, x03, x05, x07, x08, x09, x10, y02"               
##                           "x01, x03, x06, x07, x08, x09, x10, y02"               
##                           "x01, x04, x05, x06, x07, x08, x09, y02"               
##                           "x01, x04, x05, x06, x07, x08, x10, y02"               
##                           "x01, x04, x05, x06, x07, x09, x10, y02"               
##                           "x01, x04, x05, x06, x08, x09, x10, y02"               
##                           "x01, x04, x05, x07, x08, x09, x10, y02"               
##                           "x01, x04, x06, x07, x08, x09, x10, y02"               
##                           "x01, x05, x06, x07, x08, x09, x10, y02"               
##                           "x02, x03, x04, x05, x06, x07, x08, y02"               
##                           "x02, x03, x04, x05, x06, x07, x09, y02"               
##                           "x02, x03, x04, x05, x06, x07, x10, y02"               
##                           "x02, x03, x04, x05, x06, x08, x09, y02"               
##                           "x02, x03, x04, x05, x06, x08, x10, y02"               
##                           "x02, x03, x04, x05, x06, x09, x10, y02"               
##                           "x02, x03, x04, x05, x07, x08, x09, y02"               
##                           "x02, x03, x04, x05, x07, x08, x10, y02"               
##                           "x02, x03, x04, x05, x07, x09, x10, y02"               
##                           "x02, x03, x04, x05, x08, x09, x10, y02"               
##                           "x02, x03, x04, x06, x07, x08, x09, y02"               
##                           "x02, x03, x04, x06, x07, x08, x10, y02"               
##                           "x02, x03, x04, x06, x07, x09, x10, y02"               
##                           "x02, x03, x04, x06, x08, x09, x10, y02"               
##                           "x02, x03, x04, x07, x08, x09, x10, y02"               
##                           "x02, x03, x05, x06, x07, x08, x09, y02"               
##                           "x02, x03, x05, x06, x07, x08, x10, y02"               
##                           "x02, x03, x05, x06, x07, x09, x10, y02"               
##                           "x02, x03, x05, x06, x08, x09, x10, y02"               
##                           "x02, x03, x05, x07, x08, x09, x10, y02"               
##                           "x02, x03, x06, x07, x08, x09, x10, y02"               
##                           "x02, x04, x05, x06, x07, x08, x09, y02"               
##                           "x02, x04, x05, x06, x07, x08, x10, y02"               
##                           "x02, x04, x05, x06, x07, x09, x10, y02"               
##                           "x02, x04, x05, x06, x08, x09, x10, y02"               
##                           "x02, x04, x05, x07, x08, x09, x10, y02"               
##                           "x02, x04, x06, x07, x08, x09, x10, y02"               
##                           "x02, x05, x06, x07, x08, x09, x10, y02"               
##                           "x03, x04, x05, x06, x07, x08, x09, y02"               
##                           "x03, x04, x05, x06, x07, x08, x10, y02"               
##                           "x03, x04, x05, x06, x07, x09, x10, y02"               
##                           "x03, x04, x05, x06, x08, x09, x10, y02"               
##                           "x03, x04, x05, x07, x08, x09, x10, y02"               
##                           "x03, x04, x06, x07, x08, x09, x10, y02"               
##                           "x03, x05, x06, x07, x08, x09, x10, y02"               
##                           "x04, x05, x06, x07, x08, x09, x10, y02"               
##                           "x01, x02, x03, x04, x05, x06, x07, x08, y02"          
##                           "x01, x02, x03, x04, x05, x06, x07, x09, y02"          
##                           "x01, x02, x03, x04, x05, x06, x07, x10, y02"          
##                           "x01, x02, x03, x04, x05, x06, x08, x09, y02"          
##                           "x01, x02, x03, x04, x05, x06, x08, x10, y02"          
##                           "x01, x02, x03, x04, x05, x06, x09, x10, y02"          
##                           "x01, x02, x03, x04, x05, x07, x08, x09, y02"          
##                           "x01, x02, x03, x04, x05, x07, x08, x10, y02"          
##                           "x01, x02, x03, x04, x05, x07, x09, x10, y02"          
##                           "x01, x02, x03, x04, x05, x08, x09, x10, y02"          
##                           "x01, x02, x03, x04, x06, x07, x08, x09, y02"          
##                           "x01, x02, x03, x04, x06, x07, x08, x10, y02"          
##                           "x01, x02, x03, x04, x06, x07, x09, x10, y02"          
##                           "x01, x02, x03, x04, x06, x08, x09, x10, y02"          
##                           "x01, x02, x03, x04, x07, x08, x09, x10, y02"          
##                           "x01, x02, x03, x05, x06, x07, x08, x09, y02"          
##                           "x01, x02, x03, x05, x06, x07, x08, x10, y02"          
##                           "x01, x02, x03, x05, x06, x07, x09, x10, y02"          
##                           "x01, x02, x03, x05, x06, x08, x09, x10, y02"          
##                           "x01, x02, x03, x05, x07, x08, x09, x10, y02"          
##                           "x01, x02, x03, x06, x07, x08, x09, x10, y02"          
##                           "x01, x02, x04, x05, x06, x07, x08, x09, y02"          
##                           "x01, x02, x04, x05, x06, x07, x08, x10, y02"          
##                           "x01, x02, x04, x05, x06, x07, x09, x10, y02"          
##                           "x01, x02, x04, x05, x06, x08, x09, x10, y02"          
##                           "x01, x02, x04, x05, x07, x08, x09, x10, y02"          
##                           "x01, x02, x04, x06, x07, x08, x09, x10, y02"          
##                           "x01, x02, x05, x06, x07, x08, x09, x10, y02"          
##                           "x01, x03, x04, x05, x06, x07, x08, x09, y02"          
##                           "x01, x03, x04, x05, x06, x07, x08, x10, y02"          
##                           "x01, x03, x04, x05, x06, x07, x09, x10, y02"          
##                           "x01, x03, x04, x05, x06, x08, x09, x10, y02"          
##                           "x01, x03, x04, x05, x07, x08, x09, x10, y02"          
##                           "x01, x03, x04, x06, x07, x08, x09, x10, y02"          
##                           "x01, x03, x05, x06, x07, x08, x09, x10, y02"          
##                           "x01, x04, x05, x06, x07, x08, x09, x10, y02"          
##                           "x02, x03, x04, x05, x06, x07, x08, x09, y02"          
##                           "x02, x03, x04, x05, x06, x07, x08, x10, y02"          
##                           "x02, x03, x04, x05, x06, x07, x09, x10, y02"          
##                           "x02, x03, x04, x05, x06, x08, x09, x10, y02"          
##                           "x02, x03, x04, x05, x07, x08, x09, x10, y02"          
##                           "x02, x03, x04, x06, x07, x08, x09, x10, y02"          
##                           "x02, x03, x05, x06, x07, x08, x09, x10, y02"          
##                           "x02, x04, x05, x06, x07, x08, x09, x10, y02"          
##                           "x03, x04, x05, x06, x07, x08, x09, x10, y02"          
##                           "x01, x02, x03, x04, x05, x06, x07, x08, x09, y02"     
##                           "x01, x02, x03, x04, x05, x06, x07, x08, x10, y02"     
##                           "x01, x02, x03, x04, x05, x06, x07, x09, x10, y02"     
##                           "x01, x02, x03, x04, x05, x06, x08, x09, x10, y02"     
##                           "x01, x02, x03, x04, x05, x07, x08, x09, x10, y02"     
##                           "x01, x02, x03, x04, x06, x07, x08, x09, x10, y02"     
##                           "x01, x02, x03, x05, x06, x07, x08, x09, x10, y02"     
##                           "x01, x02, x04, x05, x06, x07, x08, x09, x10, y02"     
##                           "x01, x03, x04, x05, x06, x07, x08, x09, x10, y02"     
##                           "x02, x03, x04, x05, x06, x07, x08, x09, x10, y02"     
##                           "x01, x02, x03, x04, x05, x06, x07, x08, x09, x10, y02"
##                                                                                                                                                                                                                                            
## Logistic_regression.model "model.coef"                                                                                                                                                                                                     
##                           "0.668696203656747, 16.780592597224"                                                                                                                                                                             
##                           "1.49755004114903, 0.38805477621661"                                                                                                                                                                             
##                           "2.01117033761985, -1.90681178698726"                                                                                                                                                                            
##                           "2.11819933216462, -1.23776102796479"                                                                                                                                                                            
##                           "2.09841425017925, -1.04079677128695"                                                                                                                                                                            
##                           "2.0445575539551, -1.09564436482866"                                                                                                                                                                             
##                           "2.31137740706332, -2.15796963858584"                                                                                                                                                                            
##                           "1.54546928727258, -0.437015373488124"                                                                                                                                                                           
##                           "1.55740640321859, -0.850242769305833"                                                                                                                                                                           
##                           "1.57856899676931, -0.924715671671278"                                                                                                                                                                           
##                           "0.68872167235125, 20.1551316888211, -1.63063118293326"                                                                                                                                                          
##                           "1.04949045406339, 15.416438724433, -1.2021013604396"                                                                                                                                                            
##                           "0.232440021162541, 19.6218056277921, 0.616569175696555"                                                                                                                                                         
##                           "1.17803780390162, 17.2338635712334, -0.966021285498719"                                                                                                                                                         
##                           "1.13704218792369, 16.5991121957465, -0.988661384528419"                                                                                                                                                         
##                           "1.44253775295108, 18.3571835121648, -2.30578697906108"                                                                                                                                                          
##                           "0.81666171266496, 17.6316365745315, -14.5197677978805"                                                                                                                                                          
##                           "0.570725872487564, 17.9630537092392, 1.90023906949622"                                                                                                                                                          
##                           "0.693213397549626, 16.686978951978, -0.450491017759056"                                                                                                                                                         
##                           "1.94423448099136, 0.677090116122816, -1.93936749423888"                                                                                                                                                         
##                           "2.06761503004635, 0.889770828251988, -1.33329190397112"                                                                                                                                                         
##                           "2.04363738162225, 0.751287116323522, -1.08966136014927"                                                                                                                                                         
##                           "2.01031557179877, 0.286344555773028, -1.08764404686255"                                                                                                                                                         
##                           "2.31241118725687, 1.81189145715423, -2.68353739835062"                                                                                                                                                          
##                           "1.49624721668707, 0.531029801169068, -0.927693006364962"                                                                                                                                                        
##                           "1.51965787178827, 0.33863936024858, -0.774421945235587"                                                                                                                                                         
##                           "1.52817809633656, 0.521316473254372, -1.04953370435221"                                                                                                                                                         
##                           "2.43127856179784, -1.701224322088, -0.981995912433187"                                                                                                                                                          
##                           "2.23662874523177, -1.59111651397651, -0.568530279670954"                                                                                                                                                        
##                           "2.34127231470107, -1.71148664325996, -0.823846302166586"                                                                                                                                                        
##                           "2.58060396235791, -1.59703213936745, -1.79204705081671"                                                                                                                                                         
##                           "2.02635029380939, -1.91927266629873, -0.796463338365073"                                                                                                                                                        
##                           "2.04059651856036, -1.92623483991427, -1.0929128260036"                                                                                                                                                          
##                           "2.05887505532734, -1.92228765817755, -0.99325940117721"                                                                                                                                                         
##                           "2.66604546237091, -1.22325056732478, -1.02484023803678"                                                                                                                                                         
##                           "2.83154554050716, -1.45158652187052, -1.31859237703548"                                                                                                                                                         
##                           "2.60265433292415, -0.924515049770377, -1.77291370728895"                                                                                                                                                        
##                           "2.12863697511349, -1.23991457951926, -0.628001473629808"                                                                                                                                                        
##                           "2.12609113385583, -1.28399228172512, 0.624700529718901"                                                                                                                                                         
##                           "2.11822851322287, -1.23820549042896, 0.00415765860562977"                                                                                                                                                       
##                           "2.28646129042277, -0.782776196269921, -0.710067960692751"                                                                                                                                                       
##                           "2.69501363632968, -0.854934451881055, -1.94957103969404"                                                                                                                                                        
##                           "2.10672670465612, -1.04273506664758, -0.479956934480767"                                                                                                                                                        
##                           "2.18035376228645, -1.12318012054373, -1.70370542480614"                                                                                                                                                         
##                           "2.15250074956419, -1.05884680147698, -1.02631651377351"                                                                                                                                                         
##                           "2.81580355267013, -1.10411907384308, -2.13194209547658"                                                                                                                                                         
##                           "2.07362164969902, -1.12476119318319, -1.04579299014833"                                                                                                                                                         
##                           "2.09989503631232, -1.14968584856877, -1.40329304676543"                                                                                                                                                         
##                           "2.16004404372809, -1.20507723710966, -1.51200585822631"                                                                                                                                                         
##                           "2.32737600306643, -2.1709994544369, -0.750300432654388"                                                                                                                                                         
##                           "2.35983332341826, -2.21110456770819, -1.35312279145953"                                                                                                                                                         
##                           "2.37241219732943, -2.19302682973817, -1.1214225335512"                                                                                                                                                          
##                           "1.5619504796955, -0.343600004278397, -0.822110095656757"                                                                                                                                                        
##                           "1.57690296713642, 0.511681213024264, -1.06620686767142"                                                                                                                                                         
##                           "1.58082786797196, -0.283210999054289, -0.832691212570586"                                                                                                                                                       
##                           "1.01375922091229, 18.1559845482033, -1.21363215405671, -1.04094389220266"                                                                                                                                       
##                           "-0.996704193252983, 37.3084504551888, -4.07415368650811, 2.36383235027821"                                                                                                                                      
##                           "1.13602451463579, 19.8984700497552, -1.23599010427057, -0.865508983023085"                                                                                                                                      
##                           "1.16112644119544, 20.0832866037399, -1.6544997552818, -1.00169301575882"                                                                                                                                        
##                           "1.41094053283231, 19.0013023450094, -0.342906583472768, -2.20039773116748"                                                                                                                                      
##                           "0.834615507758077, 20.0700520585387, -1.30680949678515, -13.7621363042333"                                                                                                                                      
##                           "0.588019057343584, 21.4471420865841, -1.62603847223486, 1.84857885413347"                                                                                                                                       
##                           "0.695897729312526, 20.0968187660548, -1.6148794118958, -0.137022408399344"                                                                                                                                      
##                           "0.601321389645762, 18.2165575507547, -1.18742655472829, 0.627784490517512"                                                                                                                                      
##                           "1.30245652135755, 16.1833615448058, -0.783648094192719, -0.728731723155398"                                                                                                                                     
##                           "1.3770041120622, 15.451712370703, -0.999168707936808, -0.824887244749017"                                                                                                                                       
##                           "1.62990431914953, 17.287027393642, -0.797560842698127, -2.10326847783087"                                                                                                                                       
##                           "1.21554109950632, 16.8688021490593, -1.33630130607241, -14.8289957213858"                                                                                                                                       
##                           "0.960063719065411, 16.337105203007, -1.14483970237503, 1.37549832770001"                                                                                                                                        
##                           "1.09048300959425, 15.2854434077054, -1.22489907291121, -0.630436478326317"                                                                                                                                      
##                           "0.810203608367882, 19.3924947567746, 0.494952221521403, -0.929032172050606"                                                                                                                                     
##                           "0.870278232037859, 18.1381564184785, 0.337011219097253, -0.927252089936138"                                                                                                                                     
##                           "0.6044488596464, 25.7884323169183, 1.50812328357393, -2.97690008112928"                                                                                                                                         
##                           "0.119485260637863, 22.5082359526414, 1.05252850704992, -18.1687123956096"                                                                                                                                       
##                           "0.230832873130269, 20.0816329717988, 0.507219238768621, 1.50780176444443"                                                                                                                                       
##                           "0.184001870816603, 20.0697041904593, 0.756846465705648, -0.932198885176067"                                                                                                                                     
##                           "1.34117252170415, 16.9174754094851, -0.727524531590806, -0.604661170579411"                                                                                                                                     
##                           "1.74099722786058, 18.678298992718, -0.719895619086381, -2.07653522734673"                                                                                                                                       
##                           "1.39237406983289, 18.8413480720594, -1.09887410993702, -16.0093075517219"                                                                                                                                       
##                           "1.1099870300303, 17.7168695148923, -0.917894854063872, 0.827324272312955"                                                                                                                                       
##                           "1.23128432810751, 17.1667542260681, -0.994987190938616, -0.73404056953324"                                                                                                                                      
##                           "1.85460845938732, 17.9646468860274, -0.902962906494145, -2.23577234226865"                                                                                                                                      
##                           "1.37642301027168, 18.1797168565394, -1.19246063584126, -16.4203141068196"                                                                                                                                       
##                           "1.05465496297579, 17.2699339677269, -0.932120107444374, 1.08473306619122"                                                                                                                                       
##                           "1.22720228435933, 16.4046675005592, -1.06382292313989, -1.02286969880487"                                                                                                                                       
##                           "1.68112536808466, 20.1457599693932, -2.54590386860941, -17.3196454503648"                                                                                                                                       
##                           "1.37537199858986, 18.9747539011907, -2.26087278923689, 0.965989575680207"                                                                                                                                       
##                           "1.49641975540563, 18.2986829249549, -2.34316452016377, -0.80251019188788"                                                                                                                                       
##                           "0.489526796779822, 24.1167081484624, -22.1116206012691, 6.65402808232312"                                                                                                                                       
##                           "0.547994170174039, 23.0637482054117, -24.1603311322842, 3.75934567991853"                                                                                                                                       
##                           "0.574728047104545, 18.6799900965236, 3.4614487893879, -1.58235871373885"                                                                                                                                        
##                           "2.38444584536592, 1.0947128476711, -1.74168730482733, -1.11200979143281"                                                                                                                                        
##                           "2.17745026768712, 0.853212806467682, -1.60241836224534, -0.629361483084676"                                                                                                                                     
##                           "2.27511914511536, 0.587204640795021, -1.74707340348811, -0.801819732508229"                                                                                                                                     
##                           "2.58856733331142, 1.88321926898216, -1.61350989306577, -2.35951199477244"                                                                                                                                       
##                           "1.95161204371739, 0.913996576087749, -1.97348247249665, -1.66381239084576"                                                                                                                                      
##                           "1.97561965454463, 0.61893561686749, -1.95327002919678, -0.953341142626756"                                                                                                                                      
##                           "1.98700576348325, 0.821696191579669, -1.96506198650176, -1.1926148237607"                                                                                                                                       
##                           "2.6815933900891, 1.4016483881462, -1.41240560070455, -1.1622388436432"                                                                                                                                          
##                           "2.78483662322868, 0.860262186218704, -1.55158956766794, -1.31451935947582"                                                                                                                                      
##                           "2.6635413001373, 2.11423084232075, -1.07688358285663, -2.35394275359323"                                                                                                                                        
##                           "2.08047077296947, 1.05518179170804, -1.34809985665548, -1.61818734127"                                                                                                                                          
##                           "2.07681997756462, 1.0137307482732, -1.4362059459557, 1.18254432303041"                                                                                                                                          
##                           "2.06652754488465, 0.893349673817664, -1.32380005000648, -0.0881827739169734"                                                                                                                                    
##                           "2.2299623049971, 0.583091318714008, -0.837432697481294, -0.660395869779224"                                                                                                                                     
##                           "2.76594433395452, 2.20547909436405, -0.983819083928594, -2.58977719266594"                                                                                                                                      
##                           "2.0519960260523, 0.959579028707148, -1.10801105456732, -1.38191405637865"                                                                                                                                       
##                           "2.12557654754054, 0.679207248740263, -1.1621693626632, -1.5901399273321"                                                                                                                                        
##                           "2.0996001191303, 0.923793360869731, -1.1266200959774, -1.27064734887453"                                                                                                                                        
##                           "2.79352057741008, 1.64305165021464, -1.05359203277735, -2.6110889063244"                                                                                                                                        
##                           "2.02512999104353, 0.5046164386345, -1.12109827156896, -1.51826187598026"                                                                                                                                        
##                           "2.07409849988242, 0.202372234696686, -1.14263559869816, -1.35779844168325"                                                                                                                                      
##                           "2.11124055369438, 0.481786635474503, -1.20018128100656, -1.62785958121476"                                                                                                                                      
##                           "2.37413607882028, 2.31474629451151, -2.87404664150409, -3.00667399098016"                                                                                                                                       
##                           "2.35347130417444, 1.75115663977342, -2.71279581886195, -1.10789566871386"                                                                                                                                       
##                           "2.4029050774102, 2.0375054919648, -2.80998391996341, -1.60043996292036"                                                                                                                                         
##                           "1.51619670110549, 0.463205961785326, -0.787878559035084, -0.6813355484734"                                                                                                                                      
##                           "1.52833945024013, 0.518818891142321, 0.0204458139619948, -1.0543316281661"                                                                                                                                      
##                           "1.52836326078396, 0.520216553628384, -0.00980040454620268, -1.04608093042372"                                                                                                                                   
##                           "2.69957531815294, -1.33854009818292, -1.02182520083275, -0.634691572670298"                                                                                                                                     
##                           "2.94099964626959, -1.40239673449206, -1.18786429586924, -1.05423406374112"                                                                                                                                      
##                           "2.79523807408716, -1.47573577871401, -0.728093802754742, -1.51427861193822"                                                                                                                                     
##                           "2.45084993852594, -1.71587970786302, -0.984971948270776, -0.944895741978533"                                                                                                                                    
##                           "2.43117655416532, -1.70181676439708, -0.980693039267333, -0.0163513611201389"                                                                                                                                   
##                           "2.43108237264102, -1.71389588867562, -0.947221825998174, -0.300720842261382"                                                                                                                                    
##                           "2.41446282190058, -1.56236229580192, -0.327401445534941, -0.68328273842833"                                                                                                                                     
##                           "2.73272328937954, -1.34477821634631, -0.453041536685153, -1.72060026846565"                                                                                                                                     
##                           "2.25070574123353, -1.60378027130567, -0.567304549889189, -0.76171198076028"                                                                                                                                     
##                           "2.31251449145843, -1.57467809542945, -0.652000456271812, -1.559148381547"                                                                                                                                       
##                           "2.29280485492251, -1.59832453499797, -0.584524513467061, -1.03783938151711"                                                                                                                                     
##                           "2.9151416831394, -1.38084657682726, -0.841172324132141, -1.79517543888626"                                                                                                                                      
##                           "2.37818201399902, -1.72224982396248, -0.858115131552414, -1.222709957581"                                                                                                                                       
##                           "2.40716062708572, -1.7253962025854, -0.885734531734308, -1.51485764068"                                                                                                                                         
##                           "2.45653300332802, -1.7078250849221, -0.937733214193719, -1.44555610656641"                                                                                                                                      
##                           "2.60437301859346, -1.61063279566931, -1.80635752682286, -0.996279691529361"                                                                                                                                     
##                           "2.64123551347356, -1.61770487488072, -1.85192231604428, -1.50442867695944"                                                                                                                                      
##                           "2.64780008161743, -1.61070655693106, -1.82744769990097, -1.15285991483488"                                                                                                                                      
##                           "2.05233108637079, -1.93602284453226, -0.693235970610539, -1.03734962620497"                                                                                                                                     
##                           "2.05838644857909, -1.92135668116572, 0.0806312925224853, -1.0154535674121"                                                                                                                                      
##                           "2.06532056470706, -1.92941119465702, -0.530804555702405, -0.826027861033374"                                                                                                                                    
##                           "3.01341263467405, -1.39727420261412, -0.666094490952056, -0.989292403198858"                                                                                                                                    
##                           "2.96981632401164, -0.915141937666766, -0.84233633342958, -1.54744884501877"                                                                                                                                     
##                           "2.67984054390808, -1.22640845050058, -1.02813818647087, -0.69165292407587"                                                                                                                                      
##                           "2.67492867754242, -1.19309634132253, -1.04900214213257, -0.438019336730102"                                                                                                                                     
##                           "2.666830712833, -1.20649998096762, -1.02831808062775, -0.156278580835042"                                                                                                                                       
##                           "3.25141020940274, -1.12782344507001, -1.27280604739387, -1.65854437579858"                                                                                                                                      
##                           "2.87858633463813, -1.46436821410213, -1.35886032775801, -1.48407996877199"                                                                                                                                      
##                           "2.83171287821499, -1.44996686608086, -1.31940755000058, -0.0246050193939764"                                                                                                                                    
##                           "2.84750302672899, -1.39927575093498, -1.35364256811073, -0.560350224319933"                                                                                                                                     
##                           "2.6215201925276, -0.926583153757534, -1.78628959917765, -0.86697635727559"                                                                                                                                      
##                           "2.60772581364756, -0.889300125886824, -1.80638914866946, -0.421064638184681"                                                                                                                                    
##                           "2.61097971508055, -0.870479318009188, -1.81067708327389, -0.458752942658318"                                                                                                                                    
##                           "2.13963365417471, -1.29343939684951, -0.731197031496943, 0.710882600892662"                                                                                                                                     
##                           "2.13429902837159, -1.26744297648263, -0.865808083357889, 0.250509443528065"                                                                                                                                     
##                           "2.12657458467047, -1.27208602050994, 0.786542678127045, -0.223607293376754"                                                                                                                                     
##                           "2.96388023163809, -0.588917281042616, -0.833720045655648, -2.01383416861433"                                                                                                                                    
##                           "2.30828964680518, -0.775084312576649, -0.7381136350466, -0.866658066011984"                                                                                                                                     
##                           "2.38283345928093, -0.85754897205148, -0.743737261368595, -1.8602248569356"                                                                                                                                      
##                           "2.38804647116851, -0.762891910016321, -0.8224647229226, -1.40141781687965"                                                                                                                                      
##                           "2.71127066472649, -0.856099442156882, -1.96180219003542, -0.742283285916944"                                                                                                                                    
##                           "2.81155535997302, -0.950934207505874, -2.00619507527044, -2.03491504109463"                                                                                                                                     
##                           "2.76364588833927, -0.868504297747709, -1.9798861767026, -1.16886196959436"                                                                                                                                      
##                           "2.18471138532708, -1.12348436688886, -0.311203822560594, -1.67863210059899"                                                                                                                                     
##                           "2.15093548703615, -1.05935606470722, 0.56554788932343, -1.18398384054542"                                                                                                                                       
##                           "2.19471073247457, -1.11528692478073, -1.26489344581578, -0.647254434734313"                                                                                                                                     
##                           "2.86333321848014, -1.14469353136845, -2.15467029037263, -1.35711264980449"                                                                                                                                      
##                           "2.91234376344318, -1.17428891556657, -2.19826976686527, -1.85949304292016"                                                                                                                                      
##                           "2.96808730938298, -1.23366854531761, -2.18150649993002, -1.71447457842324"                                                                                                                                      
##                           "2.12331856374272, -1.17288859354179, -0.93951292549761, -1.34016556788911"                                                                                                                                      
##                           "2.15798903893349, -1.20278289961784, 0.260955877745545, -1.58122264112456"                                                                                                                                      
##                           "2.16865319197347, -1.21361960093164, -0.55468734182649, -1.33709779238537"                                                                                                                                      
##                           "2.37138128820188, -2.22000984082874, -0.619483334099716, -1.30559354209366"                                                                                                                                     
##                           "2.3705998872928, -2.19055373911295, 0.281558518588681, -1.19949595287276"                                                                                                                                       
##                           "2.38703765453247, -2.2164592954912, -0.786771932780009, -0.865842703655152"                                                                                                                                     
##                           "1.57881586802844, 0.462668910158672, -0.218018129317452, -0.981908099607258"                                                                                                                                    
##                           "-0.612735544388043, 34.0344396524625, -3.53024157775582, -0.725209321974928, 2.15233839858193"                                                                                                                  
##                           "1.24871268094069, 18.5845375212943, -1.04353987958655, -0.671596357332348, -0.675329949147343"                                                                                                                  
##                           "1.35102106579465, 18.4853187625918, -1.32129683868029, -0.806977697814745, -0.867365181590783"                                                                                                                  
##                           "1.61902444296223, 17.4819130860549, -0.0961076454559492, -0.789441526669236, -2.07558356567874"                                                                                                                 
##                           "1.20904695117223, 18.3288464041714, -0.817867012679649, -1.23482049261239, -14.7882881789054"                                                                                                                   
##                           "0.91783286613139, 19.2310042786879, -1.23028330919905, -0.979028991577156, 1.41360176270865"                                                                                                                    
##                           "1.039913415286, 17.9655657275553, -1.16176168082268, -1.06128321351814, -0.383787022313078"                                                                                                                     
##                           "-0.471819679417961, 34.67422424089, -3.49289516949371, 2.05640962771618, -0.578395913499699"                                                                                                                    
##                           "-0.455059828835479, 34.9301521423105, -3.74975242568998, 2.0514312877211, -0.673686247640521"                                                                                                                   
##                           "-0.339291860007921, 38.1146714452717, -3.0121932267242, 2.65053425642143, -2.57145152618346"                                                                                                                    
##                           "-1.01294632660243, 39.3250867606878, -3.88550864279008, 2.66037942893057, -18.7412978530923"                                                                                                                    
##                           "-0.986241892941502, 37.3694540072734, -4.02247360237569, 2.30388460554388, 0.529907677826713"                                                                                                                   
##                           "-1.03753541715238, 37.7389626981614, -4.05858962178262, 2.50844283128236, -1.19049505186775"                                                                                                                    
##                           "1.32002004525191, 19.8704923153214, -1.39453826624565, -0.573732833771126, -0.704483281583403"                                                                                                                  
##                           "1.73695947482973, 18.7424615280468, -0.0326787744069261, -0.717887530548441, -2.06693464243445"                                                                                                                 
##                           "1.36999969388682, 20.3035299800993, -0.820337002058208, -1.02682992788796, -15.687434103734"                                                                                                                    
##                           "1.05734458092142, 20.5307201190295, -1.25589303439408, -0.809546823166338, 0.91955617706485"                                                                                                                    
##                           "1.173242023837, 19.7243125196601, -1.17031496052984, -0.889980742917845, -0.488915633184566"                                                                                                                    
##                           "1.81692827867374, 18.7862095066439, -0.429800843836126, -0.910223129404319, -2.10140055329744"                                                                                                                  
##                           "1.41058053176466, 20.3749813145122, -1.28685436216416, -1.19532553691413, -16.3033580209396"                                                                                                                    
##                           "1.07953778609416, 20.8065589070051, -1.65167368279483, -0.948093857726917, 1.03624657266744"                                                                                                                    
##                           "1.21914721942863, 19.7929018098941, -1.57304036673115, -1.04880727984313, -0.697774001626218"                                                                                                                   
##                           "1.70692606297937, 19.7407221611106, 0.290368177968212, -2.64296272503072, -17.4591412768798"                                                                                                                    
##                           "1.33903061029607, 19.7021838860717, -0.36846006950722, -2.14671516649339, 0.993748060325731"                                                                                                                    
##                           "1.47333651513905, 18.7057498004321, -0.214220355551061, -2.27473890375525, -0.755467543150283"                                                                                                                  
##                           "0.525057194578708, 26.2803346918692, -1.19639643864488, -22.4393002826006, 6.59691496058628"                                                                                                                    
##                           "0.495734627465817, 29.4331052730575, -2.1071716287437, -29.2181474777397, 5.43544073318255"                                                                                                                     
##                           "0.587723580031964, 21.7800702258845, -1.48543920786327, 2.99326171610192, -1.20005719660824"                                                                                                                    
##                           "0.906310197060909, 18.465643856475, -0.799263611998743, 0.535566151088685, -0.683220144382361"                                                                                                                  
##                           "1.05823393999779, 17.2376675271702, -1.01089139459251, 0.405549435101062, -0.747461908190231"                                                                                                                   
##                           "0.794433533616128, 24.4115394906699, -0.68073748330388, 1.45155726700502, -2.76661549363298"                                                                                                                    
##                           "0.462475272875149, 22.0304687532144, -1.35821120268726, 1.15180022625387, -19.010172267181"                                                                                                                     
##                           "0.586898486782948, 18.5794169495754, -1.14841209906186, 0.557760522919389, 0.978636311925572"                                                                                                                   
##                           "0.548215057816433, 18.7663682250359, -1.22429269942007, 0.80682962498912, -1.14705253914678"                                                                                                                    
##                           "1.46145827038366, 15.9089958893444, -0.769594654633302, -0.49721575470162, -0.598658705107744"                                                                                                                  
##                           "1.7930100890454, 17.9591396942808, -0.472160439202545, -0.581520066078041, -1.99531765746932"                                                                                                                   
##                           "1.51190226156699, 18.056062696771, -0.87152513336085, -0.832000914042856, -15.8435797482099"                                                                                                                    
##                           "1.23958468830852, 16.6325769579425, -0.775514117906876, -0.687461722887018, 0.742014812958903"                                                                                                                  
##                           "1.36047170576712, 16.0978151314933, -0.79641540065626, -0.754798289116774, -0.776814433112824"                                                                                                                  
##                           "1.93998434498697, 17.195706097445, -0.588073679738561, -0.796175833189331, -2.08380591342169"                                                                                                                   
##                           "1.61761269082882, 17.5627122571278, -1.09198939549805, -1.02175095343936, -16.2944315021617"                                                                                                                    
##                           "1.31110307234043, 15.9635398040531, -0.975197109359998, -0.786616638104695, 0.778372302812991"                                                                                                                  
##                           "1.47884000939599, 15.2522164798011, -1.01854254087114, -0.905695905091342, -1.08532465809792"                                                                                                                   
##                           "1.88354243569835, 19.4942974585668, -0.925020872683787, -2.32692860233861, -17.3685031171196"                                                                                                                   
##                           "1.57333029189707, 17.7776701247838, -0.770138656356929, -2.075867843866, 0.714750985440721"                                                                                                                     
##                           "1.69580391918825, 17.2107032166798, -0.823800893956988, -2.13938603062855, -0.889725982246132"                                                                                                                  
##                           "0.862541839615118, 23.0686969565646, -1.16726347927505, -21.5044168893213, 5.84939786068634"                                                                                                                    
##                           "0.923354889171896, 22.4120810330047, -1.24551746769141, -23.6651238006473, 3.44493245547847"                                                                                                                    
##                           "0.964079838447615, 17.0491888947633, -1.14025384601096, 2.8110932515921, -1.52942947899199"                                                                                                                     
##                           "1.06188885068202, 18.4821012718058, 0.350553403752772, -0.728048376099877, -0.535513480730364"                                                                                                                  
##                           "0.918574958350674, 25.2659127272063, 1.38516503070924, -0.586633716246023, -2.73388090650845"                                                                                                                   
##                           "0.726378522394324, 23.1618379784955, 0.974325404252343, -1.05288482688673, -19.3853680369222"                                                                                                                   
##                           "0.789489921442371, 19.5777982134713, 0.461974083187498, -0.899891130691154, 0.546572489080181"                                                                                                                  
##                           "0.762963452132461, 20.0430617235705, 0.671904880528116, -0.961125590973169, -1.1582491560109"                                                                                                                   
##                           "1.01844748691314, 24.2669010930651, 1.27206718431747, -0.628888600425262, -2.80607602949034"                                                                                                                    
##                           "0.809176586172988, 21.6741812422453, 0.7808559103456, -1.07678720295954, -19.0852260122193"                                                                                                                     
##                           "0.840627573911141, 18.486514114023, 0.283912383311589, -0.88803495606574, 0.938316247829907"                                                                                                                    
##                           "0.847866234126633, 18.668402481054, 0.509624571328273, -0.989539886835507, -1.30546839918955"                                                                                                                   
##                           "0.5394957090299, 32.4072888968268, 2.33890333493668, -3.74559797712368, -26.9538904298331"                                                                                                                      
##                           "0.602660646913611, 25.8105245412388, 1.50103511034635, -2.97029098585391, 0.082533981462118"                                                                                                                    
##                           "0.539025659952532, 27.4815624378763, 1.88014193659172, -3.24260754573304, -2.12450247725766"                                                                                                                    
##                           "-0.0512420867859695, 27.6188574332837, 0.873013070063207, -24.8203729521039, 5.82940919302809"                                                                                                                  
##                           "-0.0843752010500751, 27.6174253746989, 0.970490599217661, -27.9220849975063, 3.67702249839769"                                                                                                                  
##                           "0.113610479267042, 21.6988826596953, 0.699688053476095, 3.10641137540817, -1.95239641366164"                                                                                                                    
##                           "1.94961121746674, 18.1968036640582, -0.476367340764165, -0.666577066441781, -2.10955980838184"                                                                                                                  
##                           "1.59771225337416, 18.8010864747086, -0.78207744523958, -0.790534039045017, -16.7848166562892"                                                                                                                   
##                           "1.28887285787388, 17.2649670288742, -0.700167155426502, -0.588144125110347, 0.58267141049679"                                                                                                                   
##                           "1.43417663252708, 16.7786285314319, -0.736382495894959, -0.678646706154771, -1.0246627706438"                                                                                                                   
##                           "2.03732422469583, 21.2425853440882, -0.852773893675547, -2.30392135940939, -18.4651098670104"                                                                                                                   
##                           "1.71289728665197, 18.8582707987167, -0.702321493801086, -2.0679337330813, 0.298024440597096"                                                                                                                    
##                           "1.81868138492213, 18.7112895155613, -0.753727588671497, -2.11318551108585, -0.977387186108857"                                                                                                                  
##                           "1.02116984333042, 23.7677391849336, -0.880024021530848, -21.6637493771896, 5.05212750799131"                                                                                                                    
##                           "1.10932499069656, 24.6911189103247, -1.07645690397874, -25.6628585971617, 3.53333887964069"                                                                                                                     
##                           "1.10534728637554, 18.3583153296764, -0.898530377405618, 2.06864619894213, -1.37585681474686"                                                                                                                    
##                           "2.20901982588554, 20.6642536680233, -1.14867673850703, -2.50534228044776, -19.1239241367294"                                                                                                                    
##                           "1.82008886803079, 18.2057963819733, -0.883256841426356, -2.22050780660778, 0.373312113683808"                                                                                                                   
##                           "1.98733067803265, 17.8550678677524, -1.00254108625536, -2.28848207669407, -1.32096645733299"                                                                                                                    
##                           "1.01882305062992, 23.4161818838897, -0.998927491917897, -22.0463964445815, 5.18388801018941"                                                                                                                    
##                           "1.06848267033468, 22.9138720215976, -1.03939573442913, -24.123455170488, 3.13746465955083"                                                                                                                      
##                           "1.09962112154175, 17.9731832583346, -0.992266476211018, 2.66214633025, -1.87362300250271"                                                                                                                       
##                           "1.37212357415587, 25.9673014921975, -2.44766799604627, -23.9680068517198, 5.17325640739983"                                                                                                                     
##                           "1.40652627459751, 27.4490632682191, -2.67698026802445, -28.9552248700933, 4.110266247377"                                                                                                                       
##                           "1.38942191662857, 19.7798217897077, -2.27628462631302, 2.27136246759664, -1.59078664611681"                                                                                                                     
##                           "0.380215014379361, 26.7188022628411, -26.8629183453268, 4.96861563859959, 2.36855581488448"                                                                                                                     
##                           "2.71627571804296, 1.39393861653456, -1.31337308487115, -1.21837288649561, -0.781284114952133"                                                                                                                   
##                           "2.893978503497, 1.04927321004625, -1.45268714717754, -1.31353742036439, -1.04002057003215"                                                                                                                      
##                           "2.86910530855233, 2.1513641774912, -1.47665951185331, -0.90423281360604, -2.11660018864932"                                                                                                                     
##                           "2.41193583882559, 1.33088393973199, -1.78246004385976, -1.13248207999422, -2.23560138771614"                                                                                                                    
##                           "2.38639238751465, 1.15239450762142, -1.72422621193092, -1.16487680255835, 0.564977735373762"                                                                                                                    
##                           "2.38168660194244, 1.115284371141, -1.76111566808139, -1.06122826196479, -0.435938788308216"                                                                                                                     
##                           "2.34874750311827, 0.707018971035491, -1.5759638570614, -0.397028458739719, -0.622284250643831"                                                                                                                  
##                           "2.79732480128733, 2.09948929675077, -1.28332494449047, -0.598711548569925, -2.34495256474345"                                                                                                                   
##                           "2.19213716452085, 1.11452817351667, -1.63151715051023, -0.645887659774867, -1.82054165999081"                                                                                                                   
##                           "2.25145653209935, 0.788548924998244, -1.58667700486941, -0.701084857718616, -1.42501477003988"                                                                                                                  
##                           "2.2378244804523, 1.02285693550242, -1.61187891311155, -0.665933386190066, -1.30248626293397"                                                                                                                    
##                           "2.89252602817959, 1.74646661426282, -1.4176146907929, -0.768443986757386, -2.31646305865174"                                                                                                                    
##                           "2.3021798751553, 0.866425808669275, -1.77973173067648, -0.84322796090147, -2.04595568185496"                                                                                                                    
##                           "2.34622055426876, 0.501065492601051, -1.75436969438418, -0.863333078036602, -1.39890439498385"                                                                                                                  
##                           "2.38442024855087, 0.773928148934086, -1.75493353934048, -0.923852663977223, -1.62940052160242"                                                                                                                  
##                           "2.66772190232362, 2.43333287771716, -1.65379353695704, -2.57021645512752, -3.40387714622603"                                                                                                                    
##                           "2.64088625123719, 1.82108111897845, -1.63051636279806, -2.39523907845535, -1.25413914340461"                                                                                                                    
##                           "2.69251399350464, 2.12952280756483, -1.63871398053487, -2.49842434427279, -1.67521394590541"                                                                                                                    
##                           "1.97719794255317, 0.840111349724029, -1.98163373722269, -1.51749480545708, -0.778528411026268"                                                                                                                  
##                           "1.9844151880271, 0.911237078054437, -1.97749103702462, -0.788561885330266, -1.01453822482848"                                                                                                                   
##                           "1.98916488350652, 0.811048075055848, -1.96583538987189, -0.100349529303401, -1.15832345911384"                                                                                                                  
##                           "2.99548526515057, 1.20443829474946, -1.5446039590029, -0.816476781234269, -0.9012490237936"                                                                                                                     
##                           "3.15208782594365, 2.57446198659897, -1.13516555695244, -1.02917420881874, -2.22021109643038"                                                                                                                    
##                           "2.72104067956873, 1.63624932482051, -1.43831377528335, -1.19653719791868, -2.278455959013"                                                                                                                      
##                           "2.6789940074923, 1.41449908048332, -1.42436960495109, -1.15569226452801, 0.147683916910299"                                                                                                                     
##                           "2.68235628221149, 1.41711470943606, -1.37737268021255, -1.17147797128378, -0.326600867693441"                                                                                                                   
##                           "3.30046659648097, 1.98203845487066, -1.27852829428878, -1.24146526483792, -2.21022810219601"                                                                                                                    
##                           "2.84073799140142, 1.10071203246347, -1.58764979415729, -1.3729394472714, -2.55641673574373"                                                                                                                     
##                           "2.77998695490975, 0.901509716725261, -1.58430677752375, -1.3003057496943, 0.41388889276144"                                                                                                                     
##                           "2.80058809957943, 0.888868505689295, -1.49067144568983, -1.35571408987753, -0.656743840721892"                                                                                                                  
##                           "2.73576872049532, 2.55150581231425, -1.0827994622467, -2.53441040507775, -3.38848800421255"                                                                                                                     
##                           "2.66200092319712, 2.12680088604677, -1.09117038663881, -2.3445264068938, 0.155048217492823"                                                                                                                     
##                           "2.67941939226474, 2.18916322225097, -0.977828482519801, -2.4496181414289, -0.823868094330061"                                                                                                                   
##                           "2.09703832448355, 1.27311593132693, -1.49365023886309, -2.04822974240701, 1.60098319841686"                                                                                                                     
##                           "2.08887698732831, 1.08211154113595, -1.39633243000546, -2.03078960302864, 0.407879380802847"                                                                                                                    
##                           "2.07341661356092, 1.09039508550631, -1.41764348284064, 1.69174392058332, -0.579789701307232"                                                                                                                    
##                           "2.98253185820831, 1.99281796394107, -0.758600619017224, -0.685726993671364, -2.58004468200797"                                                                                                                  
##                           "2.24653031580665, 0.813683297653011, -0.846011577953353, -0.687885533305997, -1.61022569726658"                                                                                                                 
##                           "2.33064261403428, 0.495522721016942, -0.90057484690363, -0.700528896680006, -1.77058430730666"                                                                                                                  
##                           "2.32782430859506, 0.770187920732927, -0.836580380767269, -0.769929064638121, -1.5813349798379"                                                                                                                  
##                           "2.85839837072779, 2.79696065320067, -1.02387537940437, -2.81026961545199, -3.49548245100094"                                                                                                                    
##                           "2.87525667499728, 2.14761001542543, -1.07008479740619, -2.63340188687106, -1.85164845213641"                                                                                                                    
##                           "2.89082896791883, 2.48880102912246, -1.03272519423638, -2.73143719864955, -1.79564533602743"                                                                                                                    
##                           "2.12736809038198, 0.846984433733844, -1.172110249493, -1.13143898376231, -1.46895198226181"                                                                                                                     
##                           "2.09850822190893, 0.963270550893628, -1.12897181023427, -0.335244182608805, -1.19515195381489"                                                                                                                  
##                           "2.13393299393935, 0.842222075390687, -1.16014819435869, -0.883323377342455, -0.983143980010142"                                                                                                                 
##                           "2.89483267891564, 2.19503169360407, -1.11828672755729, -2.82312422062562, -3.50981603961688"                                                                                                                    
##                           "2.882289796529, 1.55292689941532, -1.12043999032496, -2.64699374184928, -1.62317744393278"                                                                                                                      
##                           "2.98921980226438, 1.93320361957059, -1.21269527549954, -2.77211182757217, -2.17195133033752"                                                                                                                    
##                           "2.08179361403493, 0.392134908581202, -1.16666815090715, -1.31684955189395, -1.22518841401426"                                                                                                                   
##                           "2.11045534847199, 0.507110650293914, -1.20169252876361, -0.220486809816395, -1.57808029563555"                                                                                                                  
##                           "2.12009425381983, 0.446119505144986, -1.20581731952052, -0.329957209514454, -1.51505310965055"                                                                                                                  
##                           "2.40140101100276, 2.22787913982611, -2.88204612453393, -2.84347076265979, -0.816220167409332"                                                                                                                   
##                           "2.42224645931556, 2.30052935679683, -2.90223656365649, -1.96332444775469, -1.21400938387465"                                                                                                                    
##                           "2.40266447287836, 2.0390025507359, -2.81001088448117, 0.0135006771057285, -1.60527973984124"                                                                                                                    
##                           "1.52848964551745, 0.51801102706325, 0.0193321517510143, -0.00841626298729483, -1.05110564360995"                                                                                                                
##                           "3.01597743199406, -1.25375208242035, -1.18753170220121, -0.32570152124702, -0.919134364170034"                                                                                                                  
##                           "2.96895821197777, -1.1968552734826, -0.760699354604497, -0.497153981081517, -1.41383040248499"                                                                                                                  
##                           "2.71857756335743, -1.35303380840973, -1.02497127583284, -0.634276453570703, -0.915957536333761"                                                                                                                 
##                           "2.71010663835057, -1.34235915729428, -0.981529475715722, -0.66444828055593, -0.555347543398914"                                                                                                                 
##                           "2.70097358176549, -1.35072679440451, -0.983188224787371, -0.638303842864936, -0.337083140414015"                                                                                                                
##                           "3.2687478153369, -1.17501731154661, -0.933648767592444, -1.02744566059259, -1.45027195783009"                                                                                                                   
##                           "2.99450934942427, -1.41401668777867, -1.20176472865925, -1.09753073957449, -1.56838334259259"                                                                                                                   
##                           "2.94524391072544, -1.41550427243704, -1.15622527190767, -1.06720835645255, -0.43858920813761"                                                                                                                   
##                           "2.9631578429342, -1.42090664224804, -1.1154331940051, -1.09844585693682, -0.718509317373249"                                                                                                                    
##                           "2.82157852881424, -1.49057363999435, -0.730251661725675, -1.52794724786344, -1.08511560083387"                                                                                                                  
##                           "2.80835527691113, -1.49924935563244, -0.654926029705746, -1.57796364484066, -0.822731623563091"                                                                                                                 
##                           "2.80975597944452, -1.49797859876538, -0.643719522998921, -1.56763320367162, -0.670070801646217"                                                                                                                 
##                           "2.4516513543252, -1.71295333391828, -0.991900712545528, -0.955603589423072, 0.0854061217889853"                                                                                                                 
##                           "2.45008178225676, -1.71699581709221, -0.980260164815144, -0.908421075818833, -0.0399171988048939"                                                                                                               
##                           "2.43249057760427, -1.70819626888595, -0.958241974680211, 0.231842895161216, -0.365515677577856"                                                                                                                 
##                           "2.96217836122347, -1.27311264407445, -0.231137218460817, -0.754697742776873, -1.76864350353373"                                                                                                                 
##                           "2.44495769351959, -1.58014646114991, -0.310467602326191, -0.722293879041071, -1.13574414443034"                                                                                                                 
##                           "2.50623200042231, -1.54490970738578, -0.403462866755527, -0.71961981306442, -1.72239879118312"                                                                                                                  
##                           "2.52069944945727, -1.57029669698668, -0.301711931380636, -0.805004474867147, -1.40562595055823"                                                                                                                 
##                           "2.75397603466558, -1.35986205394845, -0.449287287329183, -1.73436003055839, -0.952102161618676"                                                                                                                 
##                           "2.84181133726564, -1.3168083631015, -0.552374971288092, -1.77849725612834, -1.87512795664984"                                                                                                                   
##                           "2.80377804348913, -1.35220222464399, -0.464251048471982, -1.75267566822728, -1.17181867905458"                                                                                                                  
##                           "2.32166035845855, -1.58499589693253, -0.648910230463528, -0.613364767180028, -1.50816839797107"                                                                                                                 
##                           "2.29214786660213, -1.59563747964437, -0.585602381184475, 0.179467661008646, -1.08759202795731"                                                                                                                  
##                           "2.32857341409323, -1.58430519580507, -0.638737798629732, -1.06891648994046, -0.715747977127357"                                                                                                                 
##                           "2.96575435176756, -1.38889524964981, -0.883287957550223, -1.81598021772001, -1.42658826690217"                                                                                                                  
##                           "3.01849581327384, -1.38916577157427, -0.915142775201624, -1.86480313521455, -1.89011956908692"                                                                                                                  
##                           "3.06125679278528, -1.3663776879355, -0.972444063974433, -1.84476225404745, -1.61596827698364"                                                                                                                   
##                           "2.43757541847995, -1.73463370491766, -0.913494060749834, -1.12054413811401, -1.4409484217789"                                                                                                                   
##                           "2.45712081395834, -1.70843605128791, -0.938186637299105, -0.0625540853338697, -1.42889688263376"                                                                                                                
##                           "2.47089599155591, -1.71598513197827, -0.949754869143915, -0.741025820456729, -1.21761245613064"                                                                                                                 
##                           "2.6593450886362, -1.62854861938881, -1.86186063830038, -0.865478919435174, -1.43911771811196"                                                                                                                   
##                           "2.64802310170582, -1.6110001277331, -1.82761260746071, -0.0275191135888812, -1.14527708434312"                                                                                                                  
##                           "2.66903641061997, -1.62071290259992, -1.85646474940996, -0.945605255357251, -0.853010607626462"                                                                                                                 
##                           "2.0656144966229, -1.92992018576517, -0.0385416054014501, -0.536211028202911, -0.813698950704948"                                                                                                                
##                           "3.35223549681124, -1.08537889882431, -0.495984382106627, -1.03518532295288, -1.56066932678882"                                                                                                                  
##                           "3.05344245169039, -1.41078067706824, -0.655538441184882, -1.03104109416414, -1.31804852803586"                                                                                                                  
##                           "3.02581681491396, -1.36082728609849, -0.694326673284988, -0.993285816286819, -0.547783492549022"                                                                                                                
##                           "3.02773826483321, -1.34831765233926, -0.663829262715353, -1.0233484445778, -0.52525455614671"                                                                                                                   
##                           "2.98951702018486, -0.917865698138918, -0.844100194346028, -1.55957118722229, -0.8646359549141"                                                                                                                  
##                           "3.01284125016805, -0.818938871017624, -0.903063680776087, -1.62614518348206, -1.18462644281117"                                                                                                                 
##                           "2.98245024352661, -0.850181441253155, -0.84988269530777, -1.59062354559122, -0.539163323355372"                                                                                                                 
##                           "2.68625869830835, -1.20057602416909, -1.04831161840889, -0.642937057667783, -0.368281603530932"                                                                                                                 
##                           "2.68060040739789, -1.23262314389664, -1.02714559726406, -0.744378332247776, 0.0558684238928868"                                                                                                                 
##                           "2.67453551518774, -1.19034728632129, -1.04831793093721, -0.40671097394047, -0.0456620599644766"                                                                                                                 
##                           "3.31194390297395, -1.14021369936033, -1.32069921631077, -1.6791255383748, -1.65068636493978"                                                                                                                    
##                           "3.27663747771946, -1.05996463856827, -1.30054657245269, -1.72452580716106, -0.90007737411497"                                                                                                                   
##                           "3.30228934556034, -1.02346390462862, -1.33554791213595, -1.73334559344575, -0.998076576785584"                                                                                                                  
##                           "2.87836327829068, -1.47251017132592, -1.35540020369709, -1.49754156201526, 0.118770551654788"                                                                                                                   
##                           "2.87872252636726, -1.4426804968589, -1.36716015075118, -1.29283332528241, -0.215266397443491"                                                                                                                   
##                           "2.84822775282289, -1.41594955785911, -1.34744543319, 0.426029582730288, -0.682861854593834"                                                                                                                     
##                           "2.62460315178827, -0.897487649369025, -1.81302266284751, -0.823847585915453, -0.342583650973446"                                                                                                                
##                           "2.62114203264181, -0.893121700558374, -1.80540070768994, -0.613277942546297, -0.27976975104653"                                                                                                                 
##                           "2.61211165796908, -0.862257755502662, -1.8199325591851, -0.166352132568737, -0.410303411942073"                                                                                                                 
##                           "2.13975531693714, -1.29402296610674, -0.739049455297503, 0.705173491480646, 0.00917062283347167"                                                                                                                
##                           "3.00342789218519, -0.576351042697925, -0.875823308495397, -2.03710693832645, -1.21184755151853"                                                                                                                 
##                           "3.10289268161664, -0.677450637188376, -0.876589904758247, -2.07800303725375, -2.22752488232843"                                                                                                                 
##                           "3.10139975039818, -0.560882164339998, -0.969876588138114, -2.06668684410367, -1.62285146631325"                                                                                                                 
##                           "2.39817707500485, -0.849996250613956, -0.764950780766236, -0.71128254672192, -1.80802419091639"                                                                                                                 
##                           "2.38574808468433, -0.76496437395598, -0.818174998773409, 0.386779714434716, -1.50519935879634"                                                                                                                  
##                           "2.42448906615967, -0.81890563858604, -0.813265978168887, -1.18131943369571, -1.03847151963914"                                                                                                                  
##                           "2.82159660185844, -0.95034598882009, -2.01401899607626, -0.556653209257422, -1.99155612437442"                                                                                                                  
##                           "2.76167740486559, -0.869104058041574, -1.97665400163791, 0.344790823718739, -1.26507986398904"                                                                                                                  
##                           "2.82772376267651, -0.94015387429893, -2.01148941133111, -1.5801580407442, -0.686214566797643"                                                                                                                   
##                           "2.19253329050873, -1.11374897624654, 0.307041355287826, -1.22218680631222, -0.745739679342858"                                                                                                                  
##                           "2.95126749375548, -1.20746307117897, -2.21613461802455, -1.23661291025143, -1.78123478359576"                                                                                                                   
##                           "2.96792892522931, -1.2335341188334, -2.181383097761, 0.0130591173796692, -1.71793532187848"                                                                                                                     
##                           "2.99233571142701, -1.24707764829462, -2.2087603807099, -0.998247046239706, -1.39925922000784"                                                                                                                   
##                           "2.16730131435064, -1.21215391787453, 0.139295447257227, -0.536277822043586, -1.37988125651412"                                                                                                                  
##                           "2.38613576030137, -2.21516107565231, 0.103081527674434, -0.772614320972378, -0.899145604931243"                                                                                                                 
##                           "-0.328065024820776, 32.9746416215594, -3.24595168170944, -0.506513119680716, 1.97848420703311, -0.43724475754582"                                                                                               
##                           "-0.201702152277193, 32.4728493195724, -3.3335857751404, -0.605397640039573, 1.91177508664938, -0.586946040286942"                                                                                               
##                           "-0.150572041192401, 36.1915530131843, -2.74057571578041, -0.385081332346843, 2.51979476992216, -2.48134241506528"                                                                                               
##                           "-0.539383082424068, 35.5688911920891, -3.19580020021436, -0.970164228556762, 2.45389066001262, -20.2554137612955"                                                                                               
##                           "-0.611340426669116, 34.1095488886007, -3.50496352732324, -0.714866607993387, 2.11803035779072, 0.326243955756822"                                                                                               
##                           "-0.647491389690705, 34.4743006494073, -3.49883992117624, -0.771375320652714, 2.31900243474838, -1.33348384349379"                                                                                               
##                           "1.42178667378022, 18.6412028742407, -1.20555916934005, -0.635587106548312, -0.401711810789928, -0.68631007899814"                                                                                               
##                           "1.80081542782402, 17.835353820455, 0.0601326681287553, -0.475756930958236, -0.584181534255651, -2.01234118692559"                                                                                               
##                           "1.49266025912943, 19.1293527027011, -0.604294214404144, -0.814350282899144, -0.796295616041903, -15.793502537492"                                                                                               
##                           "1.17516072693154, 19.1787619413846, -1.06448284019186, -0.660480533829849, -0.627220189849604, 0.835179737101812"                                                                                               
##                           "1.29485537435882, 18.3462754910207, -0.961828183488482, -0.689097538535864, -0.698670352728829, -0.568571898946192"                                                                                             
##                           "1.9150399209633, 17.7036093079452, -0.246844158670465, -0.564124669960211, -0.805402679621805, -2.01238719053346"                                                                                               
##                           "1.622366674844, 19.1743839948817, -0.916132628653843, -0.972207113330473, -1.04586433300204, -16.4376754759756"                                                                                                 
##                           "1.28150012951521, 19.0821391308484, -1.32796357697139, -0.780984697831704, -0.829019915611751, 0.798846921414088"                                                                                               
##                           "1.42829449690995, 18.0973774099249, -1.21263626219161, -0.837144091626255, -0.921727573757369, -0.827562641741208"                                                                                              
##                           "1.93950354168881, 18.7078284505249, 0.560616428417224, -0.966386127410566, -2.50579583678852, -17.5068873731329"                                                                                                
##                           "1.55825330870525, 18.0407351476403, -0.123171904143172, -0.759234004414875, -2.04001563632664, 0.727754837436968"                                                                                               
##                           "1.70536938497768, 17.0612457479967, 0.0726635119656573, -0.830495010985068, -2.16116149333934, -0.90644088562964"                                                                                               
##                           "0.857040008064137, 24.6798687803213, -0.817913793557746, -1.07108386533686, -21.9146359213686, 5.90383797176679"                                                                                                
##                           "0.822018313828541, 27.6951777737064, -1.65587782726943, -1.0437821425388, -27.8578329342727, 4.76900759861139"                                                                                                  
##                           "0.924351005614612, 19.5240822540184, -1.07370947709862, -0.995537114129878, 2.56723859951168, -1.25799722241064"                                                                                                
##                           "-0.245285561652471, 33.724071752728, -3.43853037541831, 1.92277437429727, -0.396813295428472, -0.478034033527767"                                                                                               
##                           "-0.0473907746047948, 36.3408048695364, -2.65844530762852, 2.44338992342602, -0.345221073965226, -2.46418980994763"                                                                                              
##                           "-0.392290278828849, 36.9622182530941, -3.21951355413088, 2.35392896282096, -0.748949635770129, -20.0637673793833"                                                                                               
##                           "-0.47378385798548, 34.6977646102835, -3.48894302582888, 2.04979450474934, -0.57449879316275, 0.0770674204462144"                                                                                                
##                           "-0.499520916615563, 35.2764040102983, -3.46650369859147, 2.22678895167235, -0.623701076228266, -1.37454284889075"                                                                                               
##                           "0.0162665763815143, 36.1527120085414, -2.79995727845221, 2.40942985996853, -0.457858669760693, -2.46171554571858"                                                                                               
##                           "-0.324433523631467, 36.3313646709567, -3.45271199052728, 2.27208389102538, -0.835361470092421, -20.2937001018512"                                                                                               
##                           "-0.457388801317936, 34.9847831940105, -3.73115345121364, 2.02940604256296, -0.665344330283001, 0.235439458162383"                                                                                               
##                           "-0.459656448949768, 35.2729585639801, -3.70476625589239, 2.19943507643665, -0.728963310017209, -1.4515264283877"                                                                                                
##                           "-0.205856134640891, 42.3007451669514, -2.46265791058402, 3.19415786690859, -3.36754610950356, -26.572441462757"                                                                                                 
##                           "-0.338077359697668, 38.073898669139, -3.02734858520327, 2.67600563220278, -2.58655947602349, -0.234206045918206"                                                                                                
##                           "-0.365394132226628, 39.3993553836903, -2.91085499211197, 2.96363513253232, -2.83318525445204, -2.21816199137415"                                                                                                
##                           "-0.978274434367044, 41.3160985369985, -3.48105784775355, 2.3499088389701, -25.7877431514648, 4.05258636357424"                                                                                                  
##                           "-1.59088443786197, 54.8631530734278, -5.2347865357161, 2.91095858728432, -51.0041625584611, 9.18640625406081"                                                                                                   
##                           "-1.05826749120843, 38.6856105217242, -3.91096493623307, 2.42972688728033, 1.86218024156677, -1.98323135831539"                                                                                                  
##                           "1.9267082932911, 18.607228852773, -0.213542414611807, -0.457226624241285, -0.680206784508009, -2.04717954309925"                                                                                                
##                           "1.59516243001892, 20.4611932523623, -0.992066513932087, -0.670426701547206, -0.856313054837327, -16.6920649569988"                                                                                              
##                           "1.25968031177728, 20.3250010527624, -1.40556368492971, -0.541230055720123, -0.687160660208792, 0.650275696393976"                                                                                               
##                           "1.38949663664868, 19.5796504700473, -1.29891151499353, -0.591944785660575, -0.749347598254764, -0.771380121910128"                                                                                              
##                           "2.12098800186448, 20.295554631706, 0.70060974199695, -0.900514897091636, -2.52933266028138, -18.7727352219714"                                                                                                  
##                           "1.70622017161184, 18.9585571637631, -0.0486185922046714, -0.698922073102292, -2.05347476139194, 0.305022557886073"                                                                                              
##                           "1.84204348455708, 18.3896900272029, 0.163132100867043, -0.765067353276795, -2.16291092799966, -1.01460867449407"                                                                                                
##                           "0.999043869629565, 25.3842318987411, -0.847054153849406, -0.803579669257876, -21.9525838973667, 5.15302158083819"                                                                                               
##                           "0.999262448485525, 29.7928906842378, -1.63965899862627, -0.948957763595727, -29.7072320881862, 4.80318480887581"                                                                                                
##                           "1.05678567783383, 20.8356186382149, -1.12805356672425, -0.804680946845715, 1.89977724220023, -1.11228466735867"                                                                                                 
##                           "2.22871054587639, 20.3474681790373, 0.241362929611944, -1.14707399737373, -2.58685292360484, -19.1789794680048"                                                                                                 
##                           "1.77916371178567, 19.0682256640098, -0.438236907680519, -0.889418171034791, -2.0826573486402, 0.398606249899222"                                                                                                
##                           "1.96278011133768, 18.2701131354068, -0.213588499996694, -1.00137662777071, -2.21909173264399, -1.2717841180878"                                                                                                 
##                           "1.06335435141897, 25.5950049649329, -1.2342651754824, -1.01189697083048, -22.4352970185478, 5.1176614541002"                                                                                                    
##                           "1.00055869733476, 29.0193118990284, -2.02276779027834, -1.00248520282845, -28.9192365932623, 4.72059541307727"                                                                                                  
##                           "1.1077539609163, 21.092421656154, -1.47549660143598, -0.989638382314727, 2.25176149840265, -1.48538926491697"                                                                                                   
##                           "1.39663014348363, 25.5463591710298, 0.27506206881201, -2.53965927638089, -23.9488047627376, 5.13968885989849"                                                                                                   
##                           "1.33954707057882, 28.9090952247552, -0.550396056038498, -2.51096332880419, -30.0258501037399, 4.49258226573369"                                                                                                 
##                           "1.37553518693841, 20.0374521776435, -0.13343737845578, -2.23436615266664, 2.25498829622244, -1.55853043499764"                                                                                                  
##                           "0.366082022349691, 31.9859386396294, -1.86814187159715, -30.8695580511564, 4.32920119452359, 3.68634649559231"                                                                                                  
##                           "1.14535127065966, 17.6457058666801, -0.784571800011361, 0.399633968875053, -0.492409184822533, -0.520716845471978"                                                                                              
##                           "0.969171010451594, 24.4918601949164, -0.428035227397831, 1.37435565343217, -0.457911617420508, -2.65033528557134"                                                                                               
##                           "0.790908673882983, 22.6680548859143, -0.936214383233315, 1.06579925896488, -0.75885433937032, -19.5936191164029"                                                                                                
##                           "0.888917747112575, 18.6249833415189, -0.793637887218325, 0.508727583943409, -0.661392752132652, 0.43676164577309"                                                                                               
##                           "0.856235616600473, 19.1333352055049, -0.825218816471747, 0.72869706560679, -0.70791051759558, -1.23815704677961"                                                                                                
##                           "1.10252860183446, 23.3883811337456, -0.562187965246587, 1.26519796743266, -0.521965019610075, -2.65499483790047"                                                                                                
##                           "0.95833751784499, 21.5865291244928, -1.14981801756242, 0.92396200341336, -0.873956174950152, -19.4785390042781"                                                                                                 
##                           "1.03474336486374, 17.4851988699127, -0.99177730114361, 0.371335160573457, -0.72490739574305, 0.591466112566451"                                                                                                 
##                           "1.03618966782401, 17.8281658549424, -1.0425278625145, 0.602468958584587, -0.81308053967209, -1.42287004050892"                                                                                                  
##                           "0.734681710472288, 31.4789048879131, -0.849846409183827, 2.3297113672264, -3.52678089329315, -27.2022170739668"                                                                                                 
##                           "0.798071796516779, 24.3722375839467, -0.68460008844547, 1.46131519580869, -2.77491739315198, -0.116748205239213"                                                                                                
##                           "0.7319890708346, 26.1265922761556, -0.717819660922214, 1.84030050621265, -3.03249579483822, -2.17456207758359"                                                                                                  
##                           "0.255202730623859, 26.9625974667528, -1.21371775706118, 1.00040695433043, -24.5910767950837, 4.94394872575523"                                                                                                  
##                           "0.230375970122971, 27.3383394548585, -1.27503374502984, 1.07461973841694, -27.7318326798819, 3.33000594024261"                                                                                                  
##                           "0.463374427541177, 20.2337904627789, -1.14950139110746, 0.763084746220726, 2.48911959373181, -1.94779463110538"                                                                                                 
##                           "1.98772386208698, 17.5742111477966, -0.42613565585356, -0.358885842091272, -0.647461195384672, -2.02911310852362"                                                                                               
##                           "1.71229558263422, 18.1007158961294, -0.857394634682167, -0.519596837071811, -0.789581411681197, -16.5752525313492"                                                                                              
##                           "1.41502367210793, 16.2188308440106, -0.764083924668688, -0.475100070148624, -0.583966415482128, 0.502274283368812"                                                                                              
##                           "1.56156891824736, 15.7459567142297, -0.787156983820283, -0.499018688546479, -0.678849621801577, -1.06828198576221"                                                                                              
##                           "2.08690414198729, 20.6460243909923, -0.549870989870267, -0.689515963708608, -2.21397095025407, -18.2862917081992"                                                                                               
##                           "1.76695147636407, 18.1276297318025, -0.470258354765437, -0.565905418469449, -1.98771529606506, 0.273454610984565"                                                                                               
##                           "1.87318811240372, 17.9733559809056, -0.483719338779792, -0.611950151472102, -2.03094961186074, -0.992702250120584"                                                                                              
##                           "1.13549268507598, 23.0892551859306, -0.842038534442202, -0.622554432798643, -21.3527404425753, 4.92994893920214"                                                                                                
##                           "1.22005407418003, 23.9591187065004, -0.790162096054368, -0.835343905046931, -25.0671766179943, 3.40107458637839"                                                                                                
##                           "1.23540799059783, 17.278279036067, -0.785153561365566, -0.663115568340623, 1.99060127314839, -1.3924207412838"                                                                                                  
##                           "2.28908932185536, 20.1987417398562, -0.653576100048523, -1.03129393673231, -2.34060501284956, -18.9072075180704"                                                                                                
##                           "1.91582995550809, 17.3651494603806, -0.581540373256111, -0.784022547604859, -2.07513619957337, 0.24903752769388"                                                                                                
##                           "2.07597895456166, 17.0881513669174, -0.600041075887562, -0.896650933007, -2.13379859648462, -1.32768411821621"                                                                                                  
##                           "1.25737560039865, 22.6348635773878, -0.987787126877437, -0.853589483907187, -21.5368694429544, 4.71893722496672"                                                                                                
##                           "1.30169675763503, 22.4059259624564, -1.04731969264758, -0.875497507493746, -23.7333552984737, 2.97739284899202"                                                                                                 
##                           "1.35258509873549, 16.6817829145671, -0.95965408839621, -0.850778361627075, 2.25489438060269, -1.79328442541964"                                                                                                 
##                           "1.55932516495374, 25.2035431251139, -0.793849612454673, -2.26469180358644, -23.5211005711727, 4.78372964563788"                                                                                                 
##                           "1.58902025123097, 26.8990814401163, -0.820214164241078, -2.48214944779776, -28.4792839329983, 3.89813392697088"                                                                                                 
##                           "1.58632361049574, 18.5926071893263, -0.766400987563484, -2.09243671295126, 1.98178398604298, -1.56264553870535"                                                                                                 
##                           "0.742022923320007, 25.9138196015081, -1.15825530237149, -26.3607387047171, 4.32576736905644, 2.33136949579765"                                                                                                  
##                           "1.11632177693306, 24.3707436229861, 1.26099603516562, -0.457172936653289, -0.398957106957602, -2.68416839482518"                                                                                                
##                           "1.00359760944991, 22.4183302854954, 0.817088580242205, -0.796067683311644, -0.652956080652357, -19.541230653911"                                                                                                
##                           "1.04207708763591, 18.6378789553006, 0.327629971453194, -0.708264062488048, -0.52811380983687, 0.422881536396778"                                                                                                
##                           "1.03622756681473, 19.1061555884001, 0.532954319370044, -0.740959550917827, -0.5922753328554, -1.3213386316149"                                                                                                  
##                           "0.887653351173838, 32.5855606976695, 2.23596143077393, -0.702620801783164, -3.49568633568405, -27.4724538062631"                                                                                                
##                           "0.939273568914803, 25.1357665688714, 1.41534428795429, -0.607773555827833, -2.75824753106169, -0.41090127198572"                                                                                                
##                           "0.861889326977524, 27.158634774941, 1.77663266262952, -0.62620102102479, -3.00216558477779, -2.19920745583977"                                                                                                  
##                           "0.473514629600244, 27.1717497465909, 0.867677828598131, -0.866483910916516, -24.3024187708864, 4.34822270633337"                                                                                                
##                           "0.510351568640458, 28.6008597651764, 0.88624652100476, -1.03023467732736, -28.8479485029438, 3.42816279767419"                                                                                                  
##                           "0.658947461443534, 21.1175201393169, 0.65363156676169, -0.873865255962937, 1.89399257117717, -1.75415930005063"                                                                                                 
##                           "1.02957751556571, 31.172240675856, 2.07736666225455, -0.790742571443085, -3.55306813122575, -27.3631565086296"                                                                                                  
##                           "1.02573031330961, 24.2108806348512, 1.28309940574295, -0.634684579330806, -2.81678880065813, -0.15849408929524"                                                                                                 
##                           "1.00139262342606, 25.9034785274438, 1.64233058863372, -0.712942680734174, -3.06431391821428, -2.31508950648528"                                                                                                 
##                           "0.540495283844521, 26.3088179460311, 0.694874524819692, -0.913908902109485, -24.2906804473877, 4.7469956695718"                                                                                                 
##                           "0.522674939631817, 26.4550251297855, 0.756092898265479, -0.933593947142903, -27.0437615029361, 3.14147206845232"                                                                                                
##                           "0.74173773851149, 20.1245600316603, 0.48758625587542, -0.925541567929708, 2.53206491054701, -2.12294046926281"                                                                                                  
##                           "0.368360904025699, 36.8313555747177, 2.25258722059529, -3.67567288258801, -32.696667329554, 3.70781527329846"                                                                                                   
##                           "0.298897978305703, 40.2808531784713, 2.29781436072972, -3.89504177417895, -40.1878810899045, 4.15518437008092"                                                                                                  
##                           "0.463247961912674, 28.7991675258284, 1.86061831186429, -3.19494318732457, 1.82194973111094, -2.78345349014439"                                                                                                  
##                           "-0.16942479269359, 30.4628682478161, 0.876768494212094, -29.9645876636158, 4.28793663657023, 2.39631403236521"                                                                                                  
##                           "2.3155109134461, 21.1645752796254, -0.517513296296427, -0.898098748714381, -2.37777614758838, -19.401613815379"                                                                                                 
##                           "1.94266738676246, 18.2387600760804, -0.473117371577593, -0.664585433687244, -2.10757292715593, 0.0673189022082191"                                                                                              
##                           "2.08285964292121, 18.147376265944, -0.482749101841356, -0.764440065706505, -2.1625775363333, -1.30988024932077"                                                                                                 
##                           "1.23583381008168, 23.3120963901162, -0.615213531853703, -0.704848297235783, -21.7075023923693, 4.49260856385107"                                                                                                
##                           "1.28615038052623, 24.1416260877336, -0.833650079018301, -0.602495123902678, -25.199833275782, 3.20920014374922"                                                                                                 
##                           "1.31237826392541, 17.9098197078916, -0.646483674864196, -0.670996950890382, 1.95029813989591, -1.63690832195402"                                                                                                
##                           "1.70578907393284, 25.9465238047701, -0.66791665615708, -2.28104771588893, -23.8199984747019, 4.17137268441134"                                                                                                  
##                           "1.76839807374556, 29.3475218513833, -0.858900168938088, -2.4703344985902, -30.7702083355238, 4.0351553636695"                                                                                                   
##                           "1.7170560639032, 19.6580813871533, -0.682418388219796, -2.090653656978, 1.45864921107765, -1.46027241332013"                                                                                                    
##                           "0.924591623127177, 27.1276414822455, -0.949209958252491, -27.5095498260869, 3.27081212943131, 2.70278134333977"                                                                                                 
##                           "1.89213784872072, 25.4142656531707, -0.996195639267646, -2.44561664323507, -24.1201993275884, 4.00959248656866"                                                                                                 
##                           "1.8756491614226, 27.1426830055639, -0.963274145084208, -2.62306098417172, -28.7510528562984, 3.52289870841895"                                                                                                  
##                           "1.88163499635583, 19.044982045316, -0.956302816107771, -2.24144486784359, 1.77765019471015, -1.91814009581769"                                                                                                  
##                           "0.884080832523058, 25.8457286460666, -0.936701800505186, -26.2705531463554, 3.90817893403383, 2.074848275042"                                                                                                   
##                           "1.25889259267266, 30.432096381336, -2.60893021646937, -31.1868480331598, 3.45128065783593, 3.0355665540368"                                                                                                     
##                           "2.99970606907427, 1.22690888071493, -1.24505379738822, -1.34344865135962, -0.48137004300601, -0.83152528863254"                                                                                                 
##                           "3.14249517696649, 2.44685722943326, -1.0836853576834, -0.993207643377741, -0.707322067208808, -2.06499701889671"                                                                                                
##                           "2.7621549411334, 1.66427447084598, -1.34434439961002, -1.24652864156765, -0.81093079980405, -2.54210557206886"                                                                                                  
##                           "2.71607518484759, 1.3950078172459, -1.31326827038043, -1.21942274369351, -0.780764009908962, 0.0123084905112647"                                                                                                
##                           "2.71695891443473, 1.41980930999407, -1.33168728680182, -1.15928743206784, -0.790275883526517, -0.515876140615224"                                                                                               
##                           "3.32099017195849, 2.03390703080029, -1.19570207774573, -1.0964611326757, -0.979830722129608, -2.02077555340506"                                                                                                 
##                           "2.96340917728034, 1.33553067849209, -1.48939949201514, -1.35374423170292, -1.10256197114076, -2.89026491357845"                                                                                                 
##                           "2.89331983625645, 1.05404286783124, -1.45152910540821, -1.31769336432183, -1.03844683114421, 0.0486523115303854"                                                                                                
##                           "2.91626683009434, 1.09118319240963, -1.47770447392449, -1.22784498007469, -1.09230643905939, -0.851118993783855"                                                                                                
##                           "2.95991426746087, 2.64797637120463, -1.52186199915745, -0.912583962450598, -2.31590412633913, -3.74252330468099"                                                                                                
##                           "2.87246485703206, 2.13195085134943, -1.48349891781157, -0.880123125411403, -2.13131982522447, -0.246759081131297"                                                                                               
##                           "2.89509190456006, 2.25694327981523, -1.5151319491147, -0.766527621215454, -2.24267389752016, -1.07838194619017"                                                                                                 
##                           "2.41934957520132, 1.46708892227104, -1.75594186296936, -1.23272154947855, -2.49236861220353, 1.02617004148406"                                                                                                  
##                           "2.41429182902239, 1.33817096830959, -1.77903750237452, -1.14856396637756, -2.35821961271692, 0.126570797423924"                                                                                                 
##                           "2.38353590051389, 1.25401698689429, -1.73914891606031, -1.13397229291714, 1.20113801026001, -0.775483033576481"                                                                                                 
##                           "2.97612378173395, 1.91949537147568, -1.22955975062181, -0.414094389506266, -0.601999333713245, -2.32808877097445"                                                                                               
##                           "2.37496109861987, 0.986551864351739, -1.60907461810783, -0.397801126106006, -0.660801232190611, -2.04671722235524"                                                                                              
##                           "2.44233779357471, 0.626568097635804, -1.55819435868918, -0.45995458551268, -0.663992761594692, -1.60639238822281"                                                                                               
##                           "2.45491898867986, 0.888522583493122, -1.58690722792562, -0.390344921962183, -0.744705389345154, -1.60733670481817"                                                                                              
##                           "2.89375966289973, 2.69631532514425, -1.30713015118125, -0.632306369443675, -2.56937937645708, -3.61149235636608"                                                                                                
##                           "2.89945382558143, 2.05047114312929, -1.25966821306926, -0.686889351102247, -2.3912810610267, -1.70290491074842"                                                                                                 
##                           "2.92426229047374, 2.38408754577344, -1.28379068496699, -0.647233585544613, -2.49164171387596, -1.7798935115769"                                                                                                 
##                           "2.25660189022793, 1.01837762560203, -1.61376757328556, -0.707446290519117, -1.60988716013736, -1.24870425043041"                                                                                                
##                           "2.23636530566254, 1.12144310848105, -1.62442407534282, -0.668641072946499, -0.873285973829063, -1.10869444447238"                                                                                               
##                           "2.26182666816405, 0.96641304558448, -1.60322735533946, -0.692866597955465, -0.622571171273678, -1.09971420080102"                                                                                               
##                           "3.00297688247456, 2.32352194486479, -1.44650240951639, -0.830986331542483, -2.5379867977215, -3.71616552200216"                                                                                                 
##                           "2.9869333570021, 1.65869689038668, -1.42281907944372, -0.839431888222029, -2.35559842017146, -1.63410706184671"                                                                                                 
##                           "3.08766489202367, 2.03364099069751, -1.40885043804535, -0.932163694403112, -2.48069528242191, -2.10321156765134"                                                                                                
##                           "2.36280003984903, 0.755883965641746, -1.78264988266303, -0.893426968138835, -1.85752242376842, -1.2128340833655"                                                                                                
##                           "2.38374319083753, 0.872177665549151, -1.7684270215568, -0.927707871356276, -0.895334665920565, -1.43223411748263"                                                                                               
##                           "2.39532230042934, 0.735707030961277, -1.75654802767552, -0.930903744619881, -0.366322313292656, -1.50737770627546"                                                                                              
##                           "2.70317427514097, 2.34252407793856, -1.6646553701936, -2.582441061403, -3.23471111021218, -0.929148751778749"                                                                                                   
##                           "2.72081712517805, 2.43749162419384, -1.66069700734445, -2.60689529170138, -2.40250648604247, -1.20941686494034"                                                                                                 
##                           "2.69477815211646, 2.11831061340233, -1.63970103938983, -2.4983767467669, -0.10568740373058, -1.63817472889294"                                                                                                  
##                           "1.98769662094043, 0.896569295218162, -1.97894840491251, -0.808445118953315, -0.154972635948083, -0.956835536392137"                                                                                             
##                           "3.46273319254314, 2.32386105529985, -1.25762940286005, -0.725083359788414, -0.878026592288315, -2.16870531459694"                                                                                               
##                           "3.05954256416758, 1.46279543389435, -1.58455676898266, -0.834427095717994, -0.951171564660458, -2.70535775327128"                                                                                               
##                           "2.99719692820154, 1.19808877962603, -1.53890675122845, -0.819307264612929, -0.902380667088878, -0.0735230173792625"                                                                                             
##                           "3.01109605833346, 1.22973141900037, -1.48351000512391, -0.817824333973465, -0.941264557916308, -0.646723011789223"                                                                                              
##                           "3.26378211204817, 3.10536716271748, -1.14841903529589, -1.07776120573484, -2.42740129020675, -3.98153489519907"                                                                                                 
##                           "3.1749366713323, 2.53512317988491, -1.07564211662595, -1.06044418025048, -2.25914033308892, -0.681350422859005"                                                                                                 
##                           "3.18424102769977, 2.68458816663713, -1.01090242718673, -1.05458970433392, -2.34233092123155, -1.02410803855253"                                                                                                 
##                           "2.71390160948604, 1.70216688339528, -1.48639957003678, -1.17398079861886, -2.41305587326463, 0.561535021222142"                                                                                                 
##                           "2.72507101066415, 1.65221467141901, -1.46931108401867, -1.19326726136475, -2.53448025654025, 0.255392333882575"                                                                                                 
##                           "2.6732777821334, 1.46994343893744, -1.40469646524815, -1.15199385705396, 0.528126806316885, -0.47087196334278"                                                                                                  
##                           "3.43646888959921, 2.48494691248646, -1.30810805360464, -1.32529821319909, -2.41477568586905, -4.15882255128242"                                                                                                 
##                           "3.31104418369517, 1.95210585791878, -1.24661408226866, -1.25453181449448, -2.23118180051552, -0.382800046327651"                                                                                                
##                           "3.3743024495556, 2.10722598683549, -1.13609685026849, -1.32841214528827, -2.35817745693938, -1.35864951722894"                                                                                                  
##                           "2.8356864087022, 1.21016847246782, -1.66048738358007, -1.34821394212856, -2.75664000339976, 0.859313836966362"                                                                                                  
##                           "2.84101547048006, 1.09652756057796, -1.57837591772961, -1.37649869585923, -2.47634058726579, -0.0857044468879508"                                                                                               
##                           "2.79506735987776, 1.02248583004304, -1.55248019096268, -1.33728501518414, 1.18845731762297, -1.00034983258024"                                                                                                  
##                           "2.73338995172972, 2.63006241557147, -1.14333951078878, -2.50521849688501, -3.5425033080232, 0.63187075627918"                                                                                                   
##                           "2.73555354325128, 2.5458962949221, -1.06609973984165, -2.54303072890703, -3.26390814063379, -0.132581905937534"                                                                                                 
##                           "2.67538239732902, 2.29254616421731, -1.02974202386608, -2.42620694812523, 0.935736134993793, -1.10896151703104"                                                                                                 
##                           "2.09562380020753, 1.27638044591447, -1.48813178739679, -1.96454834619843, 1.67690580262155, -0.103286262391839"                                                                                                 
##                           "3.09598697482774, 2.59059366502484, -0.783394441131076, -0.737452841270198, -2.80968311170828, -3.70336391003397"                                                                                               
##                           "3.11534835349415, 1.91803789110061, -0.835764738806734, -0.732804451569077, -2.62626230947616, -2.03569807799633"                                                                                               
##                           "3.17932258535132, 2.28585436940504, -0.762215403212122, -0.842035432684452, -2.74648082965026, -2.14446013889159"                                                                                               
##                           "2.33846002369835, 0.685859510728233, -0.903345302563018, -0.720919850771881, -1.35560293042939, -1.63439271660155"                                                                                              
##                           "2.32682876843792, 0.811921886271952, -0.838579505746159, -0.770714669521297, -0.364798022542301, -1.49981143648125"                                                                                             
##                           "2.36154379487636, 0.689591504666555, -0.870380258591377, -0.76915667053969, -0.877247618515637, -1.29241617821899"                                                                                              
##                           "2.94453129323509, 2.67597756716778, -1.09438818828404, -2.82401218724564, -3.24375544716436, -1.54550143473415"                                                                                                 
##                           "2.92356509245305, 2.80195233739166, -1.04801715446902, -2.84237890298455, -2.3661156414572, -1.34736685474889"                                                                                                  
##                           "2.92124480224606, 2.41928129164511, -1.0648005024024, -2.72915544946567, -0.791513425103093, -1.52929582852697"                                                                                                 
##                           "2.13374675522445, 0.891597633996272, -1.16448791195716, -0.459498582200852, -0.916573285773099, -0.868344093669447"                                                                                             
##                           "2.962600598086, 2.06367900070436, -1.17052622666485, -2.8342012557915, -3.29918841895056, -1.32251688161653"                                                                                                    
##                           "3.01675786201474, 2.20107534048195, -1.22446999134414, -2.86942403013845, -2.13899226036306, -1.76154608474525"                                                                                                 
##                           "2.9956012179155, 1.90555988625302, -1.21690233195702, -2.7711058511815, -0.25527021123746, -2.08342605917199"                                                                                                   
##                           "2.11964848675819, 0.474519189319736, -1.2079527427507, -0.268162919302819, -0.348670960707066, -1.44787249942573"                                                                                               
##                           "2.42365703369822, 2.29270929241835, -2.9023236345964, -1.9728917314621, -0.0760863103432619, -1.18465006157897"                                                                                                 
##                           "3.31026389115989, -1.07668706560241, -0.932064651870194, -0.217680769636171, -0.943974196230965, -1.42041257687597"                                                                                             
##                           "3.06259240009729, -1.27336660403077, -1.20119686763442, -0.306840161330709, -0.968035730358189, -1.48219321082393"                                                                                              
##                           "3.03041626791852, -1.25902975492492, -1.14095653401384, -0.358136123549456, -0.924849336271468, -0.66197498441812"                                                                                              
##                           "3.03454613730481, -1.27619229248851, -1.11868634932094, -0.315045445876146, -0.965394239667345, -0.68413661646444"                                                                                              
##                           "2.99300264775366, -1.21287978713663, -0.762825790092112, -0.493981143515663, -1.42688599733233, -1.04190648452314"                                                                                              
##                           "3.01113369576151, -1.19742022780537, -0.659537463470738, -0.558437565909428, -1.49498381438189, -1.21139044091457"                                                                                              
##                           "2.98385425082481, -1.21756542024453, -0.675381741346877, -0.49934962810428, -1.46705631215692, -0.674192055803978"                                                                                              
##                           "2.72604161581742, -1.35520094006624, -0.990674218613486, -0.659361370145812, -0.858371511085299, -0.46392058922439"                                                                                             
##                           "2.71711261931103, -1.35520802275095, -1.01317900085598, -0.635395443234425, -0.823789224945925, -0.100486649620097"                                                                                             
##                           "2.70818388986638, -1.34950526775288, -0.966504083495844, -0.658781246849416, -0.401178478322122, -0.22816661298386"                                                                                             
##                           "3.33154773976253, -1.18276391527879, -0.94706403686313, -1.07619044364709, -1.46805410417804, -1.67009968890696"                                                                                                
##                           "3.30025977905425, -1.19954761834585, -0.842236720762375, -1.0570667305195, -1.53086354477776, -1.13030243524468"                                                                                                
##                           "3.32245058647922, -1.19132378004621, -0.815573492370103, -1.09264601450376, -1.52849186209117, -1.06896047805116"                                                                                               
##                           "2.99598427772147, -1.42284886169571, -1.17931444304358, -1.10528341000573, -1.53750086820077, -0.299724972279953"                                                                                               
##                           "2.99518280458643, -1.42140006710451, -1.1604448256541, -1.11202624323597, -1.23546948693045, -0.382748313945416"                                                                                                
##                           "2.96314039792927, -1.42039742139001, -1.11640601055082, -1.09818828246817, 0.0219024392507076, -0.724623377736442"                                                                                              
##                           "2.83101445024613, -1.51038523878093, -0.664095517778325, -1.58406240839099, -1.00321490717805, -0.731797465986026"                                                                                              
##                           "2.82182774035642, -1.50058349627176, -0.669964015244162, -1.56040195848565, -0.665883337183426, -0.473998730669948"                                                                                             
##                           "2.81479672738852, -1.50787464776094, -0.616532885767948, -1.59548475243623, -0.502859048832793, -0.529648936572345"                                                                                             
##                           "2.4504819412338, -1.71361003458232, -0.985930652163339, -0.8843291314113, 0.135409996417685, -0.0848877501301162"                                                                                               
##                           "3.00663763487352, -1.29035279948111, -0.210677535948699, -0.802720447026806, -1.79116989914858, -1.36784469451221"                                                                                              
##                           "3.09415190767576, -1.23983411389972, -0.324065880689024, -0.799763129619537, -1.8347556870543, -2.06247685594148"                                                                                               
##                           "3.09983356210168, -1.27240743987955, -0.20157064567836, -0.894771967584677, -1.82111678128561, -1.58896164318471"                                                                                               
##                           "2.52921048596379, -1.5607162408854, -0.386620508790924, -0.75121362006507, -0.998768276766354, -1.64765496598286"                                                                                               
##                           "2.52062212537727, -1.57013538398857, -0.301827857471398, -0.804875933248141, 0.0110004289584945, -1.4085576468977"                                                                                              
##                           "2.55190417736756, -1.55777140347698, -0.354872481603414, -0.797803142734514, -1.00184927084451, -1.09604871109828"                                                                                              
##                           "2.85607337033317, -1.32952193442172, -0.546966842283982, -1.78776873445469, -0.783312302165008, -1.81227362360509"                                                                                              
##                           "2.80345782231382, -1.35139782003543, -0.464628381742159, -1.75228671201007, 0.0555065199507844, -1.18720069276178"                                                                                              
##                           "2.85991026778229, -1.32822481698814, -0.535857069942173, -1.78374271607194, -1.37450709246956, -0.7502150716364"                                                                                                
##                           "2.32893633187178, -1.58488676940233, -0.638778572860283, -0.0455168170146662, -1.07518672691716, -0.701213931381717"                                                                                            
##                           "3.06026114282734, -1.39632953608847, -0.949750560369165, -1.88089446619623, -1.31145054769969, -1.80775800436779"                                                                                               
##                           "3.06347837879041, -1.36791294904083, -0.973966629587315, -1.84599345745198, -0.183851883483238, -1.56705351864663"                                                                                              
##                           "3.09129978747182, -1.37553436100317, -0.987208254881234, -1.87551770202183, -1.10390245045524, -1.27332800586445"                                                                                               
##                           "2.47364399045908, -1.71858631690668, -0.9518868139835, -0.236453155541744, -0.772586120522866, -1.14484646338286"                                                                                               
##                           "2.67175578568173, -1.6236621162095, -1.8589645259099, -0.251758258425562, -0.980020701142091, -0.772412831894803"                                                                                               
##                           "3.40509915321943, -1.0988181035996, -0.480137540242197, -1.08712072706634, -1.5825455694093, -1.51322645217117"                                                                                                 
##                           "3.40252847780462, -0.984767662200274, -0.558873091524286, -1.04459507824922, -1.64396655492025, -1.28380573174835"                                                                                              
##                           "3.39901492918004, -0.984379840064584, -0.486875256420781, -1.09985552613307, -1.63617497316601, -0.955009503745417"                                                                                             
##                           "3.06098048090825, -1.38179064772577, -0.677857085167673, -1.03197043013278, -1.26616765539416, -0.417877449143289"                                                                                              
##                           "3.05351489047674, -1.38784325213425, -0.656159206250965, -1.03926432497222, -1.1165558769445, -0.224653892585627"                                                                                               
##                           "3.0313583399919, -1.33852404859106, -0.676807255286675, -1.02049565283606, -0.241613684790636, -0.456928647145191"                                                                                              
##                           "3.02690809732348, -0.825870319966358, -0.901399891080904, -1.63219470175666, -0.732476348105134, -1.11552547999699"                                                                                             
##                           "2.99066615799846, -0.870019024561065, -0.848813176142882, -1.58576136078849, -0.511862657975016, -0.388914892387564"                                                                                            
##                           "3.01337917264973, -0.800561368536433, -0.899054303469732, -1.6362667632442, -1.02557612464904, -0.258205481254815"                                                                                              
##                           "2.69131857800303, -1.21558134117436, -1.05134650060305, -0.830158508310923, -0.495774011284564, 0.215724316320273"                                                                                              
##                           "3.33009496591121, -1.07989612902799, -1.34173658856341, -1.73552300165482, -1.57494997012037, -0.772683080662673"                                                                                               
##                           "3.32648757281691, -1.06162125923493, -1.34819622451626, -1.72498772972468, -1.04426880203863, -0.712066242542006"                                                                                               
##                           "3.30722730631344, -1.00626380150379, -1.34006279864004, -1.75296957297183, -0.378632663492371, -0.888860421322005"                                                                                              
##                           "2.87820452829957, -1.45301578114432, -1.36235317238675, -1.23565668786276, 0.303621606568796, -0.318567069401309"                                                                                               
##                           "2.62339534235477, -0.882955711853059, -1.81817910328629, -0.657939916164538, -0.235760801064014, -0.1977163898147"                                                                                              
##                           "3.13263517288041, -0.665182325656389, -0.910138696566305, -2.09563951721015, -1.04793070548907, -2.15416694665"                                                                                                 
##                           "3.10019942502859, -0.561579484999723, -0.96840583390152, -2.0654427572336, 0.113937598925934, -1.65322754832261"                                                                                                
##                           "3.15701413044125, -0.633046859037974, -0.957159554961386, -2.09438640396872, -1.48948801360903, -1.15708165689373"                                                                                              
##                           "2.42314851649632, -0.818783703175136, -0.811939440641814, 0.140541293661388, -1.16277395308245, -1.08192315812875"                                                                                              
##                           "2.82773178177835, -0.940156969921447, -2.01149820384239, -0.000752018646151491, -1.5802618106502, -0.685971877226444"                                                                                           
##                           "2.99563038574539, -1.2496406201641, -2.21159464167954, -0.224909522012256, -1.02748200935228, -1.33016747528496"                                                                                                
##                           "-0.106997373666942, 32.0874320162364, -3.19849165440583, -0.499088097621681, 1.84952959087632, -0.259830281121448, -0.474216759170746"                                                                          
##                           "0.0184416925051951, 35.4251798937789, -2.54992909573327, -0.251900063787139, 2.3977924861508, -0.276175025945744, -2.42603264044183"                                                                            
##                           "-0.207890094874307, 34.7918132131659, -2.88314405251953, -0.6965835543684, 2.2774930833356, -0.555430738476272, -20.7861060216906"                                                                              
##                           "-0.329224829937564, 32.9885563765676, -3.24389710864474, -0.506180668672714, 1.97485585666966, -0.435186453812503, 0.0426140949674784"                                                                          
##                           "-0.354446444336553, 33.5469333031372, -3.21184897456507, -0.534894739194064, 2.15703059816386, -0.474339066039891, -1.41812319925798"                                                                           
##                           "0.128402045316173, 34.8406825927404, -2.60889134248169, -0.304087016174162, 2.33041253845422, -0.411304872685313, -2.39968698382999"                                                                            
##                           "0.00704961723318213, 33.3911073125856, -2.89714346832956, -0.843503944139168, 2.13525602295095, -0.733482876564855, -21.4286741575155"                                                                          
##                           "-0.203667359484084, 32.5036593715764, -3.32747837778772, -0.602887867473984, 1.90328462440875, -0.583818728687848, 0.0960993841161463"                                                                          
##                           "-0.198438634925097, 32.7996462489994, -3.2727700396639, -0.649383145937082, 2.07427284081472, -0.642104251407909, -1.5368104780247"                                                                             
##                           "0.0918855048267241, 39.4842143506441, -2.02109569303046, -0.6480773127824, 3.03182936541174, -3.25542933068963, -27.2856040272299"                                                                              
##                           "-0.144489593406294, 36.0935536446218, -2.75569153060505, -0.394572806905641, 2.55252447462925, -2.50046640900298, -0.326926763306105"                                                                           
##                           "-0.160225035696492, 37.3518093384359, -2.61440028619873, -0.439384972645312, 2.83534321412241, -2.73922380527445, -2.24974722423432"                                                                            
##                           "-0.527602791477183, 37.6670384044426, -2.82489080604197, -0.930452341435056, 2.15853233071433, -26.9088755230337, 3.96061047124929"                                                                             
##                           "-1.23947123264867, 52.1247772485685, -4.68381164411225, -0.814107066411416, 2.79523130212454, -48.6387964004428, 8.18512389507965"                                                                              
##                           "-0.685618293400351, 35.4716616408517, -3.39435780516888, -0.736010535546851, 2.25736515937274, 1.66349019288967, -1.99803347315235"                                                                             
##                           "1.97342644150523, 17.8317283450143, -0.127577219546773, -0.41797318866726, -0.349607572052748, -0.656227750063015, -1.9933858579732"                                                                            
##                           "1.70449984726649, 19.4486943917883, -0.785893548387963, -0.782619894138079, -0.453098562486925, -0.843570464880062, -16.6624392490383"                                                                          
##                           "1.36675480512227, 19.0572896293138, -1.21727036087358, -0.628174212702328, -0.374785146364008, -0.6706359020922, 0.577330671330717"                                                                             
##                           "1.50284308025107, 18.2724600059936, -1.09289522653603, -0.661025240402102, -0.41360428513133, -0.737540164751203, -0.847516974090762"                                                                           
##                           "2.18045708289167, 19.5727878242046, 0.784585674884299, -0.584532646268076, -0.731905867602887, -2.46046356831489, -18.5273687387577"                                                                            
##                           "1.77355653791193, 18.0285767762094, 0.0458423140833122, -0.473047796545224, -0.568320371365078, -2.00085942567257, 0.26667171955948"                                                                            
##                           "1.91362759722584, 17.4152722131977, 0.269180201076442, -0.500802178050805, -0.625726746705701, -2.11007771921083, -1.05514458077617"                                                                            
##                           "1.1106504823812, 24.4166046839031, -0.661268645656234, -0.784729008871703, -0.579417511940182, -21.6885752558969, 5.03653969285094"                                                                             
##                           "1.10380069274919, 28.6468903445236, -1.44567647480519, -0.652701221791974, -0.764581454528082, -28.7909536602502, 4.53417157133207"                                                                             
##                           "1.17832673043906, 19.4464194518091, -0.921883237259533, -0.683297029123991, -0.614979014006448, 1.86450923538839, -1.17350499781423"                                                                            
##                           "2.32649627190749, 19.6099596333816, 0.426550115399962, -0.68777333724801, -1.02023347500477, -2.47662873283961, -18.9535246595297"                                                                              
##                           "1.88808532105271, 17.9062056517528, -0.254705032259656, -0.556300536808049, -0.792603231047675, -2.00084076024242, 0.269098524804488"                                                                           
##                           "2.07467700769308, 17.1092333078549, -0.0100845610314262, -0.599039703004324, -0.89679755311867, -2.13076124812769, -1.32536950981722"                                                                           
##                           "1.26380588861352, 24.394850139731, -0.916630807403912, -0.872095386301332, -0.881618438347672, -21.9956500589826, 4.74401615371418"                                                                             
##                           "1.19950158663265, 27.7047784751893, -1.66844123257858, -0.837524885268711, -0.879165808061893, -27.9066937167372, 4.28543347262112"                                                                             
##                           "1.31592732270976, 19.3356692697445, -1.14128812203149, -0.794862173350602, -0.872839698999268, 2.0129410086268, -1.50648419316478"                                                                              
##                           "1.61187520822962, 24.3911536754858, 0.494330701211762, -0.829103939122351, -2.42269105959809, -23.4155940371642, 4.69591752743662"                                                                              
##                           "1.54961675632502, 27.6877872811401, -0.283081967590233, -0.798671179379741, -2.4018770240447, -29.0630166732883, 4.09735244854399"                                                                              
##                           "1.60270657190542, 18.3252399169054, 0.127571870340869, -0.777805870972523, -2.13012224608287, 1.99289407866604, -1.59351260163402"                                                                              
##                           "0.675408273193645, 30.4341926665441, -1.4813690264941, -0.993526664232675, -29.8301320246106, 3.96879641520365, 3.37975852586037"                                                                               
##                           "0.132955725953007, 35.3870211536369, -2.6100988197246, 2.32418391335833, -0.229858198449795, -0.353315294565716, -2.41876517681477"                                                                             
##                           "-0.0894140750718696, 35.4692695018374, -3.10461417707103, 2.16410298750592, -0.530792811436069, -0.576617378428088, -20.7072244808918"                                                                          
##                           "-0.245475209579799, 33.7260361627017, -3.43826777129592, 1.92232740973078, -0.396555590793403, -0.477959384709061, 0.00565232826901925"                                                                         
##                           "-0.24821314453218, 34.1988996783354, -3.39248203224949, 2.08541224485441, -0.429774479083675, -0.51942963925952, -1.49708601641331"                                                                             
##                           "0.209377567142055, 40.4009345839409, -1.96793636681612, 2.94313868608138, -0.53386645019829, -3.24237106232659, -27.208130785955"                                                                               
##                           "-0.0241702286897844, 36.1404921720494, -2.66588980239627, 2.48211889438373, -0.36995021798472, -2.48854329176562, -0.492421100986923"                                                                           
##                           "-0.0421864447471283, 37.6398119401077, -2.52385934695566, 2.75553079239093, -0.404368784468159, -2.72207631287475, -2.27492267670634"                                                                           
##                           "-0.474135383893233, 39.0982584957046, -2.99035016532046, 2.14939214795413, -0.623694296082231, -25.7119027831644, 3.41954526821203"                                                                             
##                           "-1.03342534118755, 52.0704514072751, -4.5651048532238, 2.63645913834688, -0.632579376958355, -48.4789204730617, 7.95966110208347"                                                                               
##                           "-0.566394737977695, 36.1961574218018, -3.41764177458915, 2.19816387535519, -0.567365766423196, 1.33942221604213, -1.89392040847181"                                                                             
##                           "0.322996156881699, 39.3378604753475, -2.09642412335336, 2.84665264164521, -0.644859265972427, -3.24465024684476, -27.4673760596709"                                                                             
##                           "0.0277879082027608, 36.0369475582959, -2.818937262339, 2.44416529884662, -0.470119737035168, -2.48309757108752, -0.38567930877422"                                                                              
##                           "0.0482571927368555, 37.1681259384551, -2.65130441435308, 2.69819643347458, -0.533137184821329, -2.71926038484043, -2.34890197522203"                                                                            
##                           "-0.356064250955242, 38.35063084233, -3.12640917700768, 2.03130251075063, -0.761213561716635, -26.532301253497, 3.6679661123572"                                                                                 
##                           "-1.09439904270515, 51.9944594771598, -4.87146118683955, 2.65521210283463, -0.600553558438569, -48.4699292229101, 8.25253252297959"                                                                              
##                           "-0.50477889751572, 36.2079502541659, -3.59564535911176, 2.14822886765618, -0.698138510564311, 1.61846621342309, -2.11770326486124"                                                                              
##                           "-0.243969117732285, 45.0015324425442, -2.19064697731314, 3.01151331130842, -3.35856851547449, -32.8419917697331, 3.28867244030841"                                                                              
##                           "-0.851978265484674, 59.5767778295239, -3.89684466436131, 3.52368727609805, -3.39343300809848, -56.4121506536667, 8.11616909162465"                                                                              
##                           "-0.411269422158947, 40.5384213100323, -2.83374098500938, 2.92913048089068, -2.8215328652021, 1.47828510988603, -2.8237811842718"                                                                                
##                           "-1.47261491074265, 53.2321225423803, -4.85050202862889, 2.7408951306784, -48.9496457461518, 1.36919692386388, 7.59011664372986"                                                                                 
##                           "2.36664140139407, 20.5256833178178, 0.503247037898786, -0.564074412579184, -0.870733050962325, -2.5382217553831, -19.5292458565111"                                                                             
##                           "1.9165703327519, 18.6747174921222, -0.217805318980316, -0.45232095230074, -0.677712184696355, -2.04320553249127, 0.0937660592246323"                                                                            
##                           "2.08596006383863, 18.1023754008898, 0.0231260918476634, -0.484825222311468, -0.763479706657364, -2.16962934904466, -1.31497168389136"                                                                           
##                           "1.23273736306039, 25.1599288489261, -1.01597799466301, -0.497686852437555, -0.775885541220607, -22.0928266457434, 4.56300110500057"                                                                             
##                           "1.1888204640301, 29.3535163426286, -1.72706407190734, -0.669924065164181, -0.667400089337727, -29.2973838379339, 4.50517858002255"                                                                              
##                           "1.27815145096971, 20.6248215804487, -1.25720601226352, -0.514950894305941, -0.740198204496586, 1.75891231098786, -1.36084492109462"                                                                             
##                           "1.78254953135324, 25.0061895577969, 0.594063381044013, -0.711865839637045, -2.47139798114008, -23.7598120067961, 4.03461206965907"                                                                              
##                           "1.74406568896133, 29.795271434809, -0.16475160442435, -0.849485280338953, -2.42319915067625, -31.0989967988248, 4.14687481140893"                                                                               
##                           "1.74252679356584, 19.2948674643232, 0.180032751460767, -0.694528372170396, -2.14508370804837, 1.46381113298444, -1.50098592817215"                                                                              
##                           "0.844818806342668, 31.7785260405574, -1.53932795839397, -0.845766603441805, -31.1034566164254, 3.00675602706241, 3.78460250008649"                                                                              
##                           "1.9113511423179, 25.0790751132573, 0.228955082364332, -0.994424005258463, -2.52273458959648, -24.1035738430537, 3.99042867088342"                                                                               
##                           "1.81083033995182, 28.4487232600561, -0.495120506613248, -0.955978976291144, -2.47111825262918, -29.6959193084767, 3.86148618542539"                                                                             
##                           "1.8657097429452, 19.3297992841834, -0.146332585404844, -0.956003310940575, -2.19462613963088, 1.758811243447, -1.88073384118739"                                                                                
##                           "0.860999773891535, 31.0115676840057, -1.84061836201487, -0.922298574815627, -30.1857525568282, 3.31839442825779, 3.38739815004353"                                                                              
##                           "1.21498241549426, 31.4385851995802, -0.394271165712664, -2.4912575000726, -31.894009786557, 3.40152501639719, 3.27482891410897"                                                                                 
##                           "1.15284051808159, 23.6952503234274, -0.405611492907703, 1.25720059644163, -0.341886641030834, -0.379857017614442, -2.603716194146"                                                                              
##                           "1.05252414383466, 22.0674287242723, -0.920367022322505, 0.918608653860287, -0.51182067003881, -0.639006171361998, -19.7033940184194"                                                                            
##                           "1.129825971581, 17.7703481108718, -0.780424467193577, 0.382077079355933, -0.478759483755222, -0.514916885728202, 0.317004434346104"                                                                             
##                           "1.11993329073657, 18.2776146027096, -0.814869897874298, 0.597613476318612, -0.493794572022075, -0.584399681816841, -1.40329111955128"                                                                           
##                           "0.927943705423185, 31.91897210829, -0.562288998829952, 2.25259195202908, -0.525325230288313, -3.40737536093909, -27.4978537500839"                                                                              
##                           "0.99117588631963, 24.3490301438662, -0.430568592412303, 1.40617152992771, -0.47932603121896, -2.67551723972323, -0.431813218653957"                                                                             
##                           "0.911346969655569, 26.3604797186671, -0.445640647929275, 1.77130204070106, -0.489809512900231, -2.91825776619896, -2.2097285508006"                                                                             
##                           "0.532391438620326, 26.7995768979414, -0.907371542691654, 0.963660107478196, -0.584043310045555, -24.2988694668845, 4.1751439073756"                                                                             
##                           "0.564952802951577, 28.1967607657019, -0.859701183194935, 0.97892254700518, -0.760555557083902, -28.5267202222116, 3.27523408283399"                                                                             
##                           "0.753487428742007, 20.1975737258325, -0.814260992772489, 0.71083820541993, -0.625923614474209, 1.80672881284104, -1.8056559163455"                                                                              
##                           "1.09231799995842, 30.7517873778322, -0.694396831417551, 2.11772381126612, -0.652193890023821, -3.39882192352399, -27.3694985629411"                                                                             
##                           "1.11688754266732, 23.2746343330424, -0.569729921332928, 1.28521457356405, -0.531116782413052, -2.67257817248501, -0.286238442506884"                                                                            
##                           "1.08388983063405, 25.045698377876, -0.583988840076699, 1.64640414335761, -0.603119329775946, -2.91276757356074, -2.32486362570941"                                                                              
##                           "0.690012662819647, 26.0357219520524, -1.05212437652195, 0.841586923694992, -0.737966217226188, -24.2134277403043, 4.20768491794627"                                                                             
##                           "0.666465394833079, 26.4993859725733, -1.10555930352246, 0.894953758972809, -0.73892484439326, -27.1048346968982, 2.96014924607005"                                                                              
##                           "0.931358569816132, 19.149560078091, -0.986640627318692, 0.582156206365423, -0.765339873616, 2.11529594135758, -2.09429107323971"                                                                                
##                           "0.540507662265906, 35.9775673582145, -0.753143476885406, 2.25432262735831, -3.48827234090218, -32.3436077893062, 3.41495638813387"                                                                              
##                           "0.463922114491664, 39.6609864047313, -0.762039203767444, 2.29622995513985, -3.69878907046894, -39.6418213909675, 3.93310002664402"                                                                              
##                           "0.6515758408049, 27.3746265145041, -0.669915155780084, 1.82497292181261, -3.00199367013115, 1.59080362022871, -2.73923971070286"                                                                                
##                           "0.127993223664102, 30.0658934988077, -1.20068035235597, 0.995468951683964, -29.7992010023514, 3.61631134779506, 2.33702284468636"                                                                               
##                           "2.34678834603823, 20.6863773135523, -0.486695966247923, -0.381237283674055, -0.876613948666135, -2.28759532025397, -19.1782376846023"                                                                           
##                           "1.98240732434868, 17.6065639641213, -0.425869978385806, -0.356485058744383, -0.645940393461342, -2.02765469510308, 0.0511927385917604"                                                                          
##                           "2.12278439177728, 17.508986762822, -0.436107312757839, -0.361419586304003, -0.74716117091426, -2.08028435338513, -1.31629492072564"                                                                             
##                           "1.34726305910625, 22.685817768199, -0.830545840039937, -0.362231495816654, -0.702895441364187, -21.4170561769029, 4.38372855480594"                                                                             
##                           "1.39615841678263, 23.4358317046356, -0.784669613239116, -0.59374154736158, -0.603138367224652, -24.6310902178256, 3.07976234529902"                                                                             
##                           "1.44067428170894, 16.8659605824054, -0.776418384746184, -0.413741019557091, -0.671291561911891, 1.87667931354667, -1.65533836517887"                                                                            
##                           "1.75094855099892, 25.4359382204223, -0.532187809792483, -0.509772252675316, -2.19431987540103, -23.5669618765646, 4.15086380996111"                                                                             
##                           "1.80842151224137, 28.7984701661453, -0.435787666617539, -0.729510066720264, -2.39446927193136, -30.2772250988626, 3.94241806964029"                                                                             
##                           "1.77153432405043, 18.9191305414499, -0.479233179019248, -0.54209637166373, -2.00929646802501, 1.44347401425691, -1.46807880170602"                                                                              
##                           "1.03157530350809, 26.4872755391123, -0.791605692899111, -0.705381691063745, -27.0162225082034, 3.26760214772112, 2.59240080660275"                                                                              
##                           "1.97071651186046, 24.9220245299452, -0.576113389014795, -0.896483426694386, -2.30213830945834, -23.7737507390893, 3.85063123880149"                                                                             
##                           "1.95267753292163, 26.7716353435777, -0.606067713142169, -0.855114598931743, -2.47336694260401, -28.4229272695881, 3.43395971588582"                                                                             
##                           "1.97217775406183, 18.2299679793581, -0.559936090017685, -0.860653196331258, -2.09986681055954, 1.61657333805875, -1.86444761032067"                                                                             
##                           "1.11325889432584, 25.2898367588847, -0.991500935573002, -0.786258855315087, -25.9268862909296, 3.53635942029734, 2.08372108663333"                                                                              
##                           "1.43456495313871, 29.8447945267435, -0.7645261899039, -2.43200465045858, -30.749520447938, 3.20000520337105, 2.96920339201014"                                                                                  
##                           "1.1324953302226, 31.6876695640594, 2.08631534892659, -0.523264866189737, -0.531607215441179, -3.43913824039248, -27.6039948649066"                                                                              
##                           "1.14310279895916, 24.2084580263422, 1.2924970075257, -0.479255181596671, -0.405180482257354, -2.7103618543252, -0.464925152394546"                                                                              
##                           "1.09605156986653, 26.1391120992892, 1.642324539305, -0.473252955971752, -0.475225819164737, -2.94871120157923, -2.3074600223261"                                                                                
##                           "0.728225081181682, 26.3595912723274, 0.742384271638312, -0.641437961372262, -0.592569177966357, -24.0459401634836, 4.04730857968667"                                                                            
##                           "0.719379687907408, 27.7294966859349, 0.783192215527534, -0.840595088580037, -0.480078988719345, -28.1353602025768, 3.19237892306253"                                                                            
##                           "0.930642310328129, 20.1666931217564, 0.519157203188067, -0.655789519621135, -0.588509416710565, 1.8445863372575, -1.90696123033469"                                                                             
##                           "0.668008682356415, 36.4192819326269, 2.18546974090386, -0.558786029628146, -3.48980983166363, -32.2487958872101, 3.09796695659157"                                                                              
##                           "0.647453923668423, 41.0748404999469, 2.1888236904436, -0.703537989944138, -3.66651129887149, -40.9753042990218, 4.05578745878926"                                                                               
##                           "0.775667702823415, 28.1187506828081, 1.77437574710157, -0.564545055601391, -2.99184981832135, 1.2509755592806, -2.63783206361819"                                                                               
##                           "0.377832474035405, 30.619296957061, 0.843808653461854, -0.921858727442185, -30.3582201966553, 2.82668699023673, 2.66978483070969"                                                                               
##                           "0.792483788537997, 35.5838673935419, 2.05024424018451, -0.679954205738189, -3.5284884602899, -32.2907149822004, 3.320687136587"                                                                                 
##                           "0.67526138603734, 38.9435920234641, 2.10889812086428, -0.606112217450882, -3.73277413291369, -39.0528299123315, 3.78960984112335"                                                                               
##                           "0.908633712731389, 27.1561475894965, 1.6410194400463, -0.675377381103591, -3.03341621686626, 1.61033172005443, -2.88511671186444"                                                                               
##                           "0.389860383967179, 28.993327136452, 0.707812830076506, -0.848912454148124, -28.8696171982681, 3.55553702994508, 2.11650657121664"                                                                               
##                           "0.207023479248607, 42.7590772992091, 2.2730374533701, -3.85791714259098, -42.0406223220453, 2.52425824514891, 3.23863857713901"                                                                                 
##                           "1.99351077644312, 25.4516903972036, -0.373934228287199, -0.826361006264092, -2.35844272282046, -23.9901529783924, 3.65995316759105"                                                                             
##                           "1.98860893973102, 28.4694284931962, -0.603831639697855, -0.663773200783998, -2.49968786285847, -29.9810946397683, 3.63694713685226"                                                                             
##                           "1.98450955845048, 19.0536953077065, -0.414168011922304, -0.760856822632799, -2.14242455093003, 1.38776680004332, -1.77224847638249"                                                                             
##                           "1.10613468171361, 26.4138231864217, -0.721936998044929, -0.577801704975763, -26.9037732624401, 3.04141149356751, 2.42441018959129"                                                                              
##                           "1.6184696894104, 31.4501050821336, -0.76227693515364, -2.44962600029678, -32.3683175504741, 2.44367818542033, 3.32752001315243"                                                                                 
##                           "1.72443836170166, 29.6074189523802, -0.894085548280147, -2.57754431311439, -30.5937053616853, 2.76548806562354, 2.69017090732077"                                                                               
##                           "3.41799871522604, 2.23767049714787, -0.99239901460141, -1.11663103371558, -0.460407380793616, -0.792714893730899, -2.0179376965094"                                                                             
##                           "3.07184317748565, 1.51751475884758, -1.2795805745154, -1.38530444681995, -0.490020062331171, -0.88860318774638, -2.93435679291281"                                                                              
##                           "3.00393073256642, 1.21057746020848, -1.24664439215064, -1.32800662229779, -0.488137385912463, -0.834687216594637, -0.188905373643317"                                                                           
##                           "3.01919448107958, 1.26158670791672, -1.27223882240711, -1.26029362662838, -0.473938814518305, -0.884439081825939, -0.816647504272438"                                                                           
##                           "3.25784336535685, 2.99490405964283, -1.11021136235825, -1.00819865963215, -0.748129815668868, -2.27625099549267, -4.04701238366654"                                                                             
##                           "3.1655535690601, 2.40472245968836, -1.08591449727954, -0.928527359959633, -0.739290021444265, -2.10603410514986, -0.718277515887886"                                                                            
##                           "3.17665660368786, 2.56834851898522, -1.113889366673, -0.848348454249128, -0.724911498929347, -2.19998558985138, -1.1438041009415"                                                                               
##                           "2.75699251100461, 1.71802568730242, -1.3418370167567, -1.28779283049338, -0.792993509333199, -2.64896637502149, 0.45853283319602"                                                                               
##                           "2.76368433187478, 1.66952118208016, -1.34219273087299, -1.25806426895926, -0.810408599115464, -2.62987142012264, 0.0901931942066715"                                                                            
##                           "2.70821456963071, 1.47409843343387, -1.33243212405439, -1.18842827827327, -0.76980349963838, 0.538901658029091, -0.663316148297787"                                                                             
##                           "3.46405818221867, 2.56816019936664, -1.2254132427129, -1.12746822726272, -1.05997533818174, -2.23161523692738, -4.27141424712753"                                                                               
##                           "3.3374908731568, 1.98747092638704, -1.20751682355723, -1.04322921323531, -0.998041120916203, -2.0527902481485, -0.598714316132247"                                                                              
##                           "3.39926489173298, 2.17397329726456, -1.2231380055048, -0.934372794429895, -1.06843560278616, -2.17962796668603, -1.4573483094338"                                                                               
##                           "2.95960275383145, 1.40002977221889, -1.47885157400177, -1.40039855867075, -1.0884282103624, -3.00450012066917, 0.512080069457406"                                                                               
##                           "2.96440124699195, 1.32467919231782, -1.49374904973976, -1.32574712414872, -1.11277267555243, -2.66978229124768, -0.241822124066391"                                                                             
##                           "2.91099737373096, 1.18839756770858, -1.46396873964137, -1.27729661883819, -1.0798561501056, 0.866140865348215, -1.09539972022753"                                                                               
##                           "2.95815180202849, 2.67721504272998, -1.51572420835878, -0.93744738230601, -2.30448932230763, -3.79804243635586, 0.244170820878085"                                                                              
##                           "2.96043900562579, 2.6365168392284, -1.53095575691897, -0.864450094773735, -2.34055672474351, -3.41718006374652, -0.358662568128679"                                                                             
##                           "2.89084716295895, 2.32410059478253, -1.50421729966051, -0.803564801298801, -2.22732311010415, 0.613113549882807, -1.25943902992756"                                                                             
##                           "2.41622857061723, 1.47452447045475, -1.75795629167094, -1.21935033132762, -2.3160425213105, 1.18085755811803, -0.222300433596027"                                                                               
##                           "3.09223685390489, 2.51899009434108, -1.25107625105473, -0.431508291089432, -0.655953009204157, -2.55828486829939, -3.7878894731261"                                                                             
##                           "3.10149500430547, 1.85309012931522, -1.20040120798669, -0.494043969558188, -0.650770164070957, -2.37711505027225, -1.87513762891607"                                                                            
##                           "3.17208040880548, 2.20872268248105, -1.21897039919876, -0.417667971599513, -0.76321622778519, -2.4954688072106, -2.09498341302662"                                                                              
##                           "2.45662256285421, 0.87407494961738, -1.58919310223885, -0.453884292906535, -0.693927242167349, -1.82818040210675, -1.41982074871701"                                                                            
##                           "2.45410753376727, 0.988353484466851, -1.60039326641036, -0.391398442765361, -0.747232365484456, -0.908068482085258, -1.40784977365567"                                                                          
##                           "2.47904731937081, 0.831524675563323, -1.57819548211517, -0.417738710840397, -0.744624653432007, -0.628709319782352, -1.40007440958221"                                                                          
##                           "2.97212372791563, 2.59438587473483, -1.28627541646129, -0.703054434893834, -2.58743803756773, -3.3927998694078, -1.37845934204293"                                                                              
##                           "2.95932709830611, 2.71323104743668, -1.30089992576378, -0.658037131556981, -2.60776109653347, -2.5501949342505, -1.29480843832027"                                                                              
##                           "2.94750784307665, 2.33268517236458, -1.2750525548084, -0.674757242699351, -2.49147742699244, -0.599139459968794, -1.57868311283596"                                                                             
##                           "2.26278836112933, 1.06661212909859, -1.61599759332327, -0.698528632022467, -0.963057821423074, -0.688839900059981, -0.863657881547911"                                                                          
##                           "3.07469712717218, 2.19681288351282, -1.44918258179648, -0.886629389339762, -2.55193899996359, -3.51632119967462, -1.3140708697375"                                                                              
##                           "3.12035450523073, 2.33752863287125, -1.43022196617847, -0.939752770712023, -2.58912317572754, -2.46284314038794, -1.6324405581088"                                                                              
##                           "3.09648395484758, 2.00023745901062, -1.41070742961798, -0.937680966742561, -2.47975700006435, -0.315194731353224, -1.99562790910906"                                                                            
##                           "2.39649641160789, 0.832079822852039, -1.77102834533469, -0.936155741255865, -0.952421839653091, -0.429445502959645, -1.27586403387491"                                                                          
##                           "2.72563632380229, 2.41580791792539, -1.6628860272406, -2.60733182351171, -2.42967436629087, -0.216893447541466, -1.12762237886033"                                                                              
##                           "3.61058466261986, 2.8677874937305, -1.28710190599372, -0.752252435522658, -0.947876482656262, -2.38597784969916, -4.33698466764351"                                                                             
##                           "3.49531540169991, 2.2751591028807, -1.18760911923058, -0.760052929224962, -0.889832376881227, -2.21472517466555, -0.836366805150424"                                                                            
##                           "3.53825847349805, 2.45293377189179, -1.11050879937843, -0.729564220990551, -0.963655494277807, -2.32438827360067, -1.35823105105292"                                                                            
##                           "3.05337275154369, 1.50410210247675, -1.61462210987667, -0.821238080049617, -0.947139805598752, -2.78700367886775, 0.361120003342293"                                                                            
##                           "3.0595829831067, 1.46013280192463, -1.57948055344063, -0.834200771872828, -0.953212099456047, -2.66206388570354, -0.0455204282975417"                                                                           
##                           "3.00209433517892, 1.28221531209483, -1.51108916106404, -0.796852632399169, -0.942780613936536, 0.539525189599998, -0.798118963168879"                                                                           
##                           "3.26940913304621, 3.08443378441358, -1.12905873736471, -1.08691621892312, -2.43681384099225, -3.93717842606892, -0.210862376762591"                                                                             
##                           "3.26471338981189, 3.09641442634519, -1.11364625948469, -1.08116301222347, -2.44590809089872, -3.73364215974575, -0.267035354473798"                                                                             
##                           "3.18390164219939, 2.68578492844587, -1.01160985716692, -1.05405474762795, -2.34202493028367, 0.0133302034354629, -1.02793283715196"                                                                             
##                           "2.71680309001073, 1.70014573838842, -1.49379294119415, -1.17573316772231, -2.5110514526277, 0.481419695247235, 0.117135004060753"                                                                               
##                           "3.43432891401091, 2.4974952739027, -1.31766200496529, -1.32204199061677, -2.4101688565827, -4.18127930586269, 0.107668767957625"                                                                                
##                           "3.45244827385734, 2.4697822892376, -1.23642157585923, -1.35477800453983, -2.45554455500448, -3.62374276252311, -0.619428822357293"                                                                              
##                           "3.36708478210074, 2.17560217192533, -1.16892487467672, -1.3193191960864, -2.34455443008906, 0.6329710159695, -1.5483451420758"                                                                                  
##                           "2.83540907498591, 1.2252650800419, -1.63726430610142, -1.35846614104839, -2.41542454674253, 1.15123654624251, -0.438481352817567"                                                                               
##                           "2.73162227776756, 2.64385218537546, -1.11471850900005, -2.52079154498113, -3.21253562912467, 0.886261493899324, -0.417888234476714"                                                                             
##                           "3.20352877094898, 2.45307628404777, -0.848248036345707, -0.775708242563171, -2.82603726561412, -3.45166637220068, -1.72805792746163"                                                                            
##                           "3.21375238354659, 2.58880631501716, -0.776911175905015, -0.845584553139245, -2.85605349703848, -2.36854689246178, -1.69702570259478"                                                                            
##                           "3.20838353055498, 2.21831716027434, -0.794346956856148, -0.84043205693998, -2.7437464523987, -0.778635273765656, -1.88116899500966"                                                                             
##                           "2.36155750122785, 0.740655568089581, -0.874270746607965, -0.770145940446368, -0.489747568738645, -0.912185337529708, -1.17084747656815"                                                                         
##                           "2.95951772714482, 2.72916011219746, -1.08514693400648, -2.84275498934217, -2.48937323487915, -0.909161112950313, -1.01750967542453"                                                                             
##                           "3.02591279078836, 2.1652660737994, -1.23041044040158, -2.86904711253097, -2.184096343407, -0.353463400915972, -1.62941439407595"                                                                                
##                           "3.36681321615652, -1.09441141126459, -0.945417401600276, -0.195059596333266, -0.99982621886497, -1.44070949483629, -1.6133823895729"                                                                            
##                           "3.3593003857831, -1.07648007813776, -0.827675322040845, -0.280569876764253, -0.953811266512559, -1.50501058903885, -1.29807127108228"                                                                           
##                           "3.35930421948021, -1.10045807058088, -0.816301508325139, -0.199567406593614, -1.01469966690707, -1.50025109797016, -1.04442274974424"                                                                           
##                           "3.07154479696965, -1.27662510272378, -1.16315709248915, -0.333597950663525, -0.969916572489499, -1.42149342052483, -0.519617844278479"                                                                          
##                           "3.06266032048789, -1.28123202550374, -1.16130676251986, -0.305106028773788, -0.982510357145793, -1.16010699974095, -0.368823822879299"                                                                          
##                           "3.03822588861654, -1.27581846947509, -1.10765984109421, -0.329241465368821, -0.962419189197289, -0.259362492371567, -0.611002506808851"                                                                         
##                           "3.02906287492849, -1.21115908617482, -0.6673478832165, -0.551854870183345, -1.50119212418393, -0.913969288618369, -1.12513972292187"                                                                            
##                           "2.99392475764543, -1.22138435554122, -0.699227172016807, -0.496911221559461, -1.46059462584265, -0.600123559774714, -0.497162769099676"                                                                         
##                           "3.01179974641926, -1.20979367437543, -0.628174144126846, -0.547808024567636, -1.51062798260233, -0.957959401189603, -0.412800513732085"                                                                         
##                           "2.72746045104648, -1.35407212076486, -0.995126306370572, -0.66055939849202, -0.909345940711512, -0.499163222069546, 0.0602413443649219"                                                                         
##                           "3.35515376939389, -1.20444348113529, -0.863042107611592, -1.09903533029908, -1.53962102182675, -1.5765132293812, -1.00467880344767"                                                                             
##                           "3.34677693825711, -1.19162482631614, -0.855108806186039, -1.1053585869827, -1.51876517372556, -1.00388557738101, -0.789468749607229"                                                                            
##                           "3.33073602836288, -1.20251743567317, -0.784813396143508, -1.09766327538895, -1.55954076553194, -0.60943399777161, -0.898380233759618"                                                                           
##                           "2.99564373365679, -1.42383055701069, -1.1563770341071, -1.11339539135202, -1.25479696145484, -0.104456657034692, -0.348110522765276"                                                                            
##                           "2.82936079865632, -1.51238600346775, -0.642120277317009, -1.59179708008831, -0.773692868098015, -0.58486963659191, -0.278045982811184"                                                                          
##                           "3.12831288413186, -1.25575050107473, -0.30389840797991, -0.839040277785947, -1.85179099578392, -1.21725959777384, -1.97512618660906"                                                                            
##                           "3.10118943555056, -1.27420682356647, -0.200142954201559, -0.896430527032569, -1.82219939331317, -0.135216179250492, -1.55304750188415"                                                                          
##                           "3.14976674827174, -1.25051413233362, -0.272835186068649, -0.884987652946118, -1.84960496382844, -1.30690647511315, -1.17927933637011"                                                                           
##                           "2.55408470602621, -1.56036516894773, -0.354165335244091, -0.799880436049692, -0.204996049158731, -1.02861882257185, -1.03300759914268"                                                                          
##                           "2.86257418813453, -1.33110980587246, -0.53589186668518, -1.78615547408867, -0.248157479124852, -1.40811979338492, -0.670747810080652"                                                                           
##                           "3.09803607476323, -1.379652080931, -0.991443710367521, -1.88002966796181, -0.451382391024369, -1.16299520572328, -1.13436528364832"                                                                             
##                           "3.44538462755314, -1.00472527689531, -0.539492808795643, -1.08995710400288, -1.65659189996277, -1.3853343718759, -1.15958382905455"                                                                             
##                           "3.41888586296127, -1.01941131775719, -0.479577274631971, -1.11444323340561, -1.62953050172858, -0.909433415188652, -0.704083446715889"                                                                          
##                           "3.42069299635186, -0.942712574827845, -0.532632322237593, -1.08845099285105, -1.67223682836664, -0.853191324369006, -0.71168846410683"                                                                          
##                           "3.05990586760304, -1.37522107786776, -0.674875096645783, -1.03569788068307, -1.17789564770022, -0.355366466347239, -0.10690248354973"                                                                           
##                           "3.02651688539807, -0.823644665858642, -0.90101274781734, -1.63310343794283, -0.70843724891635, -1.10031015925503, -0.0284099195235836"                                                                          
##                           "3.3345921857207, -1.04224128436513, -1.35487255312285, -1.74977601919474, -1.13995450559357, -0.489798440118402, -0.543372040555972"                                                                            
##                           "3.16015022835109, -0.633079153605594, -0.959517995596288, -2.0971595361501, -0.216836125721552, -1.51775284640129, -1.09009198997478"                                                                           
##                           "0.189242671452841, 34.5659583054877, -2.51167167926841, -0.236187898239131, 2.28487688908025, -0.168024504104782, -0.345393075943067, -2.38192788994895"                                                        
##                           "0.101570157126734, 33.2915740255649, -2.7645709676946, -0.705694656319661, 2.08666220684915, -0.331286742202892, -0.588250442076952, -21.4827868264867"                                                         
##                           "-0.105888725801995, 32.0759327112061, -3.19982009114778, -0.499351817912939, 1.85194477379232, -0.261156777347214, -0.47464824602067, -0.0309177773510232"                                                      
##                           "-0.105199443943161, 32.5117181874261, -3.14173387663218, -0.532693847855765, 2.01910829102487, -0.281454690958337, -0.520370824604543, -1.54512808443235"                                                       
##                           "0.318836271575189, 38.864327211702, -1.77500358826048, -0.45080687003337, 2.88655757339644, -0.40677472998745, -3.19191079874515, -27.5426885636711"                                                            
##                           "0.0427020084006325, 35.2133926929583, -2.55675995061866, -0.254709490168161, 2.43719629021597, -0.300612060683676, -2.45067664286894, -0.503597548248924"                                                       
##                           "0.0265902992528277, 36.6597138884402, -2.4084181218629, -0.279655747778907, 2.71197915305459, -0.325955231572536, -2.68246817303525, -2.28055955509613"                                                         
##                           "-0.287053343680454, 36.9329177268879, -2.633445542199, -0.726310402984658, 2.06262960745109, -0.41649099066266, -26.5884421139093, 3.54513952147487"                                                            
##                           "-0.929311176542578, 50.8511173867108, -4.34376066748483, -0.596760355317887, 2.62527890480324, -0.459237537371485, -47.4718705621031, 7.57982069173533"                                                         
##                           "-0.42239498206987, 34.4678034401609, -3.16545317578909, -0.533081430866506, 2.12906565835054, -0.417949268308789, 1.32963533884811, -1.92353641632257"                                                          
##                           "0.508148858776419, 37.3606970697909, -1.77236163100218, -0.549267548877031, 2.75199712565647, -0.568217129825017, -3.15957045391966, -27.9465194626516"                                                         
##                           "0.14540623631296, 34.667065530776, -2.6248559382213, -0.314743459861188, 2.36822741283742, -0.42392940878098, -2.42246239684824, -0.445625144476523"                                                            
##                           "0.169683688684519, 35.7703003622243, -2.44366373168476, -0.348897867030147, 2.62199854658225, -0.481295389726732, -2.65287492898869, -2.3600591040716"                                                          
##                           "-0.0590460097150144, 35.7453922750084, -2.62226918831167, -0.811657841099039, 1.91938951378541, -0.656931726974225, -27.2532010784058, 3.59536567889272"                                                        
##                           "-0.869407973968302, 50.1031995064184, -4.44827418408414, -0.718749818910588, 2.59615971611662, -0.498489307567131, -46.8590111885145, 7.54383459460605"                                                         
##                           "-0.253012851360809, 33.7625640838693, -3.19323013233981, -0.621418602829184, 2.03320701544715, -0.616185205454314, 1.47511461252189, -2.11410463467982"                                                         
##                           "0.0278404005341393, 42.3659821691904, -1.79286113547114, -0.601957677494087, 2.86494069316571, -3.24811923167898, -33.2226932417337, 3.20581144291528"                                                          
##                           "-0.663105004811347, 57.8172302632025, -3.60063079447694, -0.48251208638312, 3.43526887550571, -3.29994422885942, -54.8726765127978, 7.57411931923583"                                                           
##                           "-0.219097462902414, 38.5468558474375, -2.5659344668671, -0.404490289647456, 2.81203657542608, -2.7335113399394, 1.36830910495714, -2.79498440825944"                                                            
##                           "-1.11949227722987, 50.6377534251481, -4.28039106858944, -0.83089421905948, 2.6163134082415, -46.7339372575369, 1.55071277633159, 6.55082592527016"                                                              
##                           "2.40521227763365, 19.9299383366127, 0.577707693525325, -0.512763229130941, -0.42738278628243, -0.842192961300239, -2.46677752335679, -19.2748208672879"                                                         
##                           "1.96607532288637, 17.8809903616638, -0.130701198678978, -0.417422171949295, -0.346222756274255, -0.65444437519592, -1.99060865392334, 0.0673733506393467"                                                       
##                           "2.13936110704406, 17.2655671346985, 0.118954702513774, -0.443984816975039, -0.369966064439964, -0.741719224777134, -2.11508058718019, -1.34264278746151"                                                        
##                           "1.33497030276729, 24.2828130479347, -0.830475028023995, -0.756818812692153, -0.287436586880366, -0.76163807070192, -21.8517418770179, 4.47134862639986"                                                         
##                           "1.2884777899999, 28.2722836227293, -1.53519267379877, -0.635546825607198, -0.493848231228776, -0.659935829409744, -28.4430164429074, 4.24703504514189"                                                          
##                           "1.3913587297203, 19.308297270875, -1.05335874688113, -0.656105887972387, -0.33784197793188, -0.728529618873697, 1.7298282861638, -1.42007963103372"                                                             
##                           "1.83902471830728, 24.3386602329652, 0.669403942647267, -0.559083775421385, -0.551025710151313, -2.40414671848982, -23.4388925521048, 3.9872454770909"                                                           
##                           "1.79643367249533, 29.0201057268262, -0.0788922078142792, -0.431736641737525, -0.726269331792279, -2.37263163798555, -30.4412618409905, 3.99670813895159"                                                        
##                           "1.81397394128123, 18.3131719696055, 0.285745986228633, -0.497424383452526, -0.556038679004733, -2.09257231256405, 1.45149955241368, -1.53373675349534"                                                          
##                           "0.943351370622888, 30.7551192555602, -1.34961465913972, -0.667557510646068, -0.652202492996923, -30.3273691475551, 3.06096452307335, 3.55112678208727"                                                          
##                           "2.00685337246492, 24.313353569827, 0.388032041039118, -0.606318539610379, -0.886884231830269, -2.42572187419274, -23.7011896952593, 3.805607027994"                                                             
##                           "1.90891554247098, 27.633957155213, -0.313197643223649, -0.58149974960506, -0.855818789566123, -2.38347393690123, -29.0538020346917, 3.64973471646178"                                                           
##                           "1.97720394737847, 18.1446166683199, 0.0405908126499083, -0.563852472330194, -0.859968369551949, -2.11193136358045, 1.62064389696351, -1.87445589078506"                                                         
##                           "1.05004397314946, 29.8692001256993, -1.5219890910273, -0.813717277941329, -0.801993204464321, -29.4271596271368, 3.16354081329821, 3.16368904597134"                                                            
##                           "1.4128596407428, 30.3009283279688, -0.167795611927082, -0.752911917689798, -2.38469284451839, -31.0757952341874, 3.18608537361273, 3.07274325397036"                                                            
##                           "0.486952828796941, 38.7075098446584, -1.83110064960688, 2.75569672641169, -0.383311213110591, -0.473963882043895, -3.19578050920559, -27.6818242710125"                                                         
##                           "0.161500007957339, 35.1514538331263, -2.61614807601319, 2.36274242003233, -0.254811322145374, -0.359002818275457, -2.44389862909814, -0.529843830799271"                                                        
##                           "0.175600140524138, 36.4550503226904, -2.44572143475153, 2.61622418893722, -0.273759513478971, -0.409704548477845, -2.67531673971861, -2.35631226548348"                                                         
##                           "-0.179380454896199, 37.5797840145941, -2.8882431351461, 1.97369061351744, -0.410325463499761, -0.564433908517059, -26.2462602622544, 3.34755273793236"                                                          
##                           "-0.856237826534292, 50.9212673296276, -4.49354294802354, 2.53922820491278, -0.488864292081812, -0.363495811553484, -47.4780890062076, 7.67069403395789"                                                         
##                           "-0.316385172731969, 35.1256083758228, -3.34622124823852, 2.05976490952334, -0.372505079624877, -0.520466596457824, 1.3348699070449, -2.01997310525059"                                                          
##                           "0.0766408476922402, 43.3085301338322, -1.83950251466788, 2.83708352708392, -0.411898438043904, -3.26058934485681, -32.6450112712184, 2.94939146949798"                                                          
##                           "-0.49878073315089, 57.814285800262, -3.44936222393428, 3.30899699687015, -0.431916085489282, -3.29700268565283, -54.885993054323, 7.40305033725568"                                                             
##                           "-0.124186692397104, 38.7829699370081, -2.51676634769395, 2.75635530949665, -0.349051059931834, -2.72595423542343, 1.1789613195289, -2.73608618612462"                                                           
##                           "-0.965446287735122, 51.1443402835177, -4.30281387927193, 2.51910620662039, -0.62226674406044, -47.2458952326949, 1.09287899831423, 6.85293383721393"                                                            
##                           "0.229507248948403, 42.199598877568, -1.88150647642763, 2.71532102348507, -0.579218770682157, -3.25103777293637, -33.2147374218723, 3.1131711685239"                                                             
##                           "-0.565118511498143, 57.4657325033643, -3.67162695297128, 3.34382385141967, -0.356304606937981, -3.30771980406277, -54.5278559806276, 7.55915986585526"                                                          
##                           "-0.0139881875138982, 38.2931925330554, -2.59336330150307, 2.68090159851591, -0.507077220917305, -2.71436997554677, 1.34926681801549, -2.88811354073857"                                                         
##                           "-0.993214488374306, 50.6394297300929, -4.53106730865298, 2.50383644746399, -0.598695630466756, -46.7233150049189, 1.28530987024019, 6.81484324676954"                                                           
##                           "-0.756688578896546, 58.4606583860803, -3.51521369104618, 3.38241909824414, -3.41050632270628, -54.8511832315703, 1.4283204021839, 6.53756135534836"                                                             
##                           "2.0401225461167, 24.8265932259776, 0.420296425273146, -0.414433361732698, -0.80397759432823, -2.49155479813242, -23.9415755007732, 3.5866399447185"                                                             
##                           "1.95342644538722, 29.1090963649083, -0.244910075569322, -0.586170775321068, -0.669778780611271, -2.42861174683136, -30.4429043732711, 3.79808832114166"                                                         
##                           "1.99009308877859, 18.9696017073895, 0.0424074541971997, -0.417851313039737, -0.759112562380916, -2.15524774284515, 1.38940631729719, -1.78159244735674"                                                         
##                           "1.04212115159987, 31.1181029412006, -1.62484049492657, -0.585914813848403, -0.63999302984306, -30.4996096807513, 2.72839143564262, 3.53445139485161"                                                            
##                           "1.60325051333528, 31.7424809804497, -0.108786393224792, -0.756684306029919, -2.41854519880971, -32.5774284704408, 2.44012902524149, 3.39347374356828"                                                           
##                           "1.68126969157536, 30.5262531530823, -0.364763585601012, -0.890448056355399, -2.46682793619488, -31.2337430506034, 2.71547757209772, 2.91496559670054"                                                           
##                           "1.1502757529454, 31.1918301468619, -0.528982510969001, 2.11168097909549, -0.366010346165169, -0.50366748748517, -3.35298641844422, -27.5474871663808"                                                           
##                           "1.18079498531743, 23.5213104650518, -0.408119782939765, 1.29004095991889, -0.364007738946756, -0.386290077141151, -2.63040808036342, -0.482298379884498"                                                        
##                           "1.13145126788391, 25.4415563016629, -0.421048006130784, 1.64305196811915, -0.350612787760563, -0.457428149326449, -2.86673163401022, -2.31372692464596"                                                         
##                           "0.775132515317321, 26.0874589494618, -0.893196297252704, 0.846181655615118, -0.368897424938199, -0.578087497120928, -24.0810928443952, 3.89108059188151"                                                        
##                           "0.765630052481416, 27.3904960326166, -0.850840017488369, 0.881803376674756, -0.578073325856426, -0.468462920543605, -27.8714016959102, 3.04837422523683"                                                        
##                           "1.01583877912111, 19.3273436625264, -0.804166236151917, 0.583878646239718, -0.413799572039776, -0.580798047767768, 1.7606626309309, -1.96126327997257"                                                          
##                           "0.701034017360204, 35.9075653312959, -0.551920516191448, 2.20498024262523, -0.385095765367861, -3.4058482450049, -32.1395117906354, 3.07706996903938"                                                           
##                           "0.674639893185746, 40.554409514432, -0.465241575482332, 2.20771336492673, -0.557273202475854, -3.5899749495699, -40.5076919896914, 3.95032739720079"                                                            
##                           "0.825403328064336, 27.3177541785803, -0.440847166157589, 1.76888146054032, -0.430006713620936, -2.90869562622243, 1.23364601908419, -2.6404020061136"                                                           
##                           "0.429978733091301, 30.2992843793529, -0.85889444581924, 0.937471717294375, -0.651820872374596, -30.1346099742832, 2.78694660306862, 2.54560667536091"                                                           
##                           "0.854805417118913, 35.1334936959563, -0.624122716617409, 2.08786724018761, -0.558595085225671, -3.39150627668212, -32.0528410030107, 3.14789318126922"                                                          
##                           "0.732009509809121, 38.7167569315586, -0.651517558978666, 2.14820886162698, -0.475676488007128, -3.59248174521599, -38.8454504362779, 3.68178668511853"                                                          
##                           "0.993536078636619, 26.2378330570646, -0.545327852335043, 1.64406221938761, -0.5755666194575, -2.89309959542873, 1.4515637009713, -2.83279763510143"                                                             
##                           "0.530936683712355, 28.9790620108529, -1.05362394544858, 0.849647383135919, -0.66849938392394, -28.9739843028943, 3.13737443482173, 2.11927923375433"                                                            
##                           "0.366021418067872, 42.1107113112998, -0.719282159389549, 2.27304390729355, -3.6760887002318, -41.5377357707141, 2.324707466812, 3.15425359718505"                                                               
##                           "2.02215880028022, 25.032289785506, -0.474518704912596, -0.241176518106355, -0.804534955566597, -2.27078462075994, -23.7555598335202, 3.65353184686201"                                                          
##                           "2.01742129416811, 28.0001229142899, -0.397482455318862, -0.49204883340657, -0.647937612678587, -2.42420796200185, -29.56048890932, 3.56278175051963"                                                            
##                           "2.02442177338855, 18.4146953254175, -0.432135402719115, -0.294083468221392, -0.743719242925508, -2.0608790826253, 1.37564561422608, -1.77251987939263"                                                          
##                           "1.21236209208363, 25.7941737363329, -0.785953777554929, -0.479862909752639, -0.577560829248265, -26.4312274745972, 3.0387411632251, 2.31615193042818"                                                           
##                           "1.65599017303207, 30.9477397154146, -0.444473944693773, -0.628643803334158, -2.37207534106696, -31.924741509268, 2.4750671792522, 3.23881265456471"                                                             
##                           "1.79936055665124, 29.2519366337799, -0.572103242914119, -0.793402614825305, -2.4383772767246, -30.3217044814068, 2.65569344906764, 2.67583704943061"                                                            
##                           "0.891494716726093, 35.6039741736235, 2.05740391046563, -0.393426462503609, -0.494419383103635, -3.443540275273, -32.0860430005361, 2.99726575162819"                                                            
##                           "0.786493726021294, 40.2495569213538, 2.11075789654498, -0.59798689223423, -0.303741433957824, -3.62429787149662, -40.2502733733841, 3.88152113708375"                                                           
##                           "1.0088611931391, 27.1013409596704, 1.64177215852934, -0.410236851385428, -0.476196869601032, -2.93907564623393, 1.2522665085885, -2.74723371373102"                                                             
##                           "0.582588349414526, 29.7011609241441, 0.748894459045155, -0.739116129410583, -0.466408107145579, -29.5998543980526, 2.72947940970105, 2.45203418185816"                                                          
##                           "0.535799832640545, 42.9986414100793, 2.18258715236397, -0.626219705231607, -3.66592390512398, -42.4508571377029, 1.9009374215212, 3.42645605125112"                                                             
##                           "0.558859680217806, 41.3379635049376, 2.10458614581267, -0.556090844750839, -3.71474830913841, -40.8599740051467, 2.3136193537836, 2.99151444158661"                                                             
##                           "1.84288941040895, 30.4339761278708, -0.515996582977304, -0.648138737493455, -2.48145729016616, -31.461283514686, 2.28163278474715, 2.96774568494811"                                                            
##                           "3.56794354559731, 2.79362282700386, -1.0148734207999, -1.14807833718661, -0.480167110032612, -0.863581345802488, -2.2358291594504, -4.36246184320942"                                                           
##                           "3.45000156746857, 2.18701578893662, -0.993653413947444, -1.04319155336838, -0.495210899523485, -0.804968230083182, -2.06465754035167, -0.853023620357834"                                                       
##                           "3.49459120219246, 2.37344172617077, -1.02089100465717, -0.954508408334986, -0.454851322846245, -0.882863020506557, -2.17931646102171, -1.43737016246663"                                                        
##                           "3.06749855087224, 1.54847442853338, -1.27803755489749, -1.40894343846483, -0.480636948614649, -0.8852086572558, -2.99387801931067, 0.270125918030365"                                                           
##                           "3.07196139459228, 1.50700666463123, -1.28400506973811, -1.36252504311512, -0.487553574116758, -0.897831545104177, -2.75574800672112, -0.193241419046022"                                                        
##                           "3.01058970873398, 1.31498951946016, -1.27284485957171, -1.28929989869024, -0.45242164543861, -0.885782265048406, 0.54288248243418, -0.968992333648954"                                                          
##                           "3.26383824661137, 2.9715264861681, -1.11066615703916, -0.98581488208028, -0.758186342464653, -2.28696212511021, -3.99825807603479, -0.236329410644791"                                                          
##                           "3.2588007893474, 2.98316747167086, -1.11848573009067, -0.955865535679454, -0.750195121322484, -2.30327984476813, -3.6961166484278, -0.385784308107741"                                                          
##                           "3.17502827510916, 2.57434008261213, -1.11419613712365, -0.851956896077654, -0.722148790373385, -2.19843251141104, 0.0662855686264817, -1.16278416302969"                                                        
##                           "2.75571489989339, 1.71908785965193, -1.34288006716043, -1.28433768971932, -0.791912236687219, -2.60716178069788, 0.493295425140751, -0.0513556511504641"                                                        
##                           "3.46613052833391, 2.55621587735584, -1.22733488479213, -1.11767608554376, -1.06278205138372, -2.23599843726557, -4.25034672082971, -0.103745450676194"                                                          
##                           "3.48179562208401, 2.55333165744443, -1.23485980661455, -1.0410094880228, -1.09249014558638, -2.27897780150825, -3.66834396096967, -0.707059374304671"                                                           
##                           "3.39413332736124, 2.22011482185633, -1.21660492015204, -0.958601971411051, -1.06333904709381, -2.1706324443881, 0.427348919012142, -1.58270027172522"                                                           
##                           "2.95925611873497, 1.4172501584973, -1.48101184253981, -1.37226379324199, -1.1006815440493, -2.63015562718576, 0.826295233051916, -0.48762814843447"                                                             
##                           "2.95652191735706, 2.69797339722247, -1.52118107464982, -0.897949717594286, -2.32641727177705, -3.38714528249183, 0.56004266284883, -0.532568643056135"                                                          
##                           "3.19162660985504, 2.39936665598008, -1.22526113719991, -0.497791520538366, -0.694848761859276, -2.57900926733738, -3.56537669443761, -1.5528774083054"                                                          
##                           "3.20795977569214, 2.52759815229258, -1.23628092873563, -0.427908177224984, -0.765546427059698, -2.6091407898263, -2.54044878691447, -1.6126666369896"                                                           
##                           "3.195157662547, 2.15719071355743, -1.21021948654165, -0.445510906145995, -0.762816412543199, -2.49510694580767, -0.602571454607055, -1.89171065468143"                                                          
##                           "2.48072867876641, 0.93271921550385, -1.59194657253204, -0.4216725871505, -0.747319475592655, -0.999874978756956, -0.696079545356412, -1.15727967875561"                                                         
##                           "2.98822860592499, 2.65584250853852, -1.29090379728415, -0.691381163691442, -2.6096201216291, -2.64670396568179, -0.725512690058713, -1.03220755637029"                                                          
##                           "3.1327279050783, 2.29452620277131, -1.43306857088435, -0.947305815133674, -2.58885740310236, -2.51719830871644, -0.428914271674879, -1.47469701711953"                                                          
##                           "3.62169933541902, 2.83388225958792, -1.25566410685121, -0.766693473978096, -0.951792190766858, -2.40142225831743, -4.26793716299828, -0.352669290293139"                                                        
##                           "3.62531381445954, 2.8497009776923, -1.21614354989912, -0.75027921503581, -0.977857355420178, -2.42732547011126, -3.81931358965003, -0.591639462701433"                                                          
##                           "3.53663726942904, 2.45890869434551, -1.11413537942591, -0.726584571918159, -0.964043035467975, -2.32290945969893, 0.0687509265843391, -1.37830765657499"                                                        
##                           "3.05137968580671, 1.50718562531958, -1.60383587618158, -0.815609639149625, -0.95436570725616, -2.63517234692854, 0.486220364986582, -0.189667441495799"                                                         
##                           "3.26640650872998, 3.09057778674021, -1.11022317742693, -1.08377333023303, -2.44745117882103, -3.73825913956147, -0.0655417115643792, -0.247151255460015"                                                        
##                           "3.44580243451176, 2.53141967892406, -1.26555401096542, -1.34650253851859, -2.44328947304033, -3.58608506643263, 0.569283556747946, -0.800612372823956"                                                          
##                           "3.24823584884722, 2.51782610472934, -0.814168637761943, -0.843789090994992, -2.85584560073542, -2.49040155180193, -0.896541313602329, -1.3700748723841"                                                         
##                           "3.40570263251066, -1.09259620776956, -0.848488357669303, -0.254567539620016, -1.00302312732085, -1.51581516438407, -1.48851095077412, -1.16439879251522"                                                        
##                           "3.38084395865583, -1.10494255088294, -0.854237788935197, -0.190530210970974, -1.03030470135138, -1.49199517732657, -0.955329624317939, -0.77876885402326"                                                       
##                           "3.37915644647648, -1.09494371758971, -0.776256747286002, -0.244376070115159, -1.00403170446328, -1.53524768593021, -0.805404730738039, -0.815909450765024"                                                      
##                           "3.06920998827885, -1.2809113195932, -1.14713274843966, -0.325374440771583, -0.97884483596853, -1.22503828748374, -0.379046428365214, -0.243464490344112"                                                        
##                           "3.02682888299316, -1.21393839866672, -0.654401359287644, -0.548721308017854, -1.50624370058957, -0.783101310585817, -1.0406973529596, -0.15781504295192"                                                        
##                           "3.35918485620986, -1.20484958022705, -0.82309984836702, -1.11258960256566, -1.55454518033423, -1.14098084624485, -0.722691501063053, -0.547431096706139"                                                        
##                           "3.15581447150325, -1.25510488589742, -0.271385442861679, -0.889471333722754, -1.85406071919272, -0.428556528109791, -1.36196752010231, -1.04748751482105"                                                       
##                           "3.44600623906035, -0.978218856629919, -0.529557811117438, -1.10376351176973, -1.66891406372811, -1.07922568188009, -0.956041193450364, -0.382334806895463"                                                      
##                           "0.583920961272753, 37.3073620607874, -1.65380674849373, -0.4398142839132, 2.70679072790848, -0.261859661907279, -0.466540973765852, -3.14220708315183, -27.987975138311"                                        
##                           "0.218817578133085, 34.3190748907724, -2.51709476138389, -0.239093801405888, 2.32398737985426, -0.192579041766661, -0.351192444359717, -2.40719238635972, -0.539908977720288"                                    
##                           "0.235093389371416, 35.5691841472186, -2.34067766510728, -0.264698083007308, 2.57851919361246, -0.202164274283267, -0.402683209729837, -2.63592423539179, -2.36095237824497"                                     
##                           "-0.00140389740132009, 35.5987507995437, -2.55305726511336, -0.729947696838106, 1.90009559107685, -0.198687032653112, -0.57209672527231, -27.0407992097348, 3.44572687375423"                                    
##                           "-0.754778765587888, 49.7471673199795, -4.27620188404755, -0.594807411884391, 2.53058808939825, -0.316821118522332, -0.361942768982494, -46.5142146902956, 7.30076522049041"                                     
##                           "-0.174609242497598, 33.4410231620525, -3.09818518854608, -0.530735701696722, 1.99426932539081, -0.224214386941219, -0.521271802629864, 1.32396152758697, -2.05340368381407"                                     
##                           "0.1825413048158, 41.8171619607422, -1.64556616181222, -0.469242427650589, 2.77946226324014, -0.27675397773402, -3.20526789582207, -32.9874561766983, 2.99217222547334"                                          
##                           "-0.450261009470858, 57.0418486962453, -3.35046329817742, -0.327498016365157, 3.2965114706433, -0.336288244125915, -3.25400448666905, -54.2156434493514, 7.20832870192219"                                       
##                           "-0.0559079555710293, 37.7995683026882, -2.40284971672367, -0.275570413356721, 2.71304660325315, -0.271883525119539, -2.68626027833661, 1.16942680001398, -2.73429552469224"                                     
##                           "-0.857255521721708, 49.9160746786461, -4.05130932568068, -0.617436440811804, 2.49538638159904, -0.438805079180623, -46.2080522451282, 1.27736037931588, 6.36185294037135"                                       
##                           "0.376810687787746, 40.6412978016084, -1.62244974586404, -0.50787332672466, 2.64689633611745, -0.502026951108341, -3.16736606106396, -33.319832389908, 3.04169906093527"                                         
##                           "-0.455036803301713, 56.352292779885, -3.45705382043108, -0.427594202581631, 3.30103333460444, -0.28842275772132, -3.23900496397294, -53.5673513711907, 7.19947056560908"                                        
##                           "0.100386896154383, 36.9362346338249, -2.40603466622353, -0.319654567263535, 2.61104477455968, -0.46052499511466, -2.65229724185473, 1.27315610529454, -2.85920907560022"                                        
##                           "-0.773064633244898, 48.9215379628707, -4.10004336586167, -0.733636429096107, 2.44202748487019, -0.491081801220855, -45.2948762078173, 1.42266427614724, 6.10805120793991"                                       
##                           "-0.577280821947808, 56.9635133369108, -3.23938058827593, -0.494173325347508, 3.30054564819729, -3.31464246478019, -53.60726983311, 1.46048603871101, 6.09807762286078"                                          
##                           "2.07692354516439, 24.2759412253198, 0.48948162055076, -0.495269752870278, -0.282601702848484, -0.776277157165723, -2.42188611377924, -23.6627539206105, 3.56371505114204"                                       
##                           "1.99272949233112, 28.4549003306593, -0.168522737245762, -0.388691512231275, -0.48244052899005, -0.652651201510276, -2.37701827326998, -29.8916641865006, 3.67505730664789"                                      
##                           "2.04331125574185, 18.1271891719506, 0.137905576171389, -0.441288051045468, -0.303575414650534, -0.73746389311755, -2.10085949896334, 1.38089026713399, -1.80306625772229"                                       
##                           "1.13566409789063, 30.1553965454153, -1.43616260273184, -0.650953955288941, -0.400847654773397, -0.6316927982315, -29.7801093724422, 2.78757140521355, 3.30841289845306"                                         
##                           "1.65242857401623, 31.016498414721, -0.0247160947623204, -0.443248845349169, -0.627759082832969, -2.36523751448146, -31.9743921026137, 2.47433678170088, 3.25404132602355"                                       
##                           "1.77318058163301, 29.7952098262642, -0.204865590148009, -0.557311900228775, -0.794500566155493, -2.37994952987678, -30.7062667887853, 2.63338232733535, 2.8027692643178"                                        
##                           "0.341031339309588, 41.6769215293966, -1.71893066199499, 2.66354172324079, -0.262290131426041, -0.463560673908673, -3.21538649561945, -32.9881736848293, 2.92544090971185"                                       
##                           "-0.396430967766888, 56.932736459102, -3.39467091571563, 3.24438352374046, -0.366437439452719, -0.191904093966645, -3.26828537996487, -54.0937275316748, 7.21216455119012"                                       
##                           "0.0930761643758867, 37.594490753032, -2.43888783831481, 2.6182389484244, -0.216882325099578, -0.41161015954763, -2.67902922048704, 1.18495748546884, -2.82051561255841"                                         
##                           "-0.788850476700631, 50.0160337027463, -4.23494105831427, 2.42277875847742, -0.477374691860625, -0.365330456748462, -46.2635874870996, 1.09479850795633, 6.56601396098958"                                       
##                           "-0.449587802448424, 57.2901924474005, -3.17903448405186, 3.21074052622936, -0.414132278846586, -3.31532085690032, -54.0064999044183, 1.22393739517502, 6.23793681986358"                                        
##                           "-0.480992344558446, 56.5747163095879, -3.32078936825717, 3.21364523688148, -0.356165473785959, -3.32646184918153, -53.2183818257545, 1.39963778097826, 6.09816037364437"                                        
##                           "1.81843366174639, 30.8982604728405, -0.181725076963727, -0.504187403327695, -0.652390663791078, -2.42883463504793, -31.7864810578241, 2.27130339162043, 3.07520599508958"                                       
##                           "0.905184276430579, 35.2102703826449, -0.519986139957114, 2.08436270981978, -0.239889921869662, -0.465476959161301, -3.3609768302288, -31.9724751045312, 2.98159937080173"                                       
##                           "0.80044658483174, 39.8206409433721, -0.448303054093604, 2.13516620750851, -0.465360849096704, -0.280336653337176, -3.5503641110449, -39.8639641963624, 3.79427941254629"                                        
##                           "1.04469515580237, 26.4006364899456, -0.416332117479997, 1.64217195235239, -0.289356182820639, -0.458630022583578, -2.85781004237682, 1.23615195097955, -2.74613913882811"                                       
##                           "0.626381696684285, 29.4445173290836, -0.850230547142761, 0.848271201937808, -0.476327215661673, -0.454419803628842, -29.4312834356072, 2.6934990294469, 2.33568487290705"                                       
##                           "0.561077852990353, 42.5303152079967, -0.472704731804082, 2.2018860212734, -0.476668025818043, -3.58882533344794, -42.0394338606891, 1.92121485708893, 3.32798278122717"                                         
##                           "0.61587906494043, 41.1022236014611, -0.620120013046052, 2.14161681422415, -0.433900867060837, -3.5830636527737, -40.6952293007548, 2.18830892517415, 2.97290363813266"                                          
##                           "1.86921323620927, 30.0080894584132, -0.406818202438499, -0.400180261938936, -0.631545079714573, -2.4041164737544, -31.0861515732313, 2.31223559835192, 2.89658198630876"                                        
##                           "0.673002287250405, 42.157649646872, 2.10699885625456, -0.52145019088334, -0.299964765068423, -3.62473340142212, -41.7096161602802, 1.88849483164151, 3.25653533401589"                                          
##                           "3.57882721147416, 2.75876349083497, -1.01508027954684, -1.11498166076619, -0.49471988084644, -0.867687400962484, -2.25172381937201, -4.29239514802723, -0.361076898086632"                                      
##                           "3.58340755880117, 2.77445277459708, -1.02580895112087, -1.06462533404655, -0.474060120684107, -0.897458620251849, -2.28207434638959, -3.78412424562404, -0.670349512100526"                                     
##                           "3.49201193054102, 2.38336453210239, -1.02139854067145, -0.960604821863898, -0.449813076662517, -0.883471529834742, -2.17682506120993, 0.112888109708279, -1.4702617652776"                                      
##                           "3.06411539932592, 1.55514099197852, -1.28456664395824, -1.38828243140775, -0.468423126309554, -0.898819059486899, -2.73007436640017, 0.490779460138829, -0.338373313889587"                                     
##                           "3.25908211379202, 2.98215572209041, -1.11843341073109, -0.955260437870964, -0.750662159252317, -2.30355139998208, -3.69687804533265, -0.0112845503732356, -0.382375739340811"                                   
##                           "3.47733712897854, 2.5926558079309, -1.22944941615294, -1.06134845395756, -1.08805848879318, -2.27128101334277, -3.64615721513543, 0.363230526401228, -0.819629212166369"                                        
##                           "3.23658885717307, 2.47013640861661, -1.22629165518837, -0.461567427364201, -0.765036181468426, -2.61076245584165, -2.63757041133117, -0.728072465057334, -1.34786606252486"                                     
##                           "3.62563736141529, 2.84854262643171, -1.21544690866216, -0.75085668209408, -0.977785744247429, -2.42762122491768, -3.82030679918264, -0.0134393401466752, -0.587478833669447"                                    
##                           "3.40605595394865, -1.09930014996134, -0.814145878284682, -0.23990642321122, -1.02021115955625, -1.53052686503547, -1.11498762865283, -0.912581386363129, -0.473303984416117"                                    
##                           "0.418716192041558, 40.5303286061268, -1.56578061400218, -0.452899863752414, 2.62658417285851, -0.134590545709601, -0.450985635069604, -3.15776134941803, -33.1915105132148, 2.95292665171383"                   
##                           "-0.356136744885359, 56.2458918526092, -3.30330740163721, -0.320269672766912, 3.236526294347, -0.27664835130901, -0.181000269393232, -3.22629426773591, -53.5006970959413, 7.03700401368934"                     
##                           "0.152129832785302, 36.706352140172, -2.33534324476718, -0.260605090119262, 2.58071515884841, -0.146489093595152, -0.404702778911519, -2.63962391684186, 1.17593754926216, -2.81811804374824"                    
##                           "-0.684501689394401, 48.8469471057607, -3.99046078968209, -0.615103976326662, 2.40311914558047, -0.295435463894581, -0.363379820100141, -45.2845644185105, 1.27000404715623, 6.09552411012742"                   
##                           "-0.402089187691697, 56.5613452956119, -3.07363883562535, -0.347379952466548, 3.19669476218565, -0.310260246845438, -3.27075195119783, -53.3793344722733, 1.29056194155837, 6.02274346959161"                    
##                           "-0.381076817032697, 55.6772527325409, -3.12298612055125, -0.438011484103173, 3.17747658431669, -0.283967010572191, -3.25604362205936, -52.4975926456711, 1.4241471370113, 5.82164305054334"                     
##                           "1.85462265871744, 30.2889017958318, -0.10624860303814, -0.401458865156155, -0.394836648066181, -0.634393883386072, -2.37440656035928, -31.2849028400862, 2.30640990736552, 2.96025940316441"                    
##                           "-0.34560366120128, 56.4187276677583, -3.12591879279726, 3.14511957169254, -0.345413704854171, -0.198659318694146, -3.28666517079083, -53.2262800420029, 1.23652723293558, 6.04729702693157"                     
##                           "0.684980460553496, 41.7816085680748, -0.456168396315404, 2.13183215832271, -0.385548781019496, -0.276245156734056, -3.55020279367121, -41.3799277089828, 1.90931678215626, 3.1757332777568"                     
##                           "3.58265146470804, 2.77727900883187, -1.02595675392556, -1.06634674578683, -0.472624769818213, -0.897623243836909, -2.28134164089135, -3.78183300905402, 0.0324079226202512, -0.680335099334026"                 
##                           "-0.306928435953195, 55.7786891516782, -3.02892522533127, -0.339726565649411, 3.13624224316156, -0.247924519844417, -0.186820649951496, -3.24304934276907, -52.6797684497867, 1.29893629118191, 5.85502683461923"
##                                                                                                                                                                                                                                                    
## Logistic_regression.model "model.p-value"                                                                                                                                                                                                          
##                           "0.00722775643041945, 0.000398529990918366"                                                                                                                                                                              
##                           "8.75927785016642e-15, 0.707465377590797"                                                                                                                                                                                
##                           "1.38723397674444e-17, 0.00227879451342301"                                                                                                                                                                              
##                           "5.54601349999366e-12, 0.0162018045439829"                                                                                                                                                                               
##                           "2.27982627292277e-10, 0.0377172411928043"                                                                                                                                                                               
##                           "1.38467209316737e-10, 0.0499949029864774"                                                                                                                                                                               
##                           "4.03183522192605e-12, 0.0041568676191354"                                                                                                                                                                               
##                           "6.01713685310749e-21, 0.847236560109099"                                                                                                                                                                                
##                           "7.99148217162105e-21, 0.643797012520271"                                                                                                                                                                                
##                           "3.41003125747264e-20, 0.472126519312036"                                                                                                                                                                                
##                           "0.00787697603299773, 0.000551885525743305, 0.130718833333443"                                                                                                                                                           
##                           "0.00162915268441494, 0.00161120702350592, 0.0684862765134064"                                                                                                                                                           
##                           "0.690563982097837, 0.00115712681428488, 0.410403416799761"                                                                                                                                                              
##                           "0.00195413941660084, 0.000586909553176454, 0.0589621899545745"                                                                                                                                                          
##                           "0.0025964140198544, 0.000604959647456931, 0.0845756089287115"                                                                                                                                                           
##                           "0.000143523159522833, 0.000518014746366184, 0.0036803135184276"                                                                                                                                                         
##                           "0.000455767928981473, 0.000132088994454617, 0.014489769343504"                                                                                                                                                          
##                           "0.0411866457101891, 0.000424332870536657, 0.467865206174802"                                                                                                                                                            
##                           "0.00805818375943756, 0.000445599093719915, 0.754866117329959"                                                                                                                                                           
##                           "1.78777094925897e-14, 0.512357336476292, 0.00197016697850005"                                                                                                                                                           
##                           "4.29973465521396e-11, 0.390429358460916, 0.0114792029306148"                                                                                                                                                            
##                           "1.32925792994704e-09, 0.476089792460268, 0.0298651382373039"                                                                                                                                                            
##                           "3.60225969709317e-09, 0.781980704493529, 0.0519809504156653"                                                                                                                                                            
##                           "1.01435196378413e-11, 0.15525787392207, 0.00162508328644595"                                                                                                                                                            
##                           "6.80898697756735e-15, 0.633666943200146, 0.709012023294208"                                                                                                                                                             
##                           "4.05033279863339e-14, 0.744147758780454, 0.677242072122645"                                                                                                                                                             
##                           "8.05983521517262e-15, 0.614624641863633, 0.420575531786176"                                                                                                                                                             
##                           "5.02376362368319e-12, 0.00888939940708062, 0.0701194901447377"                                                                                                                                                          
##                           "1.31976966675044e-11, 0.0204213483273419, 0.29783840116825"                                                                                                                                                             
##                           "9.93928148104383e-12, 0.00730957386864855, 0.1574236151466"                                                                                                                                                             
##                           "8.09782830839721e-13, 0.0128597831474534, 0.0210670997929211"                                                                                                                                                           
##                           "3.18549565717309e-17, 0.00217920225438014, 0.719080491308707"                                                                                                                                                           
##                           "3.9622214058019e-17, 0.00213231338719414, 0.554714066028572"                                                                                                                                                            
##                           "5.65423744712755e-17, 0.00216044192413063, 0.440962802667257"                                                                                                                                                           
##                           "1.07293796349618e-09, 0.0202846016189857, 0.0470558251184019"                                                                                                                                                           
##                           "1.13381365255074e-09, 0.0078201268122765, 0.0222507560058937"                                                                                                                                                           
##                           "9.49776391265743e-12, 0.0862978918686677, 0.0222997754405908"                                                                                                                                                           
##                           "7.40932609834296e-12, 0.0159824235136318, 0.798827977977472"                                                                                                                                                            
##                           "5.90954459898369e-12, 0.0175068866181763, 0.784394080919509"                                                                                                                                                            
##                           "5.68543906283754e-12, 0.0209370987210499, 0.99763703117133"                                                                                                                                                             
##                           "5.41167693352666e-10, 0.151871476255582, 0.237943210701498"                                                                                                                                                             
##                           "9.70151762849973e-11, 0.0926539941978559, 0.0103767129227028"                                                                                                                                                           
##                           "2.74131792711116e-10, 0.0376021764065372, 0.830885639320715"                                                                                                                                                            
##                           "4.28130766742625e-10, 0.0290405367903136, 0.360117079348542"                                                                                                                                                            
##                           "2.71185893115326e-10, 0.0357925100567976, 0.42476307190168"                                                                                                                                                             
##                           "3.30433607706458e-10, 0.0596183778663328, 0.00468758483226667"                                                                                                                                                          
##                           "2.04105846475661e-10, 0.0456920331547859, 0.639181774316547"                                                                                                                                                            
##                           "1.72494207051621e-10, 0.0415351689374636, 0.445067876717428"                                                                                                                                                            
##                           "1.91851284209119e-10, 0.033865705149078, 0.250805052909232"                                                                                                                                                             
##                           "5.30153057939843e-12, 0.00402529152731968, 0.735047994859286"                                                                                                                                                           
##                           "4.82479269416696e-12, 0.00354498347611643, 0.455545620169854"                                                                                                                                                           
##                           "4.9788426086333e-12, 0.00376566309308126, 0.385356428677647"                                                                                                                                                            
##                           "3.06857033246472e-20, 0.883212906800515, 0.656792281294318"                                                                                                                                                             
##                           "4.15806269668792e-20, 0.860431101776682, 0.475382101470007"                                                                                                                                                             
##                           "4.88562083966235e-20, 0.895394336896255, 0.572956666398246"                                                                                                                                                             
##                           "0.00308730535963114, 0.00228624044256848, 0.276308756640051, 0.129904041291798"                                                                                                                                         
##                           "0.225873622165741, 0.000926765738578614, 0.0107367739097761, 0.0284087964331181"                                                                                                                                        
##                           "0.00336469491972132, 0.000970738863130759, 0.26584858711952, 0.100384227111037"                                                                                                                                         
##                           "0.00257717811550252, 0.000802966180292242, 0.126645672008229, 0.0815579159964709"                                                                                                                                       
##                           "0.000381072692916756, 0.00131669414615521, 0.787263037825775, 0.0126766992179572"                                                                                                                                       
##                           "0.00157680010597882, 0.000461063461513921, 0.231166522636662, 0.0575480180636495"                                                                                                                                       
##                           "0.0436899130146513, 0.000595802549586887, 0.134322001923181, 0.477791785236991"                                                                                                                                         
##                           "0.010353456906619, 0.000614872295203599, 0.139591557635418, 0.92882171618734"                                                                                                                                           
##                           "0.337499066425863, 0.00281966085486053, 0.0691562965390265, 0.402612993425798"                                                                                                                                          
##                           "0.00101405038303801, 0.00151843813174937, 0.279840787112174, 0.191680916176588"                                                                                                                                         
##                           "0.000935206729222721, 0.00182802192101966, 0.138924664430007, 0.162325852074296"                                                                                                                                        
##                           "9.80312801164792e-05, 0.00136553400427378, 0.248377564899277, 0.0100225564443058"                                                                                                                                       
##                           "6.04779484326658e-05, 0.000341381861974119, 0.0430723920068355, 0.00603782878149973"                                                                                                                                    
##                           "0.00875856498864701, 0.00177851715749284, 0.0854433062825598, 0.583111642445082"                                                                                                                                        
##                           "0.00164516529698857, 0.0018367693768889, 0.0644822640037973, 0.654757158525782"                                                                                                                                         
##                           "0.225430447914621, 0.00146585859489978, 0.505826221386431, 0.0679716914306342"                                                                                                                                          
##                           "0.226717086578366, 0.00296753647204319, 0.664978806180276, 0.115707396186093"                                                                                                                                           
##                           "0.307233070160713, 0.000306836774725938, 0.064303023940065, 0.00114919036362398"                                                                                                                                        
##                           "0.834110304654681, 0.000298866711422077, 0.180756154700314, 0.00991827641750336"                                                                                                                                        
##                           "0.691989636830527, 0.00106839942271466, 0.507215722054937, 0.549047694318202"                                                                                                                                           
##                           "0.755364043955057, 0.00107020562455014, 0.338335702031008, 0.539387863248693"                                                                                                                                           
##                           "0.00133687884425158, 0.000727120138791546, 0.200550096169233, 0.339423528156865"                                                                                                                                        
##                           "9.56561507127996e-05, 0.000719629910680104, 0.169829621919493, 0.0100266373411828"                                                                                                                                      
##                           "0.000185947203722002, 0.00016246417462086, 0.0379266302499783, 0.00703011436951117"                                                                                                                                     
##                           "0.00902701986820483, 0.000729483636357792, 0.0815561778952027, 0.734794665090443"                                                                                                                                       
##                           "0.00183246649672416, 0.000663526164999321, 0.0542868409598881, 0.599559574667403"                                                                                                                                       
##                           "0.000118586922923458, 0.000790244914973876, 0.129536908711729, 0.00524193763485125"                                                                                                                                     
##                           "0.000123348086537144, 0.000123144059310911, 0.0383120681571207, 0.00273720479241826"                                                                                                                                    
##                           "0.0113440971549562, 0.000743628373327371, 0.111649921351483, 0.654761087850054"                                                                                                                                         
##                           "0.00224400586604573, 0.000748321074004361, 0.0683266735894895, 0.481210010202229"                                                                                                                                       
##                           "7.81307134363507e-06, 0.000140696581096735, 0.00181018428308151, 0.00494056689016073"                                                                                                                                   
##                           "0.000750380024159723, 0.000600931610819902, 0.00474449008894261, 0.667749499453513"                                                                                                                                     
##                           "0.000138071240191063, 0.000585764878372592, 0.00334282157020681, 0.572374954466148"                                                                                                                                     
##                           "0.0889153348121424, 0.000163859203577836, 0.00113008504405523, 0.123134878735345"                                                                                                                                       
##                           "0.0392471320638442, 0.000110727201178415, 0.000651809846928641, 0.106585889304144"                                                                                                                                      
##                           "0.0455407187374494, 0.000483838642058834, 0.326705499842916, 0.32704739649034"                                                                                                                                          
##                           "2.53349429403528e-11, 0.293711288428291, 0.00717835170857093, 0.0455659224530074"                                                                                                                                       
##                           "8.92812762894526e-11, 0.416556561553717, 0.0189177348504489, 0.247767363082901"                                                                                                                                         
##                           "2.9529099370823e-10, 0.570845082176605, 0.0063826315274746, 0.169983249822542"                                                                                                                                          
##                           "1.95103521194609e-12, 0.131742435779106, 0.0114791888358656, 0.00709971869522675"                                                                                                                                       
##                           "1.24301795934319e-14, 0.408804957007534, 0.00171794218362641, 0.494242291067758"                                                                                                                                        
##                           "5.58644772234457e-14, 0.550303174298556, 0.00188019001385917, 0.611988476969723"                                                                                                                                        
##                           "1.73035781850521e-14, 0.426529240245966, 0.00177645224643088, 0.360222546276156"                                                                                                                                        
##                           "2.15818682900423e-09, 0.195200276497901, 0.0102348224280447, 0.026497178677403"                                                                                                                                         
##                           "3.19458310352139e-09, 0.403565599126857, 0.00559276638739012, 0.0228223586268195"                                                                                                                                       
##                           "1.43735564868254e-11, 0.0935942153958686, 0.0489672726707335, 0.00634228011164109"                                                                                                                                      
##                           "4.27419968722238e-11, 0.327587364311007, 0.0104050024074301, 0.544198214557056"                                                                                                                                         
##                           "4.07326376826033e-11, 0.344166574455241, 0.0116522776703877, 0.64336379576829"                                                                                                                                          
##                           "4.68402675086063e-11, 0.388825218360492, 0.0158731086778128, 0.950040329588572"                                                                                                                                         
##                           "4.34048145458777e-09, 0.581732056033906, 0.129803506886357, 0.277566999426231"                                                                                                                                          
##                           "1.01176431150098e-10, 0.0935867642806328, 0.0511338407610134, 0.00272173912116531"                                                                                                                                      
##                           "1.18389218712569e-09, 0.397352453984262, 0.0277120548499484, 0.575960783764532"                                                                                                                                         
##                           "2.5615754620864e-09, 0.518516121901827, 0.0238808423443355, 0.397059426029299"                                                                                                                                          
##                           "1.07623294740374e-09, 0.381834746515809, 0.0260797211043035, 0.331309766456104"                                                                                                                                         
##                           "7.33312152767527e-10, 0.189693462689143, 0.0752341214483551, 0.00215572050531591"                                                                                                                                       
##                           "3.06791798389319e-09, 0.649198193454845, 0.0462641579297767, 0.536021044227769"                                                                                                                                         
##                           "4.51628672500917e-09, 0.845139715154995, 0.0432339838069782, 0.464040342830537"                                                                                                                                         
##                           "2.3950576705961e-09, 0.640165352682311, 0.0345615688443578, 0.222410745143169"                                                                                                                                          
##                           "1.01464890920146e-11, 0.0957481435057684, 0.00110391772785489, 0.240511770433055"                                                                                                                                       
##                           "1.35505866456848e-11, 0.169385849758814, 0.00149296283565115, 0.544825157173943"                                                                                                                                        
##                           "9.57763114503234e-12, 0.11059938934051, 0.00115740825041371, 0.222684794246182"                                                                                                                                         
##                           "4.26791127318711e-14, 0.680643835165859, 0.758351171657257, 0.719019175771646"                                                                                                                                          
##                           "1.27487314083256e-14, 0.637585771889929, 0.99470298154226, 0.479029616679573"                                                                                                                                           
##                           "3.18464493286799e-14, 0.625558067431101, 0.996540038389375, 0.493452662193058"                                                                                                                                          
##                           "6.85485949986458e-10, 0.0620399705883976, 0.0620746983100019, 0.257327217437239"                                                                                                                                        
##                           "6.73489798609883e-10, 0.0379566479481969, 0.0373738137434452, 0.0784149145573042"                                                                                                                                       
##                           "5.08452138191265e-12, 0.0261486075375896, 0.195820811121407, 0.0572078680138083"                                                                                                                                        
##                           "6.59231710075981e-12, 0.00846238769973379, 0.0691349264471636, 0.687132702706182"                                                                                                                                       
##                           "5.21775361269946e-12, 0.00937178720081974, 0.0849272089477801, 0.99399943719372"                                                                                                                                        
##                           "4.87625987279088e-12, 0.00865729957422895, 0.094487640708478, 0.829058896360827"                                                                                                                                        
##                           "7.71791633768673e-11, 0.023204136485344, 0.579835362113434, 0.275805577862346"                                                                                                                                          
##                           "3.31677686627353e-11, 0.0575038140251672, 0.41687042256064, 0.027447885314324"                                                                                                                                          
##                           "1.55885332936489e-11, 0.0197148350216312, 0.299755410998214, 0.730512664707138"                                                                                                                                         
##                           "3.50444392940998e-11, 0.0218569036923175, 0.243973978326934, 0.405616852797324"                                                                                                                                         
##                           "1.7579666883734e-11, 0.0201995068003958, 0.287519410108598, 0.420466429889841"                                                                                                                                          
##                           "7.69474900369873e-11, 0.0368860737549018, 0.164908809205417, 0.0216775140626925"                                                                                                                                        
##                           "1.49446297601471e-11, 0.00701042666337679, 0.143352551212313, 0.578156857059511"                                                                                                                                        
##                           "1.48130756281946e-11, 0.00700295129942904, 0.131924324093008, 0.411899625226301"                                                                                                                                        
##                           "1.81936385209821e-11, 0.00760828877597777, 0.113316703667068, 0.273784447901306"                                                                                                                                        
##                           "1.04657577940347e-12, 0.0122761106621436, 0.0203351170681524, 0.648821298036052"                                                                                                                                        
##                           "1.10085822457697e-12, 0.012069934845471, 0.0178572609342223, 0.408455846698255"                                                                                                                                         
##                           "1.08068218080677e-12, 0.0124613962676972, 0.0191881234732746, 0.373076126462677"                                                                                                                                        
##                           "7.74371057425527e-17, 0.00206069174125092, 0.761649163605575, 0.577522827644739"                                                                                                                                        
##                           "6.76676815060214e-17, 0.00220057280647963, 0.976937274360182, 0.497520485135564"                                                                                                                                        
##                           "7.71960941057114e-17, 0.00211829454557913, 0.806512014249948, 0.577385977293538"                                                                                                                                        
##                           "1.68911845987184e-09, 0.0112261291880797, 0.238533872143298, 0.113924640778594"                                                                                                                                         
##                           "9.42564771570293e-11, 0.0984775031569614, 0.105495963270265, 0.0508762534622481"                                                                                                                                        
##                           "1.22986128726209e-09, 0.0199414542416666, 0.0467654386768389, 0.775244447684827"                                                                                                                                        
##                           "1.28810538088793e-09, 0.0296774603864103, 0.0483185147837201, 0.839935470405065"                                                                                                                                        
##                           "1.07063314743154e-09, 0.0276946787967238, 0.0468136940403284, 0.910529236437384"                                                                                                                                        
##                           "2.75408894086771e-10, 0.0474191498269278, 0.0326488751492342, 0.0324478508258526"                                                                                                                                       
##                           "1.34144664902112e-09, 0.00738225794066141, 0.0194715759624221, 0.534317289362098"                                                                                                                                       
##                           "1.15499607107433e-09, 0.0101606645673836, 0.0232110674648199, 0.99093296566531"                                                                                                                                         
##                           "1.11456567485612e-09, 0.0129886568828024, 0.0204112399506546, 0.694195342938039"                                                                                                                                        
##                           "1.19297678957819e-11, 0.085416300849985, 0.0216068299141541, 0.710735333154213"                                                                                                                                         
##                           "1.00707348644523e-11, 0.115661538936939, 0.0227244818439978, 0.834295393777409"                                                                                                                                         
##                           "9.53957311673895e-12, 0.123111173579492, 0.0212618824403282, 0.7422879660287"                                                                                                                                           
##                           "8.30137481428214e-12, 0.0169960576397765, 0.762830355013939, 0.759467049815172"                                                                                                                                         
##                           "9.56056729562164e-12, 0.0201780361083821, 0.761948405103067, 0.880616447493411"                                                                                                                                         
##                           "5.74758760941464e-12, 0.0200940017657001, 0.760250497487237, 0.884941568816442"                                                                                                                                         
##                           "3.7372949722631e-10, 0.279806767700187, 0.180809237457692, 0.00797878340145856"                                                                                                                                         
##                           "6.57640937325007e-10, 0.157164312098126, 0.223992129075074, 0.698533504215501"                                                                                                                                          
##                           "6.91477150741217e-10, 0.123751134123313, 0.216733986924357, 0.317309191703606"                                                                                                                                          
##                           "5.56817306926571e-10, 0.1663310568958, 0.179903935007339, 0.287068655569652"                                                                                                                                            
##                           "1.09762499314546e-10, 0.0927671567679748, 0.0100822814180076, 0.737117910937821"                                                                                                                                        
##                           "1.22624275479117e-10, 0.0684508370930413, 0.0086951258506307, 0.273226885913808"                                                                                                                                        
##                           "9.35714751566846e-11, 0.0898497294476573, 0.00962812344680054, 0.365860684670437"                                                                                                                                       
##                           "4.91884872155848e-10, 0.0291089845803338, 0.895424024985009, 0.369935390174329"                                                                                                                                         
##                           "2.78563346960441e-10, 0.035630959657954, 0.845735205647246, 0.428519777923757"                                                                                                                                          
##                           "4.48054520009685e-10, 0.0307719802828341, 0.556609975597008, 0.663512065415403"                                                                                                                                         
##                           "3.99638810243386e-10, 0.052682692651245, 0.00438700086244915, 0.536196569408811"                                                                                                                                        
##                           "2.96471251279908e-10, 0.0471839341114802, 0.00380318218817864, 0.313226228071117"                                                                                                                                       
##                           "3.269468883055e-10, 0.038804130975914, 0.00405970117636218, 0.196937727470843"                                                                                                                                          
##                           "2.42431723089503e-10, 0.0387061007827556, 0.684572347530931, 0.467660343601972"                                                                                                                                         
##                           "2.15528301050859e-10, 0.0343650208119269, 0.928668680135662, 0.296944107597122"                                                                                                                                         
##                           "2.06980052007802e-10, 0.0329599181884333, 0.794930289229066, 0.373328673590004"                                                                                                                                         
##                           "6.15639708278002e-12, 0.00347427068000322, 0.788283895175881, 0.473742836513668"                                                                                                                                        
##                           "5.45821371635769e-12, 0.00381778877902875, 0.920972412365778, 0.423920051572578"                                                                                                                                        
##                           "5.43076279298278e-12, 0.00353765823525293, 0.707143464678854, 0.562553574165759"                                                                                                                                        
##                           "6.65768361184107e-20, 0.875469442322578, 0.920973922394804, 0.570280890722049"                                                                                                                                          
##                           "0.49447000106854, 0.00289992752471616, 0.0337854705975275, 0.293018416107941, 0.0482667864348134"                                                                                                                       
##                           "0.00206739439115915, 0.00245756471416096, 0.356055395531298, 0.369071454966722, 0.235512382843613"                                                                                                                      
##                           "0.0013976691744668, 0.00245781938304608, 0.238515208823283, 0.254280279546119, 0.14267191553185"                                                                                                                        
##                           "0.000258818386115703, 0.00374086892111381, 0.940520814621871, 0.259667659156468, 0.0206005159367206"                                                                                                                    
##                           "0.000131612105082807, 0.000683635805618153, 0.457847942767641, 0.0709873024738017, 0.0123867522995709"                                                                                                                  
##                           "0.0152291117163865, 0.00248856476148198, 0.27248293915189, 0.158365899197838, 0.574311222975008"                                                                                                                        
##                           "0.00363174092932963, 0.00269433010770338, 0.304925682597325, 0.125083766645834, 0.79599685752139"                                                                                                                       
##                           "0.618207986390561, 0.00209960941123139, 0.0365612535735473, 0.0627249307290694, 0.278181451540082"                                                                                                                      
##                           "0.635315838417376, 0.00220720421135392, 0.0210546342935509, 0.0676606389475706, 0.259220985324607"                                                                                                                      
##                           "0.68726400802441, 0.0010034305704552, 0.077326292787087, 0.0154001673794583, 0.00689100251412966"                                                                                                                       
##                           "0.210528365270084, 0.000572656193871033, 0.0140018105078658, 0.013353338087402, 0.0246521880810744"                                                                                                                     
##                           "0.231901641737408, 0.000958623891659316, 0.0127227473643447, 0.0369755584400399, 0.805907872487302"                                                                                                                     
##                           "0.205087408577917, 0.000776400692638308, 0.0101398775739888, 0.0223941046792442, 0.488442231406679"                                                                                                                     
##                           "0.0018616230396861, 0.00107131219929548, 0.211782423403386, 0.330593922086439, 0.271437733196266"                                                                                                                       
##                           "0.000252761334914205, 0.00213326886561175, 0.980099028283371, 0.176181250595274, 0.0206969599459216"                                                                                                                    
##                           "0.000413038672883879, 0.00044196207044401, 0.461324330787869, 0.0587493704360511, 0.0192952220063893"                                                                                                                   
##                           "0.0151655166436244, 0.00117384636083348, 0.260176050744272, 0.136336104708004, 0.708913136329417"                                                                                                                       
##                           "0.00368151250406667, 0.00112140166475375, 0.299519530269942, 0.0951250592180725, 0.739146636340565"                                                                                                                     
##                           "0.000233924020471807, 0.00181048686274225, 0.733919858107177, 0.126022585175077, 0.018292263543678"                                                                                                                     
##                           "0.000182523123007311, 0.000261775261159512, 0.238658072673276, 0.0392595236996863, 0.00986449319263977"                                                                                                                 
##                           "0.0112395504301379, 0.000961160828414502, 0.128434510325321, 0.106533859899165, 0.667576910919853"                                                                                                                      
##                           "0.00279646941904408, 0.000996097479782735, 0.152198362621107, 0.0729727717691078, 0.650971964543603"                                                                                                                    
##                           "1.27189049759385e-05, 0.000323502391507361, 0.821629654646372, 0.00426331094126274, 0.00369093211631535"                                                                                                                
##                           "0.00174429857753874, 0.00150563324059032, 0.772414803569355, 0.0159249102351567, 0.660715553919453"                                                                                                                     
##                           "0.000408468517092586, 0.0016224114667254, 0.868927579393688, 0.0113451982523794, 0.604394705676227"                                                                                                                     
##                           "0.0780899199590567, 0.000196234675296563, 0.26899804811335, 0.00219343498973076, 0.132833956258216"                                                                                                                     
##                           "0.0883536047263423, 0.000289469137387784, 0.0773767694419683, 0.00102945712105185, 0.0785811598224797"                                                                                                                  
##                           "0.0475480837285518, 0.000665263464498013, 0.179788449651284, 0.379776506691937, 0.477011607348593"                                                                                                                      
##                           "0.180699706378731, 0.00267687831732796, 0.266071433747615, 0.474042179732159, 0.218683449031467"                                                                                                                        
##                           "0.15066968381028, 0.00502612715433457, 0.131705685588482, 0.602260162882423, 0.218803509160093"                                                                                                                         
##                           "0.204893800450507, 0.000721584747311655, 0.314430590949371, 0.0754269607724221, 0.00321815594308621"                                                                                                                    
##                           "0.435136287407267, 0.000377760640387479, 0.036717217625187, 0.146931095848909, 0.0030772728039814"                                                                                                                      
##                           "0.349001273298666, 0.002724458360581, 0.0815882619726829, 0.466429511476226, 0.683539459244902"                                                                                                                         
##                           "0.385342048570853, 0.00250541856190715, 0.0613897308810592, 0.309730012733464, 0.440812909261401"                                                                                                                       
##                           "0.00074723608885035, 0.00177786182701015, 0.289499109309997, 0.416291031102831, 0.351236694183849"                                                                                                                      
##                           "7.78606710171551e-05, 0.00138641494704914, 0.532308489273666, 0.30860405228391, 0.0149750014378886"                                                                                                                     
##                           "6.37969007589879e-05, 0.000328782299864632, 0.229329362248523, 0.14828151894702, 0.00503385636217372"                                                                                                                   
##                           "0.00500589879263864, 0.00183420221944891, 0.285112602628189, 0.228844340604886, 0.759235829246075"                                                                                                                      
##                           "0.000934410845078257, 0.00172247891691307, 0.273074154175059, 0.179951025862202, 0.576349081355485"                                                                                                                     
##                           "8.36741060186622e-05, 0.0015888478549027, 0.409191008342, 0.193075948505028, 0.011503542216997"                                                                                                                         
##                           "3.19787044459334e-05, 0.000300029081776801, 0.106012240839988, 0.084332244197082, 0.00236018709052944"                                                                                                                  
##                           "0.00425104377222854, 0.00228825710187681, 0.150458551336035, 0.190791859875221, 0.74277020521449"                                                                                                                       
##                           "0.000789898092737543, 0.00223258497571145, 0.132267395036445, 0.131153846131932, 0.447616261590735"                                                                                                                     
##                           "3.58216883877161e-06, 0.000261696884030603, 0.182981125775509, 0.00549013515953388, 0.00309933557601262"                                                                                                                
##                           "0.000490741886978074, 0.00163973620727909, 0.267902823333578, 0.0115052638131393, 0.748246481644574"                                                                                                                    
##                           "8.96191839842888e-05, 0.00155700070535706, 0.23508582694153, 0.00913721279178204, 0.526240979229433"                                                                                                                    
##                           "0.0179273522938487, 0.000487179525308468, 0.079168095236138, 0.00171398858497876, 0.165651414369222"                                                                                                                    
##                           "0.00653166644375142, 0.000334305531691893, 0.0598757987628506, 0.00116184025502763, 0.126578345252355"                                                                                                                  
##                           "0.00976135468614252, 0.00188300614581135, 0.0872774095291011, 0.400070508337692, 0.338240710175555"                                                                                                                     
##                           "0.151301471551549, 0.00273667725494101, 0.649338590490464, 0.197854992918545, 0.410728414226414"                                                                                                                        
##                           "0.161762520056929, 0.000437947816359209, 0.0910944956754956, 0.255239659574996, 0.00336602516381625"                                                                                                                    
##                           "0.261501288867799, 0.000257618357498749, 0.213896798919545, 0.0436086443652789, 0.00456262537236524"                                                                                                                    
##                           "0.240418320428084, 0.00150003522690478, 0.541089278926972, 0.0849437631883268, 0.817314047925769"                                                                                                                       
##                           "0.256183744108376, 0.00130504850321503, 0.392784815314551, 0.0605754312842313, 0.432622011861275"                                                                                                                       
##                           "0.163735458154974, 0.000819654382315715, 0.135809427100991, 0.316004209456534, 0.00240679074304743"                                                                                                                     
##                           "0.235745416490083, 0.000447280359719977, 0.334944510619727, 0.065645295586291, 0.00293338116271235"                                                                                                                     
##                           "0.244359340971922, 0.00288515550301014, 0.71811975246449, 0.137047730516926, 0.695059803018233"                                                                                                                         
##                           "0.240411219196444, 0.00264306099651541, 0.530810223971605, 0.0954984468032008, 0.389705283020618"                                                                                                                       
##                           "0.35496971736541, 5.19414362093547e-05, 0.00827666150880702, 0.00020300657961894, 0.000936742202183594"                                                                                                                 
##                           "0.309944304273861, 0.000317816221938969, 0.0717421671669522, 0.00140202780699832, 0.967664989181712"                                                                                                                    
##                           "0.3654319481234, 0.000233344601462116, 0.0316099356591321, 0.000698870479901305, 0.165090369053396"                                                                                                                     
##                           "0.9278464748428, 0.000154630139424656, 0.270843377003544, 0.000851152843870545, 0.165923632819503"                                                                                                                      
##                           "0.883542366482433, 0.000166964518626269, 0.218499357600394, 0.000547214111075889, 0.117754236368756"                                                                                                                    
##                           "0.848562253271577, 0.000895842156642971, 0.377954343354126, 0.347037291282896, 0.23919388821257"                                                                                                                        
##                           "9.27974816497687e-05, 0.000929203133941098, 0.407065972236271, 0.305686360880229, 0.00921571847599263"                                                                                                                  
##                           "7.45975354154513e-05, 0.000153027992725522, 0.182090250616836, 0.213083963551118, 0.00298431516545621"                                                                                                                  
##                           "0.00566816008691836, 0.000957599190499746, 0.225030811316967, 0.355952469824964, 0.805834866428329"                                                                                                                     
##                           "0.00112218248115591, 0.000878840248126844, 0.197697477225659, 0.29022325672017, 0.471379674675021"                                                                                                                      
##                           "5.61281704519382e-06, 0.000191941926222628, 0.117794757556159, 0.00539833812158341, 0.00366347489609111"                                                                                                                
##                           "0.000478189822547542, 0.000921537462185783, 0.192568810553529, 0.0105992120168876, 0.89221333228879"                                                                                                                    
##                           "8.16154839316369e-05, 0.000825291186336695, 0.154974040859703, 0.00911338227099333, 0.483354271242734"                                                                                                                  
##                           "0.0209083626869127, 0.000289249179331861, 0.104321026915497, 0.00187434636027421, 0.23177864627761"                                                                                                                     
##                           "0.00494592001162819, 0.000169298919068405, 0.0431832033236792, 0.00071312412992332, 0.107721600142764"                                                                                                                  
##                           "0.0104371759653982, 0.000801127663983053, 0.0905562307005974, 0.517246497552023, 0.388869168123252"                                                                                                                     
##                           "3.89993195053002e-06, 0.00013026150265756, 0.0556380487686888, 0.00247596321725551, 0.00115105218286042"                                                                                                                
##                           "0.000468224883700195, 0.00104150485341113, 0.145047535942637, 0.00584748954157007, 0.862914909825856"                                                                                                                   
##                           "9.25884301828872e-05, 0.000980967285641355, 0.0983206066951913, 0.00452263522877233, 0.358844734014419"                                                                                                                 
##                           "0.0172359595761414, 0.000264583565072183, 0.0905530566437299, 0.00102242398795839, 0.214878206982284"                                                                                                                   
##                           "0.0080491510778272, 0.000191627371532655, 0.0755722127747041, 0.000857379226610324, 0.171340827454985"                                                                                                                  
##                           "0.010200425167076, 0.000842688942104548, 0.0922903502710577, 0.41068366785829, 0.244329129666945"                                                                                                                       
##                           "0.00120534877108591, 0.000211188112135095, 0.00332539729336784, 0.00107740983119173, 0.20347435658821"                                                                                                                  
##                           "0.000337258810162817, 0.000139906984226697, 0.00150796020429599, 0.000461745961239092, 0.0873990441887074"                                                                                                              
##                           "0.000799811909823439, 0.000668513735889623, 0.00473171787395322, 0.438929150155877, 0.317855724981724"                                                                                                                  
##                           "0.205256235735548, 0.00015998105531129, 0.000562779339561999, 0.239276513752036, 0.256875069047437"                                                                                                                     
##                           "1.44286281804917e-09, 0.195869572971762, 0.0638199332820268, 0.0328125905466086, 0.167365750912341"                                                                                                                     
##                           "2.00259430336853e-09, 0.312428016955567, 0.0307650983926998, 0.0245671982149969, 0.0831265934163839"                                                                                                                    
##                           "7.88359865668006e-12, 0.0843670734497932, 0.0244873184297138, 0.112854494169914, 0.0164819197048235"                                                                                                                    
##                           "2.54737348475642e-11, 0.220723155590147, 0.00609369564720701, 0.0409760239952888, 0.382608686839194"                                                                                                                    
##                           "2.51863602850668e-11, 0.282500514260099, 0.00816678436359738, 0.0517164412337496, 0.8156407260022"                                                                                                                      
##                           "2.64500307587178e-11, 0.283918817828996, 0.00679978868992748, 0.0674459923990042, 0.754777071144368"                                                                                                                    
##                           "7.12679460179011e-10, 0.502846230170392, 0.0214602711492066, 0.505331003427856, 0.326251433460437"                                                                                                                      
##                           "4.01595198251619e-11, 0.101946126394865, 0.0669455772083869, 0.278159788318568, 0.00782156377803697"                                                                                                                    
##                           "7.23778035725044e-11, 0.321824869258494, 0.0170716297993171, 0.236217306123423, 0.454452886060921"                                                                                                                      
##                           "2.48361557055812e-10, 0.45222947256698, 0.0202469341931218, 0.208305775639073, 0.452163537272579"                                                                                                                       
##                           "7.65902754224668e-11, 0.330929364026434, 0.0184233379391131, 0.224911326114011, 0.319154862860317"                                                                                                                      
##                           "1.86989376838286e-10, 0.158687879012081, 0.031289054576736, 0.211429344445992, 0.00832509322248616"                                                                                                                     
##                           "2.32606569713279e-10, 0.433571502343591, 0.00559292401240456, 0.150593904542736, 0.398172116645812"                                                                                                                     
##                           "4.64580689158868e-10, 0.629158448263986, 0.00627514750078136, 0.143541454294116, 0.454215157692125"                                                                                                                     
##                           "2.35981013384039e-10, 0.453590378763858, 0.00627976763847499, 0.118972142798001, 0.222324522909655"                                                                                                                     
##                           "1.93672691749919e-12, 0.0744813019169603, 0.00977936355707207, 0.00456433003967665, 0.177388522244704"                                                                                                                  
##                           "2.94247539612787e-12, 0.144705599677819, 0.0108858254298754, 0.00644862441507637, 0.494973334942867"                                                                                                                    
##                           "2.07044676235684e-12, 0.0904055367346428, 0.0105787777580618, 0.00503090736692323, 0.202612662911366"                                                                                                                   
##                           "4.78297857611081e-14, 0.451835602167988, 0.00167579709705894, 0.544152472408206, 0.685111066574929"                                                                                                                     
##                           "1.7778462069846e-14, 0.40547807679773, 0.0017142793853682, 0.789296665555446, 0.494299964188933"                                                                                                                        
##                           "4.78770736549824e-14, 0.444996929552488, 0.00177879599594688, 0.965186296079463, 0.447777395433044"                                                                                                                     
##                           "3.62052864304724e-09, 0.261668141333815, 0.00658603731382891, 0.158082049602287, 0.152697484450478"                                                                                                                     
##                           "1.00337600589621e-10, 0.0494687775479495, 0.0458971988090377, 0.0482781318256408, 0.0114563941597256"                                                                                                                   
##                           "2.23240709753619e-09, 0.146022484871185, 0.00879121128257641, 0.0233842616249407, 0.38953307044509"                                                                                                                     
##                           "2.55626587963342e-09, 0.19946346564669, 0.0145896393162063, 0.0304702235613504, 0.951045389227442"                                                                                                                      
##                           "2.17355449020404e-09, 0.189976980656576, 0.0157564950769978, 0.0258578846426046, 0.814782993320772"                                                                                                                     
##                           "4.01791590789444e-10, 0.107490914041303, 0.0267727413890346, 0.0391612556969145, 0.0103010267218921"                                                                                                                    
##                           "2.97371341960421e-09, 0.302680822891902, 0.00460713246513415, 0.0183119172551611, 0.324397586257258"                                                                                                                    
##                           "3.53711177733908e-09, 0.393603814408992, 0.007179113653059, 0.0256994542685191, 0.860774933821917"                                                                                                                      
##                           "3.07169168080684e-09, 0.386871899523963, 0.00975638228776264, 0.0203437831782921, 0.644614644637918"                                                                                                                    
##                           "1.3852902316863e-11, 0.057781558337058, 0.0467634515272974, 0.00439856693873921, 0.201312471589477"                                                                                                                     
##                           "1.55984820489938e-11, 0.0947873523758381, 0.0606661109073123, 0.00717724446575102, 0.942702855217692"                                                                                                                   
##                           "1.40688722369532e-11, 0.084021413919524, 0.088671995641126, 0.00552455016803623, 0.557883020394753"                                                                                                                     
##                           "4.0320747555155e-11, 0.264252416995429, 0.00976349897958514, 0.43794300862541, 0.562412091690969"                                                                                                                       
##                           "4.893851945969e-11, 0.319163053145878, 0.0127858590543021, 0.505592590964262, 0.806697460545112"                                                                                                                        
##                           "4.42227443603076e-11, 0.317567216866877, 0.0135722990771488, 0.579723750865473, 0.713173865554652"                                                                                                                      
##                           "5.58818507202242e-10, 0.126835407066031, 0.165157734704905, 0.278265217620298, 0.00265965573517855"                                                                                                                     
##                           "3.5877356345324e-09, 0.472380382187553, 0.126973438128799, 0.260112317352653, 0.51156871158733"                                                                                                                         
##                           "5.60676058090209e-09, 0.638892505157689, 0.10932659419601, 0.250028740599174, 0.34464589366874"                                                                                                                         
##                           "2.78508543708318e-09, 0.464841124691175, 0.133597177718335, 0.212233233457352, 0.235284153556928"                                                                                                                       
##                           "8.88190282982256e-11, 0.0521219421730581, 0.0433630049660948, 0.00169937202461475, 0.174646596595382"                                                                                                                   
##                           "1.42252204780268e-10, 0.101835711343861, 0.038692116415041, 0.0024186791138762, 0.320564084706981"                                                                                                                      
##                           "7.96794934342603e-11, 0.0605062621025955, 0.0429029010283032, 0.00188535366241037, 0.175670492064898"                                                                                                                   
##                           "2.5430284569705e-09, 0.455061201112317, 0.0229147567291474, 0.660629608263373, 0.440954036274512"                                                                                                                       
##                           "1.10753546717977e-09, 0.390017205124079, 0.0258901219790256, 0.91286284000504, 0.421540687230314"                                                                                                                       
##                           "2.49429487508612e-09, 0.434136302249869, 0.0244865899914292, 0.693517859110813, 0.519424911240503"                                                                                                                      
##                           "5.6005223472806e-10, 0.106949482914238, 0.060922439455263, 0.00134005044624705, 0.165459075173307"                                                                                                                      
##                           "7.40014766748188e-10, 0.213961437381521, 0.0609684590703695, 0.00193591304386363, 0.381088881479734"                                                                                                                    
##                           "5.13737769848013e-10, 0.121962453095761, 0.0443670432194063, 0.00137850392786998, 0.107179672429857"                                                                                                                    
##                           "4.26879764441948e-09, 0.725026022879224, 0.0397810273460129, 0.603577091437926, 0.514444659273144"                                                                                                                      
##                           "2.47235059637454e-09, 0.642492986719248, 0.0344746428981059, 0.942890684016027, 0.296088299886571"                                                                                                                      
##                           "3.51114864401946e-09, 0.673406646303775, 0.0341533585810874, 0.881902602542966, 0.327725403374819"                                                                                                                      
##                           "1.39193295231347e-11, 0.110340257604551, 0.00107042520002846, 0.278763799100325, 0.661394851116486"                                                                                                                     
##                           "1.04561495089128e-11, 0.0933840535182028, 0.00102513943122314, 0.52339596098374, 0.412153693357667"                                                                                                                     
##                           "1.28817762762788e-11, 0.116875691209983, 0.0011575074516221, 0.995128938157318, 0.295003564270064"                                                                                                                      
##                           "4.26266125223168e-14, 0.644511178455651, 0.995015183477671, 0.997042960850339, 0.542434585509917"                                                                                                                       
##                           "1.70064072397713e-09, 0.0841606889137581, 0.0377312626959972, 0.590014626247258, 0.152111301475483"                                                                                                                     
##                           "9.05026431763048e-11, 0.101666523425565, 0.181925408716946, 0.381203036526299, 0.0792288640889997"                                                                                                                      
##                           "7.62371432054552e-10, 0.059834210196849, 0.0611681782101139, 0.258692672923851, 0.696156021311056"                                                                                                                      
##                           "8.04240802638149e-10, 0.0610989691613618, 0.0854942307833942, 0.246547429180558, 0.795511537281582"                                                                                                                     
##                           "6.75342442713807e-10, 0.0603200023178671, 0.0852145154219112, 0.255258056132317, 0.808063552515274"                                                                                                                     
##                           "2.29607540418248e-10, 0.0908262365514336, 0.113780592764021, 0.0960390170450595, 0.0693555686389086"                                                                                                                    
##                           "8.04657691496396e-10, 0.0366358482921703, 0.0354518883053816, 0.0689425964885088, 0.499377135461355"                                                                                                                    
##                           "6.87476811832348e-10, 0.0369706985997964, 0.0507002544521645, 0.076424939890271, 0.835023921015631"                                                                                                                     
##                           "6.38993082978128e-10, 0.0356978403358578, 0.0589398116467146, 0.0695411181828773, 0.611728150650823"                                                                                                                    
##                           "6.28532471560683e-12, 0.024960233047447, 0.194234400450272, 0.0554983104195386, 0.633136280515469"                                                                                                                      
##                           "5.4041382020867e-12, 0.024314994636105, 0.268444514478557, 0.052158397279788, 0.679958168001767"                                                                                                                        
##                           "4.90756447550918e-12, 0.0242846835628716, 0.276583886286904, 0.051980078372296, 0.63012494568772"                                                                                                                       
##                           "7.05700884500991e-12, 0.00902754769403192, 0.0821265187172098, 0.68510128500102, 0.969165289734465"                                                                                                                     
##                           "8.03898873623717e-12, 0.00858296141833127, 0.0888463430955542, 0.745554759366231, 0.980729835437275"                                                                                                                    
##                           "4.93007157626033e-12, 0.00917669135202039, 0.0974330205834774, 0.924875991499251, 0.812534626196322"                                                                                                                    
##                           "2.02411009279427e-10, 0.0744780256258179, 0.694976262283206, 0.237342035088962, 0.0239312616408043"                                                                                                                     
##                           "9.21516328428434e-11, 0.0220324607352524, 0.6014523975824, 0.253981061262661, 0.607018873963845"                                                                                                                        
##                           "1.2780922676817e-10, 0.0249245545869092, 0.502669595650623, 0.251276904183441, 0.356671938900494"                                                                                                                       
##                           "9.5491271313273e-11, 0.0231059690238115, 0.613431583559827, 0.208375171497723, 0.287593389348732"                                                                                                                       
##                           "3.61218031465612e-11, 0.0553410932099641, 0.42194590456101, 0.0265785605644762, 0.663630642226196"                                                                                                                      
##                           "5.24298592762808e-11, 0.0634869519033481, 0.333724032885151, 0.0232773601960145, 0.31305302465395"                                                                                                                      
##                           "3.38111986516296e-11, 0.0572168770735385, 0.408729942476068, 0.0253673592645298, 0.365158890070956"                                                                                                                     
##                           "3.94331161278275e-11, 0.0212986031901576, 0.247022079868438, 0.790534521760963, 0.424132380814082"                                                                                                                      
##                           "1.81388580527456e-11, 0.0206360967523811, 0.28677630379559, 0.948981734493572, 0.4675815155195"                                                                                                                         
##                           "3.64817519665388e-11, 0.0213502514211255, 0.25607327636877, 0.623716379846215, 0.630756639709318"                                                                                                                       
##                           "9.43834535259141e-11, 0.0360471373797674, 0.147557965074947, 0.0205880918619543, 0.512529890949224"                                                                                                                     
##                           "7.67992417121815e-11, 0.0364468754488581, 0.134141317304768, 0.0178722158104736, 0.304449782833463"                                                                                                                     
##                           "9.25790299193313e-11, 0.0400263673305935, 0.114776798018751, 0.0191049496981013, 0.224477954928409"                                                                                                                     
##                           "2.09997210737948e-11, 0.00675699178811484, 0.122190041312995, 0.622519244499192, 0.437025950219032"                                                                                                                     
##                           "2.03736178218547e-11, 0.00764230942452129, 0.113369293084931, 0.982209607412318, 0.347190755374845"                                                                                                                     
##                           "2.07075573412387e-11, 0.0074102359678402, 0.109440142259488, 0.729489129464131, 0.419083138586495"                                                                                                                      
##                           "1.3827921688785e-12, 0.0116385082687396, 0.0174447419889132, 0.703530258346754, 0.431434478211878"                                                                                                                      
##                           "1.18321696867654e-12, 0.0125371320773659, 0.0192066233055112, 0.992054954500258, 0.446024580495561"                                                                                                                     
##                           "1.26784415781608e-12, 0.0120640611540275, 0.0177866309393298, 0.653366304921767, 0.569100071740999"                                                                                                                     
##                           "1.00036626044158e-16, 0.00215165435429806, 0.989148815146618, 0.807695696205262, 0.640017538511966"                                                                                                                     
##                           "2.71684737921154e-10, 0.05953022440576, 0.378646125276989, 0.106671692007088, 0.046954255944884"                                                                                                                        
##                           "1.93579082843464e-09, 0.0105918085845796, 0.247931487264268, 0.10296209212593, 0.581791784922766"                                                                                                                       
##                           "1.92207842068826e-09, 0.0170348667019751, 0.229188194542986, 0.112331175578089, 0.797658618387743"                                                                                                                      
##                           "1.64658056225784e-09, 0.0176055375394012, 0.24101614825692, 0.105949550796623, 0.710403778107359"                                                                                                                       
##                           "1.07507949725437e-10, 0.097324361414771, 0.10549579631338, 0.0495696630886886, 0.709984633705425"                                                                                                                       
##                           "1.24121407192543e-10, 0.157760884131486, 0.0903898681598114, 0.0434606375982836, 0.555368646635105"                                                                                                                     
##                           "9.16260850577503e-11, 0.142738275858307, 0.10312650251735, 0.0474581782453979, 0.698167497573697"                                                                                                                       
##                           "1.41862046624564e-09, 0.0289937217874908, 0.0488207472193002, 0.794664982278303, 0.866970276393236"                                                                                                                     
##                           "1.28043173950941e-09, 0.0271213089944069, 0.0473205870420758, 0.794953156559204, 0.97297972519675"                                                                                                                      
##                           "1.31100160283838e-09, 0.0325212898567558, 0.048712555508722, 0.86669807484894, 0.97662903050973"                                                                                                                        
##                           "3.23614336736984e-10, 0.0453164473519765, 0.0279928089419935, 0.0307718487563186, 0.473288889671642"                                                                                                                    
##                           "2.95091914223929e-10, 0.0722055004463283, 0.0301219744390592, 0.0291794873264722, 0.650369184804818"                                                                                                                    
##                           "2.79235120625274e-10, 0.0833501248249142, 0.0269299088235165, 0.0275644267426947, 0.483784543535613"                                                                                                                    
##                           "1.34043455301045e-09, 0.00938387401362078, 0.0205299762617272, 0.531413208982019, 0.957033478122511"                                                                                                                    
##                           "1.34863670605026e-09, 0.0117860460513266, 0.0194836426022443, 0.652099964656174, 0.897985066517228"                                                                                                                     
##                           "1.08455165922984e-09, 0.0131948624467293, 0.021227572484508, 0.863763260980743, 0.663811057201112"                                                                                                                      
##                           "1.23681201043649e-11, 0.112956519555995, 0.0224519785085489, 0.728780287223918, 0.866347073029234"                                                                                                                      
##                           "1.2195080354114e-11, 0.120201513086918, 0.0217209784864681, 0.829247915464893, 0.865691566360903"                                                                                                                       
##                           "9.86875785259698e-12, 0.134084451658165, 0.0222602678251025, 0.941371352664145, 0.791437126929111"                                                                                                                      
##                           "9.56313577338635e-12, 0.0195788096284661, 0.797928040501428, 0.785156577776732, 0.996015648054961"                                                                                                                      
##                           "4.29452436392925e-10, 0.292031866940954, 0.164061319678428, 0.00748162844085922, 0.582414599746147"                                                                                                                     
##                           "3.44278164242352e-10, 0.222919757960146, 0.159931231763169, 0.00647311357213684, 0.234707341000595"                                                                                                                     
##                           "3.42257747541357e-10, 0.308007238205216, 0.127821820565238, 0.00686171496324659, 0.222429279596234"                                                                                                                     
##                           "8.21380283623275e-10, 0.128166262787354, 0.207577292600129, 0.762047806850331, 0.333339719952366"                                                                                                                       
##                           "5.93681814843625e-10, 0.16517230593618, 0.182599866470918, 0.894470626550843, 0.32065999071071"                                                                                                                         
##                           "7.02109575159771e-10, 0.14573772710064, 0.184298238612221, 0.583235746017561, 0.493760846694416"                                                                                                                        
##                           "1.37281446994366e-10, 0.0689662844884964, 0.00855568533639185, 0.811800579495118, 0.285834637390101"                                                                                                                    
##                           "9.820542423578e-11, 0.0895604669547656, 0.00977509797866112, 0.903564396943431, 0.399649574148578"                                                                                                                      
##                           "1.23435599162445e-10, 0.0728356393872137, 0.00865694072603335, 0.457974788794946, 0.649510246085492"                                                                                                                    
##                           "4.91667522706413e-10, 0.0310267066118586, 0.916939844400617, 0.576977151984005, 0.668757763710385"                                                                                                                      
##                           "3.60581940532244e-10, 0.0426806017572901, 0.00361608829365523, 0.58840004160696, 0.335246180805945"                                                                                                                     
##                           "3.64294549204532e-10, 0.0390574991357882, 0.00408459174222371, 0.996333650913842, 0.260503828971973"                                                                                                                    
##                           "3.18064971932171e-10, 0.0370839024590554, 0.00374770488090448, 0.634975856783602, 0.359301567822915"                                                                                                                    
##                           "2.43088330834867e-10, 0.0334229980260654, 0.962431771344341, 0.8048304359653, 0.429685110595244"                                                                                                                        
##                           "6.24716630076767e-12, 0.00359263151954627, 0.971466439394947, 0.716962490204605, 0.608383565033725"                                                                                                                     
##                           "0.73524934903758, 0.00389360976853936, 0.0567019975535375, 0.496361817207197, 0.0744972280239058, 0.446420763415507"                                                                                                    
##                           "0.840512056566948, 0.00492719048474415, 0.0473834553403351, 0.389350250484841, 0.0900152127197252, 0.335599244555558"                                                                                                   
##                           "0.868670663681228, 0.00245107866158029, 0.121289694266394, 0.585972455469421, 0.0238907587407871, 0.0102046238362587"                                                                                                   
##                           "0.538064595935356, 0.0020452552876986, 0.0519325889204194, 0.161211897726287, 0.0233926080714776, 0.0146205611516707"                                                                                                   
##                           "0.495866375827903, 0.00294876864382419, 0.036340954034333, 0.302327631394901, 0.0569270584936302, 0.879312025024069"                                                                                                    
##                           "0.465751482797478, 0.0024296307557506, 0.0328500060188424, 0.26333767547635, 0.0363430007837171, 0.420264048444958"                                                                                                     
##                           "0.00124402946139876, 0.002552003337827, 0.289452189965617, 0.398239850227084, 0.521600898909124, 0.289431164228763"                                                                                                     
##                           "0.000203812624848483, 0.00409102502489099, 0.963485330419106, 0.531058282220709, 0.308558356039886, 0.0254858506812356"                                                                                                 
##                           "0.000118863062079415, 0.000679651305154707, 0.588677663096526, 0.271739318525796, 0.172547244481416, 0.00874242337707307"                                                                                               
##                           "0.00962535340470062, 0.00287764749910635, 0.348468424845326, 0.377567635444569, 0.282242606864989, 0.73256805979855"                                                                                                    
##                           "0.00220394809187924, 0.002918283087167, 0.40286674045479, 0.357629219899256, 0.223431166562971, 0.695720103058244"                                                                                                      
##                           "0.000172672489840881, 0.00393463396601806, 0.847728870194866, 0.437021576824008, 0.1887230881773, 0.0258513336999247"                                                                                                   
##                           "4.5530344048005e-05, 0.000495770412256566, 0.410672945156164, 0.165509226474495, 0.0784285128827358, 0.00419741082261053"                                                                                               
##                           "0.00607216135833632, 0.00294793945863615, 0.237479043996292, 0.272255760104138, 0.16855271750662, 0.736588383803347"                                                                                                    
##                           "0.00143438710495758, 0.00314180169601167, 0.28688073819654, 0.238253485472618, 0.124498358070815, 0.582335235501627"                                                                                                    
##                           "4.84498075271686e-06, 0.000678418132881265, 0.66560007042298, 0.164636913574621, 0.00762106628963777, 0.00202607092276389"                                                                                              
##                           "0.0011301498647569, 0.00436950919726212, 0.924062952324742, 0.281951985841285, 0.0238308248214175, 0.744763319383035"                                                                                                   
##                           "0.00025609640566327, 0.00484046827666353, 0.956103238032422, 0.237942435911244, 0.0177445726659044, 0.527188494325017"                                                                                                  
##                           "0.020662450743974, 0.000610614527699897, 0.460698037504984, 0.118191545872656, 0.00222317512559529, 0.16768753101477"                                                                                                   
##                           "0.0241282444196398, 0.000792050890897592, 0.176770518611949, 0.132793031115104, 0.00174383360138905, 0.101059283465797"                                                                                                 
##                           "0.0157823809554826, 0.00271238702961837, 0.347954296143635, 0.15165832987456, 0.433881507425221, 0.448238662049496"                                                                                                     
##                           "0.807421946118234, 0.00310131947800804, 0.0406106749884646, 0.0892965539671529, 0.501661228793227, 0.467827288758609"                                                                                                   
##                           "0.960452018186273, 0.00197064864848686, 0.137640634744787, 0.0309684833105163, 0.524724634419223, 0.0104741599536209"                                                                                                   
##                           "0.670585284021129, 0.00126198985194455, 0.0505780076557069, 0.031458880049482, 0.169241404384149, 0.0154212952362411"                                                                                                   
##                           "0.617381084955167, 0.00213851900083075, 0.0372743465754485, 0.0672805556602242, 0.291096687932421, 0.971457515635398"                                                                                                   
##                           "0.59287855074382, 0.00169975194582259, 0.0350023671888812, 0.0468956207909694, 0.244428987748462, 0.400380188059781"                                                                                                    
##                           "0.986800278011844, 0.00217238204895588, 0.105436374876551, 0.0356593375199087, 0.468653751975093, 0.0102815882035531"                                                                                                   
##                           "0.734555977883115, 0.00166873791069222, 0.0338457396054972, 0.0423599609770942, 0.167241612084277, 0.0160234541447592"                                                                                                  
##                           "0.633730282011045, 0.00223607626147078, 0.0225360931842054, 0.0749641043348828, 0.268908049319316, 0.912148283364248"                                                                                                   
##                           "0.629823035803598, 0.00189998615714065, 0.0212164938385396, 0.0528824115321908, 0.225183759829112, 0.393515901189525"                                                                                                   
##                           "0.803915959732983, 0.000498923091634485, 0.150900466032631, 0.00385728745396544, 0.00129744534774474, 0.00269491883246194"                                                                                              
##                           "0.688210984651609, 0.00099636172063515, 0.0764001377232185, 0.0164317281278534, 0.00709606079206872, 0.906258137427627"                                                                                                 
##                           "0.661148915761879, 0.000733782976616424, 0.0834797734574257, 0.00827959499048319, 0.004308801052782, 0.178215634300087"                                                                                                 
##                           "0.235751616868109, 0.000666029984894267, 0.0363360687980085, 0.0353526702554151, 0.00778398018603895, 0.271031182898713"                                                                                                
##                           "0.0698034039246073, 0.000444505485553793, 0.00479213558000786, 0.00740922204914021, 0.00119551860838752, 0.0540047584840784"                                                                                            
##                           "0.199643585255464, 0.0008362408575191, 0.0145852819178575, 0.0285822407503273, 0.491585001040269, 0.282517459904963"                                                                                                    
##                           "0.000197496632975706, 0.00234002414455046, 0.870264387190888, 0.436683087991275, 0.299760802992206, 0.0221363339888501"                                                                                                 
##                           "0.000124967883736996, 0.000312122628256274, 0.375124040856901, 0.267519403137565, 0.182609461796563, 0.00767232304756881"                                                                                               
##                           "0.00794955037438336, 0.00135691076883324, 0.209470626304011, 0.366561409280354, 0.286317316007502, 0.784714610706192"                                                                                                   
##                           "0.00187965414702269, 0.00131890247457334, 0.25104809684531, 0.317609890091407, 0.246293375810521, 0.606374473590883"                                                                                                    
##                           "7.33613430656979e-06, 0.000433292573159135, 0.598431270057762, 0.100992650002879, 0.00691115484546211, 0.00188684016766803"                                                                                             
##                           "0.00109583302062278, 0.00270936272955636, 0.970507191699805, 0.201380962990343, 0.0222859005699747, 0.890190747706633"                                                                                                  
##                           "0.000232735854867646, 0.00273800379301871, 0.903521282908341, 0.15448394750406, 0.0173462588287594, 0.47555471391463"                                                                                                   
##                           "0.0259139352973047, 0.000391756435406602, 0.44532767260246, 0.148327072997321, 0.00287429483216773, 0.230118668622052"                                                                                                  
##                           "0.0163724181544514, 0.000441850254142337, 0.179464574882325, 0.0856500365091737, 0.00113482687527499, 0.089442308918851"                                                                                                
##                           "0.0163230130297533, 0.00127982441704756, 0.320131370978404, 0.140250696803845, 0.546109451389789, 0.502539644547975"                                                                                                    
##                           "5.31019571533586e-06, 0.000283107366217087, 0.851109083286428, 0.0561250785424473, 0.00575218083213432, 0.000973895659702036"                                                                                           
##                           "0.00085645806070649, 0.00227561372528994, 0.729273474786871, 0.141677850414831, 0.0201288392169298, 0.854464918471968"                                                                                                  
##                           "0.000208163455061386, 0.0025069609401581, 0.868975001294859, 0.0983543282015671, 0.0145093774065658, 0.389570269872039"                                                                                                 
##                           "0.0148242429247383, 0.00029199278553987, 0.260394611129795, 0.0879797108968888, 0.00176147902881121, 0.225844919250794"                                                                                                 
##                           "0.017569326317024, 0.000424658977495322, 0.0920865418598115, 0.0882626428791998, 0.00128674617359152, 0.119152982519164"                                                                                                
##                           "0.0106565306996196, 0.00109573440475125, 0.183949408863401, 0.0936225779020216, 0.470759914428713, 0.380087287419976"                                                                                                   
##                           "0.00145502512878435, 0.000399781986132086, 0.831225735951498, 0.00694392245238021, 0.000952696037712755, 0.204060875568795"                                                                                             
##                           "0.00178664533892484, 0.00053805476751445, 0.692073684803761, 0.00745870586524465, 0.000821998150631549, 0.0987864641896895"                                                                                             
##                           "0.0016171065424476, 0.00169732277049255, 0.91868893397871, 0.0134049685562325, 0.442757711748538, 0.33832538336872"                                                                                                     
##                           "0.242883231974144, 0.000236411676447489, 0.115381681869592, 0.000670030400297124, 0.304378261772142, 0.164426452259647"                                                                                                 
##                           "0.124946650743984, 0.00448087638842411, 0.276938018149933, 0.605629756814177, 0.418069928643085, 0.430179639221208"                                                                                                     
##                           "0.144559398951664, 0.000753494252693375, 0.563805494775429, 0.0938834256326433, 0.416021734674996, 0.00506660784220055"                                                                                                 
##                           "0.221235894408976, 0.000339973850373326, 0.190494822863898, 0.179083128612646, 0.181489699977846, 0.00269652212001485"                                                                                                  
##                           "0.192407827586187, 0.00276759363286513, 0.270033027754216, 0.503513760266179, 0.242880480677449, 0.85175987001131"                                                                                                      
##                           "0.208080991732284, 0.00234716389113258, 0.250907871354623, 0.357575547684946, 0.204543330449849, 0.399444898737696"                                                                                                     
##                           "0.136468653228309, 0.00134993306983289, 0.419134070078656, 0.137530822771868, 0.417675917953117, 0.00498891128651481"                                                                                                   
##                           "0.165128225966858, 0.000546062791137534, 0.0848189172479522, 0.258181299272041, 0.147146296202696, 0.00194232283483198"                                                                                                 
##                           "0.162371926509981, 0.00510674628827864, 0.141470362468467, 0.637318502790261, 0.237793486581718, 0.799248631286558"                                                                                                     
##                           "0.16059625431329, 0.00440619563775335, 0.11991898721774, 0.460203304368356, 0.183261916539434, 0.341398359050171"                                                                                                       
##                           "0.218914682445216, 5.9711771905506e-05, 0.20780291407966, 0.00880191276970612, 0.000607258711026347, 0.00042554401344519"                                                                                               
##                           "0.205242194570852, 0.000770325540617047, 0.314092011676568, 0.0798016241080236, 0.00349856779594941, 0.954209324590944"                                                                                                 
##                           "0.244327547269201, 0.000534885874602024, 0.288327877242389, 0.0359599012534311, 0.00193116879358793, 0.151088395615626"                                                                                                 
##                           "0.670588405547532, 0.000304906263697472, 0.0642333189477842, 0.21221498047792, 0.000978159930947893, 0.217779364198169"                                                                                                 
##                           "0.705439053515636, 0.000316928355533575, 0.050713774286851, 0.17771209297603, 0.000801274774680296, 0.14240968171211"                                                                                                   
##                           "0.467399323628054, 0.0021996537422956, 0.080914111143756, 0.338150354489935, 0.422484380475856, 0.236423257775356"                                                                                                      
##                           "7.61796926140284e-05, 0.0016473582831026, 0.575463738707227, 0.558723198425942, 0.3229392189953, 0.0140124266629043"                                                                                                    
##                           "3.05453018408071e-05, 0.00031054650835461, 0.238545321526826, 0.410008314485985, 0.22094132031106, 0.00256633208186684"                                                                                                 
##                           "0.00333374363977658, 0.00227806195832196, 0.293189639057493, 0.442639125188968, 0.366282377669982, 0.830926765178931"                                                                                                   
##                           "0.000619814223928324, 0.00215007140296616, 0.280140457049195, 0.417713509267505, 0.297482266745755, 0.450674717768162"                                                                                                  
##                           "3.57529922243177e-06, 0.000296123085313094, 0.469137176039647, 0.244182918169567, 0.00848822975706374, 0.00302655009011543"                                                                                             
##                           "0.000391942055415696, 0.00173624050930747, 0.533975831118205, 0.332399056769419, 0.0156778207095441, 0.901096952759352"                                                                                                 
##                           "6.57823374977777e-05, 0.0015841555287677, 0.523719199571627, 0.288490456620707, 0.0136843129820385, 0.475225051690859"                                                                                                  
##                           "0.0120596051282357, 0.000545462531108104, 0.246215166782523, 0.289677551573736, 0.0021364471205925, 0.238299614372315"                                                                                                  
##                           "0.00264110067629021, 0.000341429035379195, 0.27501737089899, 0.148073771614683, 0.00107457671867221, 0.120025727497869"                                                                                                 
##                           "0.00580596982333418, 0.00193021960502474, 0.28051457003496, 0.249790754850852, 0.529239617968828, 0.381425650005615"                                                                                                    
##                           "2.27763324744128e-06, 0.000222221897683055, 0.362804166682863, 0.0939100636940058, 0.0059896646390273, 0.00120527498353815"                                                                                             
##                           "0.000339131300886249, 0.00212513346456754, 0.415680163603731, 0.206433946942763, 0.0122062327072637, 0.908083806614279"                                                                                                 
##                           "6.48109095399839e-05, 0.00193309447520571, 0.401580990159179, 0.149257149581923, 0.0101290625669662, 0.352500463987465"                                                                                                 
##                           "0.00658314569530154, 0.000596840748770703, 0.145015004368091, 0.158404229405112, 0.001508387899959, 0.246965469103731"                                                                                                  
##                           "0.00279556904257919, 0.000440304600687341, 0.121384831732257, 0.145293403725643, 0.00134694720359149, 0.184489762288486"                                                                                                
##                           "0.00395449590208828, 0.00241721769248355, 0.157884877254827, 0.159312599413314, 0.469591522783737, 0.261889958696589"                                                                                                   
##                           "0.00068925634951845, 0.000414687346967892, 0.255361291407333, 0.0080186579701814, 0.00136729880804261, 0.229381616635721"                                                                                               
##                           "0.000198598661323203, 0.000276049500078089, 0.239326334019631, 0.00416272364465612, 0.000712385249025726, 0.0984885759278977"                                                                                           
##                           "0.000521652712036605, 0.00172311316038321, 0.271669110754588, 0.0113486941102465, 0.491362175033545, 0.324256570740682"                                                                                                 
##                           "0.0467373606295143, 0.000447031974543512, 0.0813710631165333, 0.00104539235835513, 0.290639967487033, 0.262618933423052"                                                                                                
##                           "0.132878251485221, 0.000846288876226508, 0.137849182118627, 0.415482245532787, 0.556897055487167, 0.0039975954684819"                                                                                                   
##                           "0.153905708638166, 0.000391021684426994, 0.311144990696876, 0.170503196481638, 0.313848043002383, 0.00279914912869144"                                                                                                  
##                           "0.162776631730756, 0.00283817736967691, 0.674454094604361, 0.217709000846403, 0.418429144243492, 0.856177442730772"                                                                                                     
##                           "0.162526445415619, 0.00242297675034653, 0.509471772182999, 0.191945902960564, 0.365410834994069, 0.374511560455399"                                                                                                     
##                           "0.165795517158065, 5.62561299838689e-05, 0.0118682956299506, 0.186622394463303, 0.000624927551653314, 0.00065676036936019"                                                                                              
##                           "0.15819660374045, 0.000479379506722196, 0.0896798414925319, 0.248979131342135, 0.00335503524288785, 0.840639913283817"                                                                                                  
##                           "0.189471920386069, 0.000324741632151277, 0.0433024408588178, 0.22704729476844, 0.0019916026288849, 0.144233425454488"                                                                                                   
##                           "0.472004957450067, 0.000221198315870933, 0.271626720391282, 0.104285350945299, 0.00122910459612307, 0.274002384951093"                                                                                                  
##                           "0.43739540975729, 0.000190279738767748, 0.260897045506687, 0.0497668408972589, 0.000560162861640674, 0.12088974355119"                                                                                                  
##                           "0.336305210555975, 0.00130010068141865, 0.40645673583244, 0.0956387489732103, 0.529403663191765, 0.288111426200942"                                                                                                     
##                           "0.14057383877103, 6.6284710920151e-05, 0.0230676763934818, 0.207307122334642, 0.000458925709296291, 0.000356750244713146"                                                                                               
##                           "0.164341833162023, 0.000882994558440125, 0.137908899796928, 0.315055520306441, 0.00259004943462749, 0.937896965887842"                                                                                                  
##                           "0.172495594330311, 0.000620522982215133, 0.0695726344824588, 0.258805148843859, 0.00144735560394387, 0.130999092584311"                                                                                                 
##                           "0.436516853347311, 0.000349931896208275, 0.391502935392214, 0.124413872521585, 0.000939722294587721, 0.236386855430664"                                                                                                 
##                           "0.457248824325091, 0.000385428721857938, 0.350297750848369, 0.115169975096084, 0.000906406122138749, 0.175178324995875"                                                                                                 
##                           "0.309696536488801, 0.00231424160528112, 0.548840525872693, 0.121467744539454, 0.41487864384304, 0.200491272152763"                                                                                                      
##                           "0.526817577200732, 3.06144795928795e-05, 0.0121813785251132, 0.000360154429484579, 0.00019018188297341, 0.233435050924459"                                                                                              
##                           "0.609527309033395, 3.44878888451381e-05, 0.00987298326157856, 0.000192111758783694, 0.000115539592769056, 0.0993828618237795"                                                                                           
##                           "0.443554784928039, 0.000254356102057315, 0.0344513818748273, 0.00094066880829191, 0.466712878669609, 0.097206972789696"                                                                                                 
##                           "0.770278366725755, 0.000160638846857259, 0.27059015645425, 0.000457882073080157, 0.283994780409591, 0.257537048634453"                                                                                                  
##                           "3.29107675630357e-06, 0.000164848194715259, 0.385216790947245, 0.170776730478782, 0.00447347831225845, 0.00133856874647288"                                                                                             
##                           "0.000372421263355734, 0.00127314065239157, 0.417670207547178, 0.309500128149122, 0.00950569379767138, 0.975119880599178"                                                                                                
##                           "7.06868677769955e-05, 0.00114181879445745, 0.404152463890018, 0.247172535888378, 0.00798439313948008, 0.357210746431687"                                                                                                
##                           "0.010120328139286, 0.000337111924638658, 0.2998242691115, 0.273567477346815, 0.00146376124738873, 0.272188094766325"                                                                                                    
##                           "0.00338227766716193, 0.000235003489330932, 0.158604667833594, 0.352240399991992, 0.00088238133376533, 0.147811926612563"                                                                                                
##                           "0.00565041423525148, 0.00104784961420631, 0.268035509314921, 0.297803238030579, 0.528772310042555, 0.309974360383979"                                                                                                   
##                           "0.000815239320654023, 0.000293646826213388, 0.230419227455074, 0.0067048238297974, 0.00147019355396713, 0.28090490207472"                                                                                               
##                           "0.000137077876109991, 0.00021827912155359, 0.120306897245841, 0.00394170294600429, 0.000567446941935501, 0.0820199141808074"                                                                                            
##                           "0.000526521611261153, 0.000999486493516143, 0.208872828307831, 0.010225902394476, 0.60141962791087, 0.357998389225627"                                                                                                  
##                           "0.0353172266905422, 0.000253413899729562, 0.0816606259922009, 0.000718207584017795, 0.406485555469345, 0.201762904625979"                                                                                               
##                           "0.000407925601530968, 0.00026893505001056, 0.103004847025043, 0.00366789689086581, 0.000781808134374323, 0.278633953318124"                                                                                             
##                           "0.000212093300612638, 0.000225896090125158, 0.114238049102824, 0.00209197802975417, 0.000618236571594816, 0.139265199519257"                                                                                            
##                           "0.000402411462212284, 0.00115306722985088, 0.116930607077511, 0.00573352289004415, 0.518853622129195, 0.230556579238817"                                                                                                
##                           "0.0448595880941053, 0.000277001007604118, 0.114198864124672, 0.00079973734145542, 0.329579486858412, 0.327182106580053"                                                                                                 
##                           "0.00278038533647553, 0.00019173898695719, 0.0022978013717656, 0.000442604841432729, 0.351127138062332, 0.169235550133888"                                                                                               
##                           "3.64056967654636e-09, 0.252118851881085, 0.0824153074601348, 0.0224918759783496, 0.434475433918796, 0.198339878628468"                                                                                                  
##                           "1.05478457003122e-10, 0.0579953543326401, 0.132843504746441, 0.0892143563147475, 0.212839007948407, 0.0203157835586197"                                                                                                 
##                           "1.46737115696313e-09, 0.1384158321036, 0.0579008960276527, 0.028544406588865, 0.153949548398692, 0.323310975154163"                                                                                                     
##                           "1.65562987440926e-09, 0.203540042269409, 0.0639502549736726, 0.0439546212089642, 0.174180129028611, 0.995842661231347"                                                                                                  
##                           "1.4555618009189e-09, 0.186878455059569, 0.0607432843889789, 0.0508613003144278, 0.163353016670656, 0.710715389063074"                                                                                                   
##                           "3.45206776794568e-10, 0.097447065152903, 0.0817140884407627, 0.066404739061425, 0.11735230648865, 0.02213152448935"                                                                                                     
##                           "1.85112311348265e-09, 0.216632639343899, 0.0269737262313298, 0.0204616300653201, 0.0678946967918177, 0.254707091366119"                                                                                                 
##                           "2.17244455938293e-09, 0.321369051455931, 0.0314518440568588, 0.0324821711654424, 0.0859575350771551, 0.983101623399173"                                                                                                 
##                           "1.86687072278178e-09, 0.292062754590854, 0.0282041775861034, 0.0420104535769938, 0.0715270358155368, 0.547297082371616"                                                                                                 
##                           "7.56737015942531e-12, 0.0476067779656643, 0.0206071626898796, 0.10780602479636, 0.0110197789169104, 0.148780601062525"                                                                                                  
##                           "8.60615510712058e-12, 0.0900279455159887, 0.024411801586747, 0.147378861756852, 0.0168833807248994, 0.907444068022249"                                                                                                  
##                           "7.46855099262367e-12, 0.071931933858502, 0.0212857683811209, 0.202210667734345, 0.0131777364102796, 0.442585951810339"                                                                                                  
##                           "2.54613667086278e-11, 0.198582556705891, 0.00716008889658637, 0.0421466311172422, 0.334206596372866, 0.696033798024642"                                                                                                 
##                           "2.92194095353645e-11, 0.220296342088529, 0.00631518636196405, 0.0522122152418535, 0.429890349666974, 0.938507578597287"                                                                                                 
##                           "2.62102185447677e-11, 0.250332266333435, 0.00767813522354371, 0.0612042185475807, 0.680131302152697, 0.620816219273633"                                                                                                 
##                           "3.26164742680152e-10, 0.133502696004615, 0.0816330407779723, 0.482454713048074, 0.353970293044399, 0.00817461660830369"                                                                                                 
##                           "5.40605168152365e-10, 0.380538634398353, 0.0191365405411975, 0.505864127762795, 0.300084094630092, 0.398372445188909"                                                                                                   
##                           "1.22995482850246e-09, 0.552178259563206, 0.0231664388680431, 0.44734216649225, 0.295441417662246, 0.394415045836267"                                                                                                    
##                           "5.10003551596329e-10, 0.397881359901756, 0.0209672760360852, 0.516387188880321, 0.247588297923551, 0.228461132835997"                                                                                                   
##                           "3.51494943982823e-11, 0.0552382728384023, 0.0624287064094063, 0.253694765154217, 0.00487396641385832, 0.154910695963311"                                                                                                
##                           "6.96409485577616e-11, 0.109829391260396, 0.0726868694761079, 0.22428214684224, 0.00693182315632937, 0.361995556141748"                                                                                                  
##                           "3.43703954182265e-11, 0.0661662583076562, 0.0677996874556845, 0.245732120024484, 0.00545459018290238, 0.178786710877073"                                                                                                
##                           "2.35317550893274e-10, 0.366374108472914, 0.0184626247993491, 0.20441933629759, 0.522689249604549, 0.517784967261044"                                                                                                    
##                           "7.72071097429651e-11, 0.313919389638083, 0.0177428357136788, 0.222886468297048, 0.767964254371376, 0.454803179406757"                                                                                                   
##                           "2.38257991342639e-10, 0.367612451010762, 0.0191901094555003, 0.215180055945926, 0.785435167634925, 0.470817165651033"                                                                                                   
##                           "1.42507327147771e-10, 0.0852601461034337, 0.0283464620467644, 0.179574662878538, 0.00515867342729919, 0.138698908450728"                                                                                                
##                           "2.16647140607725e-10, 0.179859243773973, 0.0311753921264841, 0.176111214369784, 0.00750079563770308, 0.377937976519558"                                                                                                 
##                           "1.5724474625442e-10, 0.101509232777626, 0.0332522876246096, 0.135946139114624, 0.00551658900211846, 0.119138497845742"                                                                                                  
##                           "4.21011580678778e-10, 0.496900907693866, 0.00559440145468298, 0.131311278020568, 0.456253233562249, 0.523043438453523"                                                                                                  
##                           "2.4218578348128e-10, 0.424547461904257, 0.00601853757813142, 0.117731555497812, 0.762439139261757, 0.342061605691725"                                                                                                   
##                           "3.88666857535475e-10, 0.487351808612972, 0.00625791389349926, 0.117194367181354, 0.870736274836651, 0.329075277163315"                                                                                                  
##                           "3.00273409633691e-12, 0.0868556337556148, 0.00947189170855047, 0.00438292879247946, 0.210237735293314, 0.62004134659366"                                                                                                
##                           "2.24141522359007e-12, 0.0705893986645522, 0.00966063259170957, 0.00414823799956117, 0.421568717647966, 0.412153032837139"                                                                                               
##                           "2.95412532957295e-12, 0.0978496384925268, 0.0105833387006604, 0.00502997624933244, 0.962232372782148, 0.285110426776452"                                                                                                
##                           "4.69253185256293e-14, 0.421996873681922, 0.00171461206669669, 0.78518691081818, 0.946378613566862, 0.578451124238824"                                                                                                   
##                           "3.0972175369443e-10, 0.0712998543089022, 0.0317697301768729, 0.203275089623858, 0.176804140840035, 0.0127672481101592"                                                                                                  
##                           "3.45141901140975e-09, 0.188335388192327, 0.00535173627573991, 0.15180086358839, 0.133746769196093, 0.298386578050922"                                                                                                   
##                           "4.38814726994677e-09, 0.272340970229853, 0.00987884409454532, 0.161702686487113, 0.152800856738115, 0.974743513825077"                                                                                                  
##                           "3.46270865889813e-09, 0.250019813336312, 0.0112636134571297, 0.158202999356045, 0.139222318727523, 0.646715624763293"                                                                                                   
##                           "9.42800506197602e-11, 0.0273949030544562, 0.0428353091581892, 0.0401442101448709, 0.00748630070159663, 0.135115600908907"                                                                                               
##                           "1.37609311921147e-10, 0.054198739297659, 0.0726339812404337, 0.0461368311591625, 0.0109217665617172, 0.746522481903832"                                                                                                 
##                           "9.27215574961768e-11, 0.0422840543169424, 0.0901001667223575, 0.0438213551318262, 0.00929942463587231, 0.466729754469156"                                                                                               
##                           "2.48775702387717e-09, 0.144534747333558, 0.0118158560742773, 0.0285852542044836, 0.367817429477297, 0.828283913733729"                                                                                                  
##                           "2.28786486131178e-09, 0.144450647794992, 0.0117382705963541, 0.0238470538665379, 0.406310261637841, 0.875725710650597"                                                                                                  
##                           "2.76270913284641e-09, 0.188060205000505, 0.0169947496847092, 0.0311486556477348, 0.851286190108048, 0.764462098543841"                                                                                                  
##                           "3.37036654319441e-10, 0.0586433865580917, 0.023494543509909, 0.0292194686122807, 0.00659292684779419, 0.110801970178036"                                                                                                
##                           "4.7340154876656e-10, 0.11598116684054, 0.0389376258159077, 0.0385823300803918, 0.0102966504051493, 0.854902021191369"                                                                                                   
##                           "3.85283547542516e-10, 0.088602302001529, 0.0585942696356728, 0.0295255236025356, 0.00761248065304355, 0.34140387768706"                                                                                                 
##                           "3.17334862251039e-09, 0.278048097212723, 0.00549455886521562, 0.0214396944180079, 0.290144978668303, 0.735226129914842"                                                                                                 
##                           "2.9716132837401e-09, 0.305691041823327, 0.00735996957915068, 0.0188322563023094, 0.416045640432503, 0.959007345252903"                                                                                                  
##                           "3.25720917303084e-09, 0.340704910411759, 0.00912854220573444, 0.0224483002368967, 0.677838214713472, 0.52873079549992"                                                                                                  
##                           "1.42731816663179e-11, 0.0560311748981435, 0.051003256475737, 0.00517652894148143, 0.185338580948896, 0.783093365106849"                                                                                                 
##                           "1.39823715727305e-11, 0.0584539947758576, 0.0670519030525228, 0.00455930476080116, 0.290217127772254, 0.935037963171621"                                                                                                
##                           "1.48774952387098e-11, 0.0772107021385764, 0.0820933381812445, 0.00617447890268576, 0.715786240459278, 0.480989342916462"                                                                                                
##                           "4.71237899717232e-11, 0.263638309446354, 0.011175339597843, 0.51771929184494, 0.587065394353957, 0.954333028050959"                                                                                                     
##                           "4.23363397547038e-10, 0.0687659411460713, 0.153617259029832, 0.246787332991969, 0.00158943827516377, 0.146542407570818"                                                                                                 
##                           "5.80904685020494e-10, 0.140413537131188, 0.133470981617208, 0.247268728832974, 0.00233135466697666, 0.278279572456703"                                                                                                  
##                           "3.88276339216094e-10, 0.0798663193542409, 0.167664490732001, 0.190922637993313, 0.00168181468546131, 0.112999847466369"                                                                                                 
##                           "5.2339499645001e-09, 0.544361147469831, 0.108824319255214, 0.238270245176743, 0.595630804961871, 0.389129340517347"                                                                                                     
##                           "2.84139022884058e-09, 0.46659408047544, 0.132899810659987, 0.211965983717268, 0.905315202519452, 0.319695099943558"                                                                                                     
##                           "4.63764954628628e-09, 0.520744248775432, 0.124218378694769, 0.212156487422333, 0.693943042973051, 0.404214781314678"                                                                                                    
##                           "1.33918851546565e-10, 0.0622377248761972, 0.0349962317457875, 0.00163362604108736, 0.221874798151219, 0.413128580682339"                                                                                                
##                           "8.32356839733154e-11, 0.0487155222166677, 0.0400834706683246, 0.00157027419993703, 0.44270218096474, 0.362899502162442"                                                                                                 
##                           "1.3051808991437e-10, 0.0709965534943214, 0.0405109228515922, 0.00190772070644391, 0.720116451165509, 0.319060390373747"                                                                                                 
##                           "2.49950657011256e-09, 0.430992121019639, 0.0241756795950633, 0.881342265260036, 0.683924015020675, 0.614710798378151"                                                                                                   
##                           "6.33359314423328e-10, 0.129370574812383, 0.0518253393484792, 0.00128648998945943, 0.203266291966918, 0.480659551247909"                                                                                                 
##                           "5.24623543922832e-10, 0.0993088467092181, 0.0430002089233788, 0.00118577933546243, 0.483518576935377, 0.241590324967082"                                                                                                
##                           "6.1557244989855e-10, 0.134152619097744, 0.0440355692067236, 0.00138397779464679, 0.907478690824736, 0.180230595228867"                                                                                                  
##                           "3.5564843319503e-09, 0.669404642240428, 0.0340229750400246, 0.930917132416042, 0.875808992707432, 0.405647128170831"                                                                                                    
##                           "1.38904102251991e-11, 0.0989477805035391, 0.00102402573712642, 0.523066015493806, 0.97255295272037, 0.488878480394439"                                                                                                  
##                           "4.1536545663514e-10, 0.147594284331926, 0.115321146770535, 0.717417842916983, 0.148631194244628, 0.076785846778298"                                                                                                     
##                           "1.89150068381417e-09, 0.0802526064251334, 0.0358435452364541, 0.613734110278682, 0.135582962625759, 0.525325615260471"                                                                                                  
##                           "1.90201566552386e-09, 0.0826050364090308, 0.0541012719595917, 0.560389660237409, 0.149477716341362, 0.754120071387052"                                                                                                  
##                           "1.59899474241207e-09, 0.0795019966126712, 0.058479513110654, 0.603538475890922, 0.137362832113409, 0.628308726060542"                                                                                                   
##                           "9.90968160847798e-11, 0.0979662986335388, 0.18045763141322, 0.385593979386005, 0.0771256816873601, 0.647425946231664"                                                                                                   
##                           "1.15815623395085e-10, 0.100930725183437, 0.269069473338344, 0.334940575823659, 0.0677355706216213, 0.546451247325267"                                                                                                   
##                           "8.42919965890586e-11, 0.0965802977445631, 0.25915601127326, 0.380132673694576, 0.0721057197456858, 0.627535611543905"                                                                                                   
##                           "8.63825060458713e-10, 0.0592269231064599, 0.0831600323719186, 0.2513911182908, 0.719831096217985, 0.831061839782205"                                                                                                    
##                           "8.19218473142953e-10, 0.0597164329204971, 0.0810982040982364, 0.258140518159387, 0.769600965250546, 0.951289491737959"                                                                                                  
##                           "8.15186621902474e-10, 0.0603887786216229, 0.0958930795938496, 0.252045651393838, 0.867842572230414, 0.883199777067221"                                                                                                  
##                           "2.70797571857101e-10, 0.089295386910509, 0.109127118921399, 0.0836659630014778, 0.0667158971155533, 0.461230784306755"                                                                                                  
##                           "2.34777021244321e-10, 0.0847011325072759, 0.170158801341948, 0.0884327463109677, 0.059731696570591, 0.567664684404432"                                                                                                  
##                           "2.2230081578512e-10, 0.0866534363385296, 0.184519424613282, 0.0800569713690095, 0.0590807044370124, 0.45109641192603"                                                                                                   
##                           "8.12253651369301e-10, 0.0362503010972859, 0.0472003068872427, 0.0681686388171778, 0.512952575703755, 0.88858554541543"                                                                                                  
##                           "8.09297657067548e-10, 0.0358057333587641, 0.0532917349581918, 0.0668024233933518, 0.661025264725575, 0.818230964678371"                                                                                                 
##                           "6.38414020798207e-10, 0.0363970948330741, 0.0628484509922621, 0.069910144600831, 0.992724777789681, 0.643388841141578"                                                                                                  
##                           "6.5618908818551e-12, 0.0234994920073259, 0.26258603318155, 0.0516606478092986, 0.666313992721509, 0.716600157996611"                                                                                                    
##                           "6.38537508734198e-12, 0.0241345007813618, 0.265941183833167, 0.053263039968864, 0.811909950476625, 0.773757840135449"                                                                                                   
##                           "5.18089215269393e-12, 0.0236315808858313, 0.307478420565497, 0.0506240116204626, 0.822156633124459, 0.732482047070099"                                                                                                  
##                           "8.0458203769614e-12, 0.00901646370137827, 0.0920672537878028, 0.75485345621255, 0.956258877531477, 0.963116491004295"                                                                                                   
##                           "2.24195374528022e-10, 0.0713968617261551, 0.722247761831968, 0.213483703458463, 0.0226484619576973, 0.531725795310617"                                                                                                  
##                           "2.26634746430485e-10, 0.0832459798741132, 0.589344237122292, 0.210706920943308, 0.0197742309440438, 0.270025540530186"                                                                                                  
##                           "2.05968536436853e-10, 0.0766017603555848, 0.734975809683092, 0.170458755822902, 0.0209484336087618, 0.232922186232876"                                                                                                  
##                           "1.47554566052249e-10, 0.0238480549094611, 0.52282185202721, 0.235259529887826, 0.664078950975467, 0.380403935755715"                                                                                                    
##                           "1.00997714884264e-10, 0.0233647239559109, 0.613726598670536, 0.209040496403196, 0.996882189038245, 0.353916724875402"                                                                                                   
##                           "1.34679611348983e-10, 0.0242400849195846, 0.56058885220053, 0.211774153828152, 0.645086757578658, 0.471326273664517"                                                                                                    
##                           "5.66366753145409e-11, 0.0615829305436492, 0.339664289795193, 0.0228055941544554, 0.733070947342461, 0.33207853789053"                                                                                                   
##                           "3.50313725948705e-11, 0.057760647106749, 0.408584911458049, 0.0254391026197298, 0.984061267698259, 0.429713181140519"                                                                                                   
##                           "5.23766710877303e-11, 0.0619983475337299, 0.351211656244847, 0.0231377021319597, 0.521012883288432, 0.618694666938968"                                                                                                  
##                           "3.98403095439364e-11, 0.0214852976356443, 0.256065154266048, 0.987242290527031, 0.627167511825448, 0.688190174661736"                                                                                                   
##                           "9.40469151496218e-11, 0.0357047140118857, 0.122275111468875, 0.0171963567825805, 0.56261324283411, 0.327334679619738"                                                                                                   
##                           "1.01590548225622e-10, 0.0399235717665317, 0.114517060354461, 0.0190761137732332, 0.947215419768802, 0.305448965966732"                                                                                                  
##                           "9.19532462061087e-11, 0.0389685888717569, 0.109951586836256, 0.0175790008339201, 0.600038805016937, 0.404413578787507"                                                                                                  
##                           "2.41444907829349e-11, 0.00739405678098532, 0.109024736992841, 0.933885536371169, 0.722440333116495, 0.514635651662115"                                                                                                  
##                           "1.44844855236641e-12, 0.0120203097979757, 0.0177255778799413, 0.928608513176317, 0.646997532632263, 0.661331399686747"                                                                                                  
##                           "3.13357755944893e-10, 0.0567278806875864, 0.396307076203637, 0.0939477872163035, 0.044493290305388, 0.512529439422228"                                                                                                  
##                           "3.10387220180916e-10, 0.100559582242808, 0.33054798430159, 0.103262645259663, 0.0394159863464958, 0.521484229137423"                                                                                                    
##                           "2.67243733469136e-10, 0.099853555911104, 0.388605178832827, 0.090841381083382, 0.0400725173274825, 0.501545856555793"                                                                                                   
##                           "2.12975304851055e-09, 0.0159139411163482, 0.242842136423541, 0.10246118853684, 0.603572571297829, 0.847524078597774"                                                                                                    
##                           "1.93762281394715e-09, 0.0163878905915019, 0.247640545097782, 0.101868809052137, 0.697788152654907, 0.892618106053388"                                                                                                   
##                           "1.81039995253061e-09, 0.0202957451333092, 0.244597924867905, 0.10714310819161, 0.920961659170332, 0.773123760622954"                                                                                                    
##                           "1.36780441834789e-10, 0.154821261136165, 0.0915780638877121, 0.0429943580063933, 0.761395668691386, 0.582352740280569"                                                                                                  
##                           "1.06423538046047e-10, 0.140936266327449, 0.103875729201871, 0.0483028159467922, 0.8577163870169, 0.813459797120613"                                                                                                     
##                           "1.22222111588624e-10, 0.175574451190186, 0.0924058722190169, 0.0430071601223043, 0.647569388268939, 0.86915744488675"                                                                                                   
##                           "1.62997278685414e-09, 0.0312123730992815, 0.0483976757844283, 0.774698129313552, 0.838789917179191, 0.90758626474181"                                                                                                   
##                           "3.39631602616104e-10, 0.0680517004378791, 0.0263700168025652, 0.0285275178739953, 0.503461778909804, 0.700237863761926"                                                                                                 
##                           "3.29447760284918e-10, 0.0774420560037859, 0.0258895719782492, 0.0283997713621174, 0.714309969168221, 0.670469210905074"                                                                                                 
##                           "2.89355712980106e-10, 0.0934513303814416, 0.0266120778775681, 0.0275172548349723, 0.865969150359549, 0.574746427882365"                                                                                                 
##                           "1.34669292637927e-09, 0.0120892413465611, 0.0201888030374495, 0.670220283736457, 0.902834381629086, 0.863864948290168"                                                                                                  
##                           "1.28980615456111e-11, 0.129865066701032, 0.0224075607374086, 0.819206493027687, 0.917647188699905, 0.914924746940375"                                                                                                   
##                           "3.98387673826204e-10, 0.233123501335832, 0.148350443365757, 0.006178250713491, 0.651166315005863, 0.25197433954198"                                                                                                     
##                           "3.71342995820875e-10, 0.307593029919017, 0.128977021474001, 0.00693725499078057, 0.968109859403887, 0.279015318026179"                                                                                                  
##                           "3.52835589702868e-10, 0.260154161453444, 0.132262455064995, 0.00623122470575826, 0.486710252301582, 0.454009990700766"                                                                                                  
##                           "7.86744132458835e-10, 0.145737614285826, 0.185416667022074, 0.96204151586289, 0.595177156690999, 0.539352286433"                                                                                                        
##                           "1.36876270957272e-10, 0.0729084322566678, 0.00872156268635331, 0.999792141916999, 0.46565046304194, 0.699042614287332"                                                                                                  
##                           "3.63883415275214e-10, 0.0370150262460745, 0.00374878274399472, 0.937894374923172, 0.630574277603591, 0.454640727826836"                                                                                                 
##                           "0.917062357123256, 0.00540729573169945, 0.0614519385947627, 0.50401395027864, 0.103084065932165, 0.678963060464177, 0.474659834961389"                                                                                  
##                           "0.984933691202602, 0.00312555043074262, 0.1602679175758, 0.740114375549079, 0.0354711777133615, 0.635387033466162, 0.0124122744009549"                                                                                  
##                           "0.82576345835242, 0.0026618593342824, 0.086254279621876, 0.350437223473601, 0.0380550755773515, 0.342155956203822, 0.0120814932991493"                                                                                  
##                           "0.734833159903534, 0.00396163637711481, 0.0573844879432777, 0.496771169180023, 0.0790080142877118, 0.455787406347696, 0.984211791906311"                                                                                
##                           "0.710898718603749, 0.00320249486858774, 0.0552251778045826, 0.472142941535654, 0.0551018678993001, 0.41047051631044, 0.37952567614506"                                                                                  
##                           "0.899568620679227, 0.00391272175730646, 0.14292769358893, 0.67321576362768, 0.0442793976217296, 0.522440740788085, 0.0134991563610288"                                                                                  
##                           "0.994350758988161, 0.0040485924585188, 0.0839392616593264, 0.23039910090941, 0.0568428117668752, 0.233770862252826, 0.0103842954538746"                                                                                 
##                           "0.839141989706506, 0.00499879505563566, 0.0486220547424298, 0.392812803958446, 0.0959105188499236, 0.341267634501893, 0.964021312902608"                                                                                
##                           "0.841806518694933, 0.00427224766176113, 0.0485601537877036, 0.35485648832441, 0.0689523116486203, 0.294629304117744, 0.351975983046975"                                                                                 
##                           "0.917970312491801, 0.00140852113601742, 0.255053370464714, 0.360990593055804, 0.00668167278586814, 0.00209535026386654, 0.00197821647159124"                                                                            
##                           "0.87393953923989, 0.00246698097834871, 0.119161824989857, 0.577982596772122, 0.0242865476628313, 0.0102109041029696, 0.869789210907865"                                                                                 
##                           "0.858195478698357, 0.00181554951812411, 0.133065576713154, 0.532889752670835, 0.0128076082248125, 0.00640727170510073, 0.164762069099917"                                                                               
##                           "0.548347123734695, 0.00183600769543629, 0.0976739157743853, 0.178306013310693, 0.0533248402348312, 0.00501328220701527, 0.284807166564277"                                                                              
##                           "0.179240422350337, 0.000906141928780485, 0.0134356854312111, 0.234832390764491, 0.0102869630462288, 0.00195812606686944, 0.0798861142566992"                                                                            
##                           "0.44359163090332, 0.00252419853890602, 0.0407700913227861, 0.286715382912201, 0.0435453137780217, 0.535028239928022, 0.267028841871873"                                                                                 
##                           "0.000162835074697931, 0.00417282360997157, 0.922616122276638, 0.585756638036052, 0.574102790366417, 0.320858136622612, 0.0273485835065763"                                                                              
##                           "4.65780099338708e-05, 0.000529226866438439, 0.486110235900144, 0.293921512449404, 0.480930875760366, 0.19505182417667, 0.00425865932351647"                                                                             
##                           "0.0054153809725183, 0.00313573300069112, 0.286147809620052, 0.40415236434261, 0.554906216471301, 0.303416758805082, 0.807227197941143"                                                                                  
##                           "0.00122511362225109, 0.00322887273640786, 0.343831376242202, 0.380692383843626, 0.511023791321353, 0.259497563821739, 0.567704864524044"                                                                                
##                           "4.38496209891849e-06, 0.000729272333796149, 0.55448662876346, 0.437834092982726, 0.216426933022971, 0.00917313023800155, 0.00168513864385936"                                                                           
##                           "0.000882466025139904, 0.00507350186141587, 0.972270085896727, 0.533589724263536, 0.333499541015926, 0.0271456681359739, 0.903835691750412"                                                                              
##                           "0.000183282182516815, 0.0053373324465522, 0.842025422297555, 0.510368423703021, 0.279908813574462, 0.0212470543329898, 0.456076752308262"                                                                               
##                           "0.0154123325786837, 0.000752015360763438, 0.555784252701797, 0.289399087966648, 0.332128760727031, 0.00267853433738058, 0.235166804755536"                                                                              
##                           "0.010189850402326, 0.000839820050582825, 0.243469088983002, 0.383744048808534, 0.196818179236695, 0.00165299321071715, 0.101716121128239"                                                                               
##                           "0.010172071714996, 0.00310657268887119, 0.425321067861052, 0.361939313387726, 0.293981842231933, 0.551622987848464, 0.47615774455294"                                                                                   
##                           "2.85868636410985e-06, 0.000547427849030208, 0.742464572763071, 0.340507035376702, 0.0985241091065564, 0.00916682043907157, 0.00095519841974188"                                                                         
##                           "0.000639725921587928, 0.00497566474644585, 0.84321626115385, 0.444962819393775, 0.201969214587255, 0.0274936545130649, 0.901108840718195"                                                                               
##                           "0.000148106537271912, 0.00551156227239513, 0.993891769374093, 0.410144915914866, 0.149356112719251, 0.0205171476911133, 0.36399659745485"                                                                               
##                           "0.00689938672545439, 0.000691300282326339, 0.41435284441809, 0.214025735809857, 0.146578310052158, 0.00193695378892806, 0.250115527314868"                                                                              
##                           "0.00828553584676519, 0.000922387374307158, 0.176098430904393, 0.238467841002813, 0.14411298695378, 0.00192508943682597, 0.137940620786917"                                                                              
##                           "0.00565927449267203, 0.00328723848660918, 0.320346301565212, 0.263611531782725, 0.148765143145735, 0.510151953120617, 0.366381511873036"                                                                                
##                           "0.000763729487737092, 0.000890701691478347, 0.70457495714477, 0.235933337699162, 0.0111194245242205, 0.00119008677690508, 0.232662188872026"                                                                            
##                           "0.000993996133400041, 0.00112421211565785, 0.840612585563593, 0.259235832623358, 0.0116154863971332, 0.00130351168736306, 0.118460887518518"                                                                            
##                           "0.00101721253029524, 0.00484899665966123, 0.923446277004499, 0.270633048188161, 0.0200032099908864, 0.489207094825327, 0.323343472621268"                                                                               
##                           "0.0804694429807287, 0.000676239994812869, 0.224100353282918, 0.151104835147852, 0.00123875987096251, 0.333702192761846, 0.18502381373485"                                                                               
##                           "0.896863679627078, 0.00287034607357937, 0.145775869145813, 0.0454399597073017, 0.695785038341955, 0.604510771670539, 0.0120652132642516"                                                                                
##                           "0.928258060099184, 0.00217287373773314, 0.0621477276240626, 0.0541142171571545, 0.377040894999639, 0.384934106934095, 0.0130968056229467"                                                                               
##                           "0.807757070995339, 0.00316501009947461, 0.0409858110833978, 0.0929575338883594, 0.50768352815738, 0.468310804988715, 0.997895786876899"                                                                                 
##                           "0.803117967702993, 0.00259082771590776, 0.040174523355349, 0.0682733220512844, 0.467189029742593, 0.431339219646261, 0.363651842556636"                                                                                 
##                           "0.822526627301103, 0.000974222558640384, 0.273150232034209, 0.00914153217932023, 0.337112530683709, 0.00208951553674853, 0.00199551956855807"                                                                           
##                           "0.97990906489138, 0.00202211181163616, 0.135742773259758, 0.0298964331901905, 0.50353824881842, 0.0101724401112779, 0.807105273254815"                                                                                  
##                           "0.964106038187764, 0.00140501087509433, 0.151425009491905, 0.0166747026265421, 0.457049647177797, 0.00653074021779966, 0.156001089277577"                                                                               
##                           "0.611252010862503, 0.00130460323401679, 0.0797014231876632, 0.0555614765107234, 0.2574503232425, 0.00726346197036223, 0.332247269452431"                                                                                
##                           "0.294063498583954, 0.000737678034554827, 0.016512231574559, 0.0166125247656618, 0.256519343763821, 0.00169343516883979, 0.0849740615835203"                                                                             
##                           "0.549041956496435, 0.00181536652917924, 0.0390618215458517, 0.0509921440830736, 0.296946701929251, 0.614993020500244, 0.294798048276445"                                                                                
##                           "0.74512714719526, 0.00140712935864045, 0.234060762509886, 0.0141719925190178, 0.318564344931114, 0.00201621361960827, 0.00197587469869855"                                                                              
##                           "0.977490841397423, 0.00219339070103547, 0.103156699521687, 0.0353425148282734, 0.459287478066852, 0.0101792811635607, 0.846793612376478"                                                                                
##                           "0.96066197759207, 0.00171846107220444, 0.121279328093556, 0.0210714186960129, 0.402459047833933, 0.00639699174835574, 0.151809720220147"                                                                                
##                           "0.711718937262096, 0.00164196118110475, 0.0640974971398898, 0.0762680262675248, 0.210929340022917, 0.00586087822285501, 0.301462305603211"                                                                              
##                           "0.275234480977135, 0.00081780416142889, 0.00943897323874166, 0.0174165362184659, 0.323078858737506, 0.00183213257775852, 0.0804875387516778"                                                                            
##                           "0.598888648313968, 0.00197374912456404, 0.0269842950243839, 0.0602417361456816, 0.246965586170168, 0.542524727383569, 0.249860809823338"                                                                                
##                           "0.771774930748312, 0.000556889556702633, 0.216197542382976, 0.00783038623264326, 0.00154817101326572, 0.001121415383526, 0.261025437292623"                                                                             
##                           "0.337581547101203, 0.000324316383960398, 0.0453225603199801, 0.00177506191115072, 0.00175644375569553, 0.000679239851072679, 0.0753848435691268"                                                                        
##                           "0.625463157227464, 0.000833293103040344, 0.0949897211672617, 0.00962632447502196, 0.00466539986499171, 0.539821719617183, 0.115997909355585"                                                                            
##                           "0.0996324022951381, 0.000549020751750635, 0.0172004951349091, 0.017369058692139, 0.00186932648742114, 0.721675096190505, 0.199921150805995"                                                                             
##                           "4.39084904350895e-06, 0.000367593534483367, 0.704545787996508, 0.351720246124615, 0.186744127034144, 0.00696557305580424, 0.000961185335253866"                                                                         
##                           "0.000737531740539702, 0.00311592033465311, 0.868074348503432, 0.449758225208705, 0.303473245525817, 0.023101951918587, 0.965542831765085"                                                                               
##                           "0.000166848909626046, 0.00324248681672428, 0.986225235245473, 0.411933604483681, 0.249458868724933, 0.0173528119471733, 0.365172434489254"                                                                              
##                           "0.0113083113720916, 0.000404168340534921, 0.366435211476569, 0.416985352632626, 0.233623355405645, 0.00219125856515581, 0.271837696173747"                                                                              
##                           "0.00932049858968629, 0.000519001893517062, 0.15893699031216, 0.275764197292242, 0.306119701241347, 0.00132099679520416, 0.11537975224257"                                                                               
##                           "0.00786008342021203, 0.00150155374035764, 0.269359877105548, 0.392840371446377, 0.25401299412649, 0.562987542087906, 0.417932913771345"                                                                                 
##                           "0.000898641287957236, 0.000644257646846544, 0.655122682191671, 0.205878077573525, 0.00904248616098353, 0.00115429514435492, 0.288413156451364"                                                                          
##                           "0.000625394350924072, 0.000830325022338205, 0.908318520227343, 0.129233080657672, 0.0106449425203128, 0.00100107812854446, 0.104089699066637"                                                                           
##                           "0.00102731165595823, 0.00298771453949919, 0.893734964111405, 0.206365853058584, 0.0185756818574314, 0.599700007343683, 0.351764744881424"                                                                               
##                           "0.0609282606802131, 0.000433732103596535, 0.204637371273823, 0.131752689661391, 0.000919530334734771, 0.447041168948393, 0.14781912234434"                                                                              
##                           "0.000465567574733036, 0.000475767505664925, 0.859593658864007, 0.103860158753913, 0.00789999656855129, 0.000718344329635936, 0.278694380867877"                                                                         
##                           "0.000766009159793261, 0.000758362918541522, 0.721534087253384, 0.116495361086355, 0.0092823950110599, 0.00103442954792072, 0.1494885896827"                                                                             
##                           "0.000695277800070764, 0.00267447915167474, 0.910606692804076, 0.116773569779206, 0.0160278598110394, 0.523925136931976, 0.251220362509181"                                                                              
##                           "0.0554251605827543, 0.000375454397835508, 0.124560545587645, 0.121451668329232, 0.000928318270680288, 0.401968177730244, 0.209759470120139"                                                                             
##                           "0.00675729420287636, 0.000512296553422436, 0.77569324490263, 0.00855453336364256, 0.000667525984345827, 0.360254254929848, 0.178441033190011"                                                                           
##                           "0.122618301954931, 0.00130061682307459, 0.586418698274176, 0.139152693409106, 0.569885997422553, 0.578564957941555, 0.00596208139911178"                                                                                
##                           "0.134726085767898, 0.000498176921919076, 0.200223422603049, 0.259592458641554, 0.412471764256545, 0.331765602706952, 0.00200986468538772"                                                                               
##                           "0.133944879792025, 0.00470207406976475, 0.280004306614654, 0.625789776743291, 0.436566091016978, 0.436452334679809, 0.890688930914962"                                                                                  
##                           "0.134495933041068, 0.00394250034435813, 0.259120983018294, 0.462041962097612, 0.419233248425891, 0.378836205551663, 0.343899307598794"                                                                                  
##                           "0.145737373257245, 6.371481367663e-05, 0.448894781865181, 0.0115421263760079, 0.366798953198127, 0.000980959431633362, 0.000434833835539652"                                                                            
##                           "0.141108189805848, 0.000827952432111234, 0.561488434491747, 0.0920545586624928, 0.402814955963708, 0.00500328419453523, 0.832692207289524"                                                                              
##                           "0.170011652491797, 0.000554781015451965, 0.548317848391248, 0.0442774828655021, 0.387628370701483, 0.00300924722606439, 0.141374544308895"                                                                              
##                           "0.422595700672457, 0.000332876188353249, 0.205971471956243, 0.227808146200682, 0.31322571335539, 0.00118925390948683, 0.283253209079826"                                                                                
##                           "0.394006000435666, 0.000299412949580198, 0.229922681739468, 0.219851219942368, 0.182667265131761, 0.000752025647700745, 0.136879395588998"                                                                              
##                           "0.277810675724116, 0.00231122521267495, 0.257895334344276, 0.370116945503398, 0.271734571365854, 0.542949944241835, 0.272719349256075"                                                                                  
##                           "0.116980083677472, 7.77496179951665e-05, 0.317676808674797, 0.0209971008582309, 0.311491490918814, 0.00100405207662075, 0.000281120281987931"                                                                           
##                           "0.135438234374791, 0.00149332998881337, 0.414337554208528, 0.137033299235513, 0.412098754959319, 0.00509144399549363, 0.888017642976993"                                                                                
##                           "0.144338812749972, 0.00100934333919285, 0.400847809703063, 0.0691586276087889, 0.351790341951588, 0.0030189008462187, 0.126441666429766"                                                                                
##                           "0.329977453254925, 0.000515136974060707, 0.116855423059265, 0.304171157128626, 0.227536148853047, 0.00106870389383936, 0.272460691152666"                                                                               
##                           "0.352024038222132, 0.000551948351044244, 0.0982614093848087, 0.273455375575571, 0.226314704689888, 0.00113514001446246, 0.190761837139879"                                                                              
##                           "0.21336028008121, 0.00402364128127895, 0.142895219787091, 0.475601684354193, 0.212619057787778, 0.476823542647067, 0.203820347383979"                                                                                   
##                           "0.372382006710717, 5.2120893043088e-05, 0.266847996944084, 0.0123652211170576, 0.000880643689432913, 0.000194563084932989, 0.261113157383239"                                                                           
##                           "0.4469223177263, 6.08746149871953e-05, 0.261188423877478, 0.010285837248617, 0.000513965956627408, 0.000172012452715236, 0.111357258339511"                                                                             
##                           "0.308542739799756, 0.000583560276680826, 0.324098053480955, 0.0384623040183338, 0.00232688939218702, 0.522148527791256, 0.101610899131513"                                                                              
##                           "0.835049702452281, 0.000320879940906463, 0.0671735164670424, 0.215589094274685, 0.000709662916899687, 0.339911338373263, 0.267443709463475"                                                                             
##                           "2.34033262781962e-06, 0.000253674541628143, 0.525458891603243, 0.548787028433138, 0.184074004053861, 0.00727705658208067, 0.00134176401763718"                                                                          
##                           "0.000312691198282351, 0.00219980597893185, 0.575727984535258, 0.566509165773954, 0.326424951902408, 0.0143543524720709, 0.981093331967039"                                                                              
##                           "5.79828393879428e-05, 0.00200458495572272, 0.568494780315727, 0.559105826782442, 0.261115636780895, 0.012303628426437, 0.353665724927322"                                                                               
##                           "0.00607868733644423, 0.00061272932986466, 0.254253488462418, 0.569993892415209, 0.28219766832534, 0.0017292356677089, 0.278936840049674"                                                                                
##                           "0.00194347842503223, 0.000454594031589133, 0.279774076384296, 0.34971070187082, 0.358558262706429, 0.00129591434240624, 0.163315993723669"                                                                              
##                           "0.0033173422459606, 0.00239782040856061, 0.287337932280001, 0.509524146950401, 0.304819188649787, 0.54074788362601, 0.303127894501238"                                                                                  
##                           "0.0006566908205455, 0.000439352793080508, 0.483542674123131, 0.398068300752855, 0.0102455807079667, 0.0015843052302977, 0.282944394684835"                                                                              
##                           "0.000113769980029699, 0.000327300888273312, 0.565918314487031, 0.222786931456568, 0.00593362227804179, 0.000744681076172102, 0.0885310485233862"                                                                        
##                           "0.000430621490546614, 0.00182069718662726, 0.527880227321382, 0.357307129227275, 0.0151744579793194, 0.605739222310404, 0.354526291920725"                                                                              
##                           "0.0220082750861881, 0.000470169581353648, 0.275031602822268, 0.232825367811721, 0.0010628922105518, 0.405661895138719, 0.219161249928174"                                                                               
##                           "0.000295517753331626, 0.000432080544976988, 0.423308246806765, 0.151687426745239, 0.00771990963676652, 0.00100506217453025, 0.293555432914778"                                                                          
##                           "0.000146948071571447, 0.000354613594735821, 0.399267423113213, 0.171485783037787, 0.00472543783359703, 0.000831799279805004, 0.144629024497325"                                                                         
##                           "0.00029924816110743, 0.00222635884382075, 0.435278917147836, 0.168044405504886, 0.0118092922273078, 0.554753533471133, 0.242884865225731"                                                                               
##                           "0.0189116901701644, 0.000600830737152102, 0.143402919666023, 0.196308890892555, 0.00128102870976863, 0.364612025182772, 0.322630249838039"                                                                              
##                           "0.0016617435339539, 0.000358028823105663, 0.273947910655966, 0.00555064300638303, 0.000680628396000971, 0.378990645453749, 0.17649203086598"                                                                            
##                           "0.111000172461605, 7.02035169448495e-05, 0.0222450132158221, 0.366882480046428, 0.434539145114299, 0.000751740726041007, 0.000379229039828829"                                                                          
##                           "0.12925347622489, 0.000940245613923643, 0.133706351533796, 0.400910637800848, 0.550899991166418, 0.00392544999589612, 0.820109875847594"                                                                                
##                           "0.140788980891604, 0.00063771685582207, 0.0691586835768856, 0.401295074150527, 0.486573421208933, 0.00235370604094307, 0.128037372173777"                                                                               
##                           "0.312752098502079, 0.000364994820807945, 0.358277357598206, 0.276082723037092, 0.364747499835186, 0.00118434094253723, 0.295007838735783"                                                                               
##                           "0.318883044453572, 0.000340894304043189, 0.331366769158922, 0.151160995083678, 0.466208336967414, 0.000807763531580912, 0.152413555646129"                                                                              
##                           "0.217687702303324, 0.00236935120849483, 0.520713964106408, 0.257733706003006, 0.370089864774619, 0.534785994085788, 0.251019805805121"                                                                                  
##                           "0.30656354943374, 4.37749238110722e-05, 0.0149072296146287, 0.303148106481213, 0.000787530548715814, 0.000254493972238237, 0.29919478694857"                                                                            
##                           "0.316118518838206, 4.63426071954634e-05, 0.0142800024954345, 0.192179766300276, 0.000521916052238302, 0.000140299364357235, 0.0955321532917263"                                                                         
##                           "0.25105320553023, 0.00038293423960545, 0.0442455147691382, 0.286648587630361, 0.00219492281005228, 0.613820134966811, 0.1167058810989"                                                                                  
##                           "0.572142950413917, 0.000215052801239683, 0.286614092873339, 0.085487347347663, 0.000536930995170518, 0.437552248711496, 0.21077173500223"                                                                               
##                           "0.263981099831301, 5.89565198342481e-05, 0.02598041863591, 0.281294617627785, 0.000637981173841015, 0.000176314799527603, 0.26821168012471"                                                                             
##                           "0.344812834867174, 7.2476445383391e-05, 0.0214017410967665, 0.340943341039834, 0.000386093723566872, 0.000189982341434801, 0.131668110855372"                                                                           
##                           "0.222932623243473, 0.000665757581842323, 0.0707252232379029, 0.286142703251877, 0.00175684236800973, 0.516406999895788, 0.0864653858911381"                                                                             
##                           "0.583311986348741, 0.00036959548242957, 0.383447792827241, 0.155616548081249, 0.000779823593847797, 0.348086126964656, 0.322144076347935"                                                                               
##                           "0.727566016421656, 4.15816041599542e-05, 0.011574424462253, 0.00026945991595741, 0.00011342659960316, 0.385084351398933, 0.162122243773373"                                                                             
##                           "0.000380517412091957, 0.000313146873045886, 0.536272709348345, 0.212434158336832, 0.00550032126783005, 0.000973888246595069, 0.313538262125467"                                                                         
##                           "0.000136055763319045, 0.000305239639358366, 0.320212384571076, 0.321324959229501, 0.00366579597875792, 0.000718133210668207, 0.120578238552966"                                                                         
##                           "0.000343864403913267, 0.00136612054758273, 0.483624630087907, 0.250790167296153, 0.00889880134483532, 0.611129745666696, 0.27098557041971"                                                                              
##                           "0.021968979436075, 0.000338909326931737, 0.230075135460773, 0.375534937555813, 0.00088632786128936, 0.426619040309115, 0.258069277930896"                                                                               
##                           "0.00123225769502263, 0.000278460771388783, 0.176473041728573, 0.00459363839570484, 0.000564185825824084, 0.476437927806361, 0.137593705687714"                                                                          
##                           "0.00130452479171731, 0.00030057757326444, 0.145979276041294, 0.00277712926796826, 0.000606324300766298, 0.418030792232983, 0.228747058929931"                                                                           
##                           "4.86567262394786e-10, 0.0790501661899053, 0.175054389228456, 0.0632973431919663, 0.448604831527656, 0.231311680697551, 0.0227756831095799"                                                                              
##                           "3.36622493166561e-09, 0.172664647636038, 0.0744808479155837, 0.0186081402293011, 0.429231230756085, 0.172351853071432, 0.249365733048044"                                                                               
##                           "4.26606475983182e-09, 0.266336559624722, 0.0821042359210455, 0.0317366255128499, 0.432420630174023, 0.197318080175102, 0.934258724864633"                                                                               
##                           "3.37272205142548e-09, 0.236885560449004, 0.0765865696032905, 0.0382823038172625, 0.442990847657033, 0.17594186235123, 0.562438990771265"                                                                                
##                           "9.6594378182655e-11, 0.031087500124681, 0.12355975360933, 0.0833664176194774, 0.190316212998291, 0.0132854181898623, 0.122104746644015"                                                                                 
##                           "1.3975126150399e-10, 0.0637897833165762, 0.131945501991077, 0.131835658855945, 0.200152505449756, 0.0191922988789048, 0.732997285895685"                                                                                
##                           "9.43094767934513e-11, 0.0484593763551469, 0.122793346408661, 0.167395756063166, 0.202998110750415, 0.0159311480670032, 0.416065743680521"                                                                               
##                           "1.59435094254562e-09, 0.139533040372902, 0.0584813321034197, 0.0353904600367882, 0.168836110581731, 0.310883213072551, 0.857092438491213"                                                                               
##                           "1.55243041332833e-09, 0.138797696289876, 0.0586857053916093, 0.0378153911799693, 0.154245218113524, 0.381042448468611, 0.955735222837789"                                                                               
##                           "1.80705809327423e-09, 0.184701356659002, 0.0606663418005695, 0.0522555371875386, 0.181141046073248, 0.847303015202744, 0.672148324047347"                                                                               
##                           "2.84887095556535e-10, 0.0508449855321273, 0.074709922496124, 0.0589602626117206, 0.0930127309877613, 0.0143030784582792, 0.0975249266831028"                                                                            
##                           "3.88767798209864e-10, 0.108196531039849, 0.0793373844532282, 0.0960106349504841, 0.112725273028934, 0.0212382533009579, 0.773540184850141"                                                                              
##                           "3.16505295124112e-10, 0.0781732437649823, 0.0751053185614294, 0.133924717548472, 0.0913525714756276, 0.0161210807924345, 0.30628267440279"                                                                              
##                           "1.96305289234458e-09, 0.21330554296364, 0.0284984644360668, 0.0248544213047841, 0.073269765985782, 0.242343487741911, 0.835722672845083"                                                                                
##                           "1.83768652580447e-09, 0.220841903862673, 0.026655716848967, 0.0311693762743046, 0.0671212989798394, 0.372547954782957, 0.883263364851266"                                                                               
##                           "1.9851297552768e-09, 0.270963704473052, 0.0299889270532445, 0.0405269188340853, 0.0754273832156105, 0.754880338742057, 0.487862332302717"                                                                               
##                           "7.92399441416569e-12, 0.0497302003882278, 0.0215879599169539, 0.125145552275129, 0.0119682928644396, 0.148896275390086, 0.913600532476612"                                                                              
##                           "7.6336633329361e-12, 0.0482682522572379, 0.020079467018907, 0.155434493509194, 0.0108576606574795, 0.260070906758267, 0.824089538475766"                                                                                
##                           "8.01597064550401e-12, 0.0702170782541448, 0.0225050780323718, 0.194989731646937, 0.0141001297716651, 0.807975056741542, 0.422483279189562"                                                                              
##                           "2.93388322426884e-11, 0.196911371059891, 0.00710788237545743, 0.0482329188437927, 0.437098018391299, 0.688816536523525, 0.901081886794031"                                                                              
##                           "2.44268887646173e-10, 0.0711256646845809, 0.0769986460047723, 0.466198180601533, 0.31595992120778, 0.00497565217565864, 0.133153354097088"                                                                              
##                           "4.14215317713741e-10, 0.146607202647986, 0.0899936197594635, 0.410278938581798, 0.316715077543824, 0.00717771079057939, 0.317732304470379"                                                                              
##                           "2.54532151496861e-10, 0.0851128911912668, 0.0861148223361407, 0.48339218233225, 0.247867948951084, 0.00536346672978234, 0.121321718320259"                                                                              
##                           "1.09353316090828e-09, 0.437854879673307, 0.0208753591788886, 0.454455124537644, 0.276777293320704, 0.46551142845222, 0.458572903819461"                                                                                 
##                           "5.15427267444953e-10, 0.373323631078678, 0.020149482942553, 0.515406340658684, 0.246683719569867, 0.759423294755017, 0.349830881042841"                                                                                 
##                           "1.01583947591818e-09, 0.437597026871707, 0.0218186075082608, 0.493984044308213, 0.247081600230925, 0.781526041946244, 0.366342258345625"                                                                                
##                           "6.50978497434392e-11, 0.0645523355638134, 0.0672092798605504, 0.214567197932405, 0.00462079996770316, 0.193228515288472, 0.467378236332416"                                                                             
##                           "3.57005170245629e-11, 0.0510276007202077, 0.0643142810334957, 0.237986928650331, 0.00442000775741729, 0.396566434429454, 0.380562931332615"                                                                             
##                           "6.46821793650387e-11, 0.0753693694538707, 0.0700105175687157, 0.235014276845761, 0.0054614493561134, 0.788604689739262, 0.303290132917524"                                                                              
##                           "2.33178662973421e-10, 0.343932753803294, 0.0184304861359726, 0.211481652715667, 0.746253921195648, 0.763804091373487, 0.615809019189988"                                                                                
##                           "1.85751985523459e-10, 0.103607002503143, 0.0284205520358619, 0.155624563998963, 0.00493484319993976, 0.170694116178195, 0.484302907369949"                                                                              
##                           "1.59604676367736e-10, 0.0782889027948284, 0.0308208514316428, 0.134222672243385, 0.00452795653639697, 0.409344237655053, 0.276943144987596"                                                                             
##                           "1.99665938690532e-10, 0.113364033511925, 0.0331166980341416, 0.13440022736765, 0.00553266322322961, 0.886574757041761, 0.198862172744388"                                                                               
##                           "3.89698408701815e-10, 0.454063495781975, 0.00597545594684464, 0.115487035428245, 0.749113927508681, 0.849086847853556, 0.462529462199977"                                                                               
##                           "3.13975733225701e-12, 0.0768767942672687, 0.00962782558165106, 0.00413568345646485, 0.418375674197734, 0.922476186913519, 0.508587343601502"                                                                            
##                           "2.6048706454866e-10, 0.0377836291293629, 0.0280587155405521, 0.190170583673123, 0.148381867931397, 0.00800627230779101, 0.0986758547909012"                                                                             
##                           "3.95974875902848e-10, 0.0787754825276042, 0.0526974265906893, 0.18859948359914, 0.171078964666117, 0.0118356166405867, 0.688253070096556"                                                                               
##                           "2.896823035619e-10, 0.0582823483487718, 0.068813826825357, 0.201435973414411, 0.142888615104265, 0.00933626497037452, 0.340798853011351"                                                                                
##                           "4.00442293790428e-09, 0.190307410796892, 0.00756462312967647, 0.162824499593288, 0.135991442827057, 0.290911005228453, 0.884696923897927"                                                                               
##                           "3.44613132174674e-09, 0.190642695907928, 0.00824967519852455, 0.151967481427412, 0.135525319876359, 0.382095749458364, 0.977923355213096"                                                                               
##                           "4.15021161972478e-09, 0.244514610798296, 0.0120490629236389, 0.175572754456222, 0.138973718122123, 0.845398989319636, 0.61533329887839"                                                                                 
##                           "1.22546518080434e-10, 0.0303537777044913, 0.0611500202357082, 0.0419778786316599, 0.00762040957369453, 0.147116544387286, 0.924249983779388"                                                                            
##                           "9.33656673167468e-11, 0.027716435779464, 0.0657269073259064, 0.0396861524383576, 0.00753216970575098, 0.228707146809361, 0.868188471225263"                                                                             
##                           "1.3097141314796e-10, 0.0450897662474366, 0.0975667224809358, 0.047717359774468, 0.00944841639492202, 0.995680726351866, 0.513755880696849"                                                                              
##                           "2.89698446911242e-09, 0.14523476317406, 0.0129022282988963, 0.0285471476004359, 0.410595874748211, 0.865673118371564, 0.948132724689936"                                                                                
##                           "3.89092996293721e-10, 0.0622461748891748, 0.0306280922600724, 0.0305828484845453, 0.00700762660149774, 0.113728048820226, 0.96105576529427"                                                                             
##                           "3.50154412595424e-10, 0.0592744237605516, 0.0424743140479429, 0.0271489002319335, 0.00620113088263119, 0.237275202347249, 0.704769718566551"                                                                            
##                           "4.31718660435735e-10, 0.0857399568561236, 0.0572168032892849, 0.0308854856710212, 0.00812051927540611, 0.79981351925186, 0.330094764126019"                                                                             
##                           "3.17499213965825e-09, 0.272667210177321, 0.00704192789147222, 0.0207810994791792, 0.426478587167803, 0.690415990504845, 0.80883169027518"                                                                               
##                           "1.49249814408524e-11, 0.0546858613980474, 0.0631528324434302, 0.00508496869014947, 0.297527078279166, 0.732007042766206, 0.814252387605103"                                                                             
##                           "5.03174348344535e-10, 0.0832301855462629, 0.128892057811972, 0.223611370150617, 0.00151042976216917, 0.188463805117484, 0.36209240846852"                                                                               
##                           "3.93730730031161e-10, 0.0637155421655385, 0.160101887142109, 0.19043830414749, 0.00140028428499727, 0.439595894279458, 0.259164408096488"                                                                               
##                           "5.0460351054768e-10, 0.0926166264945979, 0.157151760294768, 0.191327302794581, 0.00170457116932258, 0.724472337526205, 0.227854871452171"                                                                               
##                           "4.63655868936859e-09, 0.510960538996461, 0.122980272187443, 0.211792508202245, 0.873745348122533, 0.683749072721004, 0.502332541822707"                                                                                 
##                           "1.32944963250585e-10, 0.0562878734700485, 0.0371175969032407, 0.00156611477733523, 0.42140527995289, 0.680824343194959, 0.552128165349552"                                                                              
##                           "6.17568295763346e-10, 0.109358711838485, 0.0424106309870479, 0.00118410027068321, 0.476200650846284, 0.872234318053045, 0.345453753498447"                                                                              
##                           "4.60676059343427e-10, 0.142237547559954, 0.110570009558185, 0.747189456306644, 0.130298654177016, 0.0734631495538523, 0.478565184494766"                                                                                
##                           "4.65352289702318e-10, 0.147021641650999, 0.179376193334083, 0.646372756401353, 0.14416924034323, 0.0648381730538415, 0.516758565538753"                                                                                 
##                           "3.90693642935747e-10, 0.139815091553328, 0.185166982339011, 0.741059432999184, 0.125540706676287, 0.065367793865865, 0.461701420734483"                                                                                 
##                           "2.03630294723372e-09, 0.0792367621551189, 0.050572417590223, 0.590043329521854, 0.134589286842023, 0.55014011740693, 0.808981012541184"                                                                                 
##                           "1.89506198986317e-09, 0.0787207060568457, 0.0534226398980549, 0.616011597168851, 0.131928636354058, 0.681496187284365, 0.824233287783709"                                                                               
##                           "1.73989614505517e-09, 0.0794470721604067, 0.0650706187478454, 0.59622703607142, 0.138765443167553, 0.914557942361635, 0.699169947771199"                                                                                
##                           "1.23335359099522e-10, 0.0978647287518394, 0.264042823049863, 0.342092729488991, 0.0670548974822834, 0.697995778206621, 0.579317655081457"                                                                               
##                           "9.78654237409333e-11, 0.0958271604448726, 0.251075500188913, 0.383059009822461, 0.0736142577852749, 0.830962254998818, 0.762675599157764"                                                                               
##                           "1.1250610453448e-10, 0.0983461480295431, 0.302866062030742, 0.346140858051782, 0.0661159696962052, 0.670235401690855, 0.79158279508332"                                                                                 
##                           "1.02697976338007e-09, 0.0597305551981417, 0.0904244789258254, 0.251462352469482, 0.749525231667715, 0.836847279405559, 0.974046758559998"                                                                               
##                           "2.73867186682569e-10, 0.0839788411359615, 0.161386754177513, 0.0784876518422878, 0.0589039775811766, 0.497638655556671, 0.61500657412688"                                                                               
##                           "2.70945742738722e-10, 0.0868883955232166, 0.17150792130287, 0.077328418920355, 0.0609128066273881, 0.721377841939865, 0.635460301958236"                                                                                
##                           "2.28540794857274e-10, 0.0841686797079029, 0.209439686690388, 0.0789831192387769, 0.0565562362452834, 0.784285927439458, 0.569061214919063"                                                                              
##                           "8.16419835442242e-10, 0.0361155529177039, 0.0571052438327781, 0.0668360066720937, 0.660149355377987, 0.965414874744298, 0.851076417769898"                                                                              
##                           "6.83701115826604e-12, 0.0233339275080847, 0.293773477181915, 0.051210384926693, 0.784853927271809, 0.795414894048289, 0.880599888517475"                                                                                
##                           "2.51577232021449e-10, 0.0801370499590348, 0.6146642475811, 0.193328400835548, 0.01899425754756, 0.59436942530024, 0.292606659518331"                                                                                    
##                           "2.1844671830007e-10, 0.076593792923976, 0.737138707040383, 0.170342534325433, 0.0209390284887449, 0.961292487330421, 0.309816199123861"                                                                                 
##                           "2.35601848421672e-10, 0.0820107128475887, 0.653786231690474, 0.174305060374472, 0.0192189780221179, 0.543028815543253, 0.444708964707091"                                                                               
##                           "1.48614466443324e-10, 0.0242075862899153, 0.561524431399999, 0.211171674183196, 0.94279043528567, 0.641012462613229, 0.559165683749814"                                                                                 
##                           "5.66868188799702e-11, 0.0617188004987115, 0.35129027944107, 0.023059929625032, 0.929949798097738, 0.517539338607307, 0.705289096609516"                                                                                 
##                           "1.03033289221584e-10, 0.0385403432186416, 0.108884554827955, 0.01742271545127, 0.872998845407355, 0.586461543859676, 0.524948039253583"                                                                                 
##                           "3.48559462835452e-10, 0.0950746173239314, 0.350261435854598, 0.0927170340152873, 0.0383247823541494, 0.56186495473683, 0.566815549921354"                                                                               
##                           "3.11555296850493e-10, 0.0942135290349493, 0.396960222552557, 0.0878855774967891, 0.0409879934322115, 0.750791182284059, 0.672774697166495"                                                                              
##                           "2.99757976589324e-10, 0.121718332136176, 0.357485029903164, 0.0941366088483961, 0.0372507913562212, 0.705981511560648, 0.656329492405226"                                                                               
##                           "2.19492200365581e-09, 0.0187131850043693, 0.246921028474162, 0.103111075602451, 0.68565576557098, 0.884377565120248, 0.9545081272806"                                                                                   
##                           "1.46172218528726e-10, 0.169020166124634, 0.0920956217002873, 0.0434718660595076, 0.806533403891863, 0.627033412022787, 0.987904076440883"                                                                               
##                           "3.436540358813e-10, 0.0863174301719325, 0.0254054350885683, 0.0278212940091454, 0.693392302670113, 0.828372965110585, 0.772443651938701"                                                                                
##                           "3.96039967760748e-10, 0.260230438869294, 0.131871179591252, 0.0062309863488984, 0.940217138380139, 0.485363447006089, 0.54446252986418"                                                                                 
##                           "0.855814249521982, 0.0042491350583956, 0.167079942603096, 0.756726493361282, 0.0504030290961396, 0.787070010648509, 0.613949711761953, 0.014270610957265"                                                               
##                           "0.920139303814178, 0.0042312538712427, 0.102741326491974, 0.346228648674941, 0.0636328712827034, 0.604908517371068, 0.380704101642816, 0.00992235961563847"                                                             
##                           "0.918144792891186, 0.0055212683888843, 0.061673544536758, 0.503904963452123, 0.106376018995161, 0.680658064978944, 0.474690283976302, 0.988485422531167"                                                                
##                           "0.917534395264216, 0.00460863730672668, 0.0618883233642701, 0.475509217221128, 0.078203984042411, 0.654568691007804, 0.434322887381135, 0.342400777730258"                                                              
##                           "0.737523580797421, 0.00171526307708008, 0.328992271761183, 0.554961459214546, 0.0108970290806987, 0.496050562212008, 0.00261240366045951, 0.00173150447588476"                                                          
##                           "0.965266904978918, 0.00321356717975816, 0.158226021589506, 0.73725623692398, 0.0341317636194838, 0.611325084048059, 0.0120253434952417, 0.802898514245445"                                                              
##                           "0.97782704075017, 0.00227363320271648, 0.176753219172542, 0.712476745111467, 0.0191455284025158, 0.577305402667656, 0.00777495686233011, 0.152866611481811"                                                             
##                           "0.760392430158881, 0.00221639310896646, 0.125937401827881, 0.33067957115156, 0.0655119301385527, 0.482396978628298, 0.00525137583870202, 0.325264646976294"                                                             
##                           "0.349462813151036, 0.0010489858086682, 0.0234507965920592, 0.418967754732764, 0.0172686653047776, 0.442916465549637, 0.00213452382739559, 0.0962063223831653"                                                           
##                           "0.662428823166779, 0.0033448297497583, 0.0606942952480577, 0.473882329157138, 0.0596914991666025, 0.474670357315425, 0.6177326316898, 0.281899500897437"                                                                
##                           "0.61455667928971, 0.00232881072848054, 0.32150003851257, 0.44579555835623, 0.0175988620878055, 0.386197124132237, 0.00290794460786667, 0.00144146699367428"                                                             
##                           "0.886636066681934, 0.00400684770084656, 0.140024646891373, 0.66309038664581, 0.0431680010160481, 0.511595798018615, 0.0131989346625365, 0.823675521105099"                                                              
##                           "0.866494596084207, 0.00310325681706861, 0.164288064751224, 0.627067784887738, 0.0260923541208432, 0.457349982099736, 0.00850634153258061, 0.144768064760982"                                                            
##                           "0.950174395761886, 0.00190976885788057, 0.112023979244699, 0.244982089915486, 0.0840256341837185, 0.287498521639505, 0.00300168199446211, 0.309249827692729"                                                            
##                           "0.396032248759886, 0.00131895431451206, 0.019422598643827, 0.302031053733996, 0.0199802633882536, 0.420765535602399, 0.00252894526274816, 0.102105339742379"                                                            
##                           "0.800561481349305, 0.00435590359187952, 0.056602577334886, 0.376607163507041, 0.0763733505462153, 0.315858021978794, 0.576454688700338, 0.240849469749669"                                                              
##                           "0.974666167609383, 0.00096408323636293, 0.315180283364975, 0.394778617748264, 0.0112320488591565, 0.00245272688019853, 0.000785776686249981, 0.275149284525376"                                                         
##                           "0.474416020048149, 0.000518631146074169, 0.0687308098207078, 0.493647613017062, 0.00240233546799717, 0.00261083014528466, 0.000942108224792276, 0.0924534883100461"                                                     
##                           "0.809225345812442, 0.00198748802147788, 0.143516125925441, 0.567164716764511, 0.0142761047441885, 0.00677214754454397, 0.570522515361111, 0.116140715425612"                                                            
##                           "0.215094753916815, 0.000707207353168239, 0.0290419189827926, 0.224093472003204, 0.0197438112792443, 0.00178722933185366, 0.668758320088326, 0.202623980786245"                                                          
##                           "2.94587779421798e-06, 0.000600025263447107, 0.663288235815686, 0.501445787468278, 0.505078169531188, 0.205296451114945, 0.00945918710575074, 0.000985404324649489"                                                      
##                           "0.000615205643577658, 0.0054097447429018, 0.920961119982747, 0.586348616255471, 0.583440249374812, 0.324045065269044, 0.0283211457321965, 0.975232219708281"                                                            
##                           "0.00013525775506016, 0.00581895216875144, 0.929563689175943, 0.563544172882659, 0.554156750552673, 0.266824916882939, 0.0215344206194543, 0.353108926950123"                                                            
##                           "0.00713510474323669, 0.000748033737825531, 0.465701633133493, 0.310209566817851, 0.659064027266437, 0.248395288350403, 0.00215768443015279, 0.276490155087645"                                                          
##                           "0.00614082309042838, 0.000950943371856231, 0.217396408121501, 0.398847972540972, 0.448638738478546, 0.31656562225852, 0.00186689757428388, 0.129910542250397"                                                           
##                           "0.00524075715554864, 0.00345234903905646, 0.364270155839853, 0.384467396235907, 0.597351447785905, 0.267219076034424, 0.567883379434409, 0.394625767596241"                                                             
##                           "0.000685259820968302, 0.00101154733648558, 0.614640965756395, 0.458857503441419, 0.362615394645546, 0.0118567126586032, 0.00129791788827828, 0.292399239250034"                                                         
##                           "0.000511429701999799, 0.00127411276509541, 0.956145002891619, 0.571706276490762, 0.227469628168492, 0.0130797502612091, 0.00135028612213556, 0.114705505350802"                                                         
##                           "0.00081338495154577, 0.00557464795642714, 0.83271006735571, 0.513451927510222, 0.346789344114997, 0.0226815208330019, 0.603053341904854, 0.339836916236086"                                                             
##                           "0.0418559708848915, 0.000798947163767728, 0.273199549072022, 0.372705942335943, 0.279857678844416, 0.00134784888736753, 0.438482684553748, 0.165820053405515"                                                           
##                           "0.000319471591582102, 0.000853215298137518, 0.766526426303136, 0.401742530593515, 0.157259015466861, 0.0117093282968929, 0.000914135047049554, 0.294951131071997"                                                       
##                           "0.000529560006542151, 0.00126856957011813, 0.823724369040886, 0.42591553667264, 0.170449612539588, 0.0132037853746689, 0.00143295420747668, 0.163013331529734"                                                          
##                           "0.000517634227876719, 0.00570339232667341, 0.97555353535512, 0.438992706930595, 0.168759490830685, 0.0220651130606548, 0.554211835906545, 0.249645622759591"                                                            
##                           "0.0296753812472972, 0.000834100405201063, 0.215476627642657, 0.250194962075134, 0.188116207413319, 0.00146769152711248, 0.415769056160471, 0.224252630128962"                                                           
##                           "0.00400803167638582, 0.00106030524276801, 0.904688861405822, 0.286936572447556, 0.0130532904149552, 0.00109579380417768, 0.38228950283351, 0.198543658090283"                                                           
##                           "0.634448994417719, 0.00171480005811611, 0.311636683339108, 0.0179149241081843, 0.522422596714563, 0.494045620205474, 0.00240448789419964, 0.00170600799649149"                                                          
##                           "0.875573866224623, 0.00298051382165046, 0.143997073655599, 0.0436057532713364, 0.6690897857513, 0.598715815404931, 0.0116538559237847, 0.793003922812288"                                                               
##                           "0.862602681213342, 0.00220095958505199, 0.166105092766728, 0.0265064887347823, 0.641016943693397, 0.549813614979426, 0.00748880988889687, 0.14378869654269"                                                             
##                           "0.857206180862332, 0.00202212884713508, 0.0917604736002783, 0.0845552495310783, 0.499221167296765, 0.398131334934983, 0.00603423121941648, 0.336575267065401"                                                           
##                           "0.409108186445959, 0.000967050578629046, 0.0184647963189019, 0.0231553848549137, 0.428502381723594, 0.587856483527417, 0.00204132193183815, 0.0960089454054234"                                                         
##                           "0.753053163823453, 0.00271393718905119, 0.0444309509171911, 0.072958195547964, 0.534279283904813, 0.431552883944556, 0.614843530802583, 0.267623337430109"                                                              
##                           "0.935198338094758, 0.000928380845610049, 0.314526571782226, 0.013391762916102, 0.464513949833232, 0.0022333608904522, 0.00110247017400597, 0.307767317968264"                                                           
##                           "0.612662524286041, 0.000444456704449591, 0.0850939295506909, 0.00387332300981904, 0.450437934650129, 0.00248534754092994, 0.000816631595107338, 0.0944336914050342"                                                     
##                           "0.896523918347646, 0.00159702830839622, 0.154577057622159, 0.0172675867717177, 0.528078653434189, 0.00668513647853726, 0.627641304109742, 0.124866948636712"                                                            
##                           "0.316402166594575, 0.00061061903092775, 0.0293420326116423, 0.0260087527253186, 0.262887991600775, 0.00163414637580673, 0.757637616609016, 0.188990583611771"                                                           
##                           "0.813943364724196, 0.000958445041541999, 0.28716109236945, 0.0196022708412778, 0.370009254920496, 0.0022408177923456, 0.000770762402247317, 0.282711168975465"                                                          
##                           "0.582297662666812, 0.000577108439783131, 0.0629859630100204, 0.00427887672878276, 0.583695434395273, 0.00244806751151497, 0.00102410919360821, 0.0972367316585745"                                                      
##                           "0.988706636097089, 0.00187699750789973, 0.13257372955285, 0.0226655029584642, 0.42693596681162, 0.00672047439914003, 0.575393767079266, 0.107172193248967"                                                              
##                           "0.319623722866914, 0.000807083108477481, 0.0231948704023875, 0.0309135780867659, 0.324199330072579, 0.00219855434801659, 0.725159443106046, 0.220500206941269"                                                          
##                           "0.377437067676668, 0.000199844331008041, 0.0750264818945131, 0.00287442297746393, 0.00171683020143968, 0.000474402897873755, 0.647058337296734, 0.176356825927562"                                                      
##                           "0.00043638076658206, 0.000614374444178876, 0.752300706138114, 0.5010062837697, 0.227671561297118, 0.0088071086172824, 0.00085463750944308, 0.318012076769341"                                                           
##                           "0.00048599070265033, 0.000973590091667745, 0.863569001411887, 0.342780779673572, 0.317417609493878, 0.0108362704820853, 0.00116686581773368, 0.140609180133223"                                                         
##                           "0.000626417481709585, 0.00353300542566067, 0.974784468809826, 0.487889589496456, 0.253558153047005, 0.01839470969699, 0.610687476828641, 0.276006437839575"                                                             
##                           "0.0341650953382499, 0.000503992041753584, 0.182776149076963, 0.345113299055539, 0.329492496639577, 0.00107203979693049, 0.475248793746507, 0.184402097314328"                                                           
##                           "0.0029298097599506, 0.000837771715608385, 0.938998395955507, 0.183771861359125, 0.0112689036650101, 0.000909511469648986, 0.478029768956812, 0.160955407082057"                                                         
##                           "0.00270153313121029, 0.000742611917620229, 0.792659550038261, 0.147185243648663, 0.00991166748621217, 0.000883155757520381, 0.428689681570616, 0.236266040632623"                                                       
##                           "0.103400937661975, 8.0903096422705e-05, 0.478775097926069, 0.0211039782351095, 0.556553592130081, 0.462322586803328, 0.00118132996979219, 0.000304856150486745"                                                         
##                           "0.119061798334809, 0.00144858695413991, 0.584096637972838, 0.134628435244191, 0.550479307051215, 0.572282725502276, 0.00581554720621971, 0.813539173486736"                                                             
##                           "0.130297665299734, 0.00097906243446599, 0.572977384869029, 0.0693623524124472, 0.56280509722395, 0.506000246292707, 0.0035527072146774, 0.126142714173586"                                                              
##                           "0.285362895929695, 0.000505500259646285, 0.2151629823082, 0.30016304746588, 0.559299762610894, 0.383709419919912, 0.00118771664441173, 0.303813138752108"                                                               
##                           "0.29190537725766, 0.000495648425207024, 0.236347284185888, 0.279153761609549, 0.357586736795824, 0.483271182105264, 0.00103537683335153, 0.170326546773037"                                                             
##                           "0.182508575106602, 0.00382772399769867, 0.266064905758251, 0.472614301296271, 0.506032264666207, 0.383223654207668, 0.548041886915822, 0.236647501989267"                                                               
##                           "0.28494821183626, 5.82225996128534e-05, 0.457575229894219, 0.0144250748175802, 0.515272376620983, 0.00119944657650262, 0.00023324790764909, 0.302188776855464"                                                          
##                           "0.299162179993055, 6.53783470004425e-05, 0.531583469941079, 0.0138497511544601, 0.343506673244082, 0.00077900844146796, 0.000179236019641862, 0.103397706204233"                                                        
##                           "0.227048390410627, 0.000635863325452929, 0.552915799043088, 0.0452480621338874, 0.456541315730707, 0.00328223219948525, 0.61897352628309, 0.115756038799158"                                                            
##                           "0.524272885193359, 0.000335781644610765, 0.231426080261988, 0.242187548234869, 0.262751983460192, 0.00072498726231203, 0.439127348869835, 0.231293013636364"                                                            
##                           "0.2306182793182, 8.17416450452214e-05, 0.371060124515628, 0.0238261420812736, 0.388626762283075, 0.0012479018078396, 0.000197840750322696, 0.288041312176618"                                                           
##                           "0.308715920809402, 0.000100570476819602, 0.350266103773909, 0.0196119501531965, 0.467437841529273, 0.000788215887557833, 0.000241057207110505, 0.136945936476973"                                                       
##                           "0.188555437075691, 0.00109476008410977, 0.434454337759559, 0.070358372178803, 0.37499345772143, 0.00342567396831363, 0.55689972036639, 0.0916166741375845"                                                              
##                           "0.464132581678105, 0.000557999621234434, 0.116330199604178, 0.300021851158332, 0.27715546342815, 0.00102133283822522, 0.387357586618431, 0.319070269224539"                                                             
##                           "0.554832018698865, 7.32030348295695e-05, 0.290718367012133, 0.0118775369248195, 0.00064900531512601, 0.000170892778358489, 0.417910711246145, 0.170774923198695"                                                        
##                           "0.000323671150084794, 0.000446685755606899, 0.536012888644616, 0.708064804269544, 0.227838785181693, 0.00867656428999784, 0.00110052335816061, 0.31539654753645"                                                        
##                           "0.000115358396371671, 0.000432588554054976, 0.603279962384836, 0.446423735523861, 0.335656380314229, 0.00562672921371253, 0.000907574509453555, 0.127441489095938"                                                      
##                           "0.000289620866755644, 0.00229265073696911, 0.572331200889478, 0.64103188065857, 0.26468823996024, 0.013551953911632, 0.614799878848415, 0.270155787110282"                                                              
##                           "0.014259704197772, 0.00060965093717807, 0.279808213955739, 0.457483100451452, 0.382487599771199, 0.00128287513739895, 0.425990043657923, 0.278383923098783"                                                             
##                           "0.00105968448725916, 0.000398378578696204, 0.558672536113279, 0.302850042988264, 0.00689575997755966, 0.000728210413531281, 0.473361843242016, 0.147377397100054"                                                       
##                           "0.000976664183315111, 0.000458024585940871, 0.426750108870434, 0.207840812145797, 0.00578907460559503, 0.000813602260400792, 0.434502766786645, 0.228815454290013"                                                      
##                           "0.21972373586235, 6.5394456833277e-05, 0.025040016590004, 0.504614560922942, 0.469095854334452, 0.000908095838384164, 0.000217798145071303, 0.309282993589583"                                                          
##                           "0.276797516938066, 7.9496555964758e-05, 0.0208447332239076, 0.311203860366065, 0.661308155011176, 0.000612214833158841, 0.000200623768740679, 0.114553487882789"                                                        
##                           "0.184785916952799, 0.000719956886660557, 0.0699616451871839, 0.476130154138945, 0.486680599920335, 0.0025781920902558, 0.613875590683568, 0.10433487316557"                                                             
##                           "0.427152134610633, 0.000368108955142416, 0.354151104841345, 0.214474894912426, 0.480921545591704, 0.000765668413446525, 0.445838807781941, 0.255763673250672"                                                           
##                           "0.418867786822273, 5.84421930514684e-05, 0.0152826257262939, 0.25407382253791, 0.000593389838419559, 0.000145213838500766, 0.499560041615558, 0.142637080575355"                                                        
##                           "0.441534632343717, 8.51735796897077e-05, 0.0226625923470197, 0.383898589427969, 0.000481931257926314, 0.00018613629997429, 0.418467613438842, 0.200238960032276"                                                        
##                           "0.000921954641880995, 0.000380565169271253, 0.40350869277932, 0.335265004069552, 0.00422541633966797, 0.000712859989637319, 0.491417297847009, 0.191302483700214"                                                       
##                           "3.98859768173411e-10, 0.0410226604908358, 0.165727908836488, 0.0561194234229782, 0.432664694206624, 0.196145853320083, 0.0145326001771347, 0.0926188078197899"                                                          
##                           "6.03486216002588e-10, 0.0875955285543356, 0.174420644284392, 0.0975674165187247, 0.420704937950718, 0.224191085981816, 0.0210634163472979, 0.68233869435439"                                                            
##                           "4.37539770784848e-10, 0.0638115287955875, 0.163705596917295, 0.128352975563051, 0.455492556781945, 0.187457233107461, 0.0165490046760837, 0.313015528241053"                                                            
##                           "3.81069017803143e-09, 0.177505056035712, 0.0749223661671311, 0.0243595465950664, 0.442005648188177, 0.174647952903023, 0.247666393353621, 0.912441598974216"                                                            
##                           "3.34384914748533e-09, 0.176441400121434, 0.0738934732396211, 0.0280176708610201, 0.43194497357928, 0.171017293814317, 0.358361777603577, 0.906117874337609"                                                             
##                           "3.97459103120641e-09, 0.231774314521466, 0.0765335639896181, 0.0392169031729922, 0.470083850659425, 0.175715146318314, 0.843412568660177, 0.541195301336032"                                                            
##                           "1.22003906333457e-10, 0.0345345890834488, 0.12338221598667, 0.111604108191499, 0.190755260377308, 0.0134182829241173, 0.13393386182978, 0.915079519591983"                                                              
##                           "9.52383545558877e-11, 0.031474607930069, 0.121211475689823, 0.124843922680441, 0.189271826424724, 0.013003075939547, 0.226877079012782, 0.810149657538975"                                                              
##                           "1.31417951455885e-10, 0.0512336945598603, 0.122745056222676, 0.175594676271989, 0.211952913174301, 0.016221678365506, 0.978611925466054, 0.459728453868013"                                                             
##                           "1.91814598379372e-09, 0.139454442098393, 0.0586178956376528, 0.0395940323031455, 0.170339473589103, 0.385068771707764, 0.861572876630604, 0.977125916199084"                                                            
##                           "3.19190342657974e-10, 0.0561951933616511, 0.0747705819256513, 0.0769856560768755, 0.0936104862424322, 0.0146178982559987, 0.104718135993987, 0.962186087689881"                                                         
##                           "2.89915916160867e-10, 0.0512149158854356, 0.0726066517284452, 0.0993044298738522, 0.0857078330186208, 0.0132190502341111, 0.225934342010465, 0.663299723390407"                                                         
##                           "3.50296439752958e-10, 0.0786658016053905, 0.0770634337551703, 0.133477334626409, 0.0931979525049657, 0.0167748139675593, 0.862671486425086, 0.318046002268717"                                                          
##                           "1.96196982536014e-09, 0.208204667505759, 0.0282455614866689, 0.0306695390699548, 0.0707735366023682, 0.379104999154998, 0.768261686185133, 0.786197642479588"                                                           
##                           "8.21815498716636e-12, 0.0479497704244, 0.0211399354055021, 0.152334916505253, 0.0115919816696208, 0.264310172925202, 0.825476075354721, 0.763362339230119"                                                              
##                           "3.5428708524999e-10, 0.0845526250995003, 0.0840524144500884, 0.408241578067231, 0.288392268073232, 0.00467709044909446, 0.168068228291777, 0.413601448868273"                                                           
##                           "2.55735191631553e-10, 0.0654435561565285, 0.0818657817314734, 0.473083831331535, 0.24827836251187, 0.0043599525858577, 0.396831226739158, 0.282844134445373"                                                            
##                           "3.69896157278796e-10, 0.0962672780799978, 0.0887395193448551, 0.462104447326701, 0.247620127185394, 0.00537284357129229, 0.786863085291093, 0.224945944450155"                                                          
##                           "9.99022067963957e-10, 0.40641367199114, 0.0209084571277395, 0.490170668276925, 0.246055419338592, 0.737383173547747, 0.759419830980354, 0.506621870897413"                                                              
##                           "6.51880968188865e-11, 0.0578037907080255, 0.0666582316211117, 0.223904724216599, 0.00438574021368148, 0.38135602742562, 0.745219957030102, 0.545270171678134"                                                           
##                           "1.97432649244778e-10, 0.0878319397132603, 0.0306038105307885, 0.131877957708642, 0.00452151915201701, 0.401171900211462, 0.846016565432091, 0.39192988713721"                                                           
##                           "3.28564228579747e-10, 0.0422578338220855, 0.0422676802603625, 0.187525143721506, 0.146785982524761, 0.00797259632331927, 0.110765417213319, 0.871665781958862"                                                          
##                           "2.65161498211332e-10, 0.0382623186215963, 0.049509485917842, 0.191330589380581, 0.139180770113251, 0.00754721151954212, 0.215013803778904, 0.7155128050743"                                                             
##                           "3.62137893982961e-10, 0.0610591689140887, 0.074113484011097, 0.21093678645011, 0.142864097754542, 0.00950655563380485, 0.977652706089081, 0.387181212994678"                                                            
##                           "4.12937540227145e-09, 0.189284208626277, 0.00899689858615509, 0.167561179365458, 0.135389487631797, 0.386911118512763, 0.861808252260575, 0.916757616152874"                                                            
##                           "1.29985200783889e-10, 0.0299548635066158, 0.0727859749601021, 0.0427556794381562, 0.00761438761211395, 0.228919442460453, 0.978868873635047, 0.889466020693581"                                                         
##                           "3.92300133007575e-10, 0.0584823265029099, 0.0422627206755056, 0.0283350235505434, 0.00659234123276428, 0.242330718162093, 0.820653485010382, 0.65463145020401"                                                          
##                           "4.98744816216161e-10, 0.0730580649720127, 0.14773709435664, 0.190882355573018, 0.00139939890689613, 0.418623021451623, 0.685145581562677, 0.429271185929086"                                                            
##                           "5.02473820173111e-10, 0.14221523780942, 0.170164028762301, 0.679264349229334, 0.128642640985321, 0.0635570075740032, 0.52597550682997, 0.565239862720144"                                                               
##                           "4.53073678843275e-10, 0.138690028524261, 0.172981993887749, 0.753073108462863, 0.121257173113298, 0.0670544631704484, 0.735110652590587, 0.639764754725906"                                                             
##                           "4.35867278967555e-10, 0.141363390511754, 0.215806356369141, 0.69242371509851, 0.129441143969484, 0.0611932382639504, 0.722112110494171, 0.608582248769649"                                                              
##                           "2.10614267744212e-09, 0.0785953668251748, 0.0593224598267551, 0.601346712958568, 0.133376107565666, 0.668530749363174, 0.87583293571173, 0.896326333273438"                                                             
##                           "1.34312393907447e-10, 0.0974626146421117, 0.289169858745017, 0.345941828619183, 0.0669996773780958, 0.783384209627283, 0.646568308660329, 0.932635593754766"                                                            
##                           "2.76150575305728e-10, 0.0838766629249135, 0.193659987270063, 0.0757596799157713, 0.0574251292275231, 0.689259335964259, 0.747317467112042, 0.77010718603471"                                                            
##                           "2.56566850934329e-10, 0.0811986064788942, 0.655703548716094, 0.172797853548986, 0.019056272935756, 0.879654382606418, 0.532155389211409, 0.560076926270751"                                                             
##                           "3.51267965060575e-10, 0.113058567529407, 0.361218977086711, 0.0907095346358026, 0.0376674683052571, 0.709851122630052, 0.674850913265448, 0.840082110444366"                                                            
##                           "0.56923795790723, 0.00242682936795859, 0.362001427281985, 0.565820605482808, 0.0198539630139291, 0.681110724286832, 0.502686606128276, 0.00305587541677977, 0.00138530155787167"                                        
##                           "0.834483320702746, 0.00441696646979543, 0.165189867942068, 0.75376707682523, 0.0482690862255715, 0.759671070111376, 0.608026604593897, 0.0137611675650262, 0.789232722354028"                                           
##                           "0.819308128425538, 0.00330999052963815, 0.190345261218814, 0.728344757643423, 0.0294578744818816, 0.745720390367097, 0.558217751266511, 0.0089451259472098, 0.141067597324985"                                          
##                           "0.99884377411836, 0.00212748315119901, 0.126411177446425, 0.327884141167604, 0.0883572844589937, 0.759353327775717, 0.395664430988886, 0.00337991150580588, 0.328175444800526"                                          
##                           "0.469911000649726, 0.00133277849656287, 0.0258130225928665, 0.421388847298205, 0.0237291096147303, 0.628975511312914, 0.592256134175304, 0.00251648272127555, 0.107766158477812"                                        
##                           "0.86491337422465, 0.00473950729035922, 0.0674777721656583, 0.477378944363034, 0.0833112957464271, 0.725064151593734, 0.43462581233705, 0.617715649571207, 0.254479900099361"                                            
##                           "0.845383452120815, 0.00110266646192764, 0.363382953056947, 0.538311932930727, 0.0146835811263629, 0.647792214696555, 0.00284085605297331, 0.000823762345383792, 0.305558299302687"                                      
##                           "0.649589322955493, 0.000556910846520683, 0.0953032014636277, 0.66546237390891, 0.00406514867275102, 0.583903155962793, 0.00301605552573037, 0.000956437417264157, 0.100953875123631"                                    
##                           "0.954238002208468, 0.0025150759979838, 0.179914803383766, 0.716711383816348, 0.0198006588444087, 0.647060061475887, 0.00795587935712684, 0.630656763885934, 0.123499041352132"                                          
##                           "0.370530154270139, 0.000738529355506446, 0.0369849191326782, 0.4037901638354, 0.025878844117626, 0.46360872400845, 0.0016845747519115, 0.713971998833378, 0.190959143094351"                                            
##                           "0.688930364731773, 0.000769651481076419, 0.34420501475015, 0.477824557860174, 0.0196332727101972, 0.442293910149874, 0.00320445885881726, 0.000476444343980865, 0.292819818351238"                                      
##                           "0.661411569437371, 0.000760709932544884, 0.0827647952980834, 0.550880220274057, 0.00477879593018227, 0.662745496958693, 0.00328885962548736, 0.00123707691987863, 0.108873336763884"                                    
##                           "0.921672345035292, 0.00332453297675696, 0.173833810722311, 0.657037197497035, 0.0275816600945949, 0.47767661251678, 0.00879444537422737, 0.597295408476848, 0.108195332150572"                                          
##                           "0.436940862698451, 0.000966967971665237, 0.0348270233848983, 0.291154585438549, 0.0316226640613569, 0.427770253882932, 0.00209753025929616, 0.684313796392602, 0.217313076904942"                                       
##                           "0.51341578852578, 0.000282906718267013, 0.0985329066742552, 0.481652333169945, 0.00350831160644229, 0.00258589198673028, 0.00054768033609483, 0.630150754713189, 0.174952249895509"                                     
##                           "0.000354907114989375, 0.000918175497380169, 0.713357236790764, 0.516740769221986, 0.664257366222399, 0.247835846313348, 0.0117655422019008, 0.000985439345853581, 0.321041536657883"                                    
##                           "0.000407457612357845, 0.00141455408276565, 0.906176834757792, 0.613726426717552, 0.459741948254758, 0.332794603193839, 0.0134469660118742, 0.00150882479719483, 0.15126410438636"                                       
##                           "0.000516214503716909, 0.00610091062425474, 0.918523231121765, 0.566140070052807, 0.633492298800486, 0.270945257708188, 0.0227642631395984, 0.613270376002142, 0.268782399151918"                                        
##                           "0.0244020230768171, 0.000900401106737513, 0.246029704934675, 0.38711674157427, 0.543487374640887, 0.340999801367895, 0.00152609684472483, 0.46589834184087, 0.20455475084071"                                           
##                           "0.00247043873548574, 0.00124475035366517, 0.986167878226198, 0.561560619135843, 0.305328043708537, 0.0139768690312457, 0.00121555720973363, 0.473730713429773, 0.176257688770842"                                       
##                           "0.00201516656265301, 0.00122824434037987, 0.883916463916042, 0.444449335779592, 0.206812855421225, 0.013999020251993, 0.00124402037983815, 0.439849239306656, 0.24765178753114"                                         
##                           "0.734203580362858, 0.00109409666061728, 0.339721207634007, 0.0220060889381187, 0.665942197131005, 0.504629467144124, 0.00254938905910073, 0.000800904054790433, 0.311017458418098"                                      
##                           "0.706822758010936, 0.000618829938382098, 0.0910170636887734, 0.0055531273474675, 0.55470847011813, 0.784490872055711, 0.00278476395302611, 0.00104289551817648, 0.104471037663938"                                      
##                           "0.928024227252586, 0.00242904827198484, 0.169320993435196, 0.0271672847912363, 0.716423098219102, 0.548894175278339, 0.0076555226466577, 0.626580585116384, 0.115358386159825"                                          
##                           "0.436862757396742, 0.000794599691553161, 0.0314201031468852, 0.0343753454736654, 0.438565114704206, 0.586222276417771, 0.00193270869824647, 0.754407474884958, 0.2033434201439"                                         
##                           "0.632178868720234, 0.000252741825312937, 0.108170250817958, 0.00484955969465835, 0.467807794005645, 0.00243506606333388, 0.000494068425706207, 0.68381660403034, 0.163816943915304"                                     
##                           "0.626035699359833, 0.000337514453750386, 0.0923729298040887, 0.00592718576959579, 0.582736619768556, 0.00237961653696843, 0.000647785267204136, 0.646677978803267, 0.190335805576206"                                   
##                           "0.00197412854017205, 0.000976503599429712, 0.898009899888817, 0.420346010670912, 0.33254890512979, 0.0112822908737502, 0.00105813683521641, 0.494779977260768, 0.210217799364839"                                       
##                           "0.212572734400231, 8.32192623592164e-05, 0.486813379904658, 0.023745823931094, 0.70391166581919, 0.498911081687058, 0.00138597765252795, 0.00021814725383262, 0.311947815919405"                                        
##                           "0.269375404494714, 0.000105023625160346, 0.548292230261027, 0.0199159939835877, 0.461208687463515, 0.688044986152882, 0.000909064191602595, 0.000245738065223059, 0.121725590257025"                                    
##                           "0.171858721587091, 0.00108330394990937, 0.577578210325625, 0.0702142556117351, 0.639088737651144, 0.505798396274871, 0.00385288268239297, 0.618611282127802, 0.103937748691371"                                         
##                           "0.396747341369986, 0.000534095280864765, 0.237691738029043, 0.299096613820892, 0.455639611993819, 0.498312758284682, 0.000987801676149561, 0.447117859443401, 0.277610896112524"                                        
##                           "0.40012394356365, 8.04634781024695e-05, 0.525721312921144, 0.0148282463954083, 0.425442372546413, 0.000881315486410198, 0.000183890731150505, 0.496051669953143, 0.153046786386492"                                     
##                           "0.399668921792868, 0.000119319343741401, 0.375197492354172, 0.0208472843628501, 0.508406846804559, 0.000925314417025819, 0.000239832191387577, 0.440799547047656, 0.200401269579818"                                    
##                           "0.000817247187080506, 0.000518773247455924, 0.595233512641905, 0.542543001733044, 0.350533574148462, 0.0064665501618264, 0.00088881881933857, 0.48829062350835, 0.200973835627351"                                      
##                           "0.362176461909875, 9.60267101310548e-05, 0.0218627521027929, 0.38508089419403, 0.666161991110378, 0.00068923529845132, 0.000205165767116786, 0.501441672298947, 0.168925447122644"                                      
##                           "4.90106764720236e-10, 0.0459572455555885, 0.165611153263905, 0.0788732504180735, 0.424040302852218, 0.19410564262722, 0.0144066884607991, 0.104298769775191, 0.868714542287224"                                         
##                           "3.98857990718862e-10, 0.0415202184059179, 0.161689818131525, 0.094227545853071, 0.43885250100772, 0.182590424628997, 0.0134913325969876, 0.213879107148495, 0.679013618604706"                                          
##                           "5.41630035020069e-10, 0.0664467740361005, 0.163563754931952, 0.134328948812961, 0.467532181878009, 0.187326546663886, 0.0168714870815187, 0.963442060965278, 0.35577263137239"                                          
##                           "3.93643949397343e-09, 0.175419801174588, 0.0738449944831021, 0.0292624147145039, 0.456361809425832, 0.170923875068093, 0.362813656273815, 0.859662111456628, 0.85141203266911"                                          
##                           "1.30960378155554e-10, 0.0337477265984867, 0.121274992106682, 0.133655727380014, 0.196101947137344, 0.013180932038396, 0.227487790949891, 0.996376613310368, 0.829256840518088"                                          
##                           "3.20037019372247e-10, 0.052543235814434, 0.0742397477705277, 0.100788685134407, 0.0872990947871577, 0.0137138389759128, 0.229051025167772, 0.883794189352204, 0.645227352680291"                                        
##                           "3.60361226520776e-10, 0.0737835894684159, 0.0846569856368716, 0.446621886510536, 0.248006004718402, 0.00432916043718445, 0.381487552149557, 0.743835453857568, 0.435731703097999"                                       
##                           "3.28616146894722e-10, 0.0406676409892949, 0.0545408099738652, 0.198476799075985, 0.139278369085029, 0.00764709516846682, 0.215707569635998, 0.995651038922695, 0.743371529770696"                                       
##                           "5.0778128365516e-10, 0.140229052797002, 0.199967475293605, 0.698380916529732, 0.124639025355517, 0.0620816532828081, 0.696668673220705, 0.689147015936103, 0.801909716418355"                                           
##                           "0.664656538285528, 0.000846461408006552, 0.368778748197934, 0.552186423957109, 0.021072151715831, 0.83506920814168, 0.516782265502303, 0.00329452306146577, 0.000522262703098625, 0.308834118191629"                    
##                           "0.736060004301176, 0.000746290383122345, 0.100709148199927, 0.67299811117041, 0.00571390008609993, 0.673532706760676, 0.797214074449338, 0.00337701145609831, 0.00118984096066668, 0.11051261370064"                    
##                           "0.88425880794964, 0.00357693571129057, 0.193533052127485, 0.7326000388158, 0.0301733881005324, 0.816962608509786, 0.557128815702952, 0.0091391824976868, 0.62939687351787, 0.11420031074085"                            
##                           "0.497322325664086, 0.000951258907012842, 0.0394490160524897, 0.406436747564965, 0.0339482548858287, 0.65263241642092, 0.591236038434773, 0.00198933365896736, 0.712270430689618, 0.205971255770462"                     
##                           "0.669212763373751, 0.00030796287556803, 0.118691829131381, 0.646775522174971, 0.00499853432011886, 0.613537823144765, 0.0029773122947912, 0.000550962814293565, 0.665615921114215, 0.166063915128238"                   
##                           "0.700631255301253, 0.000416642267632577, 0.111533395428763, 0.540233634564718, 0.00625764001958175, 0.667133414884538, 0.00322967798813007, 0.000703945737016183, 0.634536037357186, 0.186869522560469"                 
##                           "0.00171641667192591, 0.00137994971872652, 0.940451098614556, 0.602151689685535, 0.550714728956773, 0.349006303319445, 0.0141315420815333, 0.00135766444014552, 0.490238108462356, 0.224922298128471"                    
##                           "0.731719891269405, 0.000360826675848491, 0.114225538121826, 0.00681206065641693, 0.577389535334003, 0.777199805378795, 0.00272141257758536, 0.000638330483658246, 0.679407352381975, 0.17534372262375"                  
##                           "0.35479894914792, 0.000124625908918587, 0.541955159472229, 0.0208944782424578, 0.547934703280445, 0.693200484258567, 0.00101879442375022, 0.000249689636322063, 0.497800120800178, 0.178152743365093"                   
##                           "4.89903139661943e-10, 0.0438837215375155, 0.161684608362785, 0.100681099084327, 0.44738959803569, 0.182611544870831, 0.0136941575547526, 0.21488429646938, 0.989551035843515, 0.703738824288701"                        
##                           "0.760545745626362, 0.000424519721356408, 0.124194705949948, 0.654677681747827, 0.00689951819957313, 0.706389507590286, 0.790999168519989, 0.00332586750117629, 0.0006976328680638, 0.662406674015935, 0.177230876449307"
```
