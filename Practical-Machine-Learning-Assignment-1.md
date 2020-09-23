---
title: "Activity Recognition Using Predictive Analytics"
author: "Mohd.Faijanur-Rob-Siddiquee"
date: "September 23, 2020"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

## Overview

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit, it is now possible to collect a large amount of data about personal activity relatively inexpensively. The aim of this project is to predict the manner in which participants perform a barbell lift. The data comes from http://groupware.les.inf.puc-rio.br/har wherein 6 participants were asked to perform the same set of exercises correctly and incorrectly with accelerometers placed on the belt, forearm, arm, and dumbell.  

For the purpose of this project, the following steps would be followed:

1. Data Preprocessing.
2. Model Selection.
3. Validating the Model.
4. Final Model Testing.

## Data Preprocessing 

First, we load the training and testing set from the online sources and then split the training set further into training and test sets. 


```r
library(caret)
trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if (!file.exists("training.csv")){
    download.file(trainurl, "training.csv")
}
if (!file.exists("testing.csv")){
    download.file(testurl, "testing.csv")
}
training <- read.csv("training.csv")
testing  <- read.csv("testing.csv")
```


```r
dim(training)
```

```
## [1] 19622   160
```

```r
dim(testing)
```

```
## [1]  20 160
```

```r
str(training)
```

```
## 'data.frame':	19622 obs. of  160 variables:
##  $ X                       : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ user_name               : chr  "carlitos" "carlitos" "carlitos" "carlitos" ...
##  $ raw_timestamp_part_1    : int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
##  $ raw_timestamp_part_2    : int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
##  $ cvtd_timestamp          : chr  "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" "05/12/2011 11:23" ...
##  $ new_window              : chr  "no" "no" "no" "no" ...
##  $ num_window              : int  11 11 11 12 12 12 12 12 12 12 ...
##  $ roll_belt               : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt              : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt                : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt        : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ kurtosis_roll_belt      : chr  "" "" "" "" ...
##  $ kurtosis_picth_belt     : chr  "" "" "" "" ...
##  $ kurtosis_yaw_belt       : chr  "" "" "" "" ...
##  $ skewness_roll_belt      : chr  "" "" "" "" ...
##  $ skewness_roll_belt.1    : chr  "" "" "" "" ...
##  $ skewness_yaw_belt       : chr  "" "" "" "" ...
##  $ max_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_belt            : chr  "" "" "" "" ...
##  $ min_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_belt          : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_belt            : chr  "" "" "" "" ...
##  $ amplitude_roll_belt     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_pitch_belt    : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_yaw_belt      : chr  "" "" "" "" ...
##  $ var_total_accel_belt    : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_roll_belt        : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_roll_belt           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_pitch_belt       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_pitch_belt          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_yaw_belt         : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_yaw_belt            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ gyros_belt_x            : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y            : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z            : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x            : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y            : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z            : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x           : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y           : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z           : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm                : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm               : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm                 : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm         : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ var_accel_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_roll_arm         : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_pitch_arm        : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ avg_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ stddev_yaw_arm          : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ var_yaw_arm             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ gyros_arm_x             : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y             : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z             : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x             : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y             : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z             : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x            : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y            : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z            : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ kurtosis_roll_arm       : chr  "" "" "" "" ...
##  $ kurtosis_picth_arm      : chr  "" "" "" "" ...
##  $ kurtosis_yaw_arm        : chr  "" "" "" "" ...
##  $ skewness_roll_arm       : chr  "" "" "" "" ...
##  $ skewness_pitch_arm      : chr  "" "" "" "" ...
##  $ skewness_yaw_arm        : chr  "" "" "" "" ...
##  $ max_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_roll_arm            : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_arm           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_arm             : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_roll_arm      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_pitch_arm     : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ amplitude_yaw_arm       : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ roll_dumbbell           : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell          : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell            : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ kurtosis_roll_dumbbell  : chr  "" "" "" "" ...
##  $ kurtosis_picth_dumbbell : chr  "" "" "" "" ...
##  $ kurtosis_yaw_dumbbell   : chr  "" "" "" "" ...
##  $ skewness_roll_dumbbell  : chr  "" "" "" "" ...
##  $ skewness_pitch_dumbbell : chr  "" "" "" "" ...
##  $ skewness_yaw_dumbbell   : chr  "" "" "" "" ...
##  $ max_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_picth_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ max_yaw_dumbbell        : chr  "" "" "" "" ...
##  $ min_roll_dumbbell       : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_pitch_dumbbell      : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ min_yaw_dumbbell        : chr  "" "" "" "" ...
##  $ amplitude_roll_dumbbell : num  NA NA NA NA NA NA NA NA NA NA ...
##   [list output truncated]
```

It could be seen that there are 19622 rows and around 160 columns in the training dataset and the testing dataset contains the same number of columns (i.e., 160) and only 20 columns. 

###  DataCleaning

As we can see that in the training dataset, which has 19622 row and 160 columns, most of them are mostly filled with NA and empty values and the first 7 ones are just not related to the functioning of the model as they mostly talk about ID and stamps
So lets clean the training data and remove them on the basis that if the column has more than 90% of its values as NAs or Empty will be removed.


```r
empty_cols <- which(colSums(is.na(training) | training == "") > 0.9*dim(training)[1])
clean_training <- training[, -empty_cols]
clean_training <- clean_training[, -(1:7)]

dim(clean_training)
```

```
## [1] 19622    53
```

Now lets see that if the available variables contain a bit of significant variation in them otherwise they would not be much helpful for the training of the machine learning algorithm.


```r
no_var <- nearZeroVar(clean_training)
no_var
```

```
## integer(0)
```

This means that all the current variables report some variation, i.e., we do not need to remove any more variables from the data set.

### Data Partition

In this step, we are just splitting the data in the ratio of 70 to 30 percents. The 70 percent of the data will be used for training the algorithm and the remaining 30 percent will be used to test its accuracy.


```r
set.seed(7777) ## Setting the seed for reproducibility

in_train <- createDataPartition(clean_training$classe, p = 0.7, list = FALSE)
train_data <- clean_training[in_train, ]
test_data <- clean_training[-in_train, ]
dim(train_data)
```

```
## [1] 13737    53
```



## Model Selection

### Building the Models

1. It is determined that this is a classification problem and the aim of the comparison is to discover which algorithm suits the data better.
2. For this problem the 2 best methods which could be selected could be the Random Forest and Gradient Boosting Methods
3. The Kappa metric is selected as the comparison criteria.
4. To reduce the risk of over fitting, a 10-fold cross validation is employed during model building.


```r
set.seed(7777) ## Setting the seed for reproducibility
fitControl <- trainControl(method = "cv", number = 10)
gbm_fit <- train(classe ~ ., data = train_data, method = "gbm", metric = "Kappa", trControl = fitControl, verbose = FALSE)
rf_fit <- train(classe ~ ., data = train_data, method = "rf", metric = "Kappa", trControl = fitControl)
```

### Comparing the models


```r
library(lattice)
r_vals <- resamples(list(rf = rf_fit, gbm = gbm_fit))
summary(r_vals)
```

```
## 
## Call:
## summary.resamples(object = r_vals)
## 
## Models: rf, gbm 
## Number of resamples: 10 
## 
## Accuracy 
##          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## rf  0.9869091 0.9897978 0.9923581 0.9920653 0.9947224 0.9963610    0
## gbm 0.9497817 0.9604949 0.9636099 0.9628028 0.9666967 0.9708667    0
## 
## Kappa 
##          Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
## rf  0.9834380 0.9870930 0.9903326 0.9899622 0.9933251 0.9953977    0
## gbm 0.9363796 0.9500266 0.9539645 0.9529410 0.9578795 0.9631571    0
```


```r
bwplot(r_vals, metric = "Kappa", main = "Random Forest vs Gradient Boosting")
```

![](Practical-Machine-Learning-Assignment-1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


* The models are then compared using the `resamples` function
* Based on the plot above, it can be determined that the Random Forest algorithm fares better than the Gradient Boosting algorithm for this data set, achieving a Kappa mean value of 0.99. It can also be seen that the Random Forest algorithm also displays less spread than Gradient Boosting.
* Therefor, taking the Random Forest model for the data set.

## Validating the Model


```r
rf_fit
```

```
## Random Forest 
## 
## 13737 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 12362, 12365, 12363, 12363, 12364, 12363, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9920653  0.9899622
##   27    0.9914828  0.9892257
##   52    0.9827471  0.9781719
## 
## Kappa was used to select the optimal model using the largest value.
## The final value used for the model was mtry = 2.
```

Now lets use the `confusionMatrix` function from the caret package on this modeland try to predict the test set


```r
confusionMatrix(as.factor(test_data$classe), predict(rf_fit, test_data))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1672    2    0    0    0
##          B   10 1126    3    0    0
##          C    0    6 1018    2    0
##          D    0    0   16  946    2
##          E    0    0    0    1 1081
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9929          
##                  95% CI : (0.9904, 0.9949)
##     No Information Rate : 0.2858          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.991           
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9941   0.9929   0.9817   0.9968   0.9982
## Specificity            0.9995   0.9973   0.9983   0.9964   0.9998
## Pos Pred Value         0.9988   0.9886   0.9922   0.9813   0.9991
## Neg Pred Value         0.9976   0.9983   0.9961   0.9994   0.9996
## Prevalence             0.2858   0.1927   0.1762   0.1613   0.1840
## Detection Rate         0.2841   0.1913   0.1730   0.1607   0.1837
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      0.9968   0.9951   0.9900   0.9966   0.9990
```


## Final Model Testing

#### Here are the final results


```r
final_results <- predict(rf_fit, newdata = testing)
as.data.frame(final_results)
```

```
##    final_results
## 1              B
## 2              A
## 3              B
## 4              A
## 5              A
## 6              E
## 7              D
## 8              B
## 9              A
## 10             A
## 11             B
## 12             C
## 13             B
## 14             A
## 15             E
## 16             E
## 17             A
## 18             B
## 19             B
## 20             B
```
