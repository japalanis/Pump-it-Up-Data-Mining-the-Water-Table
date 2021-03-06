# Pump-it-Up-Data-Mining-the-Water-Table

Using data from Tanzanian Ministry of Water and Taarifa, we aim to predict which pumps are functional, which need repair, and which don’t work at all. An
understanding of which waterpoints can fail
would allow improved maintenance operations
and ensure that clean water is available to
communities across Tanzania [1] . We employed
algorithms such as Decision Tree Classifier,
Random Forest Classifier, Gradient Boost
Machine, Support Vector Machine and
XGBoost algorithm. Along with these, we also
generated rules on performing association rule
mining to find top rules that lead to functional
and nonfunctional pumps.

```{r message = F, warning = F}
library(tidyverse)
library(dplyr)
library(corrplot)
library(ggplot2)
library(caret)
library(randomForest)
library(gbm)
library(arules)
library(arulesViz)
library(kernlab)
library(pROC)
library(GoodmanKruskal)
```

```{r message = F, warning = F}
pump_data <- read.csv("mainFile.csv", header = TRUE)
sapply(pump_data, function(x) sum(is.na(x)))
```

### Removing unnecessary columns
```{r message = F, warning = F}
pump_data[, c(2,9,10,12,13,17,40,20,25,26,31,21,22,35)] <- NULL
```

### Permit and public meeting
```{r message = F, warning = F}
levels(pump_data$permit)[levels(pump_data$permit) == ""] <- NA
levels(pump_data$public_meeting)[levels(pump_data$public_meeting) == ""] <- NA
impute.mode <- function(x) {
  uni <- unique(x)
  mod <- uni[which.max(tabulate(match(x,uni)))]
  replace(x, is.na(x), mod)
}
pump_data <- pump_data %>%
  mutate(
    permit = impute.mode(permit),
    public_meeting = impute.mode(public_meeting)
    
  )
```

### Funder- subsetted the top 15 funder and considered the rest as "other"
```{r message = F, warning = F}
pump_data$funder <- tolower(pump_data$funder)
pump_data$funder[pump_data$funder %in% c(" ", "", "0", "_", "-")] <- "other"
funder.top <- names(summary(as.factor(pump_data$funder)))[1:15]
pump_data$funder[!(pump_data$funder %in% funder.top)] <- "other"
pump_data$funder <- as.factor(pump_data$funder)
```

### Installer
```{r message = F, warning = F}
pump_data$installer <- tolower(pump_data$installer)
pump_data$installer[pump_data$installer %in% c(" ", "", "0", "_", "-")] <- "other"
installer.top <- names(summary(as.factor(pump_data$installer)))[1:15]
pump_data$installer[!(pump_data$installer %in% installer.top)] <- "other"
pump_data$installer <- as.factor(pump_data$installer)
```

### Duplicates:
```{r message = F, warning = F}
pump_data <- pump_data[!duplicated(pump_data), ]
```

```{r message = F, warning = F}
#create new col to extract year from date recorded
pump_data$year_recorded <- str_sub(pump_data$date_recorded, -2, -1)
#Now use year_recorded and construction_year to find age of the well
pump_data$year_recorded <- as.numeric(pump_data$year_recorded)

pump_data$construction_year <- as.numeric(pump_data$construction_year)

pump_data$year_recorded <-  pump_data$year_recorded + 2000

#before using construction year, deal with its null values 
medianYr <- median(pump_data$construction_year)
pump_data$construction_year[pump_data$construction_year == 0] <- medianYr

### Now we can create Age column to include in our model and drop unnecessary cols like construction yr and date recorded
pump_data$age <- pump_data$year_recorded - pump_data$construction_year
pump_data$age[pump_data$age < 0] <- 0

pump_data <- pump_data[, -2]
pump_data <- pump_data[, -14]

### Deal with gps height
medGPS <- median(pump_data$gps_height)
pump_data$gps_height[pump_data$gps_height <= 0] <- medGPS

### Drop basin and lga because it does not provide any help with the model
pump_data <- pump_data[, -7]
pump_data <- pump_data[, -9]
#population
medPop <- median(pump_data$population)
pump_data$population[pump_data$population == 0] <- medPop

### Drop water qu. and only consider water qu. group 
pump_data <- pump_data[, -16]

### Drop  waterpoint type
pump_data <- pump_data[, -13]
pump_data <- pump_data[, -13]
pump_data <- pump_data[, -16]
pump_data <- pump_data[, -16]
pump_data <- pump_data[, -17]
```




### obtaining class variables from csv file
```{r message = F, warning = F}
class_var <- read.csv('ClassVariables.csv', header = TRUE)
```

### Adding class variables to the main dataframe using id
```{r message = F, warning = F}
pump_data <- merge(pump_data, class_var, by = 'id', all.x = TRUE)
``` 

### Removing id, longtitude and latitude columns
```{r message = F, warning = F}
pump_data[, c(1,5,6)] <- NULL
```

### Removing variables with zero association
```{r message = F, warning = F}
gkm <- GKtauDataframe(pump_data)
plot(gkm)
pump_data[,c(7,8,13,14)] <- NULL
```

### Discretization for ARules - GPS height, year, age, population
```{r message = F, warning = F}
### Age
pump_data_arules <- pump_data %>%
  mutate(age = ifelse(age < 20, "new", ifelse(age>=20 && age<33, "medium", "old")))

### Population
pump_data_arules <- pump_data_arules %>%
  mutate(population = ifelse(population < 25, "low", ifelse(population>=25 && population<215, "medium", "high")))

### Gps_height
pump_data_arules <- pump_data_arules %>%
  mutate(gps_height = ifelse(gps_height < 500, "low", ifelse(gps_height>=500 && gps_height<1300, "medium", "high")))

pump_data_arules$gps_height <- as.factor(pump_data_arules$gps_height)
pump_data_arules$region_code <- as.factor(pump_data_arules$region_code)
pump_data_arules$district_code <- as.factor(pump_data_arules$district_code)
pump_data_arules$population <-as.factor(pump_data_arules$population)
pump_data_arules$age <- as.factor(pump_data_arules$age)
```

### Splitting the dataset into training and validation data in 80:20 ratio 
```{r message = F, warning = F}
set.seed(188)
train_index <- createDataPartition(pump_data$status_group, p = 0.8, list = FALSE)
pump_train <- pump_data[train_index, ]
pump_val <- pump_data[-train_index, ]
```

### Normalization
```{r message = F, warning = F}
pump_train2 <- pump_train %>%
  mutate_if(is.numeric, scale)

pump_val2 <- pump_val %>%
  mutate_if(is.numeric, scale)
```

### Status of the total pumps

```{r}
ggplot(data=pump_data, aes(x=status_group)) +
  geom_bar(fill="brown",width=0.5)+ggtitle("Status of the total pumps")
```

### Status of the pumps based on Quantity
```{r}

ggplot(data=pump_data, aes(x=quantity,fill=status_group)) +
  geom_bar()+ggtitle("Status of the pumps based on Quantity")+scale_fill_manual(values=c("pink","darkred","maroon"))
```

### Status of the pumps based on Quality
```{r}
ggplot(data=pump_data, aes(x=quality_group,fill=status_group)) +
  geom_bar()+ggtitle("Status of the pumps based on Quality")+scale_fill_manual(values=c("lightgreen","gold","darkgreen"))
```

### Status of the pumps based on Extraction Type class

```{r}
ggplot(data=pump_data, aes(x=extraction_type_class,fill=status_group)) +
  geom_bar()+ggtitle("Status of the pumps based on Extraction Type class")+scale_fill_manual(values=c("cadetblue2","aquamarine","cyan4"))
```

```{r}
ggplot(subset(pump_data, age > 0), aes(x = age)) +
  geom_histogram(binwidth = 5,fill="cyan4") +
  facet_grid( ~ status_group)
```

### Status of the pumps based on Payment

```{r}
ggplot(data=pump_data, aes(x=payment,fill=status_group)) +
  geom_bar()+ggtitle("Status of the pumps based on Payment")+scale_fill_manual(values=c("darkseagreen","darkolivegreen2","darkolivegreen"))
```

## Modeling

## 1. Association rule

### Transactions
```{r message = F, warning = F}
fac_var <- sapply(pump_data_arules, is.factor)
pump <- as(pump_data_arules[, fac_var], "transactions")
```

### Rules
```{r message = F, warning = F}
rules <- apriori(data = pump, parameter = list(supp = 0.01, conf = 0.5, minlen = 5),
                 appearance = list(default = "lhs", rhs = c("status_group=functional", 
                                                            "status_group=functional needs repair",
                                                            "status_group=non functional")),
                 control = list(verbose = FALSE))

subset_rules <- which(colSums(is.subset(rules, rules)) > 1)
rules <- sort(rules[-subset_rules], by = "lift", descreasing = TRUE)
```


## 2. Decision tree
```{r message = F, warning = F}
start_time <- Sys.time()
dt_model <- train(status_group ~ ., data = pump_train, method = "rpart",
                  tuneGrid = expand.grid(cp = seq(0, 0.1, 0.01)),
                  trControl = trainControl(method = "cv", number = 3))
dt_time <- Sys.time() - start_time
dt_time
dt_model
```

```{r message = F, warning = F}
predict_dt <- predict(dt_model, newdata = pump_val, type = "raw")
confusionMatrix(predict_dt, pump_val$status_group)
```

## 3. Random forest
```{r message = F, warning = F}
start_time <- Sys.time()
rf_model <- train(status_group ~ ., data = pump_train2, method = "rf",
                   trGrid = expand.grid(.mtry = c(2, 5, 9, 15)),
                   trControl = trainControl(method = "cv", number = 3))
rf_time <- Sys.time() - start_time
rf_time
rf_model
```

```{r message = F, warning = F}
predict_rf <- predict(rf_model, newdata = pump_val2)
confusionMatrix(predict_rf, pump_val2$status_group)
```

## 4. Gradient boost machine
```{r message = F, warning = F, results = 'hide'}
start_time <- Sys.time()
gbm_model <- train(status_group ~ ., data = pump_train2, method = "gbm",
                   trControl = trainControl(method = "cv", number = 3))
gbm_time <- Sys.time() - start_time
```

```{r message = F, warning = F}
gbm_time
gbm_model
```

```{r message = F, warning = F}
predict_gbm <- predict(gbm_model, newdata = pump_val2,
                       trControl = trainControl(method = "cv", number = 3))
confusionMatrix(predict_gbm, pump_val2$status_group)
```

## 5. Non-linear support vector machine
```{r message = F, warning = F}
start_time <- Sys.time()
svm_model <- ksvm(status_group ~ ., data = pump_train2, method = "svmRadial",
                       tuneGrid = expand.grid(sigma = seq(0, 1, 0.1, 0.01),
                                              C = seq(0, 1, 0.1, 0.01)),
                       trControl = trainControl(method = "cv", number = 3))
svm_time <- Sys.time() - start_time
svm_time
svm_model
```

```{r message = F, warning = F}
predict_svm <- predict(svm_model, newdata = pump_val2)
confusionMatrix(predict_svm, pump_val2$status_group)
```

## 6. Extreme gradient boost
```{r message = F, warning = F, results = 'hide'}
start_time <- Sys.time()
xgb_model <- train(status_group ~ ., data = pump_train2, method = "xgbTree",
                   trGrid = expand.grid(nrounds = 2, 
                                        max_depth = c(5, 10, 15), 
                                        eta = c(0.01, 0.001, 0.0001), 
                                        gamma = c(1, 2, 3), 
                                        colsample_bytree = c(0.4, 0.7, 1.0), 
                                        min_child_weight = c(0.5, 1, 1.5)),
                   trControl = trainControl(method = "cv", number = 3,
                                            allowParallel = TRUE))
xgb_time <- Sys.time() - start_time
xgb_time
xgb_model
```

```{r message = F, warning = F}
predict_xgb <- predict(xgb_model, newdata = pump_val2)
confusionMatrix(predict_xgb, pump_val2$status_group)
```

#### roc- dt
```{r message=F, warning=F}
dt_pred_prob <- predict(dt_model, newdata = pump_val, type = "prob")
roc_curve1 <- roc(pump_val$status_group, dt_pred_prob$functional)
roc_curve2 <- roc(pump_val$status_group, dt_pred_prob$'functional needs repair')
roc_curve3 <- roc(pump_val$status_group, dt_pred_prob$'non functional')
plot(roc_curve1, col = "red", main = "Decision Tree ROC")
plot(roc_curve2, col = "blue", add = TRUE)
plot(roc_curve3, col = "green", add = TRUE)
legend(0.35, 0.2, legend = c("functional", "functional needs repair", "non functional"), lty = 1,
       col=c("red", "blue", "green"), bty = "n")
```

#### roc- rf
```{r message=F, warning=F}
rf_pred_prob <- predict(rf_model, newdata = pump_val2, type = "prob")
roc_curve1 <- roc(pump_val2$status_group, rf_pred_prob$functional)
roc_curve2 <- roc(pump_val2$status_group, rf_pred_prob$'functional needs repair')
roc_curve3 <- roc(pump_val2$status_group, rf_pred_prob$'non functional')
plot(roc_curve1, col = "red", main = "Random Forest ROC")
plot(roc_curve2, col = "blue", add = TRUE)
plot(roc_curve3, col = "green", add = TRUE)
legend(0.35, 0.2, legend = c("functional", "functional needs repair", "non functional"), lty = 1,
       col=c("red", "blue", "green"), bty = "n")
```
