#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


library(tidyverse)
library(dplyr)
library(caret)
library(randomForest)
library(gbm)
library(arules)
library(arulesViz)
library(kernlab)
library(pROC)

pump_data <- read.csv("mainFile.csv", header = TRUE)

pump_data[, c(2,9,10,12,13,17,40,20,25,26,31,21,22,35)] <- NULL

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

pump_data$funder <- tolower(pump_data$funder)
pump_data$funder[pump_data$funder %in% c(" ", "", "0", "_", "-")] <- "other"
funder.top <- names(summary(as.factor(pump_data$funder)))[1:15]
pump_data$funder[!(pump_data$funder %in% funder.top)] <- "other"
pump_data$funder <- as.factor(pump_data$funder)

pump_data$installer <- tolower(pump_data$installer)
pump_data$installer[pump_data$installer %in% c(" ", "", "0", "_", "-")] <- "other"
installer.top <- names(summary(as.factor(pump_data$installer)))[1:15]
pump_data$installer[!(pump_data$installer %in% installer.top)] <- "other"
pump_data$installer <- as.factor(pump_data$installer)

pump_data <- pump_data[!duplicated(pump_data), ]

#create new col to extract year from date recorded
pump_data$year_recorded <- str_sub(pump_data$date_recorded, -2, -1)
#Now use year_recorded and construction_year to find age of the well
pump_data$year_recorded <- as.numeric(pump_data$year_recorded)

pump_data$construction_year <- as.numeric(pump_data$construction_year)

pump_data$year_recorded <-  pump_data$year_recorded + 2000

#before using construction year, deal with its null values 
medianYr <- median(pump_data$construction_year)
pump_data$construction_year[pump_data$construction_year == 0] <- medianYr

#now we can create Age column to include in our model and drop unnecessary cols like construction yr and date recorded
pump_data$age <- pump_data$year_recorded - pump_data$construction_year
pump_data$age[pump_data$age < 0] <- 0

pump_data <- pump_data[, -2]
pump_data <- pump_data[, -14]

#deal with gps height
medGPS <- median(pump_data$gps_height)
pump_data$gps_height[pump_data$gps_height <= 0] <- medGPS

#drop basin and lga because it does not provide any help with the model
pump_data <- pump_data[, -7]
pump_data <- pump_data[, -9]
#population
medPop <- median(pump_data$population)
pump_data$population[pump_data$population == 0] <- medPop

#drop water qu. and only consider water qu. group 
pump_data <- pump_data[, -16]

#drop   waterpoint type
pump_data <- pump_data[, -13]
pump_data <- pump_data[, -13]
pump_data <- pump_data[, -16]
pump_data <- pump_data[, -16]
pump_data <- pump_data[, -17]

class_var <- read.csv('ClassVariables.csv', header = TRUE)

pump_data <- merge(pump_data, class_var, by = 'id', all.x = TRUE)

pump_data[, c(1,5,6)] <- NULL

pump_data[,c(7,8,13,14)] <- NULL

#age
pump_data_arules <- pump_data %>%
    mutate(age = ifelse(age < 20, "new", ifelse(age>=20 && age<33, "medium", "old")))

#population
pump_data_arules <- pump_data_arules %>%
    mutate(population = ifelse(population < 25, "low", ifelse(population>=25 && population<215, "medium", "high")))

#gps_height
pump_data_arules <- pump_data_arules %>%
    mutate(gps_height = ifelse(gps_height < 500, "low", ifelse(gps_height>=500 && gps_height<1300, "medium", "high")))

pump_data_arules$gps_height <- as.factor(pump_data_arules$gps_height)
pump_data_arules$region_code <- as.factor(pump_data_arules$region_code)
pump_data_arules$district_code <- as.factor(pump_data_arules$district_code)
pump_data_arules$population <-as.factor(pump_data_arules$population)
pump_data_arules$age <- as.factor(pump_data_arules$age)

fac_var <- sapply(pump_data_arules, is.factor)
pump <- as(pump_data_arules[, fac_var], "transactions")

set.seed(188)
train_index <- createDataPartition(pump_data$status_group, p = 0.8, list = FALSE)
pump_train <- pump_data[train_index, ]
pump_val <- pump_data[-train_index, ]

pump_train2 <- pump_train %>%
    mutate_if(is.numeric, scale)

pump_val2 <- pump_val %>%
    mutate_if(is.numeric, scale)

test1 <- data.frame(funder=NA,gps_height=NA,installer=NA,region_code=NA,district_code=NA,population=NA,
                   extraction_type_class=NA,payment=NA,quality_group=NA,quantity=NA,age=NA)
test2 <- data.frame(funder=NA,gps_height=NA,installer=NA,region_code=NA,district_code=NA,population=NA,
                    extraction_type_class=NA,payment=NA,quality_group=NA,quantity=NA,age=NA)
test3 <- data.frame(funder=NA,gps_height=NA,installer=NA,region_code=NA,district_code=NA,population=NA,
                    extraction_type_class=NA,payment=NA,quality_group=NA,quantity=NA,age=NA)
test4 <- data.frame(funder=NA,gps_height=NA,installer=NA,region_code=NA,district_code=NA,population=NA,
                    extraction_type_class=NA,payment=NA,quality_group=NA,quantity=NA,age=NA)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$arules <- renderPrint({
        
        rules <- apriori(data = pump, parameter = list(supp = input$sup, conf = input$con, minlen = input$len),
                         appearance = list(default = "lhs", rhs = input$stat),
                         control = list(verbose = FALSE))
        
        subset_rules <- which(colSums(is.subset(rules, rules)) > 1)
        rules <- sort(rules[-subset_rules], by = "lift", descreasing = TRUE)
        
        inspect(head(rules, 3))
    })
    
    dt_model <- reactive({train(status_group ~ ., data = pump_train, method = "rpart", cp = input$cp)})
    
    output$dt <- renderPrint({
        
        confusionMatrix(predict(dt_model(), newdata = pump_val), pump_val$status_group)$overall[1]
    })
    
    output$dt_test <- renderPrint({
        
       test1$funder<-as.factor(input$dt1)
       test1$gps_height<-as.numeric(input$dt2)
       test1$installer<-as.factor(input$dt3)
       test1$region_code<-as.numeric(input$dt4)
       test1$district_code<-as.numeric(input$dt5)
       test1$population<-as.numeric(input$dt6)
       test1$extraction_type_class<-as.factor(input$dt7)
       test1$payment<-as.factor(input$dt8)
       test1$quality_group<-as.factor(input$dt9)
       test1$quantity<-as.factor(input$dt10)
       test1$age<-as.numeric(input$dt11)
       
       predict(dt_model(), newdata = test1)
    })
    
    rf_model <- reactive({train(status_group ~ ., data = pump_train2, method = "rf", .mtry = input$mtry)})
    
    output$rf <- renderPrint({
        
        confusionMatrix(predict(rf_model(), newdata = pump_val2), pump_val$status_group)$overall[1]
    })
    
    output$rf_test <- renderPrint({
        
        test2$funder<-as.factor(input$rf1)
        test2$gps_height<-as.numeric(input$rf2)
        test2$installer<-as.factor(input$rf3)
        test2$region_code<-as.numeric(input$rf4)
        test2$district_code<-as.numeric(input$rf5)
        test2$population<-as.numeric(input$rf6)
        test2$extraction_type_class<-as.factor(input$rf7)
        test2$payment<-as.factor(input$rf8)
        test2$quality_group<-as.factor(input$rf9)
        test2$quantity<-as.factor(input$rf10)
        test2$age<-as.numeric(input$rf11)
        
        predict(rf_model(), newdata = test2)
    })
    
    svm_model <- reactive({ksvm(status_group ~ ., data = pump_train2, method = "svmRadial", sigma = input$sigma, 
                                C = input$c)})
    
    output$svm <- renderPrint({
        
        confusionMatrix(predict(svm_model(), newdata = pump_val2), pump_val$status_group)$overall[1]
    })
    
    output$svm_test <- renderPrint({
        
        test3$funder<-as.factor(input$svm1)
        test3$gps_height<-as.numeric(input$svm2)
        test3$installer<-as.factor(input$svm3)
        test3$region_code<-as.numeric(input$svm4)
        test3$district_code<-as.numeric(input$svm5)
        test3$population<-as.numeric(input$svm6)
        test3$extraction_type_class<-as.factor(input$svm7)
        test3$payment<-as.factor(input$svm8)
        test3$quality_group<-as.factor(input$svm9)
        test3$quantity<-as.factor(input$svm10)
        test3$age<-as.numeric(input$svm11)
        
        predict(svm_model(), newdata = test3)
    })
    
    xgb_model <- reactive({train(status_group ~ ., data = pump_train2, method = "xgbTree", 
                       nrounds = input$nr, max_depth = input$mdp)})
    
    output$xgb <- renderPrint({
        
        confusionMatrix(predict(xgb_model(), newdata = pump_val2), pump_val$status_group)$overall[1]
    })
    
    output$xgb_test <- renderPrint({
        
        test4$funder<-as.factor(input$xgb1)
        test4$gps_height<-as.numeric(input$xgb2)
        test4$installer<-as.factor(input$xgb3)
        test4$region_code<-as.numeric(input$xgb4)
        test4$district_code<-as.numeric(input$xgb5)
        test4$population<-as.numeric(input$xgb6)
        test4$extraction_type_class<-as.factor(input$xgb7)
        test4$payment<-as.factor(input$xgb8)
        test4$quality_group<-as.factor(input$xgb9)
        test4$quantity<-as.factor(input$xgb10)
        test4$age<-as.numeric(input$xgb11)
        
        predict(xgb_model(), newdata = test4)
    })

})
