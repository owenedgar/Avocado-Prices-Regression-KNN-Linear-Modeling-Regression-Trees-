KNN - Regression

rm(list = ls())
library("FNN")
library("tidyverse")
library("rpart")
library("rpart.plot")
library("caret")
library("forecast")


avo = read_csv('avocado.csv')
avo <- avo %>%
  select(-Date)
avo = avo %>%
  mutate(id = 1:nrow(avo))

avo = avo %>%
  mutate(
    avo_type_organic = if_else(type == "organic", 1, 0),
    avo_type_conventional = if_else(type == "conventional", 1, 0))


avo <- avo %>%
  mutate(
    avo_region_North = if_else(region %in% c("GreatLakes", "Midsouth", "Plains", "Northeast") & !(region %in% c("South", "West", "East", "Central")), 1, 0),
    avo_region_South = if_else(region %in% c("DallasFtWorth", "Houston", "Southeast", "Tampa") & !(region %in% c("North", "West", "East", "Central")), 1, 0),
    avo_region_East = if_else(region %in% c("Boston", "BuffaloRochester", "HartfordSpringfield", "NewYork", "NorthernNewEngland", "Philadelphia", "Pittsburgh", "Syracuse") & !(region %in% c("North", "South", "West", "Central")), 1, 0),
    avo_region_West = if_else(region %in% c("Boise", "California", "LasVegas", "LosAngeles", "PhoenixTucson", "Sacramento", "SanDiego", "SanFrancisco", "WestTexNewMexico", "Northwest") & !(region %in% c("North", "South", "East", "Central")), 1, 0),
    avo_region_Central = if_else(region %in% c("Chicago", "Columbus", "Detroit", "GrandRapids", "Indianapolis", "KansasCity", "Louisville", "Nashville", "OhioValley", "StLouis") & !(region %in% c("North", "South", "East", "West")), 1, 0)
  )



## Total Volume
b1 = mean(avo$`Total Volume`)
b2 = sd(avo$`Total Volume`)


## Type 4046
c1 = mean(avo$'4046')
c2 = sd(avo$'4046')


## Type 4225
d1 = mean(avo$'4225')
d2 = sd(avo$'4225')




e1 = mean(avo$'4770')
e2 = sd(avo$'4770')




f1 = mean(avo$`Total Bags`)
f2 = sd(avo$`Total Bags`)




g1 = mean(avo$`Small Bags`)
g2 = sd(avo$`Small Bags`)




h1 = mean(avo$`Large Bags`)
h2 = sd(avo$`Large Bags`)




i1 = mean(avo$`XLarge Bags`)
i2 = sd(avo$`XLarge Bags`)




k1 = mean(avo$year)
k2 = sd(avo$year)


avo = avo %>%
  mutate(
    `Total Volume_norm` = (`Total Volume` - b1) / b2,
    `4046_norm` = (`4046` - c1) / c2,
    `4225_norm` = (`4225` - d1) / d2,
    `4770_norm` = (`4770` - e1) / e2,
    `Total Bags_norm` = (`Total Bags` - f1) / f2,
    `Small Bags_norm` = (`Small Bags` - g1) / g2,
    `Large Bags_norm` = (`Large Bags` - h1) / h2,
    `XLarge Bags_norm` = (`XLarge Bags` - i1) / i2,
    year_norm = (year - k1) / k2)


avo_input_norm <- avo %>%
  select(
    `Total Volume_norm`,`4046_norm`,`4225_norm`,`4770_norm`,`Total Bags_norm`,`Small Bags_norm`,`Large Bags_norm`,
    `XLarge Bags_norm`, year_norm,
    avo_type_organic,
    avo_type_conventional,
    avo_region_North, avo_region_South, avo_region_East, avo_region_West, avo_region_Central
  )



newdata <- as_tibble(list(
  
  
  `Total Volume` = 150000,
  `4046` = 30000,
  `4225` = 50000,
  `4770` = 20000,
  `Total Bags` = 75000,
  `Small Bags` = 50000,
  `Large Bags` = 20000,
  `XLarge Bags` = 5000,
  year = 2018,
  region = 'Sacramento',
  type = 'organic'
))


newdata = newdata %>%
  mutate(
    `Total Volume_norm` = (`Total Volume` - b1) / b2,
    `4046_norm` = (`4046` - c1) / c2,
    `4225_norm` = (`4225` - d1) / d2,
    `4770_norm` = (`4770` - e1) / e2,
    `Total Bags_norm` = (`Total Bags` - f1) / f2,
    `Small Bags_norm` = (`Small Bags` - g1) / g2,
    `Large Bags_norm` = (`Large Bags` - h1) / h2,
    `XLarge Bags_norm` = (`XLarge Bags` - i1) / i2,
    year_norm = (year - k1) / k2)
newdata <- newdata %>%
  mutate(
    avo_region_North = if_else(region %in% c("GreatLakes", "Midsouth", "Plains", "Northeast") & !(region %in% c("South", "West", "East", "Central")), 1, 0),
    avo_region_South = if_else(region %in% c("DallasFtWorth", "Houston", "Southeast", "Tampa") & !(region %in% c("North", "West", "East", "Central")), 1, 0),
    avo_region_East = if_else(region %in% c("Boston", "BuffaloRochester", "HartfordSpringfield", "NewYork", "NorthernNewEngland", "Philadelphia", "Pittsburgh", "Syracuse") & !(region %in% c("North", "South", "West", "Central")), 1, 0),
    avo_region_West = if_else(region %in% c("Boise", "California", "LasVegas", "LosAngeles", "PhoenixTucson", "Sacramento", "SanDiego", "SanFrancisco", "WestTexNewMexico", "Northwest") & !(region %in% c("North", "South", "East", "Central")), 1, 0),
    avo_region_Central = if_else(region %in% c("Chicago", "Columbus", "Detroit", "GrandRapids", "Indianapolis", "KansasCity", "Louisville", "Nashville", "OhioValley", "StLouis") & !(region %in% c("North", "South", "East", "West")), 1, 0),
    avo_type_organic = if_else(type == "organic", 1, 0),
    avo_type_conventional = if_else(type == "conventional", 1, 0))


  

newdata_input_norm = newdata %>%
  select(
    `Total Volume_norm`,`4046_norm`,`4225_norm`,`4770_norm`,`Total Bags_norm`,`Small Bags_norm`,`Large Bags_norm`,
    `XLarge Bags_norm`, year_norm,
    avo_region_North, avo_region_South, avo_region_East, avo_region_West, avo_region_Central,
    avo_type_organic,
    avo_type_conventional )


avo_output = avo$AveragePrice




# Initialize an empty data frame to store results
knn_results <- data.frame(K = numeric(), Predicted_Value = numeric())



  # Perform KNN regression
  knn_result = knn.reg(avo_input_norm, newdata_input_norm, avo_output, 135)
  ## sqrt(18249)

# Print the results
print(knn_result)

accuracy(knn_result)
set.seed(30)

train=avo%>%
  sample_frac(.75)

validation=avo%>%
  slice(setdiff(avo$id,train$id))


validation = validation %>%
  mutate(price_prediction = predict(knn_result, validation))

# Fit the model on the training set
set.seed(30)
model <- train(
  AveragePrice~., data = train, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
)
# Plot model error RMSE vs different values of k
plot(model)
# Best tuning parameter k that minimize the RMSE
model$bestTune
# Make predictions on the test data
predictions <- model %>% predict(test.data)
head(predictions)
# Compute the prediction error RMSE
RMSE(predictions, test.data$medv)





###############################################

rm(list = ls())


library("FNN")
library("tidyverse")
library("rpart")
library("rpart.plot")
library("caret")
library("forecast")

avo = read_csv('avocado.csv')

avo <- avo %>%
  select(-Date) %>%
  mutate(id = 1:nrow(avo))



avo = avo %>%
  mutate(
    avo_type_organic = if_else(type == "organic", 1, 0),
    avo_type_conventional = if_else(type == "conventional", 1, 0))
avo <- avo %>%
  mutate(
    avo_region_North = if_else(region %in% c("GreatLakes", "Midsouth", "Plains", "Northeast") & !(region %in% c("South", "West", "East", "Central")), 1, 0),
    avo_region_South = if_else(region %in% c("DallasFtWorth", "Houston", "Southeast", "Tampa") & !(region %in% c("North", "West", "East", "Central")), 1, 0),
    avo_region_East = if_else(region %in% c("Boston", "BuffaloRochester", "HartfordSpringfield", "NewYork", "NorthernNewEngland", "Philadelphia", "Pittsburgh", "Syracuse") & !(region %in% c("North", "South", "West", "Central")), 1, 0),
    avo_region_West = if_else(region %in% c("Boise", "California", "LasVegas", "LosAngeles", "PhoenixTucson", "Sacramento", "SanDiego", "SanFrancisco", "WestTexNewMexico", "Northwest") & !(region %in% c("North", "South", "East", "Central")), 1, 0),
    avo_region_Central = if_else(region %in% c("Chicago", "Columbus", "Detroit", "GrandRapids", "Indianapolis", "KansasCity", "Louisville", "Nashville", "OhioValley", "StLouis") & !(region %in% c("North", "South", "East", "West")), 1, 0)
  )
 


avo.mlr <- lm(`AveragePrice` ~ `Total Volume` + `4046` + `4225` + `4770` + `Total Bags` +
                `Small Bags` + `Large Bags` + `XLarge Bags` + `year` +
                `avo_type_organic` + `avo_type_conventional` +
                avo_region_North + avo_region_East + avo_region_South+ avo_region_West +
                 avo_region_South  + avo_region_Central
              , data = avo)

summary(avo.mlr)



set.seed(30)
train=avo%>%
  sample_frac(.75)

validation=avo%>%
  slice(setdiff(avo$id,train$id))

train.lm<-lm(`AveragePrice` ~ `Total Volume` + `4046` + `4225` + `4770` + `Total Bags` +
               `Small Bags` + `Large Bags` + `XLarge Bags` + `year` +
               `avo_type_organic` + `avo_type_conventional` +
               avo_region_North + avo_region_East + avo_region_South+ avo_region_West +
               avo_region_South  + avo_region_Central
             , train)

summary(train.lm)

validation=validation%>%
  mutate(AveragePrice_pred=predict(train.lm,validation))
accuracy(validation$AveragePrice,validation$AveragePrice_pred)

train=train%>%
  mutate(AveragePrice_pred=predict(train.lm,train))
accuracy(train$AveragePrice,train$AveragePrice_pred)



Regression Trees
avo.ct=rpart(AveragePrice~Total.Volume+X4046+X4225+X4770+Total.Bags+Small.Bags+
               Large.Bags+XLarge.Bags+year+type+region+X,
             train,method="class",cp=.00001,minsplit=10,xval=10)
plotcp(avo.ct)

