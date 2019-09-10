
library(tidyverse)
library(hrbrthemes)
library(ggsci)
library(h2o)
library(caret)
library(autoMLviz)


df <- read_csv("global_terror_clean.csv") %>%
  mutate_if(is.character,factor) %>%
  select(-nkill,-nwound,-date,-summary,-guncertain1,
         -eventid,-gname,-multiple,-suicide,-target1,
         -individual) %>%
  mutate(success = factor(success))


set.seed(42)
index <- createDataPartition(df$success, p = 0.7, list = FALSE)

train_data <- df[index, ]
test_data  <- df[-index, ]

h2o.init(nthreads = -1) 
# if not working, use command line
#cd Downloads/h2o-3.24.0.5  #navigate to wherever you have the h2o installation
#java -jar h2o.jar

h2o.init(startH2O = F) 

h2o.no_progress()

train_hf <- as.h2o(train_data)
test_hf <- as.h2o(test_data)

response <- "success"
features <- setdiff(colnames(train_hf), response)

summary(train_hf$success, exact_quantiles = TRUE)
summary(test_hf$success, exact_quantiles = TRUE)

# train with autoML
aml <- h2o.automl(x = features, 
                  y = response,
                  training_frame = train_hf,
                  leaderboard_frame = test_hf,
                  balance_classes = TRUE)

# View the AutoML Leaderboard
aml@leaderboard

best_model <- aml@leader

perf <- h2o.performance(best_model, test_hf)

pred <- h2o.predict(best_model, test_hf[, -1])

h2o.mean_per_class_error(best_model,xval = TRUE)
h2o.mean_per_class_error(perf)

h2o.auc(perf)
plot(perf)

h2o.confusionMatrix(perf) # or..
perf@metrics$cm$table



autoMLviz::auc_bars(aml,test_data=test_hf)
autoMLviz::varImp_ggplot(aml)
autoMLviz::lift4gains2(aml)



