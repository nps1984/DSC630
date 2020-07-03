library(readr)
library(tidyverse)
library(broom)
library(tidyr)
library(dplyr)
library(magrittr)
library(lattice)
library(car)
require(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(factoextra)

sample <- readr::read_csv("C:\\cygwin64\\home\\salis\\data\\expedia\\sample.csv")
destinations <- readr::read_csv("C:\\cygwin64\\home\\salis\\data\\expedia\\destinations.csv")
test <- readr::read_csv("C:\\cygwin64\\home\\salis\\data\\expedia\\test.csv")

train <- merge(x = sample[,c("srch_adults_cnt","srch_children_cnt","srch_rm_cnt","srch_destination_id",
                         "srch_destination_type_id","is_booking","hotel_cluster")],
               y = destinations,
               by="srch_destination_id")

testing <- merge(x = test[,c("srch_adults_cnt","srch_children_cnt","srch_rm_cnt","srch_destination_id",
                             "srch_destination_type_id","is_booking")],
               y = destinations,
               by="srch_destination_id")

# Check structure of train
#str(train)

# do pca on train to reduce dimensions
res.pca <- prcomp(train, scale = T)

# plot scree
fviz_eig(res.pca)

summary(res.pca)
res.pca$center[1:15]
res.pca$x[1:5,1:3]

var_explained <- res.pca$sdev^2/sum(res.pca$sdev^2)

res.pca$x %>% 
  as.data.frame %>%
  ggplot(aes(x=PC156,y=PC1)) + geom_point(size=4) +
  theme_bw(base_size=32) + 
  labs(x=paste0("PC156: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC1: ",round(var_explained[2]*100,1),"%")) +
  theme(legend.position="top")


# Drop variables
search_params <- expedia %>%
  select(c("srch_ci","srch_co","srch_adults_cnt","srch_children_cnt","srch_rm_cnt","srch_destination_id",
           "srch_destination_type_id","is_booking","hotel_cluster"))

#factor_cols <- c("srch_adults_cnt","srch_children_cnt","srch_rm_cnt","srch_destination_id","srch_destination_type_id","is_booking","hotel_cluster")
factor_cols <- c("srch_destination_id", "srch_destination_type_id",
                 "srch_adults_cnt","srch_children_cnt","srch_rm_cnt","is_booking",
                 "d1", "d2", "d3")

search_params[factor_cols] <- lapply(search_params[factor_cols], factor)

head(search_params)
na.omit(search_params)
glimpse(search_params)

fit <- randomForest(hotel_cluster ~ srch_destination_id + srch_destination_type_id + d1 + d2 +d3,   data=train)
p <- predict(fit,testing,type='class')
predicted <- cbind(testing,p)

cluster_count <- train %>% group_by(hotel_cluster) %>% tally()
round((cluster_count$n/nrow(train))*100,2)


pred_cluster_count <- predicted %>% group_by(round(p)) %>% tally()
round((pred_cluster_count$n/nrow(predicted))*100,2)
