library(readr)
library(tidyverse)
library(broom)
library(tidyr)
library(dplyr)
library(magrittr)
library(lattice)
library(car)
require(caTools)

setwd('C:/cygwin64/home/salis/git/repos/DSC630/NSalisbury_Week2_MarketingPromotion')
dodger.data <- readr::read_csv("dodgers.csv")

# created ordered factors for day of week and month
dodger.data$dow_ordered <- with(data=dodger.data, 
                  ifelse((day_of_week == "Monday"),1,
                  ifelse((day_of_week == "Tuesday"),2,
                  ifelse((day_of_week == "Wednesday"),3,
                  ifelse((day_of_week == "Thursday"),4,
                  ifelse((day_of_week == "Friday"),5,
                  ifelse((day_of_week == "Saturday"),6,7)))))))


dodger.data$mon_ordered <- with(data=dodger.data, 
                       ifelse((month == "MAR"),3,
                       ifelse((month == "APR"),4,
                       ifelse((month == "MAY"),5,
                       ifelse((month == "JUN"),6,
                       ifelse((month == "JUL"),7,
                       ifelse((month == "AUG"),8,
                       ifelse((month == "SEP"),9,
                       ifelse((month == "OCT"),10,11)))))))))

# put labels on factors
dodger.data$dow_ordered <- factor(dodger.data$dow_ordered, levels = 1:7, labels=c("Mon","Tue","Wed", "Thur",
                                                                                  "Fri","Sat","Sund"))
dodger.data$mon_ordered <- factor(dodger.data$mon_ordered, levels = 3:11, labels=c("March","April","May","June","July",
                                                                                   "Aug","Sept","Oct","Nov"))


# plot based on day of week and month
with(data=dodger.data, plot(dow_ordered, attend/1000, xlab = "Day of Week", ylab = "Attendance",
                             col = "dodgerblue", las = 1))

with(data=dodger.data, plot(mon_ordered, attend/1000, xlab = "Month", ylab = "Attendance",
                             col = "dodgerblue", las = 1))

# Use xyplot 
group.labels <- c("No Bobblehead","Bobblehead")
group.symbols <- c(21,24)
group.colors <- c("black","black")
group.fill <- c("yellow","dodgerblue")
xyplot(attend/1000 ~ temp | skies + day_night,
       data = dodger.data, groups = bobblehead, pch = group.symbols,
       aspect = 1, cex = 1.5, col = group.colors, fill = group.fill,
       layout = c(2,2), type = c("p","g"), 
       strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),
       xlab = "Temperature", ylab = "Attendance",
       key = list(space = "top",
                  text = list(rev(group.labels), col = rev(group.colors)),
                  points = list(pch = rev(group.symbols), col = rev(group.colors),
                                fill = rev(group.fill))))


set.seed(456)

# create a set of identifiers (train/test)
train_test <- c(rep(1,length=trunc((2/3)*nrow(dodger.data))),
                rep(2,length=(nrow(dodger.data) - trunc((2/3)*nrow(dodger.data))
                             )))

# draw a sample from identifiers and apply to our dataset
dodger.data$train_test <- sample(train_test)

# put labels on identifier
dodger.data$train_test <- factor(dodger.data$train_test, levels=c(1,2), labels=c("TRAIN","TEST"))

# split data int train and test sets
train_set <- subset(dodger.data, train_test == "TRAIN")
test_set <- subset(dodger.data, train_test == "TEST")

# train our model
train.model <- lm('attend ~ mon_ordered + dow_ordered + bobblehead', data = train_set)

# summarize model (tuesdays in june!)
summary(train.model)

# run predict on training model and add predictes values to training set
train_set$predict <- predict(train.model)

# run predict on training model, passing new data and add fitted values to training set
test_set$predict <- predict(train.model, newdata = test_set)

# response variance, rounded
round((with(test_set,cor(attend,predict)^2)),digits=3)

# plot pairs of variables using xyplot
dodger_plotter <- rbind(train_set, test_set)
group.labels <- c("No Bobblehead","Bobblehead")
group.symbols <- c(21,24)
group.colors <- c("black","black")
group.fill <- c("yellow","dodgerblue")
xyplot(predict ~ attend | train_test,
       data = dodger_plotter, groups = bobblehead, cex = 2, 
       pch = group.symbols, col = group.colors, fill = group.fill,
       layout = c(2,1), xlim = c(20000,65000), ylim = c(20000,65000),
       aspect=1, type = c("p","g"), 
       panel=function(x,y, ...)
        {panel.xyplot(x,y,...)
          panel.segments(25000,25000,60000,60000,col="black", cex=2)
         },
       strip=function(...) strip.default(...,style=1),
       xlab = "Actual Attendance", 
       ylab = "Predicted Attendance",
       key = list(space = "top",
                  text = list(rev(group.labels), col = rev(group.colors)),
                  points = list(pch = rev(group.symbols), 
                  col = rev(group.colors),
                  fill = rev(group.fill))))

# apply model to full data set (tuesdays in june again)
my.model <- lm('attend ~ mon_ordered + dow_ordered + bobblehead', data = dodger.data)

# summarize model
summary(my.model)

# Explore some plots
plot(my.model)

### different model
dodger.data.2 <- readr::read_csv("dodgers.csv")

# created ordered factors for day of week and month
dodger.data.2$dow_ordered <- with(data=dodger.data.2, 
                                ifelse((day_of_week == "Monday"),1,
                                 ifelse((day_of_week == "Tuesday"),2,
                                  ifelse((day_of_week == "Wednesday"),3,
                                   ifelse((day_of_week == "Thursday"),4,
                                    ifelse((day_of_week == "Friday"),5,
                                     ifelse((day_of_week == "Saturday"),6,7)))))))


dodger.data.2$mon_ordered <- with(data=dodger.data.2, 
                                ifelse((month == "MAR"),3,
                                 ifelse((month == "APR"),4,
                                  ifelse((month == "MAY"),5,
                                   ifelse((month == "JUN"),6,
                                    ifelse((month == "JUL"),7,
                                     ifelse((month == "AUG"),8,
                                      ifelse((month == "SEP"),9,
                                       ifelse((month == "OCT"),10,11)))))))))

# put labels on factors
dodger.data.2$dow_ordered <- factor(dodger.data.2$dow_ordered, levels = 1:7, labels=c("Mon","Tue","Wed", "Thur",
                                                                                  "Fri","Sat","Sund"))
dodger.data.2$mon_ordered <- factor(dodger.data.2$mon_ordered, levels = 3:11, labels=c("March","April","May","June","July",
                                                                                   "Aug","Sept","Oct","Nov"))


train_index <- sample(1:nrow(dodger.data.2), 0.8 * nrow(dodger.data.2))
test_index <- setdiff(1:nrow(dodger.data.2), train_index)


# Build X_train, y_train, X_test, y_test
#X_train <- dodger.data.2[train_index,] %>% select(mon_ordered,dow_ordered,day_night,bobblehead)
XX_train <- dodger.data.2[train_index,]
y_train <- dodger.data.2[train_index,]

#X_test <- dodger.data.2[test_index,] %>% select(month,day_of_week,opponent,day_night,fireworks,bobblehead)
XX_test <- dodger.data.2[test_index,]
y_test <- dodger.data.2[test_index,]

model2 <- glm(attend ~ month + day_of_week + day_night + bobblehead, data = XX_train)
summary(model2)

XX_train$predict <- predict(model2)
XX_test$predict <- predict(model2, newdata = XX_test)

model2 %>% augment() %>% 
  ggplot()  + 
  geom_point(aes(.fitted, attend)) + 
  geom_smooth(aes(.fitted, attend), method = "lm", se = FALSE, color = "lightgrey") + 
  labs(x = "Actual", y = "Fitted")


dodger.data.2$predict <- predict(model)

#model3 <- glm(attend ~ month + day_of_week + day_night + bobblehead, data = dodger.data.2)
model3 <- glm(attend ~ month + day_of_week + bobblehead, data = dodger.data.2)
summary(model3)
model3 %>% augment()


dodger_plotter2 <- rbind(train_set, test_set)
group.labels <- c("No Bobblehead","Bobblehead")
group.symbols <- c(21,24)
group.colors <- c("black","black")
group.fill <- c("yellow","dodgerblue")
xyplot(predict ~ attend | train_test,
       data = dodger_plotter2, groups = bobblehead, cex = 2, 
       pch = group.symbols, col = group.colors, fill = group.fill,
       layout = c(2,1), xlim = c(20000,65000), ylim = c(20000,65000),
       aspect=1, type = c("p","g"), 
       panel=function(x,y, ...)
       {panel.xyplot(x,y,...)
         panel.segments(25000,25000,60000,60000,col="black", cex=2)
       },
       strip=function(...) strip.default(...,style=1),
       xlab = "Actual Attendance", 
       ylab = "Predicted Attendance",
       key = list(space = "top",
                  text = list(rev(group.labels), col = rev(group.colors)),
                  points = list(pch = rev(group.symbols), 
                                col = rev(group.colors),
                                fill = rev(group.fill))))


dodger_plotter2$attend_diff <- dodger_plotter2$attend - dodger_plotter2$predict
#(which.max(dodger_plotter2$attend_diff) && 
subset(dodger_plotter2, bobblehead=="YES") %>% slice(which.max(attend_diff))

