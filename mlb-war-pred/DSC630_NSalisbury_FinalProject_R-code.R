library(tidyverse)



# load data
hitData <- read_csv(file = 'C:\\cygwin64\\home\\salis\\git\\repos\\DSC630\\Final-Project\\all_hit_data.csv')
hitData
hitData[c('1B','3B','WAR')]
modelData <- subset(hitData, select=c('Age','WAR','AB_3','AVG_3','BABIP_3','BB%_3','BB/K_3','Contact%_3','FB%_3','G_3',
                         'GB%_3','GDP_3','H_3','HBP_3','HR_3','HR/FB_3','Hard%_3','IFFB%_3','ISO_3',
                         'K%_3','LD%_3','OPS_3','PA_3','R_3','RAR_3','RBI_3','SB_3','SF_3','SH_3',
                         'SO_3','WAR_3','wOBA_3','wRAA_3','wRC_3','AB_2','AVG_2','BABIP_2','BB%_2',
                         'BB/K_2','Contact%_2','FB%_2','G_2','GB%_2','GDP_2','H_2','HBP_2','HR_2',
                         'HR/FB_2','Hard%_2','IFFB%_2','ISO_2','K%_2','LD%_2','OPS_2','PA_2','R_2',
                         'RAR_2','RBI_2','SB_2','SF_2','SH_2','SO_2','WAR_2','wOBA_2','wRAA_2','wRC_2',
                         'AB_1','AVG_1','BABIP_1','BB%_1','BB/K_1','Contact%_1','FB%_1','G_1','GB%_1',
                         'GDP_1','H_1','HBP_1','HR_1','HR/FB_1','Hard%_1','IFFB%_1','ISO_1','K%_1',
                         'LD%_1','OPS_1','PA_1','R_1','RAR_1','RBI_1','SB_1','SF_1','SH_1','SO_1',
                         'WAR_1','wOBA_1','wRAA_1','wRC_1'))

# fill missing values with mean
modelData <- modelData %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) 

modelData

# built train/test

set.seed(100)
trainRows <- sample(1:nrow(modelData), 0.8*nrow(modelData))
trainingData <- modelData[trainRows,]
testData <- modelData[-trainRows,]

trainingData
testData

# build model (use top 10 features from python code)
lmMod <- lm(WAR ~ RAR_3 + WAR_3 + WAR_2 + OPS_1 + R_1 + RAR_1 + WAR_1 + wRAA_1 + wRC_1, data=trainingData) 

cor(trainingData)

summary(trainingData)
trainingData[i]
trainingData

# predict
distPred <- predict(lmMod, testData[c('RAR_3','WAR_3','RAR_2','WAR_2','OPS_1','R_1','RAR_1','WAR_1','wRAA_1','wRC_1')])


# Summarize results
summary(lmMod)
AIC (lmMod) 

actuals_preds <- data.frame(cbind(actuals=testData$WAR, predicteds=distPred))
head(actuals_preds)
actuals_pred