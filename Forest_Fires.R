
# Load libraries and forest 

library(plyr); library(dplyr)
library(tidyverse)
library(naniar)
library(mapview)
library(SpatialEpi)

forest = read.csv('forest.csv')




# Evaluate missing forest 


forest %>% is.na() %>% colSums()

gg_miss_var(forest)


# Exploratory forest Analysis 

# Re factor character variables and change to numerical

forest$month = factor(forest$month,levels = c('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec'))
levels(forest$month)

forest$day = factor(forest$day,levels = c('sun','mon','tue','wed','thur','fri','sat'))
levels(forest$day)



#Correlation of numeric variables

library(ggcorrplot)

numeric = select(forest,-c(month,day))

corr = round(cor(numeric),2)
ggcorrplot(corr)


# Check for multicolinearity and drop variables with > 20 VIF

library(faraway)


vif(numeric)

# Multicolinearity does not seem to be present bas3ed on VIF
# VIF < 5 cutoff used


# Split training and test forest

library(faraway)

set.seed(6021) ##for reproducibility to get the same split
sample<-sample.int(nrow(forest), floor(.50*nrow(forest)), replace = F)
train<-forest[sample, ] ##training forest frame
test<-forest[-sample, ] ##test forest frame



pairs(train,lower.panel = NULL)


# Automated Search procedures to find the right model

library(leaps)

#Set up full and intercept only regression models

full_model = lm(area~.,data = train) 

intercept_only = lm(area~1,data = train)

# Run all possible regressions

all_regressions = regsubsets(area~.,data = train,nbest=1)
summary(all_regressions)


# Coefficients of best ,models based on ADJ-R2 , CP, and BIC

coef(all_regressions, which.max((summary(all_regressions)$adjr2)))

coef(all_regressions,which.min(summary(all_regressions)$cp))

coef(all_regressions,which.min(summary(all_regressions)$bic))




# Forward Selection 

step(intercept_only,scope = list(lower=intercept_only,upper=full_model),direction='forward')


# Fit the model 

model = lm(data=train,area~.)

summary(model)



# Check Model assumptions

# Confidence interval for the mean response

confint(model,level = 0.95)

# Fitted Values & Residuals

yhat = model$fitted.values
residuals = model$residuals

train2 = data.frame(yhat,residuals)  # was not able to add to original forest frame because of differing lengths

ggplot(train2,aes(x=yhat,y=residuals))+
  geom_point()+
  geom_hline(yintercept=0,color='red')+
  labs(x='Fitted Y',y='Residuals',title= 'Residual Plot')




qqnorm(residuals)
qqline(residuals,color='red')



# ACF Plot

acf(residuals,main='ACF')



# Transform Y 

ystar = log(train$area+1)
train = data.frame(train,ystar)

model_ystar = lm(ystar~.,data=train)

# new plots

yhat2 = model_ystar$fitted.values
residuals2 = model_ystar$residuals

train2 = data.frame(train2,yhat2,residuals2)


ggplot(train2,aes(x=yhat2,y=residuals2))+
  geom_point()+
  geom_hline(yintercept=0,color='red')+
  labs(x='Fitted Y',y='Residuals',title= 'Transformed Residual Plot')


# QQ plot

qqnorm(residuals)
qqline(residuals,color='red')


## Model overfit, needs more analysis ##










###############################################################################
# Geospatial Analysis
###############################################################################


# mapView(forest, xcol = "X", ycol = "Y", crs = 4269, grid = FALSE)
# 
# 
# coord = forest %>%
#   select(X,Y)
# 
# lat_long = grid2latlong(coord)
# 
# mapView(lat_long, xcol = "x", ycol = "y", crs = 4269, grid = FALSE)


###############################################################################