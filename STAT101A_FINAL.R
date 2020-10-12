#read csv
covid <- read.csv('kolko_covid_shareable_dataset.csv')

#dataset simplification
covid <- covid[, -c(6,7)]
covid <- covid[, -5]
covid <- covid[, -c(1, 2, 3)]
covid <- covid[, -c(1, 14)]

#creating categorical variable
summary(covid$hhsize)
hhsize_c <- character(nrow(covid))

for (i in 1:nrow(covid)) {
  if (!is.na(covid$hhsize[i])) {
    if (covid$hhsize[i] <= 2.35) {
      hhsize_c[i] <- "Small"
    } else if (covid$hhsize[i] > 2.35 && covid$hhsize[i] < 2.63) {
      hhsize_c[i] <- "Mid"
    } 
    else if (covid$hhsize[i] >= 2.63 ) {
      hhsize_c[i] <- "Large"
    }
  }
}

hhsize_c <- as.factor(hhsize_c)
table(hhsize_c)



#Change hhsize into cateogiral and delete original hhsize
covid$hhsize_c <- hhsize_c
covid <- covid[, -7]  




#backwards elimination
#install.packages('leaps')
library(leaps)
b <- regsubsets(x = pcases ~ ., data = covid, y = covid$pcases, method= "backward")
summary(b)

#forward model
f <- regsubsets(x = pcases ~ ., data = covid, y = covid$pcases, method= "forward")
summary(f)

covidf <- covid[, c(11,3, 8, 12, 17, 18, 20, 21, 23, 25)] #significant cols
covidb <- covid[, c(11,3, 8, 12,17, 18, 20, 21, 23)] #significant cols



#COVID BACKWARDS MODEL ADJUSTMENTS
########
#logarithmic transformations (covidb)
library(car)
symbox(~pcases, data = covid)
covidb$pcases <- log(covid$pcases)

symbox(~dist, data = covid)
covidb$dist = log(covid$dist)

symbox(~black_pct, data = covid)
covidb$black_pct <- log(covid$black_pct)

symbox(~hisp_pct, data = covid)
covidb$hisp_pct <- log(covid$hisp_pct)

symbox(~wfh_share, data = covid)
covidb$wfh_share <- log(covid$wfh_share)

symbox(~italy_born, data = covid)
covidb$italy_born <- log(covid$italy_born)

symbox(~transit_modeshare, data = covid)
covidb$transit_modeshare <- log(covid$transit_modeshare)

symbox(~age60plus, data = covid)
covid$age60plus <- log(covid$age60plus)


#removing NA vals
#install.packages('IDPmisc')
library(IDPmisc)
covidb <- NaRV.omit(covidb)

#creating backwards-elim mlr model
modelb = lm(formula = pcases ~ temp_mar20+dist+age60plus+black_pct+hisp_pct+wfh_share+italy_born+transit_modeshare+hhsize_c*age60plus, data = covid)

summary(modelb)


library(car)
library(effects)

install.packages("effects")
library(effects)
plot(allEffects(modelb),ask=FALSE)

#Check if there the interaction effect is significant or not 
model_interaction <- lm(pcases~hhsize_c+age60plus+hhsize_c*age60plus,data=covid)
plot(allEffects(model_interaction),ask=FALSE)
summary(model_interaction)



#need to remove all non statistically-significant data 
modelb = lm(formula = pcases ~ temp_mar20 + dist + black_pct + hisp_pct + transit_modeshare + interaction, data = covidb)

#rewritten modelb w/ all sig data
summary(modelb)
#plot(modelb)

#COVID FORWARD MODEL ADJUSTMENTS
####
#same process for forward process (covidf)
#logarithmic transformations
#symbox(...)
#needed to log following values
covidf$pcases <- log(covid$pcases)
covidf$dist = log(covid$dist)
covidf$age60plus = log(covid$age60plus)
covidf$black_pct <- log(covid$black_pct)
covidf$hisp_pct <- log(covid$hisp_pct)
covidf$wfh_share <- log(covid$wfh_share)
covidf$italy_born <- log(covid$italy_born)
covidf$transit_modeshare <- log(covid$transit_modeshare)

#removing NA vals
#install.packages('IDPmisc')
library(IDPmisc)
covidf <- NaRV.omit(covidf)

#creating forwards-elim mlr model
modelf = lm(formula = pcases ~ ., data = covidf)

summary(modelf)
#need to remove all non-statistically significant data
modelf = lm(formula = pcases ~ temp_mar20 + dist + black_pct + hisp_pct + transit_modeshare + hhsize_c, data = covidf)
#rewritten modelf with all sig data
summary(modelf)
#plot(modelf)

###
covidf$italy_born <- covid$italy_born
covidf$italy_born <- NaRV.omit(covidf$italy_born)
model2 <- lm(formula = pcases ~ temp_mar20 + dist + black_pct + hisp_pct + transit_modeshare + hhsize_c+italy_born, data = covidf)
summary(model2)
View(covidf)



####CORRELATION MATRIX W/ BOTH MODELS
cmat <- cor(cbind(fitted(modelb),fitted(modelf)))
rownames(cmat) <- colnames(cmat) <-c("backward","forward")
cmat
#The correlation between the backwards and forwards model is nearly perfect
#thus either model works equally well in terms of predictive power

#WE WILL NOW DO CROSS-VALIDATION ON OUR BACKWARD MODEL (either model works)


#CROSS VALIDATION STEP:
#CREATING TRAINING SET AND TEST SET
######
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
split = sample.split(covidb$pcases, SplitRatio = 0.7)
training_set = subset(covidb, split == TRUE)
test_set = subset(covidb, split == FALSE)

modelb = lm(formula = pcases ~ temp_mar20 + dist + black_pct + hisp_pct + transit_modeshare + interaction, data = training_set)
#revised modelb on just the training_set
y_pred = predict(modelb, newdata = test_set)
cor(y_pred, test_set$pcases)
#Correlation between pcases computed on the base of the training model and the actual pcases in the testing sample is pretty high indicating good prediction power.

#CROSS VALIDATION STEP:
#CREATING TRAINING SET AND TEST SET
######
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
split = sample.split(covidf$pcases, SplitRatio = 0.7)
training_set = subset(covidf, split == TRUE)
test_set = subset(covidf, split == FALSE)
modelf = lm(formula = pcases ~ ., data = training_set)
#revised modelb on just the training_set
y_pred = predict(modelf, newdata = test_set)
cor(y_pred, test_set$pcases)
#Correlation between pcases computed on the base of the training model and the actual pcases in the testing sample is pretty high indicating good prediction power.
#final plot
plot(modelf)

summary(modelf)



## LOGISTICS MODEL
summary(covid$pcases)
covid$pcasesCut <- cut(covid$pcases,br=c(0,85.4,12087.9),labels= c(0,1),right=F)
m1<-glm(pcasesCut~dist, family=binomial,data= covid)
summary(m1)
plot(m1)
hist(covid$dist)
dist_log <- log(covid$dist)
hist(dist_log)

