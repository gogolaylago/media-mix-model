# disable scientific notation because it's annoying
options(scipen=999, digits=5)

#  Load data
df <- read.csv(file = "data.csv", header=TRUE, sep=",")

# # 75% of the sample size
# smp_size <- floor(0.75 * nrow(df))
# 
# # set the seed to make your partition reproductible
# # set.seed(123)
# train_ind <- sample(seq_len(nrow(df)), size = smp_size)
# 
# train <- df[df_ind, ]
# test <- df[-df_ind, ]

# create a scatter plot matrix
pairs(df[,!(colnames(df)=="Week")])

# run all predictors on sales
mod0 <- lm(Units.Sold~., data=df[,!(colnames(df)=="Week")])
summary(mod0)

# check for potential collinearity by calculating the variance inflation factor:
# vif = the ratio of the variance of a predictor when fitting the full model / the variance of a predictor if fit on its own
# rule of thumb: if vif exceeds 5 or 10 then collinearity may exist
library(car) # the car package has the vif function
vif(mod0)

# adding adstock to predictors
# an adstock rate or 50% assumes that 
# the marketing effect of today's ad will reduce by half tomorrow.
# formula: tomorrow's advertising impact = base + adstockRate * today's advertising impact
predictors <- df[,!(colnames(df) %in% c("Week","Units.Sold"))]

displayAdstock <- 0.5898
# searchAdstock <- 0
# adstockRate <- 0.50
# for(i in colnames(predictors)) {
#   predictors[i] <- as.numeric(filter(predictors[i], filter=adstockRate, method="recursive"))
# }
predictors$Display <- as.numeric(filter(predictors$Display, filter=displayAdstock, method="recursive"))
# predictors$Search <- as.numeric(filter(predictors$Search, filter=searchAdstock, method="recursive"))


burt <- predictors
burt$Units.Sold <- df$Units.Sold

# run all predictors on sales after accounting for the adstock rate
mod1<-lm(Units.Sold~., data=burt)
summary(mod1)

# # stepwise regression: backward vs. forward
# mod2 = step(mod1, direction="backward", trace=0)
# summary(mod2)

# create new dataset with predictors with significant p-value
pVal <- 0.05
newBurt <- burt[, colnames(burt) %in% c(names(summary(mod1)$coefficients[summary(mod1)$coefficients[, 4]<= pVal, 4]), "Units.Sold")]

mod2<-lm(Units.Sold~., data=newBurt)
summary(mod2)

mod3<-lm(Units.Sold~.^2, data=newBurt)
summary(mod3)

# model selection: can first use anova to see if the two models are significantly different from each other (p-value < 0.05)
anova(mod2, mod3)
# model selection: pick the one with higher R Squared and RSE
summary(mod2)$sigma # calculate RSE. RSE is the phrase in R language, same as RMSE.
summary(mod3)$sigma

# creating a 3D scatter plot
require(scatterplot3d)
s3d <- scatterplot3d(newBurt,
                     pch=16,
                     highlight.3d = TRUE,
                     type = "h",
                     main = "3D Scatterplot")
plane <- lm(Units.Sold~., data=newBurt)
s3d$plane3d(plane)


# predict random unknown data with mod3
totalImp <- 1000000000
Display <- seq(0, totalImp, by = 50000000)
Search <- totalImp-Display

newdata <- data.frame(Display = Display, Search = Search)
predictions <- data.frame(predict(mod3, newdata, interval="prediction",level=0.95),newdata)

