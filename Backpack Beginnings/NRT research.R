library(readxl)
library(randomForest)
library(car)
library(plot3D)
library(plot3Drgl)

set.seed(79971319)
p <- 6 # number of predictors

# Read data sets
gcs1617 <- read_excel("C:/Users/oldir/Desktop/ISEN 794 - Masters Supervised Research (NRT Research)/External Data/Guilford County Schools Data for R.xlsx", sheet = "2016-2017 Condensed")
View(gcs1617)

train.dat1617 <- gcs1617[1:20,]
prediction.dat1617 <- gcs1617[-(1:20),]
n1 <- nrow(train.dat1617)


gcs1718 <- read_excel("C:/Users/oldir/Desktop/ISEN 794 - Masters Supervised Research (NRT Research)/External Data/Guilford County Schools Data for R.xlsx", sheet = "2017-2018 Condensed")
View(gcs1718)

train.dat1718 <- gcs1718[1:21,]
prediction.dat1718 <- gcs1718[-(1:21),]
n2 <- nrow(train.dat1718)


gcscombined <- read_excel("C:/Users/oldir/Desktop/ISEN 794 - Masters Supervised Research (NRT Research)/External Data/Guilford County Schools Data for R.xlsx", sheet = "Combined Condensed")
View(gcscombined)

train.datcombined <- gcscombined[1:41,]
prediction.datcombined <- gcscombined[-(1:41),]
n3 <- nrow(train.datcombined)


# Current trend analysis
train.FBPTotal <- NULL
train.PctEDS <- NULL
train.PctSevAbs <- NULL
train.PctMinority <- NULL
train.TitleI_NoYes <- NULL
train.PctLowInc <- NULL

for (i in 1:n2) {
  train.FBPTotal[i] <- sum(c(train.dat1718[i,]$FBPTotal,
                        gcs1617[pmatch(train.dat1718[i,]$School,
                                             gcs1617$School),]$FBPTotal))
  train.PctEDS[i] <- mean(c(train.dat1718[i,]$PctEDS,
                        gcs1617[pmatch(train.dat1718[i,]$School,
                                       gcs1617$School),]$PctEDS))
  train.PctSevAbs[i] <- mean(c(train.dat1718[i,]$PctSevAbs,
                            gcs1617[pmatch(train.dat1718[i,]$School,
                                           gcs1617$School),]$PctSevAbs))
  train.PctMinority[i] <- mean(c(train.dat1718[i,]$PctMinority,
                            gcs1617[pmatch(train.dat1718[i,]$School,
                                           gcs1617$School),]$PctMinority))
  train.TitleI_NoYes[i] <- mean(c(train.dat1718[i,]$TitleI_NoYes,
                            gcs1617[pmatch(train.dat1718[i,]$School,
                                           gcs1617$School),]$TitleI_NoYes))
  train.PctLowInc[i] <- mean(c(train.dat1718[i,]$PctLowInc,
                             gcs1617[pmatch(train.dat1718[i,]$School,
                                            gcs1617$School),]$PctLowInc))
}



train.FBPtable <- data.frame(School = train.dat1718$School, SumFBPTotal = train.FBPTotal, AvgPctEDS = train.PctEDS, AvgPctSevAbs = train.PctSevAbs, RurUrb = train.dat1718$RurUrb, AvgPctMinority = train.PctMinority, AvgTitleI_NoYes = train.TitleI_NoYes, AvgPctLowInc = train.PctLowInc)
train.FBPtable <- train.FBPtable[order(-train.FBPtable$SumFBPTotal),]

scatter2D(train.FBPtable$AvgPctMinority, train.FBPtable$SumFBPTotal, colvar = train.FBPtable$SumFBPTotal, xlab = "AvgPctMinority", ylab = "SumFBPTotal", pch = 16, bty ="n", type ="p", clab = "FBPTotal")
scatter2D(train.FBPtable$AvgPctLowInc, train.FBPtable$SumFBPTotal, colvar = train.FBPtable$SumFBPTotal, xlab = "AvgPctLowInc", ylab = "SumFBPTotal", pch = 16, bty ="n", type ="p", clab = "FBPTotal")

row.names(train.FBPtable) <- NULL
train.FBPtable$SumFBPTotal <- NULL
train.FBPtable
write.table(train.FBPtable, file = "TrendAnalysis.csv", sep=",", row.names = FALSE)




# 2016-2017 data analysis
# Full model with all main predictors
fullmodel1 <- lm(Score ~ PctEDS+PctSevAbs+RurUrb+PctMinority+TitleI_NoYes+PctLowInc, data = train.dat1617)

# Diagnostic check for full model
par(mfrow = c(2,2))
plot(fullmodel1)
round(cor(train.dat1617[,-1]), 2)
vif(fullmodel1)

# Remove PctMinority due to collinearity
fullmodel1a <- update(fullmodel1, .~. -PctMinority)
vif(fullmodel1a)
plot(fullmodel1a)

# Backward variable selection process
summary(fullmodel1a)
summary(update(fullmodel1a, .~. -TitleI_NoYes))
summary(update(fullmodel1a, .~. -TitleI_NoYes-PctEDS))
summary(update(fullmodel1a, .~. -TitleI_NoYes-PctEDS-PctSevAbs))
summary(update(fullmodel1a, .~. -TitleI_NoYes-PctEDS-PctSevAbs-RurUrb))

# Backward Selection with second-order "corrected" Akaike Information Criterion (AICc)
step(fullmodel1a, direction = "backward", k = 2*(n1/(n1-p-1)))

# Forward variable selection process
nullmodel1 <- lm(Score ~ 1, data = train.dat1617)
summary(nullmodel1)

summary(update(nullmodel1, .~. +PctEDS))
summary(update(nullmodel1, .~. +PctSevAbs))
summary(update(nullmodel1, .~. +RurUrb))
summary(update(nullmodel1, .~. +TitleI_NoYes))
summary(update(nullmodel1, .~. +PctLowInc)) #include

summary(update(nullmodel1, .~. +PctLowInc+PctEDS))
summary(update(nullmodel1, .~. +PctLowInc+PctSevAbs))
summary(update(nullmodel1, .~. +PctLowInc+RurUrb))
summary(update(nullmodel1, .~. +PctLowInc+TitleI_NoYes))

# Forward Selection with AICc
step(nullmodel1, scope = list(lower = nullmodel1, upper = fullmodel1a), direction = "forward", k = 2*(n1/(n1-p-1)))

# Stepwise Selection with AICc (considering all combinations of interaction effects)
step(fullmodel1a, scope = list(lower = nullmodel1, upper = ~PctEDS*PctSevAbs*RurUrb*TitleI_NoYes*PctLowInc), k = 2*(n1/(n1-p-1)), direction = "both")

# Final 2016-2017 model
finalmodel1617 <- lm(Score ~ PctLowInc, data = train.dat1617)

# Diagnostic checks for final 2016-2017 model
plot(finalmodel1617)
par(mfrow = c(1,1))
summary(finalmodel1617)

# Predictions for final model
pred1617 <- predict.lm(finalmodel1617, newdata = prediction.dat1617)
pred1617a <- data.frame(School = prediction.dat1617[,1], PredictedScore = pred1617)
(pred1617a <- pred1617a[order(-pred1617a$PredictedScore),])
hist(pred1617a$PredictedScore, main="Final Model, 2016-2017 Prediction Data", xlab="Scores")
write.table(pred1617a, file = "Predictions1617.csv", sep=",", row.names = FALSE)

# 2016-2017 data visualization
pred1617_best <- pred1617a[order(pred1617a$School),]
predictiondat1617order <- prediction.dat1617[order(prediction.dat1617$School),]
finalcols1617 <- c("Level", "PctLowInc")
predictiondat1617order2 <- predictiondat1617order[finalcols1617]
finaldata1617 <- cbind(pred1617_best, predictiondat1617order2)
finaldata1617 <- finaldata1617[order(-finaldata1617$PredictedScore),]

# 2D training data scatterplot
scatter2D(train.dat1617$PctLowInc, train.dat1617$Score, colvar = train.dat1617$Score, main = "2016-2017 Training Data Scatterplot", xlab = "PctLowInc", ylab = "Score", pch = 16, bty ="n", type ="p", clab = "Score")
abline(finalmodel1617, col = "red")

# 2D prediction data scatterplot of predictions
with(finaldata1617, text2D(PctLowInc, PredictedScore, 
                           labels = School, colvar = Level, 
                           xlab = "PctLowInc", ylab = "PredictedScore", 
                           main = c("2016-2017 Prediction Data:", "Scatterplot by School Name"), cex = 0.6, 
                           clab = "Level", adj = 0.5, font = 2))

# Histogram of PctLowInc
hist(finaldata1617$PctLowInc, main="2016-2017 Prediction Data:\nHistogram of PctLowInc", xlab="PctLowInc")
###############################################################################################




# 2017-2018 data analysis
# Full model with all main predictors
fullmodel2 <- lm(Score ~ PctEDS+PctSevAbs+RurUrb+PctMinority+TitleI_NoYes+PctLowInc, data = train.dat1718)

# Diagnostic check for full model
par(mfrow = c(2,2))
plot(fullmodel2)
round(cor(train.dat1718[,-1]), 2)
vif(fullmodel2)

# Remove PctMinority due to collinearity
fullmodel2a <- update(fullmodel2, .~. -PctMinority)
vif(fullmodel2a)
plot(fullmodel2a)

# Backward variable selection process
summary(fullmodel2a)
summary(update(fullmodel2a, .~. -PctEDS))
summary(update(fullmodel2a, .~. -PctEDS-PctLowInc))
summary(update(fullmodel2a, .~. -PctEDS-PctLowInc-RurUrb))
summary(update(fullmodel2a, .~. -PctEDS-PctLowInc-RurUrb-TitleI_NoYes))

# Backward Selection with AICc
step(fullmodel2a, direction = "backward", k = 2*(n2/(n2-p-1)))

# Forward variable selection process
nullmodel2 <- lm(Score ~ 1, data = train.dat1718)
summary(nullmodel2)

summary(update(nullmodel2, .~. +PctEDS))
summary(update(nullmodel2, .~. +PctSevAbs))
summary(update(nullmodel2, .~. +RurUrb))
summary(update(nullmodel2, .~. +TitleI_NoYes))
summary(update(nullmodel2, .~. +PctLowInc)) #include

summary(update(nullmodel2, .~. +PctLowInc+PctEDS))
summary(update(nullmodel2, .~. +PctLowInc+PctSevAbs))
summary(update(nullmodel2, .~. +PctLowInc+RurUrb))
summary(update(nullmodel2, .~. +PctLowInc+TitleI_NoYes))

# Forward Selection with AICc
step(nullmodel2, scope = list(lower = nullmodel2, upper = fullmodel2), direction = "forward", k = 2*(n2/(n2-p-1)))

# Stepwise Selection with AICc (considering all combinations of interaction effects)
step(fullmodel2a, scope = list(lower = nullmodel2, upper = ~PctEDS*PctSevAbs*RurUrb*TitleI_NoYes*PctLowInc), k = 2*(n2/(n2-p-1)), direction = "both")

# Random Forest analysis (starting with full model)
print(randomForest(Score ~ PctEDS+PctSevAbs+RurUrb+TitleI_NoYes+PctLowInc, data = train.dat1718))
print(randomForest(Score ~ PctSevAbs + TitleI_NoYes, data = train.dat1718))
print(randomForest(Score ~ PctLowInc, data = train.dat1718))
print(randomForest(Score ~ PctSevAbs, data = train.dat1718))


# Final 2017-2018 models
finalmodel1718_1 <- lm(Score ~ PctSevAbs + TitleI_NoYes, data = train.dat1718)
finalmodel1718_2 <- lm(Score ~ PctLowInc, data = train.dat1718)
finalmodel1718_3 <- lm(Score ~ PctSevAbs, data = train.dat1718)

# Diagnostic checks for final 2017-2018 models
plot(finalmodel1718_1)
plot(finalmodel1718_2)
plot(finalmodel1718_3)
par(mfrow = c(1,1))

# Linear regression output for full model and final 2017-2018 models
summary(fullmodel2a)
summary(finalmodel1718_1)
summary(finalmodel1718_2)
summary(finalmodel1718_3)

# Predictions for final models
pred1718_1 <- predict.lm(finalmodel1718_1, newdata = prediction.dat1718)
pred1718_1a <- data.frame(School = prediction.dat1718[,1], PredictedScore = pred1718_1)
(pred1718_1a <- pred1718_1a[order(-pred1718_1a$PredictedScore),])
hist(pred1718_1a$PredictedScore, main="Final Model 1, 2017-2018 Prediction Data", xlab="Scores")
write.table(pred1718_1a, file = "Predictions1718_1.csv", sep=",", row.names = FALSE)

pred1718_2 <- predict.lm(finalmodel1718_2, newdata = prediction.dat1718)
pred1718_2a <- data.frame(School = prediction.dat1718[,1], PredictedScore = pred1718_2)
(pred1718_2a <- pred1718_2a[order(-pred1718_2a$PredictedScore),])
hist(pred1718_2a$PredictedScore, main="Final Model 2, 2017-2018 Prediction Data", xlab="Scores")
write.table(pred1718_2a, file = "Predictions1718_2.csv", sep=",", row.names = FALSE)

pred1718_3 <- predict.lm(finalmodel1718_3, newdata = prediction.dat1718)
pred1718_3a <- data.frame(School = prediction.dat1718[,1], PredictedScore = pred1718_3)
(pred1718_3a <- pred1718_3a[order(-pred1718_3a$PredictedScore),])
hist(pred1718_3a$PredictedScore, main="Final Model 3, 2017-2018 Prediction Data", xlab="Scores")
write.table(pred1718_3a, file = "Predictions1718_3.csv", sep=",", row.names = FALSE)

# The choice of best linear regression model for 2017-2018 training data:
# Score ~ PctLowInc
bestmodel1718 <- finalmodel1718_2

# 2017-2018 data visualization
pred1718_best <- pred1718_2a[order(pred1718_2a$School),]
predictiondat1718order <- prediction.dat1718[order(prediction.dat1718$School),]
finalcols1718 <- c("Level", "PctLowInc")
predictiondat1718order2 <- predictiondat1718order[finalcols1718]
finaldata1718 <- cbind(pred1718_best, predictiondat1718order2)
finaldata1718 <- finaldata1718[order(-finaldata1718$PredictedScore),]

# 2D training data scatterplot
scatter2D(train.dat1718$PctLowInc, train.dat1718$Score, colvar = train.dat1718$Score, main = "2017-2018 Training Data Scatterplot", xlab = "PctLowInc", ylab = "Score", pch = 16, bty ="n", type ="p", clab = "Score")
abline(bestmodel1718, col = "red")

# 2D prediction data scatterplot of predictions
with(finaldata1718, text2D(PctLowInc, PredictedScore, 
                           labels = School, colvar = Level, 
                           xlab = "PctLowInc", ylab = "PredictedScore", 
                           main = c("2017-2018 Prediction Data:", "Scatterplot by School Name"), cex = 0.6, 
                           clab = "Level", adj = 0.5, font = 2))

# Histogram of PctLowInc
hist(finaldata1718$PctLowInc, main="2017-2018 Prediction Data:\nHistogram of PctLowInc", xlab="PctLowInc")
###############################################################################################



# Combined data analysis
# Full model with all main predictors
fullmodel3 <- lm(Score ~ PctEDS+PctSevAbs+RurUrb+PctMinority+TitleI_NoYes+PctLowInc, data = train.datcombined)

# Diagnostic check for full model
par(mfrow = c(2,2))
plot(fullmodel3)
round(cor(train.datcombined[,-1]), 2)
vif(fullmodel3)

# Backward variable selection process
summary(fullmodel3)
summary(update(fullmodel3, .~. -PctEDS))
summary(update(fullmodel3, .~. -PctEDS-PctSevAbs))
summary(update(fullmodel3, .~. -PctEDS-PctMinority-TitleI_NoYes))
summary(update(fullmodel3, .~. -PctEDS-PctMinority-TitleI_NoYes-PctSevAbs))

# Backward Selection with AICc
step(fullmodel3, direction = "backward", k = 2*(n3/(n3-p-1)))

# Forward variable selection process
nullmodel3 <- lm(Score ~ 1, data = train.datcombined)
summary(nullmodel3)

summary(update(nullmodel3, .~. +PctEDS))
summary(update(nullmodel3, .~. +PctSevAbs))
summary(update(nullmodel3, .~. +RurUrb))
summary(update(nullmodel3, .~. +PctMinority))
summary(update(nullmodel3, .~. +TitleI_NoYes))
summary(update(nullmodel3, .~. +PctLowInc)) # include

summary(update(nullmodel3, .~. +PctLowInc+PctEDS))
summary(update(nullmodel3, .~. +PctLowInc+PctSevAbs))
summary(update(nullmodel3, .~. +PctLowInc+RurUrb)) #include
summary(update(nullmodel3, .~. +PctLowInc+PctMinority))
summary(update(nullmodel3, .~. +PctLowInc+TitleI_NoYes))

summary(update(nullmodel3, .~. +PctLowInc+RurUrb+PctEDS))
summary(update(nullmodel3, .~. +PctLowInc+RurUrb+PctSevAbs))
summary(update(nullmodel3, .~. +PctLowInc+RurUrb+PctMinority))
summary(update(nullmodel3, .~. +PctLowInc+RurUrb+TitleI_NoYes))

# Forward Selection with AICc
step(nullmodel3, scope = list(lower = nullmodel3, upper = fullmodel3), direction = "forward", k = 2*(n3/(n3-p-1)))

# Stepwise Selection with AICc (considering all combinations of interaction effects)
step(fullmodel3, scope = list(lower = nullmodel3, upper = ~PctEDS*PctSevAbs*RurUrb*PctMinority*TitleI_NoYes*PctLowInc), k = 2*(n3/(n3-p-1)), direction = "both")

# Final Combined models
finalmodelcombined_1 <- lm(Score ~ RurUrb+PctLowInc, data = train.datcombined)
finalmodelcombined_2 <- lm(Score ~ PctLowInc, data = train.datcombined)

# Diagnostic checks for final Combined models
plot(finalmodelcombined_1)
plot(finalmodelcombined_2)
par(mfrow = c(1,1))

# Random Forest analysis (starting with full model)
print(randomForest(Score ~ PctEDS+PctSevAbs+RurUrb+PctMinority+TitleI_NoYes+PctLowInc, data = train.datcombined))
print(randomForest(Score ~ RurUrb+PctLowInc, data = train.datcombined))
print(randomForest(Score ~ PctLowInc, data = train.datcombined))

# Linear regression output for full model and final Combined models
summary(fullmodel3)
summary(finalmodelcombined_1)
summary(finalmodelcombined_2)

# Predictions for final models
predcombined_1 <- predict.lm(finalmodelcombined_1, newdata = prediction.datcombined)
predcombined_1a <- data.frame(School = prediction.datcombined[,1], PredictedScore = predcombined_1)
(predcombined_1a <- predcombined_1a[order(-predcombined_1a$PredictedScore),])
hist(predcombined_1a$PredictedScore, main="Final Model 1, Combined Prediction Data", xlab="Scores")
write.table(predcombined_1a, file = "PredictionsCombined_1.csv", sep=",", row.names = FALSE)

predcombined_2 <- predict.lm(finalmodelcombined_2, newdata = prediction.datcombined)
predcombined_2a <- data.frame(School = prediction.datcombined[,1], PredictedScore = predcombined_2)
(predcombined_2a <- predcombined_2a[order(-predcombined_2a$PredictedScore),])
hist(predcombined_2a$PredictedScore, main="Final Model 2, Combined Prediction Data", xlab="Scores")
write.table(predcombined_2a, file = "PredictionsCombined_2.csv", sep=",", row.names = FALSE)

# The choice of best linear regression model for Combined training data:
# Score ~ RurUrb + PctLowInc
lmtraincombined <- finalmodelcombined_1

# Combined data visualization
predcombined_best <- predcombined_1a[order(predcombined_1a$School),]
predictiondatcombinedorder <- prediction.datcombined[order(prediction.datcombined$School),]
finalcolscombined <- c("Level", "RurUrb", "PctLowInc")
predictiondatcombinedorder2 <- predictiondatcombinedorder[finalcolscombined]
finaldatacombined <- cbind(predcombined_best, predictiondatcombinedorder2)
finaldatacombined <- finaldatacombined[order(-finaldatacombined$PredictedScore),]


# 3D interactive training data scatterplot
scatter3d(train.datcombined$RurUrb, train.datcombined$PctLowInc, train.datcombined$Score, xlab = "RurUrb", ylab = "PctLowInc", zlab = "Score")

# Prediction data 3D scatterplot of predictions
with(finaldatacombined, text3D(RurUrb, PctLowInc, PredictedScore, 
                           labels = School, colvar = Level, 
                           col = gg.col(100), theta = 60, phi = 20,
                           xlab = "RurUrb", ylab = "PctLowInc", zlab = "PredictedScore", 
                           main = "Combined Prediction Data: Scatterplot by School Name", cex = 0.6, 
                           bty = "g", ticktype = "detailed", d = 2,
                           clab = "Level", adj = 0.5, font = 2))
plotrgl() # make plot interactive

###############################################################################################
# Function for plotting "fancy" 3D histograms
hist3D_fancy<- function(x, y, break.func = c("Sturges", "scott", "FD"), breaks = NULL,
                        colvar = NULL, col="white", clab=NULL, phi = 5, theta = 25, ...){
  
  # Compute the number of classes for a histogram
  break.func <- break.func [1]
  if(is.null(breaks)){
    x.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(x),
                       scott = nclass.scott(x),
                       FD = nclass.FD(x))
    y.breaks <- switch(break.func,
                       Sturges = nclass.Sturges(y),
                       scott = nclass.scott(y),
                       FD = nclass.FD(y))
  } else x.breaks <- y.breaks <- breaks
  
  # Cut x and y variables in bins for counting
  x.bin <- seq(min(x)-0.2, max(x), length.out = x.breaks)
  y.bin <- seq(min(y), max(y), length.out = y.breaks)
  xy <- table(cut(x, x.bin), cut(y, y.bin))
  z <- xy
  
  xmid <- 0.5*(x.bin[-1] + x.bin[-length(x.bin)])
  ymid <- 0.5*(y.bin[-1] + y.bin[-length(y.bin)])
  
  oldmar <- par("mar")
  par (mar = par("mar") + c(0, 0, 0, 2))
  hist3D(x = xmid, y = ymid, z = xy, ...,
         zlim = c(-max(z)/2, max(z)), zlab = "counts", bty= "g", 
         phi = phi, theta = theta,
         shade = 0.2, col = col, border = "black",
         d = 1, ticktype = "detailed")
  
  scatter3D(x, y,
            z = rep(-max(z)/2, length.out = length(x)),
            colvar = colvar, col = gg.col(100),
            add = TRUE, pch = 18, clab = clab,
            colkey = list(length = 0.5, width = 0.5,
                          dist = 0.05, cex.axis = 0.8, cex.clab = 0.8)
  )
  par(mar = oldmar)
}

# Prediction data 3D histogram
hist3D_fancy(finaldatacombined$RurUrb, finaldatacombined$PctLowInc, colvar = finaldatacombined$PredictedScore, clab = "Score", main = c("Combined Prediction Data: Histogram of RurUrb & PctLowInc"))
plotrgl() # make plot interactive
###############################################################################################