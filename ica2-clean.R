library(mgcv)


########################### Data Preprocessing #########################
########################################################################
#
#   In this part, we read and combine data into one dataframe and 
#   use clustering method to group the UAs. 
#
ObesityData <- read.csv("obesity.csv", header = TRUE, na.strings = "-1")
IndicatorNames <- read.csv("IndicatorNames.csv", header = TRUE)
UARegions <- read.csv("UARegions.csv", header = TRUE)
#
#   Obesity rate is better for exploratory analysis
#   so add a new variable called ObesRate to ObesityData.
#
ObesityData$ObesRate <- ObesityData$obesity / ObesityData$popCount 
                      
ObesityData <- merge(ObesityData, UARegions)  # Combine data
ObesityData <- ObesityData[order(ObesityData$ID, ObesityData$Region), ]
#
#   Match the unit of Hospital Admission with others: per 1000
#
ObesityData$X22401 <- ObesityData$X22401 / 100 

ncol(ObesityData) # 20 columns
#
#   Give variables in the data meaningful names
#
Xid <- colnames(ObesityData)[6:18]
id <- gsub("X", "", Xid)
IndicatorID <- IndicatorNames$IndicatorID
idname <- IndicatorNames[
  sapply(id, function(x)grep(x, IndicatorID, fixed = TRUE)), ]

colnames(ObesityData)[6:18] <- 
  c("% Pupil absence", "Violent offences/1k", "Emergency hospital/1k", 
    "% Fuel poverty", "Winter deaths index", "Sexual offences/1k", 
    "% Pop18-", "% Pop65+", "% Econnomic inactive", "Air pollution", 
    "Home affordability", "Gender pay gap", "Weekly earnings")

#
#   Group UAs using clustering method
#
NumVars <- 5:18         #  Columns containing all numeric covariates
UASummaries <- aggregate(ObesityData[,NumVars], 
                         by=list(ObesityData$UA), FUN=mean) 
                        #  Means & SDs of all numeric covariates 
                        #  for each ethnic group
rownames(UASummaries) <- UASummaries[,1]
UASummaries <- scale(UASummaries[,-1]) 
                        #  Standardise to mean 0 & SD 1
Distances <- dist(UASummaries) #  Pairwise distances
ClusTree <- hclust(Distances, method="complete") #  Do the clustering

plot(ClusTree, xlab="Unitary Authority", ylab="Separation")
abline(h=8, col="red", lty=2)

NewGroups <- paste("UAGrp", cutree(ClusTree,h=8),sep="") 
                        #  323 UAs and their respective groups 
                        #  in alphabetical order
table(UARegions$Region[match(rownames(UASummaries), UARegions$UA)], 
      NewGroups)
 
UA <- unique(ObesityData$UA) #  list all 323 UAs in order
UAGroups <- data.frame(UA, NewGroups)
allData <- merge(ObesityData, UAGroups) #  Combine all data together 
allData <- allData[order(allData$ID, allData$UA), ]
row.names(allData)<-allData$ID
#
#   define appropriate covariate names
#
Year <- allData$Year
ObesCount <- allData$obesity
PopCount <- allData$popCount
ObesRate <- allData$ObesRate
Absence <- allData$`% Pupil absence`
Violence <- allData$`Violent offences/1k`
Hospital <-  allData$`Emergency hospital/1k`
Fuel <- allData$`% Fuel poverty`
Death <- allData$`Winter deaths index`
Sexual <- allData$`Sexual offences/1k`
Pop18 <- allData$`% Pop18-`
Pop65 <- allData$`% Pop65+`
EconInactive <- allData$`% Econnomic inactive`
Pollution <- allData$`Air pollution`
Afford <- allData$`Home affordability`
Gender <- allData$`Gender pay gap`
Earnings <- allData$`Weekly earnings`
Region <- allData$Region
Groups <- allData$NewGroups

#################### Exploratory Data Analysis (EDA) ###################
########################################################################
#
#   We start by understanding the structure and getting summaries 
#   for all the data we have. 
#    
print(str(allData))
print(summary(allData))
#
#   We do boxplots of obesity against two geographic variables to 
#   visualise variations of obesity in each region.
#   Then, looking at the summary of linear models with only one of 
#   these two variables at a time to compare which one is better 
#   at explaining variation.
#
par(mfrow=c(2,1),lwd=2,mar=c(2,2,2,2),mgp=c(2,0.75,0))
boxplot(ObesRate ~ Region)
boxplot(ObesRate ~ Groups)
summary(lm(ObesRate ~ Region, data = allData)) 
                              #  R-squared:  0.31, residual sd = 0.031
summary(lm(ObesRate ~ Groups, data = allData)) 
                              #  R-squared:  0.41, residual sd = 0.028
#
#   We look at the respective means and variances 
#   (a robust method, median absolute deviation) of all the numeric 
#   variables across eight groups. 
#   Then we do conditional plots for each numeric covariate with the 
#   response variable across eight groups to observe trends. 
# 
GroupMeans <- aggregate(allData[, 6:18], 
                        by=list(allData$NewGroups), mean, na.rm = TRUE)
GroupMads <- aggregate(allData[, 6:18], 
                       by=list(allData$NewGroups), mad, na.rm = TRUE)

for (i in 6:18) {
  coplot(ObesRate[1:1785] ~ allData[1:1785, i] | Groups[1:1785],
         xlab = c(paste("Obesity rate against", 
                        colnames(allData)[i], 
                        "across eight groups"), "Groups"), 
         ylab = "Obesity rate", 
         number = c(2,4))
}

pdf("coplotAf.pdf")
coplot(ObesRate[1:1785] ~ Afford[1:1785] | Groups[1:1785],
       xlab = "Obesity rate against Afford across eight groups",  
       ylab = "Obesity rate" )
dev.off()
pdf("coplotE.pdf")
coplot(ObesRate[1:1785] ~ Earnings[1:1785] | Groups[1:1785],
       xlab = "Obesity rate against Earnings across eight groups",  
       ylab = "Obesity rate" )
dev.off()
#
#   Absence, Hospital, Pop18, Afford and Earnings seem to show 
#   different relationships with Obesity for different groups. 
#   The rest show similar trends with Obesity for all groups 
#   so we plot them with Obesity again as a whole. 
#
par(mfrow=c(3,3),lwd=2,mar=c(2,2,2,2),mgp=c(1,0.25,0))
plot(Violence, ObesRate)    #  substantial non-linear relationship
plot(Fuel, ObesRate)    #  substantial linearity
plot(Death, ObesRate)  #  random scatter
plot(Sexual, ObesRate)      #  substantial non-linear relationship
plot(Pop65, ObesRate)       #  some linearity
plot(EconInactive, ObesRate)  #  significant linearity
plot(Pollution, ObesRate)     #  some linearity
plot(Gender, ObesRate)     #  random scatter
#
#   We continue to plot covariates against one another, 
#   a few catched our eyes.
#
par(mfrow=c(1,1),lwd=2,mar=c(2,2,2,2),mgp=c(2,0.75,0))
plot(data.frame(Violence, Sexual, Pop18, Pop65, Pollution,
                Afford, Earnings))
pdf("po-18-65.pdf")
par(mfrow=c(1,1),lwd=2,mar=c(2,2,2,2),mgp=c(2,0.75,0))
plot(data.frame(Pop18, Pop65, Pollution))
dev.off()

pdf("collinearity.pdf")
par(mfrow=c(1,2),lwd=2,mar=c(5,2,5,2),mgp=c(1.2,0.5,0))
plot(Sexual, Violence, main = "Violence against Sexual")
                                             #  extreme correlation
cor(Sexual, Violence)     #  0.89
plot(Afford, Earnings, main = "Earnings vs Afford") 
                                             #  significant correlation
cor(Afford, Earnings) #  0.68
dev.off()
#
#   We perform Principal Components Analysis (PCA) on three groups 
#   of related covariates that descibe similar aspects of the 
#   groups to combine their effects. 
#
PCA.Crime <- prcomp(data.frame(Violence, Sexual), scale. = TRUE)
summary(PCA.Crime)             #  PC1 can explain 95% of the variation
allData$CrimePC <- PCA.Crime$x[,1]

PCA.Afford <- prcomp(data.frame(Afford, Earnings), scale. = TRUE)
summary(PCA.Afford)        #  PC1 can explain 84% of the variation
allData$AffordPC <- PCA.Afford$x[, 1]

PCA.Pollute <- prcomp(data.frame(Pop18, Pop65, Pollution), scale. = TRUE)
par(mfrow=c(1,1),lwd=2,mar=c(2,2,2,2),mgp=c(2,0.75,0))
biplot(PCA.Pollute)
summary(PCA.Pollute)    #  PC1 (explains 68%) and PC2 adds up to 
                       #  89% of the variation
print(PCA.Pollute)
allData$PollutePC1 <- PCA.Pollute$x[, 1]
allData$PollutePC2 <- PCA.Pollute$x[, 2]

############################ Model Building ############################
########################################################################
#
#  Building on the knowledge we got from exploratory data analysis (EDA), 
#  we build Ordinary Linear Models (OLMs) at first. 
#  Prinipal components (PCs) calculated above are used in the model.
#

lmtrial1 <- lm(ObesRate ~ factor(Year) + Groups + Absence + CrimePC + 
                 Hospital + Fuel + Death + PollutePC1 + 
                 PollutePC2 + EconInactive + AffordPC + 
                 Gender, data = allData)
summary(lmtrial1)       #  Death, and PollutePC2 show high p-values
lmtrial2 <- update(lmtrial1, .~.- Death - PollutePC2)
anova(lmtrial1, lmtrial2, test = "F")   #  lmtrial2 is prefered
summary(lmtrial2)
#
#   From EDA and context, we get intuition about possible interactions.
#   An examination of the coplots make them more visual and detailed. 
#   Absence, PollutePC1 and AffordPC seem to have different slopes 
#   for different groups. 
#   When values of PollutePC1 is high, AffordPC seems to 
#   affect Obesity rate differently. Others are not visually obvious. 
#   Apart from visual inspection, we use anova on linear models to 
#   further validate our inspection. 
#
par(mfrow=c(1,1),lwd=2,mar=c(2,2,2,2),mgp=c(2,0.75,0))
InteractData <- data.frame(Absence, allData$PollutePC1, Hospital, 
                           allData$AffordPC)
for (i in 1:4) {
  coplot(ObesRate[1:1785] ~ InteractData[1:1785,i] | Groups[1:1785], 
       xlab = c(paste("Obesity rate against", 
                      colnames(InteractData)[i], "across eight groups"), 
                      "Groups"), 
       ylab = "Obesity rate")
}
coplot(ObesRate[1:1785] ~ AffordPC[1:1785] | PollutePC1[1:1785], 
       xlab = c("Obesity rate against AffordPC given PollutePC", 
                "PollutePC"), 
       ylab = "Obesity rate", data = allData)

lmtrial3 <- update(lmtrial2, .~. + Absence:Groups + PollutePC1:Groups +
                     AffordPC:Groups + AffordPC:PollutePC1)
summary(lmtrial3)
lmtrial4 <- update(lmtrial3, .~. - PollutePC1:Groups)
anova(lmtrial3, lmtrial4, test = "F") # lmtrial 3 is prefered

lmtrial5 <- update(lmtrial3, .~. - Absence:Groups)
anova(lmtrial3, lmtrial5, test = "F") # lmtrial 3 is prefered

lmtrial6 <- update(lmtrial3, .~. - AffordPC:Groups)
anova(lmtrial3, lmtrial6, test = "F") # lmtrial 3 is prefered
#
#   We continue to check assumptions on lmtrial3 by 
#   examining the residual plots.
#
rstlm <- rstandard(lmtrial3)
fittedlm <- fitted(lmtrial3)
par(mfrow=c(1,1),lwd=2,mar=c(3,3,3,3),mgp=c(2,0.75,0))
plot(fittedlm, rstlm)     #  randomly scatter, but larger residuals 
                          #  than expected
par(mfrow=c(2,2),lwd=2,mar=c(2,2,2,2),mgp=c(2,0.75,0))
plot(lmtrial3, 1:4)     # Heavy-tailed residual distribution 
                        # in QQ plot, assumption of normality violated.
pdf("QQ-plot-lm.pdf")
par(mfrow = c(1,1), mar = c(3,3,3,3), mgp=c(2,0.75,0))
plot(lmtrial3, which = 2)
dev.off()
#
#   As the assumption of normality is shown to be violated from the 
#   QQ plot and due to reasons from the context. 
#   We continue to construct Generalized Linear Models (GLMs) to 
#   relax the assumption of normality. GLMs of Poisson distribution 
#   with log link is suitable based on the context. 
#   We use log(PopCount) as our already known intercept in GLMs. 
#   As we would always like to know the amount of deviance explained 
#   by the model, we create a function for it, 
#   as well as for the dispersion parameter of GLMs. 
#
nullglm <- glm(ObesCount ~ offset(log(PopCount)), data = allData,
               family = poisson(link = "log")) 

deviance <- function(model) {
  summary <- summary(model)
  1 - summary$deviance / summary$null.deviance
}

dispersion <- function(model) {
  sum( resid(model,type="pearson")^2 ) / model$df.residual
}

glmtrial1 <- glm(ObesCount ~ offset(log(PopCount)) + factor(Year) + 
                   Groups + Absence + CrimePC + Hospital + Fuel + 
                   PollutePC1 + EconInactive + AffordPC + Gender, 
                   family = poisson(link = "log"), data = allData)
summary(glmtrial1)     #  Gender has very high p-value
deviance(glmtrial1)    #  70.5% deviance explained
dispersion(glmtrial1)  #  3.79 very far from 1
#
#   Remove irrelevant Gender and add 
#   possible interactions of interest. 
#   Compare nested GLMs using anova with Chi-squared test. 
#
glmtrial2 <- update(glmtrial1, .~. - Gender + 
                      Absence:Groups + PollutePC1:Groups + 
                      Hospital:Groups + AffordPC:Groups + 
                      AffordPC:PollutePC1)
summary(glmtrial2)
deviance(glmtrial2)    #  76.1% deviance explained
dispersion(glmtrial2)  #  3.16 still far from 1

glmtrial3 <- update(glmtrial2, .~. - PollutePC1:Groups)
anova(glmtrial2, glmtrial3, test = "Chi")     #  glmtrial2 prefered
summary(glmtrial3)

glmtrial4 <- update(glmtrial3, .~. - Hospital:Groups)
summary(glmtrial4)
anova(glmtrial3, glmtrial4, test = "Chi")     #  glmtrial2 prefered
#
#   Check assumptions of lineariy
#
par(mfrow=c(2,2),lwd=2,mar=c(2,2,2,2),mgp=c(2,0.75,0))
plot(glmtrial2, 1:4)      #  residuals are larger than expected 
rstglm <- rstandard(glmtrial2)
fittedglm <- fitted(glmtrial2)
plot(allData$CrimePC[1:1785], rstglm) #  decreasing variance, 
                                      #  assumption violated
plot(allData$AffordPC[1:1785], rstglm) #  decreasing variance, 
                                           #  assumption violated
plot(allData$PollutePC1[1:1785], rstglm) #  random
plot(Absence[1:1785], rstglm)  #  random
plot(Hospital[1:1785], rstglm) #  random
plot(Fuel[1:1785], rstglm) #  random
plot(EconInactive[1:1785], rstglm) #  random
plot(Groups[1:1785], rstglm)
plot(factor(Year[1:1785]), rstglm)
#
#   Based on EDA and residual plots above, we use log transformation 
#   on CrimePC and AffordEarnPC to mitigare their lack of linearity 
#   with Obesity. We transform their ordinary values first and then 
#   perform PCA as to aviod any negative values produced after PCA.
#
PC.LogCrime <- prcomp(data.frame(log(Violence), log(Sexual)), 
                      scale. = TRUE)
allData$LogCrimePC <- PC.LogCrime$x[, 1]
PC.LogEarn <- prcomp(data.frame(log(Afford), log(Earnings)), 
                     scale. = TRUE)
allData$LogEarnPC <- PC.LogEarn$x[, 1]

pdf("transC.pdf")
par(mfrow=c(1,2),lwd=2,mar=c(5,2,5,2),mgp=c(1.2,0.5,0))
plot(allData$CrimePC[1:1785], rstglm, xlab = "CrimePC",
     main = "stansard residual vs CrimePC")
plot(allData$LogCrimePC[1:1785], rstglm, xlab = "LogCrimePC",
     main = "standard residual vs LogCrimePC")
dev.off()
pdf("transE.pdf")
par(mfrow=c(1,2),lwd=2,mar=c(5,2,5,2),mgp=c(1.2,0.5,0))
plot(allData$CrimePC[1:1785], rstglm, xlab = "AffordPC",
     main = "stansard residual vs AffordPC")
plot(allData$LogEarnPC[1:1785], rstglm, xlab = "LogEarnPC",
     main = "standard residual vs LogEarnPC")
dev.off()
#
#   Use transformed PCs and their interactions
#
glmtrial5 <- update(glmtrial2, .~. - CrimePC - AffordPC - 
                      AffordPC:Groups - AffordPC:PollutePC1 + 
                      LogCrimePC + LogEarnPC + LogEarnPC:Groups + 
                      LogEarnPC:PollutePC1)
summary(glmtrial5)    #  Hospital p-value is large
deviance(glmtrial5)   #  76.4% deviance explained
dispersion(glmtrial5) #  3.10 still far from 1
#
#   As dispersion parameter continues to be far from 1, 
#   we decide to use quasipoisson as our distribution to 
#   relax the assumption that dispersion parameter is 1. 
#
glmtrial6 <- update(glmtrial5, .~., family = quasipoisson(link = "log"))
summary(glmtrial6)  #  p-values of Hospital, FuelPoor, Groups and 
                    #  interactions increase
deviance(glmtrial6)    #  76.4%

glmtrial7 <- update(glmtrial6, .~. - PollutePC1:Groups)
deviance(glmtrial7)    #  76.4%
summary(glmtrial7)  #  Hospital and its interaction have high p-value 
anova(glmtrial6, glmtrial7, test = "F")   #  glmtrial7 is prefered

glmtrial8 <- update(glmtrial7, .~. - Hospital:Groups)
deviance(glmtrial8)    #  76.1%
summary(glmtrial8)  #  p-value of Hospital restored to be small

glmtrial9 <- update(glmtrial8, .~. - Absence:Groups)
deviance(glmtrial9)    #  75.8%
summary(glmtrial9)
#
#   We plot the model to observe potential outliers 
#   and check assumptions.
#
par(mfrow=c(2,2),lwd=2,mar=c(2,2,2,2),mgp=c(2,0.75,0))
plot(glmtrial9, 1:4)
extreme <- allData[c(504, 565, 1761),]
GroupMeans$ObesRate <- tapply(ObesRate, Groups, mean, na.rm=TRUE)
GroupMads$ObesRate <- tapply(ObesRate, Groups, mad, na.rm=TRUE)
#
#   We continue to check the assumption of linearity and 
#   whether or not any important variables are left out. 
#   All residiual plots show substantial randomization and 
#   residuals are around the range of +/-2. 
#
rstglm <- rstandard(glmtrial9)
fittedglm <- fitted(glmtrial9)
plot(Absence[1:1785], rstglm) 
plot(allData$LogCrimePC[1:1785], rstglm)
plot(Fuel[1:1785], rstglm)
plot(EconInactive[1:1785], rstglm)
plot(allData$PollutePC1[1:1785], rstglm)
plot(allData$LogEarnPC[1:1785], rstglm)
plot(Gender[1:1785], rstglm)
plot(Death[1:1785], rstglm)
plot(Hospital[1:1785], rstglm)
plot(Groups[1:1785], rstglm)
plot(factor(Year[1:1785]), rstglm)

glmfinal <- glmtrial9  # glmtrial9 is our final model

summary(glmfinal)

############################## Prediction ##############################
########################################################################
ObesPred <- data.frame(predict(glmfinal, newdata = allData, 
                               se.fit = TRUE))[1786:2232, ]
Fit <- exp(ObesPred$fit)  #  fitted values on the scale of the response 
SE <- exp(ObesPred$se.fit) # standard error on the scale of the response
SD <- (Fit + (SE)^2)^0.5     # estimated standard deviation 
                             # of prediction error
Prediction <- data.frame(allData$ID[1786:2232], Fit, SD)
write.table(Prediction, file = "_pred.dat", sep = "",
            row.names = FALSE, col.names = FALSE)

############################### The End ################################

