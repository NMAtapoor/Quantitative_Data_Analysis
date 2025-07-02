#install.packages("ggraph")
#install.packages("qgraph")
#install.packages("plotrix")
#install.packages("factoextra")
library(Amelia)
library(ppcor)
library(psych)
library(ggraph)
library(qgraph)
library(tidygraph)
library(ppcor)
library(psych)
library(nFactors)
library(GPArotation)
library(cluster)
library(fpc)
library(factoextra)
library(car)
library(corrplot)
library(relaimpo)
library(RcmdrMisc)
library(plotrix)
library(lsr)
library(readxl)
library(car)

# function reading the given data file;
getData <- function(dataFile){
  setwd(dirname(file.choose()))
  getwd()
  data_set <- read.csv(dataFile, stringsAsFactors = FALSE)
  
  return(data_set)
}

DS7006E = getData("DS7006E.csv")

attach(DS7006E)

#=======================================================================================================
# Check for missing values in the data set;
#===============================================================================================
missing.data <- complete.cases(DS7006E)
missing.data
# Check for missing data column wise and sum them
apply(DS7006E, MARGIN = 2, FUN = function(x) sum(is.na(x)))


missmap(DS7006E, col = c("red", "blue"), legend = TRUE) # Check missing data visually;

# Remove missing data if found
DS7006E <- na.omit(DS7006E)

# =========================================================================================================
# Checking for outliers
#==========================================================================================================

# set column and rows, and margin parameters;
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))

# create box plor for the dependent variable
covidBox <- boxplot(Total_Death, xlab="Covid-19 Deaths",
        col = "cyan4", border = "brown",varwidth = TRUE, outcex = .5,
        outpch = 20, whisklty = 3,frame.plot = FALSE,cex.axis = 0.5, las = 1 )

# draw Cleveland dotplot to check for outliers;
dotchart(Total_Death,
         ylab = "Order of Covid-19 Deaths Data",lcolor = "yellow",color = "cyan3",
         xlab = "Range of Covid-19 Deaths Data", frame.plot = FALSE, pch = 20)


#==============================================================================================================
# Checking for dependent variable normality:
#==============================================================================================================
# set rows, columns, and margines parameters for graph;
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
qqnorm((Total_Death),ylab = "Sample Quantiles: Covid-19 Deaths", xlab = "Theoretical Quantiles: Covid-19 Deaths",main = "Q-Q Plot: Covid-19 Total Deaths",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline((Total_Death), col=6)

hist(Total_Death, col = "cyan4", main = "Covid-19 Deaths Distribution", 
     xlab = "Covid-19 Deaths Number")

ks.test(Total_Death, "pnorm", mean(Total_Death), sd(Total_Death))

#==================================================================================================================
# Standardize the data based on per 1000 population
#==================================================================================================================

#add more column data containing standerdized variables;
DS7006E <- within(DS7006E, pTotal_Death <- (Total_Death/Total_Age * 1000))
DS7006E <- within(DS7006E, pTotal_Death2020 <- (Total_Death2020/Total_Age * 1000))
DS7006E <- within(DS7006E, pTotal_Death2021 <- (Total_Death2021/Total_Age * 1000))

DS7006E <- within(DS7006E, pChild_Age <- (Child_Age/Total_Age * 1000))
DS7006E <- within(DS7006E, pTeen_Age <- (Teen_Age/Total_Age * 1000))
DS7006E <- within(DS7006E, pYoung_Age <- (Young_Age/Total_Age * 1000))
DS7006E <- within(DS7006E, pOld_Age <- (Old_Age/Total_Age * 1000))

DS7006E <- within(DS7006E, pTotal_Disability <- (Total_Disability/Total_Age * 1000))
DS7006E <- within(DS7006E, pHigh_Disability <- (High_Disability/Total_Age * 1000))
DS7006E <- within(DS7006E, pLow_Disability <- (Low_Disability/Total_Age * 1000))
DS7006E <- within(DS7006E, pNo_Disability <- (No_Disability/Total_Age * 1000))

DS7006E <- within(DS7006E, pHW_Total <- (HW_Total/Total_Age * 1000))
DS7006E <- within(DS7006E, pPart_Time <- (Part_Time/Total_Age * 1000))
DS7006E <- within(DS7006E, pFull_Time <- (Full_Time/Total_Age * 1000))
DS7006E <- within(DS7006E, pOver_Time <- (Over_Time/Total_Age * 1000))

DS7006E <- within(DS7006E, pWhite <- (White/Total_Age * 1000))
DS7006E <- within(DS7006E, pMixed <- (Mixed/Total_Age * 1000))
DS7006E <- within(DS7006E, pTotal_Death <- (Total_Death/Total_Age * 1000))

#==================================================================================================================
# Check for outliers in standardized DV variables using box plot and Cleveland dotplot
#==================================================================================================================

par(mfrow = c(3, 2), mar = c(5, 4, 1, 1))

covBoxTotal <- boxplot(pTotal_Death, xlab="Covid-19 Deaths/1000",
                    col = "cyan4", border = "brown",varwidth = TRUE, outcex = .5,
                    outpch = 20, whisklty = 3,frame.plot = FALSE,cex.axis = 0.5, las = 1 )

dotchart(pTotal_Death,
         ylab = "Order of Covid-19 Deaths Data/1000",lcolor = "yellow",color = "cyan3",
         xlab = "Range of Covid-19 Deaths Data/1000", frame.plot = FALSE, pch = 20)

covBox2020 <- boxplot(pTotal_Death2020, xlab="Covid-19 Deaths 2020/1000",
                      col = "cyan2", border = "brown1",varwidth = TRUE, outcex = .5,
                      outpch = 20, whisklty = 3,frame.plot = FALSE,cex.axis = 0.5, las = 1 )

dotchart(pTotal_Death2020,
         ylab = "Order of Covid-19 Data(2020)/1000",lcolor = "yellow",color = "cyan3",
         xlab = "Range of Covid-19 Deaths Data(2020)/1000", frame.plot = FALSE, pch = 20)

covBox2021 <- boxplot(pTotal_Death2021, xlab="Covid-19 Deaths 2021/1000",
                      col = "cyan2", border = "brown1",varwidth = TRUE, outcex = .5,
                      outpch = 20, whisklty = 3,frame.plot = FALSE,cex.axis = 0.5, las = 1 )

dotchart(pTotal_Death2021,
         ylab = "Order of Covid-19 Data(2021)/1000",lcolor = "yellow",color = "cyan3",
         xlab = "Range of Covid-19 Deaths Data(2021)/1000", frame.plot = FALSE, pch = 20)


#function defined to detect outliers and other associate informations;
findOutlier <- function(boxP,dataVar,dataSet, empVec,colmNames){
  for (i in 1:length(boxP$group)) {
    empVec[i] <- which(dataVar == boxP$out[i])
  }
  outData <- dataSet[empVec,colmNames]
  return(outData)
}
# Prepare parameters for the function
colN1 <- c("Geo_Code","Geography","Total_Death", "pTotal_Death","Total_Age")
colN2 <- c("Geo_Code","Geography","Total_Death2020", "pTotal_Death2020","Total_Age")
colN3 <- c("Geo_Code","Geography","Total_Death2021", "pTotal_Death2021","Total_Age")
empV1 <- c()
empV2 <- c()
empV3 <- c()
# Call function for displaying outliers in each dependent variables
outDataT <- findOutlier(covBoxTotal,pTotal_Death,DS7006E,empV1,colN1)
outData21 <- findOutlier(covBox2020,pTotal_Death2020,DS7006E,empV2,colN2)
outData22 <- findOutlier(covBox2021,pTotal_Death2021,DS7006E,empV3,colN3)


# Check for normality after standardization of dependent variables
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
qqnorm((pTotal_Death),ylab = "Sample Quantiles: Covid-19 Deaths/1000", xlab = "Theoretical Quantiles: Covid-19 Deaths/1000",main = "Q-Q Plot: Covid-19 Total Deaths/1000",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline(pTotal_Death, col=6, lwd = 1)

hist(death2, prob = TRUE, col = "cyan4", main = "Covid-19 Deaths Distribution/1000", 
     xlab = "Covid-19 Deaths Number/1000")
rug (pTotal_Death, col = "red")
# Draw probability density line to the histogram;
lines(density(death2),lwd = 2,col = "yellow")
# Calcualte the x and y axises of the data points 
xAxis <- seq(from = min(pTotal_Death), to = max(pTotal_Death), by = 0.1)
yAxis = dnorm(xfit, mean(pTotal_Death), sd(pTotal_Death))
#Draw normal line for the distribution in histogram;
lines(xAxis, yAxis, lwd = 2, col = "blue", cex = 0.5)
legend("topright", legend = c("Density curve", "Normal curve"),cex = 0.7,
      col=c("yellow","blue"), lwd=2, lty=c(1,1))

#test the normality of standard dependent variable;
shapiro.test(pTotal_Death)
ks.test(death2, "pnorm", mean(pTotal_Death), sd(pTotal_Death))

#Check for the normality of Covid-19 Total Deaths in 2021 and 2021
par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
qqnorm((pTotal_Death2020),ylab = "Sample Quantiles: Covid-19 Deaths(2020)/1000", xlab = "Theoretical Quantiles: Covid-19 Deaths(2020)/1000",main = "Q-Q Plot: Covid-19 Total Deaths/1000",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline(pTotal_Death2020, col=6, lwd = 1)

hist(pTotal_Death2020, prob = TRUE, col = "cyan4", main = "Covid-19 Deaths(2020)/1000", 
     xlab = "Covid-19 Deaths Number/1000")
rug (pTotal_Death2020, col = "red")
# Draw probability density line to the histogram;
lines(density(pTotal_Death2020),lwd = 2,col = "yellow")
# Calcualte the x and y axis data points 
x20 <- seq(from = min(pTotal_Death2020), to = max(pTotal_Death2020), by = 0.1)
y20 = dnorm(x20, mean(pTotal_Death2020), sd(pTotal_Death2021))
#Draw normal line for the distribution in histogram;
lines(x20, y20, lwd = 2, col = "blue", cex = 0.5)

legend("topright", legend = c("Density curve", "Normal curve"),cex = 0.7,
       col=c("yellow","blue"), lwd=2, lty=c(1,1))


qqnorm((pTotal_Death2021),ylab = "Sample Quantiles: Covid-19 Deaths 2021/1000",
       xlab = "Theoretical Quantiles: Covid-19 Deaths2021/1000",
       main = "Covid-19 Total Deaths(2021)/1000",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline(pTotal_Death2021, col=6, lwd = 1)

hist(pTotal_Death2021, prob = TRUE, col = "cyan4", main = "Covid-19 Deaths(2021)/1000", 
     xlab = "Covid-19 Deaths(2021)/1000")
rug (pTotal_Death2021, col = "red")
# Draw probability density line to the histogram;
lines(density(pTotal_Death2021),lwd = 2,col = "yellow")
# Calcualte the x and y axis data points 
x21 <- seq(from = min(pTotal_Death2021), to = max(pTotal_Death2021), by = 0.1)
y21 = dnorm(x21, mean(pTotal_Death2021), sd(pTotal_Death2021))
#Draw normal line for the distribution in histogram;
lines(x21, y21, lwd = 2, col = "blue", cex = 0.5)

legend("topright", legend = c("Density curve", "Normal curve"),cex = 0.7,
       col=c("yellow","blue"), lwd=2, lty=c(1,1))


ks.test(pTotal_Death2020, "pnorm", mean(pTotal_Death2020), sd(pTotal_Death2020))
ks.test(pTotal_Death2021, "pnorm", mean(pTotal_Death2021), sd(pTotal_Death2021))

# =================================================================================================================
# Determine Dependent and Independent Variables Correlations Coefficients
# =================================================================================================================
# a function illustrate correlation coefficients between variables of a given data set; 
drawCor <- function(dataSet){
  pairs.panels(dataSet, smooth = TRUE, scale = TRUE, density = TRUE,  
               ellipses = FALSE,method = "spearman", pch = 20,lm = FALSE,
               cor = TRUE,jiggle = TRUE,factor = 2,hist.col = "cyan4",
               stars = TRUE, ci = TRUE,col = "cyan2") }
# create data objects for each theme in the data set;
pAge <- DS7006E[,c("pChild_Age","pTeen_Age","pYoung_Age","pOld_Age")]
pDisability <- DS7006E[,c("pHigh_Disability","pLow_Disability","pNo_Disability")]
pDistance <- DS7006E[,c("pTotal_Dist","pDist_0to5km","pDist_5to20km","pDist_20to40km","pDist_40orOver","pFrom_Home")]
pHoursWork <- DS7006E[,c("pHW_Total","pPart_Time","pFull_Time","pMales","pFemale")]
pEthnicity <- DS7006E[,c("pWhite","pMixed","pAsian","pBlack","pOther_Ethnic")]

#call the function to visualized the correlations for variables of each theme;
drawCor(pAge)
drawCor(pDisability)
drawCor(pDistance)
drawCor(pHoursWork)
drawCor(pEthnicity)

# create data object for dependent and independent variables;
selected_IV_DV <- DS7006E[,c("pTotal_Death","pChild_Age","pTeen_Age","pHigh_Disability","pDist_0to5km"
                                ,"pDist_5to20km","pDist_20to40km","pDist_40orOver","pFrom_Home",
                                "pPart_Time","pFemale","pMixed")]
#Draw the correlation matrix for dependent and independent variables;
selected_IV_DV_Cor <- cor(selected_IV_DV, method = "spearman")
drawCor(selected_IV_DV)

# Test Correlation Coefficients between Dependent and Independent Vars;
cor.test(pTotal_Death, pChild_Age, method = "spearman")
cor.test(pTotal_Death, pTeen_Age, method = "spearman")
cor.test(pTotal_Death, pYoung_Age, method = "spearman")
cor.test(pTotal_Death, pOld_Age, method = "spearman")

cor.test(pTotal_Death, pHigh_Disability, method = "spearman")
cor.test(pTotal_Death, pLow_Disability, method = "spearman")
cor.test(pTotal_Death, pNo_Disability, method = "spearman")

cor.test(pTotal_Death, pTotal_Dist, method = "spearman")
cor.test(pTotal_Death, pDist_0to5km, method = "spearman")
cor.test(pTotal_Death, pDist_5to20km, method = "spearman")
cor.test(pTotal_Death, pDist_20to40km, method = "spearman")
cor.test(pTotal_Death, pDist_40orOver, method = "spearman")
cor.test(pTotal_Death, pFrom_Home, method = "spearman")
cor.test(pTotal_Death, pDist_Other, method = "spearman")

cor.test(pTotal_Death, pHW_Total, method = "spearman")
cor.test(pTotal_Death, pPart_Time, method = "spearman")
cor.test(pTotal_Death, pFull_Time, method = "spearman")
cor.test(pTotal_Death, pOver_Time, method = "spearman")
cor.test(pTotal_Death, pMales, method = "spearman")
cor.test(pTotal_Death, pFemale, method = "spearman")

cor.test(pTotal_Death, pWhite, method = "spearman")
cor.test(pTotal_Death, pMixed, method = "spearman")
cor.test(pTotal_Death, pAsian, method = "spearman")
cor.test(pTotal_Death, pBlack, method = "spearman")
cor.test(pTotal_Death, pOther_Ethnic, method = "spearman")


# draw partial correlation between dependent and independent variables;
selected_IV_DV_df <- data.frame(selected_IV_DV)
pcor <- pcor(selected_IV_DV_df, method = "spearman")
pcor.test(pTotal_Death,pMixed,pWhite, method = "spearman")
pcor.test(pTotal_Death,pMixed,pAsian, method = "spearman")
pcor.test(pTotal_Death,pMixed,pBlack, method = "spearman")
pcor.test(pTotal_Death,pMixed,pBlack, method = "spearman")

# Test the normality of selected IVs normality;
ks.test(pChild_Age, "pnorm", mean(pChild_Age), sd(pChild_Age))
ks.test(pTeen_Age, "pnorm", mean(pTeen_Age), sd(pTeen_Age))
ks.test(pHigh_Disability, "pnorm", mean(pHigh_Disability), sd(pHigh_Disability))
ks.test(pDist_0to5km, "pnorm", mean(pDist_5to20km), sd(pDist_5to20km))
ks.test(pDist_5to20km, "pnorm", mean(pDist_5to20km), sd(pDist_5to20km))
ks.test(pDist_20to40km, "pnorm", mean(pDist_20to40km), sd(pDist_20to40km))
ks.test(pDist_40orOver, "pnorm", mean(pTotal_Death2021), sd(pDist_40orOver))
ks.test(pFrom_Home, "pnorm", mean(pFrom_Home), sd(pFrom_Home))
ks.test(pPart_Time, "pnorm", mean(pPart_Time), sd(pPart_Time))
ks.test(pFemale, "pnorm", mean(pFemale), sd(pFemale))
ks.test(pMixed, "pnorm", mean(pMixed), sd(pMixed))

# Create data object for all the selected IVs after resolving collinearity;
selected.IVs <- DS7006E[,c("pChild_Age","pTeen_Age","pHigh_Disability",
                     "pDist_0to5km","pDist_5to20km","pDist_20to40km","pDist_40orOver",
                     "pFrom_Home","pPart_Time","pFemale","pMixed")]
# create correlation matrix for the selected IVs
select.IVs.cormatrix <- cor(selected.IVs, use = "pairwise.complete.obs", method = "spearman")
select.IVs.cormatrix

#=========================================================================================================
# Data Analysis
#=========================================================================================================
# check the summary of the dependent variable;
describe(pTotal_Death)
summary(pTotal_Death)
# check variances of dependent variables;
var(pTotal_Death)
var(pTotal_Death2020)
var(pTotal_Death2021)

# run hypothesis test on the dependent variables;
t.test(pTotal_Death, pTotal_Death2020, paired = TRUE, var.equal = FALSE)
t.test(pTotal_Death, pTotal_Death2021, paired = TRUE, var.equal = FALSE)
t.test(pTotal_Death2020, pTotal_Death2021, paired = TRUE, var.equal = FALSE)
wilcox.test(pTotal_Death,pTotal_Death2020, paired = TRUE)

# Axis data points for Covid-19 total deaths; 
xTCovid <- seq(from = min(pTotal_Death), to = max(pTotal_Death), by = 0.1)
yTCovid = dnorm(xTCovid, mean(pTotal_Death), sd(pTotal_Death))

#Axis points for Covid-19 2020
xTCovid20 <- seq(from = min(pTotal_Death2020), to = max(pTotal_Death2020), by = 0.1)
yTCovid20 = dnorm(xTCovid20, mean(pTotal_Death2020), sd(pTotal_Death2020))

# Axis points for Covid-19 deaths in 2021
xTcov21 <- seq(from = min(pTotal_Death2021), to = max(pTotal_Death2021), by = 0.1)
yTcov21 <- dnorm(xTcov21, mean(pTotal_Death2021), sd(pTotal_Death2021))
x <- seq(from = 0, to = 7, by = 0.1)
y <- dnorm(x, min(pTotal_Death),0.4)

#Plot the line
plot(x,y, frame.plot = FALSE, type = "n",xlab = "Covid Proportion Death")
lines(xTCovid20, yTCovid20, lwd = 2, col = "red", cex = 0.5)
lines(xTcov21,yTcov21,lwd = 2, col = "green",cex = 0.5)
lines(xTCovid, yTCovid, lwd = 2, col = "blue", cex = 0.5)

legend("topright", legend = c("Total Death", "Total Death 2021","Total Death 2022"), 
       col = c("blue", "red","green"),lty = 1, )

# draw the vertical for the means of dependent variable distribution line;
abline(v = mean(pTotal_Death), col = "blue", lwd = 1, lty = 4)
abline(v = mean(pTotal_Death2021), col = "green", lwd = 1, lty = 4)
abline(v = mean(pTotal_Death2020), col = "red", lwd = 1, lty = 4)

# run hypothesis test on some of the Independent Variables;
t.test(pTotal_Death, pChild_Age, paired = FALSE, var.equal = FALSE)
wilcox.test(pTeen_Age,pHigh_Disability, paired = FALSE)
wilcox.test(pMixed,pTotal_Death, paired = FALSE)

# ================================================================================================================================
# Factor Analysis
#=================================================================================================================================

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.5, proceed to factor analysis
selected.IVs <- DS7006E[,c("pChild_Age","pTeen_Age","pHigh_Disability","pDist_0to5km"
                             ,"pDist_5to20km","pDist_20to40km","pDist_40orOver","pFrom_Home",
                           "pPart_Time","pFemale","pMixed")]
# KMO test for determining factorability of the variables;                          
KMO(cor(selected.IVs))

all.IVs <- DS7006E[,c("pChild_Age","pTeen_Age","pYoung_Age","pOld_Age","pHigh_Disability",
                        "pLow_Disability","pNo_Disability","pTotal_Dist","pDist_0to5km",
                        "pDist_5to20km","pDist_20to40km","pDist_40orOver","pFrom_Home","pHW_Total",
                        "pPart_Time","pFull_Time","pOver_Time","pMales","pFemale","pWhite","pMixed",
                        "pAsian","pBlack","pOther_Ethnic")]

KMO(cor(all.IVs))

# get eigenvalues: eigen() uses a correlation matrix 
eigenV <- eigen(cor(all.IVs))
round(eigenV$values,3)
# plot a scree plot of eigenvalues
op <- par(mar = c(5, 8, 4, 2) + 0.1)
plot(eigenV$values, type="b", frame.plot = FALSE, col="cyan4",xlim = c(0,12),xlab="variables", ylim = c(0,10))

# calculate cumulative proportion of eigenvalue and plot
eigenV.sum<-0
for(i in 1:length(eigenV$value)){
  eigenV.sum<-eigenV.sum+eigenV$value[i]
}
eigenPv.List1<-1:length(eigenV$value)
for(i in 1:length(eigenV$value)){
  eigenPv.List1[i]=eigenV$value[i]/eigenV.sum
}
eigenCv.List2<-1:length(eigenV$value)
eigenCv.List2[1]<-eigenPv.List1[1]
for(i in 2:length(eigenV$value)){
  eigenCv.List2[i]=eigenCv.List2[i-1]+eigenPv.List1[i]
}
eigenCv.List2

# plot the cumulative eigenvalue for identifying numbers of factors;
plot (eigenCv.List2, type="b", col="brown",xlim = c(0,25),ylim = c(0.3,1), xlab="Number of Components",
      frame.plot = FALSE,ylab ="Cumulative Proportion of Eigen Value")

# perform principal component analysis on all independent variables;
PCA <- principal(all.IVs, nfactors=5, rotate="varimax")


#========================================================================================================
#Clustering 
#==========================================================================================================

set.seed(12345)

clusData <- DS7006E[,c("Total_Death","Part_Time","From_Home","Young_Age")]

clusData[clusData ==0] <-NA
clusData <- na.omit(clusData)
MyCluster <- apply(clusData, MARGIN = 2, FUN = function(X) (log10(X)))

# K-Means Cluster Analysis
kmeanClust <- kmeans(MyCluster, 5) # 5 cluster solution
# Visualize the cluster
fviz_cluster(kmeanClust, data =MyCluster,palette = c("cyan3","yellow4","red","blue","green"), 
             geom = "point", ellipse.type = "convex", ggtheme = theme_bw())

# Prepare clustered data
ClusData <- data.frame(DS7006E$Geo_Code)
names(ClusData)[1] <- "Geo_Code"
ClusData <- within(ClusData, Area_name <- DS7006E$Geography)
ClusData <- within(ClusData, Total_Death <- DS7006E$pTotal_Death)
ClusData <- within(ClusData, Part_Time <- DS7006E$pPart_Time)
ClusData <- within(ClusData, From_Home <- DS7006E$pFrom_Home)
ClusData <- within(ClusData, Young_Age <- DS7006E$pYoung_Age)
ClusData[ClusData==0] <- NA
ClusData <- na.omit(ClusData)
ClusData <- within(ClusData, Cluster <- fit$cluster)
# writer clustered data into a CSV file;
write.csv(ClusData,file.choose())

#read the clustered data for visualizing into 3d pie chart;
Death_Cluster = getData("Death_Cluster.csv")
#define labales for pie charts;
cluster <- c("First","Second","Third","Fourth","Fifth")
#draw 3d pie chart based on clustered total number of deaths;
pie3D(Death_Cluster$Death.1000,labels=cluster,explode=0.05,labelcex = 0.8,
      main="Death Proportion in Each Cluster",labelcol = "brown")

#======================================================================================================
# Modeling: Regression models
#======================================================================================================
# create data object from all the independent variables;
all.IVs.PCA <- DS7006E[,c("pChild_Age","pTeen_Age","pYoung_Age","pOld_Age","pHigh_Disability",
                          "pLow_Disability","pNo_Disability","pTotal_Dist","pDist_0to5km","pDist_5to20km",
                        "pDist_20to40km","pDist_40orOver","pFrom_Home","pHW_Total","pPart_Time","pFull_Time",
                        "pOver_Time","pMales","pFemale","pWhite","pMixed","pAsian","pBlack","pOther_Ethnic")]
#creat PCA factors from all variables;
PCA <- principal(all.IVs.PCA, nfactors=5, rotate="varimax")

#Create regression model based on the PCA factors;
modelPCA.a <- lm(pTotal_Death~.,data.frame(PCA$scores))

#check the summary of regression attributes;
summary(modelPCA.a)
#Check for collinearity in the model;
vif(modelPCA.a)
sqrt(vif(modelPCA.a)) > 2

#Check for normality of the model residuals;
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
#Q-Q plot for model residuals;
qqnorm(modelPCA.a$residuals,ylab = "Sample Quantiles:PCA Model Residuals",
       xlab = "Theoretical Quantiles: PCA Model Residuals",
       main = "PCA Model Residuals Normality",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline(modelPCA.a$residuals, col=6, lwd = 1)
# the model residual histogram;
hist(modelPCA.a$residuals, prob = TRUE, col = "cyan4", main = "PCA Model Residual Distribution", 
     xlab = "PCA Model Residual")
rug (modelPCA.a$residuals, col = "red")
# Draw probability density line to the histogram;
lines(density(modelPCA.a$residuals),lwd = 2,col = "yellow")
# Calcualte the x and y axis data points 
x21 <- seq(from = min(modelPCA.a$residuals), to = max(modelPCA.a$residuals), by = 0.1)
y21 = dnorm(x21, mean(modelPCA.a$residuals), sd(modelPCA.a$residuals))
#Draw normal line for the distribution in histogram;
lines(x21, y21, lwd = 2, col = "blue", cex = 0.5)

legend("topright", legend = c("Density curve", "Normal curve"),cex = 0.7,
       col=c("yellow","blue"), lwd=2, lty=c(1,1))
#Test the normality of residual distribution using KS-Test;
ks.test(modelPCA.a$residuals,"pnorm",mean(modelPCA.a$residuals),sd(modelPCA.a$residuals))

#creat data object from all the indpendent variables;
all.IVs.a <- DS7006E[,c("pChild_Age","pTeen_Age","pYoung_Age","pHigh_Disability","pLow_Disability",
                     "pDist_0to5km","pDist_5to20km","pDist_20to40km","pDist_40orOver","pFrom_Home",
                      "pPart_Time","pFull_Time","pOver_Time","pFemale","pWhite","pMixed",
                     "pAsian","pBlack")]
#Create regression model from all the indepndent variables;
modelAll.a <- lm(pTotal_Death ~.,data = data.frame(all.IVs.a))

summary(modelAll.a)
vif(modelAll.a)
sqrt(vif(modelAll.a)) > 2
ks.test(modelAll.a$residuals,"pnorm",mean(modelAll.a$residuals),sd(modelAll.a$residuals))

#creating the best model out of modelAll.a
modelAll.best.a <- stepwise(modelAll.a, direction = "forward")
#checking for collinearity between the model regressors 
vif(modelAll.best.a)
sqrt(vif(modelAll.best.a)) > 2
summary(modelAll.best.a)

#check for the normality of the model residuals;
mabResidual <- modelAll.best.a$residuals
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
qqnorm(mabResidual,ylab = "Sample Quantiles:PCA Model Residuals",
       xlab = "Theoretical Quantiles: PCA Model Residuals",
       main = "PCA Model Residuals Normality",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline(mabResidual, col=6, lwd = 1)

hist(mabResidual, prob = TRUE, col = "cyan4", main = "PCA Model Residual Distribution", 
     xlab = "PCA Model Residual")
rug (mabResidual, col = "red")
# Draw probability density line to the histogram;
lines(density(mabResidual),lwd = 2,col = "yellow")
# Calcualte the x and y axis data points 
x21 <- seq(from = min(mabResidual), to = max(mabResidual), by = 0.1)
y21 = dnorm(x21, mean(mabResidual), sd(mabResidual))
#Draw normal line for the distribution in histogram;
lines(x21, y21, lwd = 2, col = "blue", cex = 0.5)

legend("topright", legend = c("Density curve", "Normal curve"),cex = 0.7,
       col=c("yellow","blue"), lwd=2, lty=c(1,1))

ks.test(mabResidual,"pnorm",mean(mabResidual),sd(mabResidual))

#create data object from all selected variables;
selected.IVs.a <- DS7006E[,c("pChild_Age","pTeen_Age","pDist_0to5km","pDist_5to20km",
                             "pDist_20to40km","pDist_40orOver","pFrom_Home","pHigh_Disability",
                             "pPart_Time","pFemale","pMixed")]

#create regression model based on the selected variables;
modelSIV.a <- lm(pTotal_Death~.,data = data.frame(selected.IVs.a))

summary(modelSIV.a)
#check for collinearity
vif(modelSIV.a)
sqrt(vif(modelSIV.a)) > 2

# refined model based on removing some of the selected variales due to collinearity;
selected.IVs.b <- DS7006E[,c("pChild_Age","pTeen_Age","pDist_5to20km","pDist_20to40km","pFrom_Home",
                             "pHigh_Disability","pPart_Time","pFemale","pMixed")]
modelSIV.b <- lm(pTotal_Death~.,data = data.frame(selected.IVs.b))

summary(modelSIV.b)
#check for collinearity;
vif(modelSIV.b)
sqrt(vif(modelSIV.b)) > 2
#check for the model residuals normality;
msivResidual <- modelSIV.b$residuals
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
qqnorm(msivResidual,ylab = "Sample Quantiles:Selected IVs Model Residuals",
       xlab = "Theoretical Quantiles: Selected IVs Residuals",
       main = "Selected IVs Residuals Normality",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline(msivResidual, col=6, lwd = 1)

hist(msivResidual, prob = TRUE, col = "cyan4", main = "Selected IVs Residual Distribution", 
     xlab = "Selected IVs Residual")
rug (msivResidual, col = "red")
# Draw probability density line to the histogram;
lines(density(msivResidual),lwd = 2,col = "yellow")
# Calcualte the x and y axis data points 
x21 <- seq(from = min(msivResidual), to = max(msivResidual), by = 0.1)
y21 = dnorm(x21, mean(msivResidual), sd(msivResidual))
#Draw normal line for the distribution in histogram;
lines(x21, y21, lwd = 2, col = "blue", cex = 0.5)

legend("topright", legend = c("Density curve", "Normal curve"),cex = 0.7,
       col=c("yellow","blue"), lwd=2, lty=c(1,1))

ks.test(msivResidual,"pnorm",mean(msivResidual),sd(msivResidual))

#create best model out of the refined version through stepwise process;
modelSIV.best <- stepwise(modelSIV.b, direction = "forward")

summary(modelSIV.best)
#check for collinearity
vif(modelSIV.best)
sqrt(vif(modelSIV.best)) > 2

#chekc for the model residuals normality;
msivbResidual <- modelSIV.best$residuals
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
qqnorm(msivbResidual,ylab = "Sample Quantiles:Selected IVs Best Model Residuals",
       xlab = "Theoretical Quantiles: Selected IVs Best Residuals",
       main = "Selected IVs Best Residuals Normality",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline(msivbResidual, col=6, lwd = 1)

hist(msivbResidual, prob = TRUE, col = "cyan4", main = "Select IVs Best Residual Distribution", 
     xlab = "Selected IVs Best Model Residual")
rug (msivbResidual, col = "red")
# Draw probability density line to the histogram;
lines(density(msivbResidual),lwd = 2,col = "yellow")
# Calcualte the x and y axis data points 
x21 <- seq(from = min(msivbResidual), to = max(msivbResidual), by = 0.1)
y21 = dnorm(x21, mean(msivbResidual), sd(msivbResidual))
#Draw normal line for the distribution in histogram;
lines(x21, y21, lwd = 2, col = "blue", cex = 0.5)

legend("topright", legend = c("Density curve", "Normal curve"),cex = 0.7,
       col=c("yellow","blue"), lwd=2, lty=c(1,1)) 

ks.test(msivbResidual,"pnorm",mean(msivbResidual),sd(msivbResidual))

#compare all models through R-squared, AIC, number of variables, normality of residuals
AIC(modelPCA.a,modelAll.a,modelAll.best.a,modelSIV.b,modelSIV.best)

#check the relative importent regressors in the selected best model;
calc.relimp(modelAll.best.a, type = c("lmg"), rela = TRUE)

#select variables for the best final model;
finalRegressors <- DS7006E[,c("pPart_Time","pFrom_Home","pYoung_Age")]
#creat final model;
finalModel <- lm(pTotal_Death~., data = data.frame(finalRegressors))

summary(finalModel)
#check for collinearity in the best final model;
vif(finalModel)

#scatter plot for final model residuals and fitted values;
plot(finalModel$residuals ~ finalModel$fitted.values,col = "blue", pch = 20,frame.plot = FALSE,  
     xlim = c(0.8,3.5),xlab = "Fitted values", ylab = "Residuals")

#scatter plot for covid-19 deaths and final model fitted values;
plot(pTotal_Death ~ finalModel$fitted.values,col = "blue", pch = 20,frame.plot = FALSE,  
     xlim = c(0.8,3.5), xlab = "Covid Deaths Number", ylab = "Fitted Value")

plot(x = finalModel, which = 3, col = "blue", pch = 20,frame.plot = FALSE)
yhat <- fitted.values( object = finalModel )
plot( x = yhat, y = pTotal_Death,xlab = "Fitted Values",ylab = "Observed Values",col = "blue", pch = 20,
      frame.plot = FALSE
        )
# checking the the linearity of the final model 
plot(x = finalModel, which = 1, xlim = c(0.8,3.5),lwd = 2,col = "blue",pch = 20, frame.plot = FALSE)

residualPlots( model = finalModel, col = "blue",pch = 20 )

plot( x = finalModel, which = 2,frame.plot = FALSE, pch = 20, col = "blue")

plot( x = finalModel, which = 3,xlim = c(0.8,3.5),lwd = 2,frame.plot = FALSE, pch = 20, col = "blue")




qqnorm(finalModel$residuals,ylab = "Sample Quantiles:Final Model Residuals",
       xlab = "Theoretical Quantiles: Final Model Residuals",
       main = "Final Model Residuals Normality",col = 4, 
       frame.plot = FALSE, pch = 20, cex.axis = 0.7 )
qqline(finalModel$residuals, col=6, lwd = 1)
