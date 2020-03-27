##Project: Prediction of Subsidy Awarded to an Organization##




library(mosaic) 
library(ggplot2) 
library(ggthemes) 
library(plotly) 
library(dplyr)
library(gmodels)
library(gplots)
library(psych)
library(tidyverse)
library(car) # for detailed correlation plot 
library(corrplot) # for correlation plot
library(plotrix)
library(Hmisc)
load("finalreport.RData")
dim(Subsidy_Data)

#cleaning the dependent variable, removing $ and ,
Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal<-gsub(",", "", Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal) 
Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal= as.numeric(gsub("[\\$,]", "", Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal)) 
Subsidy_Data<-Subsidy_Data[!is.na(Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal),]

#Dependent variable distribution
plot(density(Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal),main = 'Density Plot of Subsidy Value',col='coral')
#Log transform since dependent variable is skewed right
plot(density(log(Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal+1)),main = 'Density Plot of Subsidy Value') 
#zero inflated dependent variable

#Removing zeros
Subsidy_Data<-Subsidy_Data[Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal!=0,]
Subsidy_Data$subval<-Subsidy_Data$Subsidy.Value.Adjusted.For.Megadeal
#checking for any NA in dependent variable
anyNA(Subsidy_Data$subval)


plot(density(log(Subsidy_Data$subval+1)),main = 'Density Plot of Log of Subsidy Value') 
Subsidy_Data$subval<-log(Subsidy_Data$subval+1)
summary(Subsidy_Data$subval)

#Removing unwanted variables
Subsidy_Data <- subset(Subsidy_Data, select = -c(2,4,5,6,7,8,9,11,12,17,18,19,20,21,22,24,25,26,27,28))

####YEAR

anyNA(Subsidy_Data$Year)
#Replacing NA with median YEAR
year.median <- median(Subsidy_Data$Year, na.rm=TRUE)
Subsidy_Data$Year[is.na(Subsidy_Data$Year)] =year.median
year.median
anyNA(Subsidy_Data$Year)

#univariate analysis for YEAR 
summary(Subsidy_Data$Year)
sd(Subsidy_Data$Year)

table(Subsidy_Data$Year)
#we see we dont have much data for years until 2000, so we can drop the other years
Subsidy_Data2<-Subsidy_Data[Subsidy_Data$Year>1999 & Subsidy_Data$Year<2017,]


plot(Subsidy_Data2$Year,Subsidy_Data2$subval,main = 'Years Vs Log of Subsidy',col='turquoise')
#looks more like a categorical variable than numerical

cor(Subsidy_Data2$Year,Subsidy_Data2$subval)


#creating a new column no.of years, how many years before 2018 was the subsidy granted
Subsidy_Data2$no.ofyears<-abs(max(Subsidy_Data2$Year)-Subsidy_Data2$Year)
plot(density(Subsidy_Data2$no.ofyears),main = 'Density Plot of No.of Years')#the distributions looks right skewed

#sqrt transformation for no.of years
plot(density(sqrt(Subsidy_Data2$no.ofyears)),main = 'Density Plot of No.of Years')
Subsidy_Data2$no.ofyears<-sqrt(Subsidy_Data2$no.ofyears)

cor(Subsidy_Data2$no.ofyears,Subsidy_Data2$subval)


#Hypothesis testing to see the significance of no.of years in determining Log subsidy
cor.test(Subsidy_Data2$subval,Subsidy_Data2$no.ofyears)


####UNEMPLOYMENT RATE%
install.packages('tidyr')
library(tidyr)

unemp<-read.csv('unemp.csv')

#to make sure the datasets can be combined by values in the column states
Subsidy_Data2$Location<-tolower(Subsidy_Data2$Location)
unemp$Location<-tolower(unemp$Location)
unemp_DF <- unemp %>% gather(Year, unemprate_percent, c(2:18)) #to convert the wide data longer 
unemp_DF$Year<-as.numeric(gsub("[\\X,]", "", unemp_DF$Year))
head(unemp_DF)

str(unemp_DF)

Subsidy_Data_merge<-merge(x = Subsidy_Data2, y = unemp_DF, by = c('Location','Year'))#To merge the unemp_DF with the Subsidy_Data
head(Subsidy_Data_merge$unemprate_percent)

dim(Subsidy_Data_merge)


#univariate

plot(density(sqrt(Subsidy_Data_merge$unemprate_percent)))#We did the square root transformation on the Unemployment rate to conform with normal distribution.

Subsidy_Data_merge$unemprate_percent<-sqrt(Subsidy_Data_merge$unemprate_percent)
summary(Subsidy_Data_merge$unemprate_percent)

#Bivariate 

cor(Subsidy_Data_merge$unemprate_percent,Subsidy_Data_merge$subval, use="complete.obs", method="pearson") 
#weak negative correlation


plot(Subsidy_Data_merge$unemprate_percent,Subsidy_Data_merge$subval,col='light grey',main = 'Plot of Unemployment Rate Vs Log Subval')
abline(lm(Subsidy_Data_merge$subval~Subsidy_Data_merge$unemprate_percent), col="coral", lwd=2.5)

#Hypothesis
cor.test(Subsidy_Data_merge$unemprate_percent,Subsidy_Data_merge$subval)
#the true correlation is not zero
#The p-value indicates that there is statistically significant correlation between the unemployment rate and average Log of subsidy at 95% confidence.

####POLITICAL PARTY(VOTING)
elections<-read.csv('elections.csv')

elections$Location<-tolower(elections$Location)
elections_DF <- elections %>% gather(Year, Voting, c(2:18)) #to convert the wide data longer 

head(elections_DF)
str(elections_DF)

elections_DF$Voting<-as.factor(elections_DF$Voting) #created a column Voting- a categorical variable with two levels- R(Republican) and D(Democrat)
elections_DF$Year<-as.numeric(gsub("[\\X,]", "", elections_DF$Year))

Subsidy_Data_merge<-merge(x = Subsidy_Data_merge, y = elections_DF, by = c('Location','Year'))#To merge the elections_DF with the Subsidy_Data
head(Subsidy_Data_merge$Voting)

#univariate 
summary(Subsidy_Data_merge$Voting)
table(Subsidy_Data_merge$Voting) 

t <- table(Subsidy_Data_merge$Voting) 
# Bar plot 

ptab<-prop.table(t)
ptab<-ptab*100 # Convert to percentages 

barplot(ptab, main = "Bar Plot", 
        xlab = "Voting(Political Party)",
        ylab = "Proportion", 
        col=c('salmon','turquoise'))

box()

#Bivariate
# Using dplyr
voting_subval<- Subsidy_Data_merge %>% group_by(Voting) %>% 
  summarise(avg = mean(subval),
            median = median(subval), std = sd(subval))
voting_subval

#Density plot
r<-Subsidy_Data_merge[Subsidy_Data_merge$Voting=="R",]
d<-Subsidy_Data_merge[Subsidy_Data_merge$Voting=="D",]
plot(density(r$subval), col="salmon", lwd=2.5, main="Distribution of Log Subsidy by  Voting(Political party)")
lines(density(d$subval), col="turquoise", lwd=2.5)
legend("topright", 
       legend = c('Republican', 'Democrat'), 
       fill = c("salmon", "turquoise"))

#Box plot
boxplot(subval ~Voting, data=Subsidy_Data_merge, 
        main="Comparative boxplot of Log Subsidy Value by Voting(Political Party)", 
        col=c("salmon", "turquoise") )

#ANOVA

voting_subval.aov <- aov(subval~Voting, data=Subsidy_Data_merge)
voting_subval.aov
summary(voting_subval.aov) # Null rejected.
#there is a statistical difference between the average Log of Subsidy of the two levels in the Voting(political party).

#Tukey pairwise comparisons
TukeyHSD(voting_subval.aov)

# verify ANOVA assumptions
dev.off()
par(mfrow=c(2,2))
plot(voting_subval.aov) # We have a few "outliers"
# Normal distribution assumption 
dev.off()


#to test the hypothesis that average value of log subsidy from Republicans is more than that of Democrats
subval_null_vote <- t.test(Subsidy_Data_merge$subval[Subsidy_Data_merge$Voting=="R"], 
                           alternative="greater",
                           mu=mean(Subsidy_Data_merge$subval[Subsidy_Data_merge$Voting=="D"]), conf.level=0.95)
subval_null_vote

#the low value of p value indicates that null can be rejected and the alternative that 
#the average value of log subsidy of Republicans is higher than that of the Democrats.

####LOCATION

#dividing location into 5 US regions for better analysis
Subsidy_Data_merge$Location <- as.factor(tolower(Subsidy_Data_merge$Location))

Subsidy_Data_merge$Region[Subsidy_Data_merge$Location %in% c("texas","oklahoma", "new mexico","arizona")]<-"Southwest"
Subsidy_Data_merge$Region[Subsidy_Data_merge$Location %in% c("colorado","wyoming", "montana","idaho", "washington", "oregon","utah", "nevada","california","alaska", "hawaii",'district of columbia')]<-"West"
Subsidy_Data_merge$Region[Subsidy_Data_merge$Location %in% c("ohio","indiana", "michigan","illinois", "missouri", "wisconsin","minnesota", "iowa","kansas","nebraska", "south dakota", "north dakota")]<-"Midwest"
Subsidy_Data_merge$Region[Subsidy_Data_merge$Location %in% c("west virginia","virginia", "kentucky","tennessee", "north carolina", "south carolina","georgia", "alabama","mississippi","arkansas", "louisiana", "florida")]<-"Southeast"
Subsidy_Data_merge$Region[Subsidy_Data_merge$Location %in% c("maine","massachusetts", "rhode island","connecticut", "new hampshire", "vermont","new york", "pennsylvania","new jersey","delaware", "maryland")]<-"Northeast"
Subsidy_Data_merge$Region[Subsidy_Data_merge$Location %in% c("united states")]<-"United states"

#convert regions to factor variable
Subsidy_Data_merge$Region<-as.factor(Subsidy_Data_merge$Region)
anyNA(Subsidy_Data_merge$Region) # to check for any NA values in the data

#Univariate Analysis
levels(Subsidy_Data_merge$Region)


t <- table(Subsidy_Data_merge$Region)
t
#There are approximately similar number of subsidy provided in northeast and southeast, midwest and west, southwest has the least number of subsidies provided.
#Subsidies provided by federal gov are accumulated under United states.

ptab <- prop.table(t) #to get proportional table
ptab<-ptab*100
ptab

barplot(ptab, main = "Region-wise subsidy Bar Plot", xlab = "Region", ylab = "Proportion" , col=c("orange", "steelblue", "yellow", "green", "red"), ylim=c(0,30))
box()


#Analyse one factor and one numeric variable(region(location) and subsidy value adjusted)

library(psych)
describeBy(Subsidy_Data_merge$subval , Subsidy_Data_merge$Region)
#OR
aggregate(subval ~ Region , data=Subsidy_Data_merge, FUN="mean")
aggregate(subval ~ Region , data=Subsidy_Data_merge, FUN="sd")

#log subsidy value arranged highest to lowest region wise
RegionwiseSubsidy <- aggregate(subval ~ Region, data=Subsidy_Data_merge, FUN="mean")
x <- sort(RegionwiseSubsidy$subval, decreasing = T)
x <- RegionwiseSubsidy[order(RegionwiseSubsidy$subval, decreasing=T),]
TopRegion <- head(x, n=6)
TopRegion

#Companies for which physical presence in a particular state is unknown are listed under United #States as region. These form the group of companies to receive large number of subsidies but #they cannot be said to belong to a particular region in the US.
#Midwest region of the US receives the highest number of subsidies.
#West region receives the lowest subsidies.

#Visualize the data
options(scipen = 99)
boxplot(subval ~ Region, data=Subsidy_Data_merge, 
        main="Comparative boxplot of Log Subsidy Value by Region", xlab = "Region", ylab = "Log of Subsidy value", 
        col=c("lightgreen", "lightblue", "coral", "darkgrey", "red", "grey") )


#Anova Test
Subsidy_Data_merge %>% group_by(Region) %>% summarise(avg = mean(subval), med = median(subval), std = sd(subval)) 
subsidy_source.aov <- aov(subval~Region, data=Subsidy_Data_merge)
subsidy_source.aov 
summary(subsidy_source.aov)

#The p-value is less than 0.05, and therefore we reject the null hypothesis. There is a statistically #significant difference between the average Log Subsidy value awarded to companies in #different regions.


#Economically more progressive vs less progressive states and Log Subsidy
mis<-Subsidy_Data_merge[Subsidy_Data_merge$Location=="mississippi",]
ark<-Subsidy_Data_merge[Subsidy_Data_merge$Location=="arkansas",]
wash<-Subsidy_Data_merge[Subsidy_Data_merge$Location=="washington",]
mas<-Subsidy_Data_merge[Subsidy_Data_merge$Location=="massachusetts",]

plot(density(wash$subval), col="salmon", main="Distribution of Log Subsidy by State")
lines(density(ark$subval), col="turquoise")
lines(density(mis$subval), col="blue")
lines(density(mas$subval), col="grey")
legend("topright", 
       legend = c("Washington", "Arkansas",'mississippi','massachusetts'), 
       fill = c("salmon", "turquoise",'blue','grey'))

#t.test for the hypothesis: Economically less progressive states are provided more subsidy(average log subsidy) as compared to Economically more progressive states.

subval_null <- t.test(Subsidy_Data_merge$subval[Subsidy_Data_merge$Location=="arkansas"|Subsidy_Data_merge$Location=="mississippi"], 
                      alternative="greater",
                      mu=mean(Subsidy_Data_merge$subval[Subsidy_Data_merge$Location=="massachusetts"|Subsidy_Data_merge$Location=="washington"]), conf.level=0.95)
subval_null

Subsidy_Data_merge$subval[Subsidy_Data_merge$Location=="kentucky"]
levels(Subsidy_Data_merge$Location)
Subsidy_Data_merge[Subsidy_Data_merge$Location=="kentucky",]
Subsidy_Data_merge$Location<-as.factor(Subsidy_Data_merge$Location)
table(Subsidy_Data_merge$Location)

####PARENT SECTOR

#Parent Industries of companies have been divided into 4 main sectors like Primary, Secondary, Tertiary and Quaternary based on type of sector. There are four types of industry. These are primary, secondary, tertiary and quaternary.
Subsidy_Data_merge$ParentSector<-rep(NA, nrow(Subsidy_Data_merge))
Subsidy_Data_merge$ParentSector[Subsidy_Data_merge$Major.Industry.of.Parent=='agribusiness'| Subsidy_Data_merge$Major.Industry.of.Parent=='mining and minerals'|  Subsidy_Data_merge$Major.Industry.of.Parent==""]<-'Primary'
Subsidy_Data_merge$ParentSector[Subsidy_Data_merge$Major.Industry.of.Parent=='utilities and power generation'| Subsidy_Data_merge$Major.Industry.of.Parent=='electrical and electronic equipment'|Subsidy_Data_merge$Major.Industry.of.Parent=='chemicals'| Subsidy_Data_merge$Major.Industry.of.Parent=='miscellaneous manufacturing'|Subsidy_Data_merge$Major.Industry.of.Parent=='industrial equipment'| Subsidy_Data_merge$Major.Industry.of.Parent=='automotive parts'|Subsidy_Data_merge$Major.Industry.of.Parent=='beverages'| Subsidy_Data_merge$Major.Industry.of.Parent=='housewares and home furnishings'| Subsidy_Data_merge$Major.Industry.of.Parent=='household and personal care products'| Subsidy_Data_merge$Major.Industry.of.Parent=='paper and packaging'| Subsidy_Data_merge$Major.Industry.of.Parent=='food products'| Subsidy_Data_merge$Major.Industry.of.Parent=='construction and engineering'| Subsidy_Data_merge$Major.Industry.of.Parent=='oilfield services and supplies'| Subsidy_Data_merge$Major.Industry.of.Parent=='metals'| Subsidy_Data_merge$Major.Industry.of.Parent=='oil and gas'| Subsidy_Data_merge$Major.Industry.of.Parent=='motor vehicles'| Subsidy_Data_merge$Major.Industry.of.Parent=='apparel'| Subsidy_Data_merge$Major.Industry.of.Parent=='building materials'| Subsidy_Data_merge$Major.Industry.of.Parent=='miscellaneous energy products and systems'| Subsidy_Data_merge$Major.Industry.of.Parent=='heavy equipment'| Subsidy_Data_merge$Major.Industry.of.Parent=='paints and coatings'| Subsidy_Data_merge$Major.Industry.of.Parent=='tobacco']<-'Secondary'
Subsidy_Data_merge$ParentSector[Subsidy_Data_merge$Major.Industry.of.Parent=='freight and logistics'| Subsidy_Data_merge$Major.Industry.of.Parent=='healthcare services'|Subsidy_Data_merge$Major.Industry.of.Parent=='wholesalers'|Subsidy_Data_merge$Major.Industry.of.Parent=='retailing'|Subsidy_Data_merge$Major.Industry.of.Parent=='diversified'|Subsidy_Data_merge$Major.Industry.of.Parent=='business services'|Subsidy_Data_merge$Major.Industry.of.Parent=='medical equipment and supplies'|Subsidy_Data_merge$Major.Industry.of.Parent=='financial services'|Subsidy_Data_merge$Major.Industry.of.Parent=='hotels'|Subsidy_Data_merge$Major.Industry.of.Parent=='entertainment'|Subsidy_Data_merge$Major.Industry.of.Parent=='restaurants and foodservice'|Subsidy_Data_merge$Major.Industry.of.Parent=='real estate'|Subsidy_Data_merge$Major.Industry.of.Parent=='airlines'|Subsidy_Data_merge$Major.Industry.of.Parent=='waste management and environmental services'|Subsidy_Data_merge$Major.Industry.of.Parent=='railroads'|Subsidy_Data_merge$Major.Industry.of.Parent=='pipelines'|Subsidy_Data_merge$Major.Industry.of.Parent=='industrial services']<-'Tertiary'  
Subsidy_Data_merge$ParentSector[Subsidy_Data_merge$Major.Industry.of.Parent=='information technology'| Subsidy_Data_merge$Major.Industry.of.Parent=='miscellaneous services'|Subsidy_Data_merge$Major.Industry.of.Parent=='telecommunications'|Subsidy_Data_merge$Major.Industry.of.Parent=='media'|Subsidy_Data_merge$Major.Industry.of.Parent=='private equity (including portfolio companies)'|Subsidy_Data_merge$Major.Industry.of.Parent=='aerospace and military contracting'|Subsidy_Data_merge$Major.Industry.of.Parent=='pharmaceuticals'|Subsidy_Data_merge$Major.Industry.of.Parent=='information services']<-'Quaternary'
Subsidy_Data_merge$ParentSector<-as.factor(Subsidy_Data_merge$ParentSector)

#Subsidy_Data_merge$ParentSector <- relevel(Subsidy_Data_merge$ParentSector, ref=2) 

Subsidy_Data_merge$ParentSector1[Subsidy_Data_merge$ParentSector=='Primary'
                                 |Subsidy_Data_merge$ParentSector=='Tertiary']<-'PrimaryandTertiary'
Subsidy_Data_merge$ParentSector1[Subsidy_Data_merge$ParentSector=='Secondary']<-'Secondary'
Subsidy_Data_merge$ParentSector1[Subsidy_Data_merge$ParentSector=='Quaternary']<-'Quaternary'

#table(Subsidy_Data_merge$ParentSector1)



#Univariate parentsector analysis

t <- table(Subsidy_Data_merge$ParentSector)
ptab<-prop.table(t)
ptab<-ptab*100 # Convert to percentages

#Visualization :-
barplot(ptab, main = "Parent Sector VS Subsidy",
        xlab = "Sector",
        ylab = "Subsidy",
        col=c("orange", "blue","pink", "red"),
        ylim=c(0,95))
box() #barplot for comparison
#Outliers
boxplot(Subsidy_Data_merge$subval~ Subsidy_Data_merge$ParentSector, main = "Parent Sector VS Subsidy",
        xlab = "Sector",
        ylab = "Subsidy",
        col=c("orange", "blue", "pink", "red"))
box()

# Outliers and removing them :-
b_pricemodel <- boxplot(Subsidy_Data_merge$subval~Subsidy_Data_merge$ParentSector, plot=F)
outliersdf_pricemodel <- b_pricemodel$out
outliersdf_pricemodel <- Subsidy_Data_merge[Subsidy_Data_merge$subval %in% outliersdf_pricemodel,] 
del_pricemodel <- as.numeric(rownames(outliersdf_pricemodel))
Subsidy_Data_merge2 <- Subsidy_Data_merge[-del_pricemodel, ]
par(mfrow=c(2,1))
boxplot(Subsidy_Data_merge$subval~Subsidy_Data_merge$ParentSector, col=c("red", "orange", "green"))
boxplot(Subsidy_Data_merge2$subval~Subsidy_Data_merge2$ParentSector, col=c("red", "orange", "green"))

#Bivariate 

boxplot(Subsidy_Data_merge$subval~Subsidy_Data_merge$ParentSector, main="Distribution of Subsidy acc to Parent Sector", xlab="Parent Sector", col=c("black", "pink"))

Subsidy_Data_merge %>% group_by(Subsidy_Data_merge$ParentSector) %>% summarise(avg = mean(Subsidy_Data_merge$subval), median = median(Subsidy_Data_merge$subval), sd = sd(Subsidy_Data_merge$subval), freq=n())

# Correlation & AOV TEST 

emp_train.aov <- aov(Subsidy_Data_merge$subval~Subsidy_Data_merge$ParentSector, data=Subsidy_Data_merge)
emp_train.aov
summary(emp_train.aov) #Hence all the sectors are important because we have subsetted pirmary and Tertiary together
TukeyHSD(emp_train.aov) #pairwise comparison
dev.off()

par(mfrow=c(2,2))
plot(emp_train.aov) # We have a few "outliers"
# Normal distribution assumption
dev.off()
emp_train.fit <- lm(Subsidy_Data_merge$subval~Subsidy_Data_merge$ParentSector, data=Subsidy_Data_merge)
qqPlot(emp_train.fit, col="steelblue", pch=16, envelope=T, col.lines=palette()[1]) #plotting regression line
#The p-value is less than 0.05, and therefore we reject the null hypothesis. There is a statistically significant difference between the average Log Subsidy value awarded to different sectors.

####TYPE of SUBSIDY

#Evaluating Type of Subsidy

#Converting all values to lower case
Subsidy_Data_merge$Type.of.Subsidy <- as.factor(tolower(Subsidy_Data_merge$Type.of.Subsidy))
str(Subsidy_Data_merge$Type.of.Subsidy)
table(Subsidy_Data_merge$Type.of.Subsidy)

t <- table(Subsidy_Data_merge$Type.of.Subsidy)
ptab<-prop.table(t)
ptab<-ptab*100


barplot(ptab, main = "Bar Plot", 
        xlab = "Types of Subsidy", 
        ylab = "Proportion", 
        col=c("salmon", "turquoise"), 
        ylim=c(0,95))

box()

#Binning these 14 categories into 4 as per WTO standardization

Subsidy_Data_merge$Subsidy.Category[Subsidy_Data_merge$Type.of.Subsidy=="federal grant"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="grant"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="infrastructure assistance"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="training reimbursement"]<-"Cash Subsidy"


Subsidy_Data_merge$Subsidy.Category[Subsidy_Data_merge$Type.of.Subsidy=="federal allocated tax credit"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="property tax abatement"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="tax credit/rebate"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="tax credit/rebate and grant"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="tax credit/rebate; property tax abatement"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="tax increment financing"]<-"Tax Concessions"


Subsidy_Data_merge$Subsidy.Category[Subsidy_Data_merge$Type.of.Subsidy=="grant/loan hybrid program"] <-"Loan Guarantees"

Subsidy_Data_merge$Subsidy.Category[Subsidy_Data_merge$Type.of.Subsidy=="cost reimbursement"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="enterprise zone"|
                                      Subsidy_Data_merge$Type.of.Subsidy=="megadeal"]<-"Government Procurement Policies"


Subsidy_Data_merge$Subsidy.Category <- as.factor(Subsidy_Data_merge$Subsidy.Category)
str(Subsidy_Data_merge$Subsidy.Category)
table(Subsidy_Data_merge$Subsidy.Category)
tabr <- table(Subsidy_Data_merge$Subsidy.Category)
ptabr<-prop.table(tabr)
ptabr<-ptabr*100

barplot(ptabr, main = "Bar Plot of Subsidy Types", 
        xlab = "Types of Subsidy", 
        ylab = "Proportion", 
        col=c("salmon", "turquoise"), 
        ylim=c(0,95))

dev.off()
#Bivariate analysis of Subsidy value and Types of Subsidy

aggregate(subval ~ Subsidy.Category, data=Subsidy_Data_merge, FUN="mean")

#Visual analysis

boxplot(subval ~ Subsidy.Category, data=Subsidy_Data_merge, main="Subsidy Value based on Type of Subsidy", 
        xlab="Type of Subsidy", ylab="Subsidy Value",
        col=c("salmon", "turquoise"))

#Anova Test
Subsidy_Data_merge %>% group_by(Subsidy.Category) %>% summarise(avg = mean(subval), med = median(subval), std = sd(subval)) 
subsidy_type.aov <- aov(subval~Subsidy.Category, data=Subsidy_Data_merge)
subsidy_type.aov 
summary(subsidy_type.aov)
#There is relationship between Subsidy Types
TukeyHSD(subsidy_type.aov)

# All are significant except Tax Concessions-Government Procurement Policies which is insignificant

####OWNERSHIP STRUCTURE

Subsidy_Data_merge$Ownership.Structure <- as.factor(tolower(Subsidy_Data_merge$Ownership.Structure))
#Since ownership structure had many levels, we have grouped all the joint ventures into one.
Subsidy_Data_merge$Ownership<- gsub("^joint.*", "Joint Venture", Subsidy_Data_merge$Ownership.Structure)
Subsidy_Data_merge$Ownership <- as.factor(Subsidy_Data_merge$Ownership)
#Univariate Analysis
table(Subsidy_Data_merge$Ownership) 
levels(Subsidy_Data_merge$Ownership)
levels(Subsidy_Data_merge$Ownership.Structure)
# Converting to 2 groups

Subsidy_Data_merge$Ownership.Group[Subsidy_Data_merge$Ownership=="alaska native-owned"|Subsidy_Data_merge$Ownership=="cooperative"|
                                     Subsidy_Data_merge$Ownership=="employee-owned"|Subsidy_Data_merge$Ownership=="government-owned"|
                                     Subsidy_Data_merge$Ownership=="government sponsored & publicly traded"|Subsidy_Data_merge$Ownership=="Joint Venture"|
                                     Subsidy_Data_merge$Ownership=="mutual"|Subsidy_Data_merge$Ownership=="non-profit"|
                                     Subsidy_Data_merge$Ownership=="out of business"|Subsidy_Data_merge$Ownership=="privately held"] <-"Group1"


Subsidy_Data_merge$Ownership.Group[Subsidy_Data_merge$Ownership=="publicly traded"] <-"Group2"  
Subsidy_Data_merge$Ownership.Group <- as.factor(Subsidy_Data_merge$Ownership.Group)
#Univariate analysis of Ownership Group
t <- table(Subsidy_Data_merge$Ownership.Group)
barplot(t, main = "Ownership Group Bar Plot",
        xlab = "Ownership Groups", ylab = "Frequency", col=c("salmon", "turquoise"))


#Bivariate Analysis of Ownership Group and Subsidy Value
aggregate(subval ~ Ownership.Group, data=Subsidy_Data_merge, FUN="mean")

boxplot(subval ~ Ownership.Group, data=Subsidy_Data_merge, main="Subsidy Value with respect to Ownership Groups", 
        xlab="Ownership Groups", ylab="Subsidy Value",
        col=c("salmon", "turquoise"))

#Anova Test
Subsidy_Data_merge %>% group_by(Ownership.Group) %>% summarise(avg = mean(subval), med = median(subval), std = sd(subval)) 
subsidy_type.aov <- aov(subval~Ownership.Group, data=Subsidy_Data_merge)
subsidy_type.aov 
summary(subsidy_type.aov)
#There is relationship between Ownership groups
TukeyHSD(subsidy_type.aov)

#The p- value indicates that we fail to reject the null and  there is  statistically significant correlation between the average Log Subsidy value awarded to different ownership groups.

####SUBSIDY SOURCE

#Convert Source of subsidy to factor
Subsidy_Data_merge$Subsidy.Source<-as.factor(Subsidy_Data_merge$Subsidy.Source)

#to merge "local" and "Local" into one level
Subsidy_Data_merge$Subsidy.Source<-factor(Subsidy_Data_merge$Subsidy.Source, levels = c("federal","local","Local","multiple","state"), labels = c("federal","local","local","multiple","state"))

#univariate
levels(Subsidy_Data_merge$Subsidy.Source)

t<-table(Subsidy_Data_merge$Subsidy.Source)  
t

#There are four categories of subsidy source: federal , local, multiple and state.
#Highest number of subsidies are awarded by the state.
#Very few companies receive multiple subsidies.

ptab <- prop.table(t) #to get proportional table
ptab<-ptab*100
ptab



barplot(ptab, main = "Subsidy source Bar Plot", xlab = "Subsidy Source", ylab = "Proportion" , col=c("orange", "steelblue", "yellow", "green"), ylim=c(0,60))
box()


#Analyse one factor and one numeric variable(subsidy source and subsidy value adjusted)

library(psych)
describeBy(Subsidy_Data_merge$subval , Subsidy_Data_merge$Subsidy.Source)
#OR
aggregate(subval ~Subsidy.Source , data=Subsidy_Data_merge, FUN="mean")
aggregate(subval ~Subsidy.Source , data=Subsidy_Data_merge, FUN="sd")

#log subsidy value arranged highest to lowest source wise
SourcewiseSubsidy <- aggregate(subval ~ Subsidy.Source, data=Subsidy_Data_merge, FUN="mean")
x <- sort(SourcewiseSubsidy$subval, decreasing = T)
x <- SourcewiseSubsidy[order(SourcewiseSubsidy$subval, decreasing=T),]
TopRegion <- head(x, n=4)
TopRegion
#Companies with multiple source receive highest average log subsidy value.
#Companies with only state source of subsidy receive lowest average log subsidy value.


#Visualize the data
options(scipen = 99)
boxplot(subval ~Subsidy.Source, data=Subsidy_Data_merge, 
        main="Comparative boxplot of Log Subsidy Value by Subsidy Source", xlab = "Souce of Subsidy", ylab = "Log of Subsidy value",
        col=c("lightgreen", "lightblue", "coral", "darkgrey") )

#Anova Test
Subsidy_Data_merge %>% group_by(Subsidy.Source) %>% summarise(avg = mean(subval), med = median(subval), std = sd(subval)) 
subsidy_source.aov <- aov(subval~Subsidy.Source, data=Subsidy_Data_merge)
subsidy_source.aov 
summary(subsidy_source.aov)
#The p-value is less than 0.05, and therefore we reject the null hypothesis. There is a statistically #significant difference among the average Log Subsidy value awarded to companies by different #sources. Companies with multiple source (subsidies from state as well as local) receive the #highest average (log)subsidy value.

#write.csv(Subsidy_Data_merge,'Subsidy_Data_merge.csv')

####MODEL 


mod1<-lm(subval~Subsidy.Source+Ownership.Group+Region+no.ofyears
         +Subsidy.Category+ParentSector, 
         data=Subsidy_Data_merge)
msummary(mod1)
#15.49
#The Adjusted R- squared for the model was 15.49%, that is the model could explain 15.49% variance in the Log of subsidy value. Some variables used were statistically insignificant in determining the Log of subsidy at 95% confidence.

mod2<-lm(subval~Subsidy.Source+Ownership.Group+Region+no.ofyears
         +Type.of.Subsidy+ParentSector, 
         data=Subsidy_Data_merge)
msummary(mod2)
#20.41
#The adjusted R- squared for the model was 20.41%, that is the model could explain 20.41% variance in the Log of subsidy value. All variables used were statistically significant in determining the Log of subsidy at 95% confidence.


mod3 <- lm(subval~Subsidy.Source+unemprate_percent+Ownership.Group+Region+no.ofyears
           +Type.of.Subsidy+ParentSector1+Voting, 
           data=Subsidy_Data_merge) 
msummary(mod3)
#20.52
#The adjusted R- squared for the model was 20.52%, that is the model could explain 20.52% variance in the Log of subsidy value. All variables used were statistically significant in determining the Log of subsidy at 95% confidence.

par(mfrow=c(2,2))
plot(mod1,main = 'Political Party Vs Log Subsidy')
# Normal distribution assumption 
dev.off()

outliers <- c(14591,10479,9607,14400)
Subsidy_Data3<-Subsidy_Data_merge[-outliers,]


#Final model after removing the outliers
mod4 <- lm(subval~Subsidy.Source+unemprate_percent+Ownership.Group+Region+no.ofyears
           +Type.of.Subsidy+ParentSector1+Voting, 
           data=Subsidy_Data3) 
msummary(mod4) 
#20.57
#The model can explain 20.57% variance in the Log of Subsidy value. All the independent variables, numerical and all levels of categorical are statistically significant in determining the Log of Subsidy value. 









