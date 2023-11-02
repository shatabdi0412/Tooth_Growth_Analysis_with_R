# Loading reqiured libraries and the dataset
library(ggplot2)
data('ToothGrowth')

# Basic Summary and Structure of the Data
head(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)
dim(ToothGrowth)

# Basic Exploratory Analysis 
g<- ggplot(data=ToothGrowth, aes(x=paste(supp,dose),y=len, fill=factor(dose)))
g<- g+geom_boxplot()
g<- g+labs(title= "Tooth Growth of 60 guinea pigs by dosage and delivery method of vitamin C", x = "Dose in milligrams/day", y = "Tooth Lengh")
g<- g+scale_fill_discrete(name="dosage type")
g<- g+theme(plot.title=element_text(hjust=0.5))
print(g)

# Data Manipulation to Separated Data by Dosage
data_0.5<-subset(ToothGrowth, dose==0.5)
data_1<-subset(ToothGrowth, dose==1)
data_2<-subset(ToothGrowth, dose==2)

# Performing T-test to Accept or Reject Hypothesis
t1<-t.test(len~supp, data=data_0.5)
t2<-t.test(len~supp, data=data_1)
t3<-t.test(len~supp, data=data_2)

# Creating Dataframes to Analyize the Results
Conclusion<-data.frame("p-value"=c(t1$p.value,t2$p.value,t3$p.value),
                       "Low-Confidence"=c(t1$conf.int[1],t2$conf.int[1],t3$conf.int[1]),
                       "High-Confidence"=c(t1$conf.int[2],t2$conf.int[2],t3$conf.int[2]),
                       "OJ mean" = c(t1$estimate[1],t2$estimate[1],t3$estimate[1]),
                       "VC mean" = c(t1$estimate[2],t2$estimate[2],t3$estimate[2]),
                       row.names = c("Dosage_.5","Dosage_1","Dosage_2"))

Conclusion

# Conclusion
# Since P value for dosage 0.5 and 1 are less than threshold value (0.5), we reject the null hypothesis which states that there is no relation between tooth growth and supplement method for 0.5 and 1mg dose.
# Since P value for dosage 2mg is greater than threshold value (0.5), we accept the null hypothesis which states that there is no relation between tooth growth and supplement method for 2mg dose.
# From datafarme last 2 columns, it is concluded that OJ provides more growth than VC when dosage is 0.5mg or 1mg