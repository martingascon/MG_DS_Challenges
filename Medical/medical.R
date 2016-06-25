# Martin Gascon. Insight Data Science. 
# ============================================================
# There is a single csv file with what presumably is medical data (12 cols) for 669 patients. 
# This breast cancer databases was obtained from the University of Wisconsin Hospitals, Madison from Dr. William H. Wolberg. 

# Attributes 2 through 10 have been used to represent instances.
# Each instance has one of 2 possible classes: benign or malignant.
# -- Size of data set: only 369 instances (at that point in time)
# -- Collected classification results: 1 trial only
# -- Two pairs of parallel hyperplanes were found to be consistent with 50% of the data
# -- Accuracy on remaining 50% of dataset: 93.5%
# -- Three pairs of parallel hyperplanes were found to be consistent with 67% of data
# -- Accuracy on remaining 33% of dataset: 95.9%
  
# -- Size of data set: only 369 instances (at that point in time)
# -- Applied 4 instance-based learning algorithms 
# -- Collected classification results averaged over 10 trials
# -- Best accuracy result: 
# -- 1-nearest neighbor: 93.7%
# -- trained on 200 instances, tested on the other 169
# -- Also of interest:
#   -- Using only typical instances: 92.2% (storing only 23.1 instances)
# -- trained on 200 instances, tested on the other 169

# 4. Relevant Information:
  
# Samples arrive periodically as Dr. Wolberg reports his clinical cases.
# The database therefore reflects this chronological grouping of the data.
# This grouping information appears immediately below, having been removed
# from the data itself:
  
# Group 1: 367 instances (January 1989)
# Group 2:  70 instances (October 1989)
# Group 3:  31 instances (February 1990)
# Group 4:  17 instances (April 1990)
# Group 5:  48 instances (August 1990)
# Group 6:  49 instances (Updated January 1991)
# Group 7:  31 instances (June 1991)
# Group 8:  86 instances (November 1991)
# -----------------------------------------
#  Total:   699 points (as of the donated datbase on 15 July 1992)

#Note that the results summarized above in Past Usage refer to a dataset
#of size 369, while Group 1 has only 367 instances.  This is because it
#originally contained 369 instances; 2 were removed.  The following
#statements summarizes changes to the original Group 1's set of data:

#####  Group 1 : 367 points: 200B 167M (January 1989)
#####  Revised Jan 10, 1991: Replaced zero bare nuclei in 1080185 & 1187805
#####  Revised Nov 22,1991: Removed 765878,4,5,9,7,10,10,10,3,8,1 no record
#####                  : Removed 484201,2,7,8,8,4,3,10,3,4,1 zero epithelial
#####                  : Changed 0 to 1 in field 6 of sample 1219406
#####                  : Changed 0 to 1 in field 8 of following sample:
#####                  : 1182404,2,3,1,1,1,2,0,1,1,1

# 5. Number of Instances: 699 (as of 15 July 1992)
# 6. Number of Attributes: 10 plus the class attribute
# 7. Attribute Information: (class attribute has been moved to last column)

#  Attribute                     Domain
# -- -----------------------------------------
# 1. Sample code number            id number
# 2. Clump Thickness               1 - 10
# 3. Uniformity of Cell Size       1 - 10
# 4. Uniformity of Cell Shape      1 - 10
# 5. Marginal Adhesion             1 - 10
# 6. Single Epithelial Cell Size   1 - 10
# 7. Bare Nuclei                   1 - 10
# 8. Bland Chromatin               1 - 10
# 9. Normal Nucleoli               1 - 10
# 10. Mitoses                       1 - 10
# 11. Class:                        (2 for benign, 4 for malignant)

# 8. Missing attribute values: 16
# There are 16 instances in Groups 1 to 6 that contain a single missing 
# (i.e., unavailable) attribute value, now denoted by "?".  

# 9. Class distribution:
#  Benign: 458 (65.5%)
#  Malignant: 241 (34.5%)
# For ease of analysis, we will be working with an aggregated version of the dataset.  
# The data is aggregated by customer, and info from different shops are pivoted to new columns. 
# We've added this file (csv format) to the "Data Challenges/Supermarket" Dropbox folder.


#================================
#load libraries 
library(mlbench)  # for Machine learning
library(e1071)  #  for Naives Bayes
library(corrplot) # for Corr plot



# set working Directory
setwd('~/Dropbox/a_Insight/c_MG_DS_challenges/Medical/') 

# read the Minimum wage from  (1976 to 2014) 
data = read.table("./data.csv", sep=",",head=TRUE)   
head(data)
colnames(data)
dim(data)
summary(data) 

#================================ Exploratory analysis
# Analysis

## alternatively:
m <- naiveBayes(data[,4:12], data[,3])
m
table(predict(m, data[,4:12]), data[,3])
iris

m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris), iris[,5])






# What is the most expensive market?

super = c("S1","S2","S3","S4","S5")
price = c(summary(data$avg_price_shop_1)[4],summary(data$avg_price_shop_2)[4],summary(data$avg_price_shop_3)[4],summary(data$avg_price_shop_4)[4],summary(data$avg_price_shop_5)[4])
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1)) 
names(price) = super
barplot(price, main="Average price", xlab="Supermarket", ylab = "Avg. price (Euros)" )

# The most expensive is S1 followed by S2. S4 is the cheapest. 

 
ave_dist = c(summary(data$distance_shop_1)[4],summary(data$distance_shop_2)[4],summary(data$distance_shop_3)[4],summary(data$distance_shop_4)[4],summary(data$distance_shop_5)[4])
ave_dist = ave_dist/1000*1.6
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))
names(ave_dist) = super
barplot(ave_dist, main="Average distance", xlab="Supermarket", ylab = "Avg. distance (miles)" )

# The closest is S3 followed by S5. S4 is the farthest. Kind of makes sense because S4 is the cheapest.  

ave_amou_purch = c(summary(data$amount_purchased_shop_1)[4],summary(data$amount_purchased_shop_2)[4],summary(data$amount_purchased_shop_3)[4],summary(data$amount_purchased_shop_4)[4],summary(data$amount_purchased_shop_5)[4])
ave_amou_purch
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))
names(ave_amou_purch) = super
barplot(ave_amou_purch, main="Average amount purchased", xlab="Supermarket", ylab = "Avg. amount purchased (Euros)" )

# Eventhough S1 is the most expensive is the one with higher ave of product purchased. S4 is the lowest.


ave_prod_purch = c(summary(data$products_purchased_shop_1)[4],summary(data$products_purchased_shop_2)[4],summary(data$products_purchased_shop_3)[4],summary(data$products_purchased_shop_4)[4],summary(data$products_purchased_shop_5)[4])
ave_prod_purch
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))
names(ave_prod_purch) = super
barplot(ave_prod_purch, main="Average Number of products purchased", xlab="Supermarket", ylab = "Avg. Number of products purchased" )
#legend("topright", inset=0.01, "Eventhough S1 is the most\n expensive is the one with\n higher ave of product purchased.")
# Eventhough S1 is the most expensive supermarket, it is the one with higher ave of product purchased. S4 is the lowest.

#===================

# number of supers visited per user

users = nrow(data)
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))

#bins<-c(0.5,1.5,2.5,3.5,4.5,5.5) 
h=hist(data$shops_used,breaks=5)
h$density = h$counts/users*100

plot(h,freq=FALSE,  xlab="number of supermarkets", ylab="% of users", main="Histogram of shops used", col="lightgreen", xlim=c(1,5.5),xaxt="n" )
axis(1, at=seq(1.5, 5.5, by=1), labels=c( "1", "2", "3", "4", "5") )
abline(v=summary(data$shops_used)[4]+0.5, col="red",lwd=2)
mtext(summary(data$shops_used)[4], side=1, line=-18, at=3.6, col="red") 
mtext("mean =", side=1, line=-18, at=3.2, col="red") 
# Most of the users (60%) go to a 1 supermarket. 26% of users go to 2 supermarkets. 11 % goes to 3 supermarkets and only 3% goes to 4 supermarkets. 
# In average users visit 2.38 supermarkets

# those visiting only one supermarket 
s1_only = subset(data,products_purchased_shop_1>0 & shops_used==1)
s2_only = subset(data,products_purchased_shop_2>0 & shops_used==1)
s3_only = subset(data,products_purchased_shop_3>0 & shops_used==1)
s4_only = subset(data,products_purchased_shop_4>0 & shops_used==1)
s5_only = subset(data,products_purchased_shop_5>0 & shops_used==1)


## How far do they come to shop in their supermarket
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))
h1 = hist(s1_only$distance_shop_1,breaks=50)
h2 = hist(s2_only$distance_shop_2,breaks=50)
h3 = hist(s3_only$distance_shop_3,breaks=50)
h4 = hist(s4_only$distance_shop_4,breaks=50)


# ==========================================  HERE






h1 = hist(s1_only$products_purchased_shop_1,breaks=100)
h1 = hist(s1_only$unique_products_purchased_shop_1,breaks=100)
h1 = hist(s1_only$amount_purchased_shop_1,breaks=100)
h1 = hist(s1_only$avg_purchase_shop_1,breaks=50)
h1 = hist(s1_only$avg_purchase_shop_1,breaks=50)


head(s2_only)
h2 = hist(s2_only$products_purchased_shop_2,breaks=100)
h2 = hist(s2_only$unique_products_purchased_shop_2,breaks=100)
h2 = hist(s2_only$amount_purchased_shop_2,breaks=100)
h2 = hist(s2_only$avg_purchase_shop_2,breaks=50)



# who visits which supermarket

# from those visiting more 1 supermarket



s1_another = subset(data,products_purchased_shop_1>0)
s1d<-dim(s1_another)[1]
table(s1_another$amount_purchased_shop_1>0 & s1_another$amount_purchased_shop_1>0)[2]/s1d
table(s1_another$amount_purchased_shop_1>0 & s1_another$amount_purchased_shop_2>0)[2]/s1d
table(s1_another$amount_purchased_shop_1>0 & s1_another$amount_purchased_shop_3>0)[2]/s1d
table(s1_another$amount_purchased_shop_1>0 & s1_another$amount_purchased_shop_4>0)[2]/s1d
table(s1_another$amount_purchased_shop_1>0 & s1_another$amount_purchased_shop_5>0)[2]/s1d

s2_another = subset(data,products_purchased_shop_2>0)
s2d<-dim(s2_another)[1]
table(s2_another$amount_purchased_shop_2>0 & s2_another$amount_purchased_shop_1>0)[2]/s2d
table(s2_another$amount_purchased_shop_2>0 & s2_another$amount_purchased_shop_3>0)[2]/s2d
table(s2_another$amount_purchased_shop_2>0 & s2_another$amount_purchased_shop_4>0)[2]/s2d
table(s2_another$amount_purchased_shop_2>0 & s2_another$amount_purchased_shop_5>0)[2]/s2d

 
s3_another = subset(data,products_purchased_shop_3>0)
s3d<-dim(s3_another)[1]
table(s3_another$amount_purchased_shop_3>0 & s3_another$amount_purchased_shop_1>0)[2]/s3d
table(s3_another$amount_purchased_shop_3>0 & s3_another$amount_purchased_shop_2>0)[2]/s3d
table(s3_another$amount_purchased_shop_3>0 & s3_another$amount_purchased_shop_4>0)[2]/s3d
table(s3_another$amount_purchased_shop_3>0 & s3_another$amount_purchased_shop_5>0)[2]/s3d
 
s4_another = subset(data,products_purchased_shop_4>0)
s4d<-dim(s4_another)[1]
table(s4_another$amount_purchased_shop_4>0 & s4_another$amount_purchased_shop_1>0)[2]/s4d
table(s4_another$amount_purchased_shop_4>0 & s4_another$amount_purchased_shop_2>0)[2]/s4d
table(s4_another$amount_purchased_shop_4>0 & s4_another$amount_purchased_shop_3>0)[2]/s4d
table(s4_another$amount_purchased_shop_4>0 & s4_another$amount_purchased_shop_5>0)[2]/s2d


s5_another = subset(data,products_purchased_shop_5>0)
s5d<-dim(s5_another)[1]
table(s5_another$amount_purchased_shop_5>0 & s5_another$amount_purchased_shop_1>0)[2]/s5d
table(s5_another$amount_purchased_shop_5>0 & s5_another$amount_purchased_shop_2>0)[2]/s5d
table(s5_another$amount_purchased_shop_5>0 & s5_another$amount_purchased_shop_3>0)[2]/s5d
table(s5_another$amount_purchased_shop_5>0 & s5_another$amount_purchased_shop_4>0)[2]/s5d



sup_corrs = matrix(nrow=5,ncol=5)
for(i in 1:5)
{
  aux =paste("products_purchased_shop_",as.character(i),sep="") 
  subtable = subset(data,eval(parse(text = aux))>0)
  total<- dim(subtable)[1]
  for(j in 1:5)
  {
    aux2 = paste("s",as.character(i),"_another$amount_purchased_shop_",as.character(i),sep="")
    aux3 = paste("s",as.character(i),"_another$amount_purchased_shop_",as.character(j),sep="")
    sup_corrs[i,j] =  round(table(eval(parse(text = aux2))>0 & eval(parse(text = aux3))>0)[2]/total*100,1)
    if (i==j)  sup_corrs[i,j]=100
  }
}
sup_corrs 
M<-sup_corrs[1:5,1:5]/100 
str(M)
corrplot(sup_corrs, method="color")
corrplot(M, method = "number",cl.lim=c(0,1))




h1 = hist(s1_another$products_purchased_shop_1,breaks=100)
h1 = hist(s1_only$unique_products_purchased_shop_1,breaks=100)
h1 = hist(s1_only$amount_purchased_shop_1,breaks=100)
h1 = hist(s1_only$avg_purchase_shop_1,breaks=50)




#===================

# distribution of price vs distance. I need to calculate distance btw supermarkets
dist_matrix = matrix(nrow=5,ncol=5)

# order by distance to each supermarket
dist_s1 <- data[order(data$distance_shop_1),]
dist_s2 <- data[order(data$distance_shop_2),]
dist_s3 <- data[order(data$distance_shop_3),]
dist_s4 <- data[order(data$distance_shop_4),]
dist_s5 <- data[order(data$distance_shop_5),]

par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))
#mean(dist_s5$distance_shop_1[1:1000])
#hist(dist_s5$distance_shop_1[1:1000],breaks=5)

for(i in 1:5)
  {
  for(j in 1:5)
    {
    sup = paste("dist_s",as.character(i),"$distance_shop_",as.character(j),"[1:1000]",sep="")
    dist_matrix[i,j]=round(mean(eval(parse(text = sup))),0)  
    if (i==j) dist_matrix[i,j]=0
    }
  }

dist_matrix
#[,1] [,2] [,3] [,4] [,5]
#[1,]    0 3098 1500 1438  601
#[2,] 3267    0 1646 4331 2563
#[3,] 1729 1617    0 2710  942
#[4,] 1570 4306 2687    0 1808
#[5,]  936 2513  893 1850    0

# let's average the values and put them in miles. Remove lower diagonal
dist_matrix_ave= matrix(nrow=5,ncol=5)

for(i in 1:5)
{
  for(j in 1:5)
  {
    dist_matrix_ave[i,j]=round(((dist_matrix[i,j]+dist_matrix[j,i])*0.5)/1000/1.6,1)
    if (i>j) dist_matrix_ave[i,j]=0
  }
}

dist_matrix_ave

#[,1] [,2] [,3] [,4] [,5]
#[1,]    0    2    1  0.9  0.5
#[2,]    0    0    1  2.7  1.6
#[3,]    0    0    0  1.7  0.6
#[4,]    0    0    0  0.0  1.1
#[5,]    0    0    0  0.0  0.0

# I can put them in a map with this information. 
# S2 is 1 mi from S3 and S1 and S3 is 2 miles from S1 which mean they are in the same line.
# S4 can have two position (up-left) or (down-left) from S1. S5 is in the same plane between S1 and S3. 


# Now let's see the distribution the purchase distribution for customers close to a supermarket

# let's check density at at least 30-35% of the minimal distance between 2 supermarkets (0.5 mi = 800 mts) which is around 300 mts.
rad = 0.3 # in km
area = 3.141592654*rad^2 # in km2 
d1 <-dim(subset(data,distance_shop_1<300))[1]/area
d2 <-dim(subset(data,distance_shop_2<300))[1]/area
d3 <-dim(subset(data,distance_shop_3<300))[1]/area
d4 <-dim(subset(data,distance_shop_4<300))[1]/area
d5 <-dim(subset(data,distance_shop_5<300))[1]/area

density_at_300m = c(d1,d2,d3,d4,d5)
par(mfrow = c(1, 1))
par(mar=c(5.1,4.1,4.1,2.1))
plot(density_at_300m, xlab= "Supermarket") 
# [1]  198.0595 9262.8177 7989.5781 4728.6702 5471.3933
# I can guess that S1 is in a low density area (200 cust/km2). S2/S3 are in the most dense areas (9300-8000 cust/km2)
# S4-S5 are in the intermediate density 4000-5000 cust/km2)


# let's take into account the minimal distante to each supermarket
#mdts = min_dist_to_each_sup
mdts <- c(dist_s1$distance_shop_1[1],dist_s2$distance_shop_2[1], dist_s3$distance_shop_3[1],dist_s4$distance_shop_4[1],dist_s5$distance_shop_5[1])
mdts
# [1] 93.283410 11.190418 17.844347  6.642472 25.460707
# Supermarket 1 is quite big (or isolated) since first neighbohr is at 93 mts, 
# S5 is the second largerst. S2-3-4 are relatively small or buildings. 

dist = 266

n_s1 <- subset(data,distance_shop_1<dist+mdts[1])
n_s2 <- subset(data,distance_shop_2<dist+mdts[2])
n_s3 <- subset(data,distance_shop_3<dist+mdts[3])
n_s4 <- subset(data,distance_shop_4<dist+mdts[4])
n_s5 <- subset(data,distance_shop_5<dist+mdts[5])




hist(n_s1$products_purchased,breaks=100)
hist(n_s2$products_purchased,breaks=100)
hist(n_s3$products_purchased,breaks=100)
hist(n_s4$products_purchased,breaks=100)
hist(n_s5$products_purchased,breaks=100)


dim(n_s1)
dim(n_s2)
dim(n_s3)
dim(n_s4)
dim(n_s5)


######  

# how many of buying in S1 buy s2,3,4,5





