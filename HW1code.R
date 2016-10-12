setwd("~/Desktop/308HW1")
data<-read.table("Medicare_Provider_Util_Payment_PUF_CY2014.txt", sep="\t", na.strings = "NA", nrows = 92)
datafull=read.table("/Users/niheer/Desktop/308HW1/Medicare_Provider_Util_Payment_PUF_CY2014.txt", header=TRUE, sep="\t")
datafull = Medicare_Provider_Util_Payment_PUF_CY2014

install.packages('data.table')
library(data.table)


datafull = datafull[-2,]
header = datafull[1,]
setnames(datafull, colnames(header)) <- header

data<-datafull[sample(1:nrow(datafull),1000000, replace=FALSE),] #make the dataset more manageable 

############################ DATA CLEANING ################################

data<-data[-4] #remove MI
data<-data[-8] #remove street 1
data<-data[-20] #remove street 2
data<-data[-19] #remove zip code
data<-data[-18] #remove place of service
data<-data[-17] #remove hcpcs code
data<-data[-16] #remove hcpcs description
data<-data[-15] #remove hcpcs drug indicator
data<-data[-14]#remove bene service count
data<-data[-8]#remove unique count
data<-data[-7]#remove place of service
data<-data[-7]#remoe line svc count


data<-na.omit(data) #removing non participants of medicare

data[data$V13==""]<-NA
data<-na.omit(data) #removing unknown countries 

data[data=="ZZ"]<-NA
data<-na.omit(data) #removing unknown states 

data[data=="AA"]<-NA
data[data=="AE"]<-NA
data[data=="AP"]<-NA
data[data=="AS"]<-NA
data[data=="GU"]<-NA
data[data=="MP"]<-NA
data[data=="PR"]<-NA
data[data=="VI"]<-NA
data<-na.omit(data) #removing all foreign countries to focus on USA

data[data$V7==""]<-NA
data<-na.omit(data) #remove unknown entity codes

data[data$V2==""]<-NA
data<-na.omit(data) #remove records with unknown individual/organization names

data[data$V23==""]<-NA
data<-na.omit(data)
data[data$V24==""]<-NA
data<-na.omit(data)
data[data$V25==""]<-NA
data<-na.omit(data)
data[data$V26==""]<-NA
data<-na.omit(data) #Removing medicare amount, charged amount and medicare payment with unknown values

data<-data[-10]#remove medicare part indicator

data$V23 = as.numeric(as.character(data$V23))#converting char to numeric
data$V24 = as.numeric(as.character(data$V24))
data$V25 = as.numeric(as.character(data$V25))
data$V26 = as.numeric(as.character(data$V26))

names(data)[10] = c("allowed") #renamimg columns
names(data)[11] = c("charged")
names(data)[12] = c("paid")
names(data)[5] = c("gender")
names(data)[7] = c("state")



data<-data[-8]#remov country since all american now

names(data)[8] = c("type")
names(data)[1] = c("ID")
names(data)[2] = c("lastName")
names(data)[3] = c("FirstName")
names(data)[4] = c("qualification")
summary(data)
dataclean = data

######################## DATA PREPARATION #################################

#checking for outliers 
install.packages('Hmisc')
library(Hmisc)

#summary of data and removal of outliers
summary(data$allowed)
hist(data$allowed, main = "Medicare Allowed")
data = data[which(data$allowed <= 500),]

hist(data$allowed,"Medicare Allowed")
summary(data$allowed) #outliers appreared to be removed

summary(data$charged)
data = data[which(data$charged <= 1000),]

hist(data$charged, main = "Amount charged by Provider") #outliers appeared to be removed

summary(data$paid)
data = data[which(data$paid <= 250),]
hist(data$paid, main = "Amount paid by Medicare")

data1 =data
#categorizing states
levels(data$state) <- list(N.EAST ="CT",N.EAST ="RI",N.EAST ="MA",N.EAST ="VT",N.EAST ="NH",N.EAST ="ME",N.EAST ="PA",N.EAST ="NJ",N.EAST ="NY", SOUTH="DE",SOUTH="MD",SOUTH="DC",SOUTH="WV",SOUTH="VA",SOUTH="NC",SOUTH="KY",SOUTH="NC",SOUTH="TN",SOUTH="SC",SOUTH="GA",SOUTH="AL",SOUTH="MS",SOUTH="FL",SOUTH="AR",SOUTH="LA",SOUTH="OK",SOUTH="TX", MIDWEST="ND", MIDWEST="MN",MIDWEST="SD",MIDWEST="NE",MIDWEST="KS",MIDWEST="IA",MIDWEST="MO",MIDWEST="WI",MIDWEST="IL",MIDWEST="MI",MIDWEST="IN",MIDWEST="OH", WEST="WA", WEST="OR",WEST="CA",WEST="NV", WEST="ID", WEST="MT", WEST="WY", WEST="UT", WEST="CO",WEST="AZ", WEST="NM")
barplot(prop.table(table(data$state, main="PROVIDERS BY REGION")))

barplot(prop.table(table(data$gender, main="PROVIDERS BY REGION")))

plot(data$state, data$gender, main  = "genders across regions")

#creating new attibutes and transformation/standardization

data$PAY = data$charged-data$paid
data$DIFF= data$allowed - data$paid

#creating tranformation and normalize functions and applying them to the relevant attributes
logf<-function(x){log(x)}
normalize<-function(x){(x-min(x))/(max(x)-min(x))}

datat = data
datat$allowed = logf(datat$allowed)
datat$allowed = normalize(datat$allowed)

datat$charged = logf(datat$charged)
datat$charged = normalize(datat$charged)

datat$paid = logf(datat$paid)
datat$paid = normalize(datat$paid)

datat$PAY = logf(datat$PAY)
datat$PAY = normalize(datat$PAY)

datat$DIFF = logf(datat$DIFF)
datat$DIFF = normalize(datat$DIFF)

datas = data
datas$allowed = logf(datas$allowed)
datas$allowed = normalize(datas$allowed)

#creation of categorical variables

datac = data

datac$WEST<-ifelse(datac$state=="WEST",1,0)
#datar$NOTWEST<-ifelse(datar$NPPES_PROVIDER_STATE!="WEST",1,0)
datac$N.EAST<-ifelse(datac$state=="N.EAST",1,0)
#datar$NOTNORTHEAST<-ifelse(datar$NPPES_PROVIDER_STATE!="NORTHEAST",1,0)
datac$SOUTH<-ifelse(datac$state=="SOUTH",1,0)
#datar$NOTSOUTH<-ifelse(datar$NPPES_PROVIDER_STATE!="SOUTH",1,0)
datac$MIDWEST<-ifelse(datac$state=="MIDWEST",1,0)
#datar$NOTMIDWEST<-ifelse(datar$NPPES_PROVIDER_STATE!="MIDWEST",1,0)



datac$FEMALE<-ifelse(datac$gender =="F",1,0)
datac$MALE<-ifelse(datac$gender =="M",1,0)
datac$ORGANIZATION<-ifelse(datac$gender =="",1,0)

#plots
datap<-data[sample(1:nrow(data1),10000, replace=FALSE),]
na.omit(datap)
plot(datap$state, datap$PAY)
install.packages("ggplot2")
library("ggplot2")
qplot(datap$state, datap$PAY, main = "Amount paid by patient by state", xlab = "state", ylab = "pay")
qplot(datap$gender, datap$PAY, main = "Amount paid by patient by gender", xlab = "gender", ylab = "pay" )
qplot(datap$state, datap$DIFF, main = "Medicare difference by state", xlab = "state", ylab = "difference")
qplot(datap$gender, datap$DIFF ,main = "Medicare difference by gender", xlab = "gender", ylab = "difference")
qplot(datap$gender, datap$charge ,main = "amount charged by gender", xlab = "gender", ylab = "charge")


############################## CLUSTERING #####################################
datac$PAY = datat$PAY
datac$DIFF = datat$DIFF


datac<-na.omit(datac)

mydata <- datac[,c( 13,14,15,16,17,18,19,20,21)]


wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
fit<-kmeans(mydata,9)
fit

mydata <- data.frame(mydata, fit$cluster)

aggregate(mydata,by=list(fit$cluster),FUN=mean)




#Silouette score
num<-sapply(mydata,as.numeric);
samp<-num[sample(nrow(mydata), 10000), ]


s12<-clValid(samp,12,clMethods = c("kmeans"),validation = "internal", maxitems = 10000);
summary(s12)
s8<-clValid(samp,11,clMethods = c("kmeans"),validation = "internal", maxitems = 10000);

s9<-clValid(samp,9,clMethods = c("kmeans"),validation = "internal", maxitems = 10000);

s10<-clValid(samp,10,clMethods = c("kmeans"),validation = "internal", maxitems = 10000);
summary(s9)
