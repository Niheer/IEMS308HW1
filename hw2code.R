strinfo=fread("C:/Users/nts067/Desktop/strinfo.csv", sep="auto", header="auto") 
skstinfo=fread("C:/Users/nts067/Desktop/skstinfo.csv", sep="auto", header="auto") 
skstinfo$V5<-NULL #remove meaningless comumn
colnames(skstinfo)<-c("SKU", "STORE", "COST", "RETAIL") #label columns
skuinfo=fread("C:/Users/nts067/Desktop/skuinfo.csv", sep=',', header="auto") 
skuinfo$V10<-skuinfo$V11<-NULL
colnames(skuinfo)<-c("SKU", "DEPT", "CLASSID", "STYLE", "COLOR", "SIZE", "PACKSIZE", "VENDOR", "BRAND")


deptinfo=fread("C:/Users/nts067/Downloads/deptinfo.csv", sep="auto", header="auto") 
sqldf('select STORE, CITY from test where STATE ="FL"') #select states for each store using SQL

trnsact=fread("C:/Users/nts067/Desktop/trnsact.csv", sep=',', header=FALSE, select=c(1,2,3,4,6,7,8))
trnsact<-trnsact1[which(trnsact1$V7=='P'),]
colnames(skuinfo)<-c("SKU", "DEPT", "CLASSID", "STYLE", "COLOR", "SIZE", "PACKSIZE", "VENDOR", "BRAND")

trnsacr<-trnsact[sample(1:nrow(trnsact),25000000, replace=FALSE),] #sampled 20% of data

colnames(trnsact)<-c("SKU", "STORES", "REGISTER", "TRANNUM", "DATE", "STYPE", "QUANTITY") ##add column names
sqldf('select * from trnsact where V7="R"') 
trnsact<-sqldf("select * from trnsact where trnsact.V7='P'") #deleting all returned items

######################################### Data processing #########################
#creating factors
deptinfo$DEPTDESC<-as.factor(deptinfo$DEPTDESC)
skuinfo$CLASSID<-as.factor(skuinfo$CLASSID) 
skuinfos$STYLE<-as.factor(skuinfo$STYLE)
skuinfo$COLOR<-as.factor(skuinfo$COLOR)
skuinfo$SIZE<-as.factor(skuinfo$SIZE)
skuinfo$PACKSIZE<-as.factor(skuinfo$PACKSIZE)
skuinfo$VENDOR<-as.factor(skuinfo$VENDOR)
skuinfo$BRAND<-as.factor(skuinfo$BRAND)
strinfo$CITY<-as.factor(strinfo$CITY)
strinfo$STATE<-as.factor(strinfo$STATE)library(data.table)
trnsact$STYPE<-as.factor(trnsact$STYPE)
trnsact$DATE<-as.date(trnsact$DATE) #transform dates to as date


levels(strinfo$STATE) <- list(NORTHEAST ="CT",NORTHEAST ="RI",NORTHEAST ="MA",NORTHEAST ="VT",NORTHEAST ="NH",NORTHEAST ="ME",NORTHEAST ="PA",NORTHEAST ="NJ",NORTHEAST ="NY", SOUTH="DE",SOUTH="MD",SOUTH="DC",SOUTH="WV",SOUTH="VA",SOUTH="NC",SOUTH="KY",SOUTH="NC",SOUTH="TN",SOUTH="SC",SOUTH="GA",SOUTH="AL",SOUTH="MS",SOUTH="FL",SOUTH="AR",SOUTH="LA",SOUTH="OK",SOUTH="TX", MIDWEST="ND", MIDWEST="MN",MIDWEST="SD",MIDWEST="NE",MIDWEST="KS",MIDWEST="IA",MIDWEST="MO",MIDWEST="WI",MIDWEST="IL",MIDWEST="MI",MIDWEST="IN",MIDWEST="OH", WEST="WA", WEST="OR",WEST="CA",WEST="NV", WEST="ID", WEST="MT", WEST="WY", WEST="UT", WEST="CO",WEST="AZ", WEST="NM") #Create regions
barplot(prop.table(table(strinfo$STATE))) #creating regions
summary(store)

profit<-as.data.frame((skstinfo$RETAIL)-(skstinfo$COST)) #calculate profits
summary(profit)
summary(trnsact$QUANTITY) #mean quantity 
sd(unlist(profit))

################################### Data analysis ########################################

# profit analysis
cost_tab<-as.data.frame(sqldf('Select COST as "Cost" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE group by STATE'))

retail_tab<-as.data.frame(sqldf('Select STATE, AVG("RETAIL") as "Cost" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE group by STATE'))
cost_tab<-as.data.frame(sqldf('Select STATE, AVG("Cost") as "Cost" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE, STATE=SOUTH'))
profit_tab<-as.data.frame((retail_tab$Cost)-cost_tab$Cost)
barplot(prop.table(table(profit_tab))) 

#analyzing profit by region

cost_tab<-as.data.frame(sqldf('Select COST as "Cost" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE and STATE="SOUTH"'))

retail_tab<-as.data.frame(sqldf('Select RETAIL as "retail" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE and STATE="SOUTH"'))
profit_tab_S<-as.data.frame((retail_tab)-cost_tab)
sd(unlist(profit_tab_S))
boxplot(profit_tab_S)



cost_tab<-as.data.frame(sqldf('Select COST as "Cost" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE and STATE="MIDWEST"'))
retail_tab<-as.data.frame(sqldf('Select RETAIL as "retail" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE and STATE="MIDWEST"'))
profit_tab_MW<-as.data.frame((retail_tab)-cost_tab)
sd(unlist(profit_tab_MW))
boxplot(profit_tab_MW)


brands<-cost_tab<-as.data.frame(sqldf('Select COST as "Cost" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE and STATE="WEST"'))
retail_tab<-as.data.frame(sqldf('Select RETAIL as "retail" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE and STATE="WEST"'))
profit_tab_W<-as.data.frame((retail_tab)-cost_tab)
sd(unlist(profit_tab_W))
boxplot(profit_tab_W)
summary(profit_tab_W)
summary(profit_tab_S)
summary(profit_tab_MW)

cost_tab<-as.data.frame(sqldf('Select COST as "Cost" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE and STATE="NORTHEAST"'))
retail_tab<-as.data.frame(sqldf('Select RETAIL as "retail" from skstinfo, strinfo where skstinfo.STORE=strinfo.STORE and STATE="WEST"'))
profit_tab_W<-as.data.frame((retail_tab)-cost_tab)

summary(cost_tab)
boxplot(profit_tab)

sd(skstinfo$COST)
sd(skstinfo$RETAIL)
Profit<-as.data.frame(skstinfo$RETAIL-skstinfo$COST)
Profit<-unlist(Profit)
sd(Profit)
boxplot(Profit)
mean(Profit)
summary(Profit)

sqldf('Select BRAND, count("SKU") from skuinfo order by BRAND') ##sku per brand

sqldf('Select BRAND, count("SKU") as "SKUs" from skuinfo group by BRAND order by BRAND desc')
sqldf('Select BRAND, count("REGISTER") as "Tot_transactions" from skuinfo, trnsact where trnsact.SKU=skuinfo.SKU group by BRAND order by BRAND desc')
sqldf('Select BRAND, count("SKU") as "SKUs" from skuinfo ')
skstinfo$COST<-as.numeric(unlist(skstinfo$COST))

sqldf('select avg(Quantity) from trnsact')
boxplot(trnsact$QUANTITY)

trnsact_brand<-sqldf('Select count(STORES),STATE from trnsact, strinfo where STORE=STORES group by STATE') ##transaction per brand
transact_brand<-sqldf('Select BRAND, count("STORE") as "Number_Transactions" from trnsact, skuinfo where trnsact.SKU=skuinfo.SKU group by BRAND order by Number_Transactions desc')
transact_brand_tab<-as.data.frame(transact_brand)
transact_brand_tab[1:20,1:2]


SKU_brand<-sqldf('Select BRAND, count("SKU") as "Number_SKU" from skuinfo group by BRAND')
ordering_brands<-sqldf('Select Brand, Number_SKU from SKU_brand order by Number_SKU desc')
ordering_brands[1:20,1:2]
View(ordering_brands)

ordering_brands<-ordering_brands[-c(9), ] 
ordering_brands_subset <- subset(ordering_brands, ordering_brands$Number_SKU >= 14000)

barplot((ordering_brands_subset$Number_SKU), xlab = c('PoloFas BrownSh EnzoAng LizClai Roundtre TheUnit EmmaJam TommyHi 9West HartSch')) ##bar plot brands by SKU

tot_brand_trans<-c(32080854, 111649094)
barplot(tot_brand_trans, xlab='Tot. Transactions Chosen Brands vs Tot. Transactions')

ordered_SKU_brand<-sqldf('select count(SKU) as "Brand_SKU_Total", ordering_brands_subset.BRAND from skuinfo, ordering_brands_subset where ordering_brands_subset.BRAND=skuinfo.BRAND group by ordering_brands_subset.BRAND order by "Brand_SKU_Total" desc
                         ')
ordered_SKU_brand<-as.data.frame(ordered_SKU_brand)
barplot(ordered_SKU_brand, xlab = 'PoloFas Brown SH Liz Clai Roundtre Emma Jam Tommy Hi Cabernet Koret of Lancome Clinique') ##bar plot brands by transaction
head(trnsact, n=10)
trnsact<-sqldf('Select * from trnsact, skuinfo where skuinfo.SKU=trnsact.SKU and (skuinfo.Brand="POLO FAS" or skuinfo.Brand="BROWN SH" or skuinfo.Brand="LIZ CLAI" or skuinfo.Brand="ROUNDTRE" or skuinfo.Brand="EMMA JAM" or skuinfo.Brand="TOMMY HI" or skuinfo.Brand="CABERNET" or skuinfo.Brand="KORET OF" or skuinfo.Brand="LANCOME" or skuinfo.Brand="CLINIQUE") ') ##select subset of data w 10 largest brands
trnsact$DATE<-as.Date(trnsact$DATE)
trnsact$DATE<-as.Date(trnsact$DATE)
trnsact$Basket_ID<-paste(trnsact$TRANNUM, trnsact$REGISTER, trnsact$STORES, trnsact$DATE, collapse=NULL, sep="") ##create basket id
trnsact<-trnsact[,c("Basket_ID", "SKU")]

# creating basket ID's
mean(as.numeric(levels(skuinfo$BSIZE))[skuinfo$BSIZE]) #find average size of basket
boxplot(as.numeric(levels(skuinfo$BSIZE))[skuinfo$BSIZE])
write.csv(trnsact, file="trnsactf.csv",row.names=FALSE) #write into csv
trnsact$Basket_ID<-as.factor(trnsact$Basket_ID)
basket_form<-read.transactions("C:/Users/nts067/Documents/trnsactf.csv", cols=c(1, 2), format="single",rm.duplicates=TRUE) #transaction form applied



###################################### Association #######################################

rules<-as.data.frame
rules<-apriori(basket_form, parameter=list(supp=0.0001, conf=0.3, minlen=2)) #apriori association
rules.sorted <- sort(rules, by="lift")
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# removing redundant rules (pruning)
rules.pruned <- rules.sorted[!redundant]
prules_tab<-as.table(rules.pruned)
inspect(rules.pruned)
write(rules.pruned, file="rules_pruned", sep=",")
rules_pruned_frame<-read.table("C:/Users/nts067/Desktop/abb/rules_pruned.csv", sep=",")
rules_pruned_frame
head(rules_pruned_frame, n=100)
sqldf('Select rules, confidence, support, lift from rules_pruned_frame where confidence>=0.3 and lift>=100 and support>=0.0001 order by lift desc')
inspect(rules.sorted)
number_trnasactions<-rules_pruned_frame$support
number_trnasactions<-number_trnasactions*19864762
head(number_trnasactions, n=100)
rules_pruned_frame$number_transaction<-number_trnasactions

head(rules_pruned_frame, n=10)

basket_size<-sqldf('select Basket_ID, count(SKU) as "basket size" from trnsact group by Basket_ID') ##find basket size
summary(basket_size)
SKU_rules<-c(989823,4980033,919823,4980033,3966618,4346618,929823,4980033,869823,4980033,2903090,2863090,2753090,2903090,2683090,2883090,4640325,2637662,2843090,2883090,5600671,957662,2893090,2703090,2703090,2853090,2853090,2893090,6139962,6319962,2716578,3908011,3988011,2726578,3998011,3908011,3898011,4440924,3968011,3690654,3898011,3968011,3898011,3988011,2716578) #create vector with SKUs in rule
SKU_rules<-as.data.frame(SKU_rules)
SKU_rules

plot(rules.pruned, method="scatterplot")
plot(rules.pruned, method="graph")