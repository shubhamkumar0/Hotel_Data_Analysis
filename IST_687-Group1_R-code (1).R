# Reading the data

setwd("C:\\Users\\shubh\\OneDrive\\Desktop\\IntroDS\\paper")
library("readxl")
library("ggplot2")
library(ggmap)
city<- read_excel("City.xlsx")
resort <- read_excel("Resort.xlsx")
dim(resort)
# Customer segmentation

summary(city)
summary(resort)

#####################################
#MAP CUSTOMER COUNTRY
#####################################
# map of country where visitors come from

#CITY
world <- map_data("world")
# getting country names for the country code
c_code <- read_excel("ISO_codes.xlsx")
str(c_code)
c_code <- c_code[c(1,4)]
names(c_code)[1] <- 'Country_name'
names(c_code)[2] <- 'Country_code'
world$region <- tolower(world$region)
c_code$Country_name <- tolower(c_code$Country_name)
df<- as.data.frame(table(city$Country))
names(df)[1] <- 'Country_code'
df <- merge(df, c_code, by.x="Country_code", by.y="Country_code")
# Retrieve the map data
customer.maps <- map_data("world", region = df$Country_name)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- customer.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
customer.maps$region <- tolower(customer.maps$region)
library(plotly)
customer.maps <- merge(customer.maps, df, by.x="region", by.y="Country_name")
p<-ggplot(customer.maps, aes(x = long, y = lat, text =paste("country:", region))) +
  geom_polygon(aes( group = group, fill = Freq))
fig <- ggplotly(p)
fig


#RESORT
df<- as.data.frame(table(resort$Country))
names(df)[1] <- 'Country_code'
df <- merge(df, c_code, by.x="Country_code", by.y="Country_code")
# Retrieve the map data
customer.maps <- map_data("world", region = df$Country_name)
region.lab.data <- customer.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))
customer.maps$region <- tolower(customer.maps$region)
library(plotly)
customer.maps <- merge(customer.maps, df, by.x="region", by.y="Country_name")
p<-ggplot(customer.maps, aes(x = long, y = lat, text =paste("country:", region))) +
  geom_polygon(aes( group = group, fill = Freq))
fig <- ggplotly(p)
fig

##Histograms
ggplot(data=city) + geom_histogram(mapping=aes(x=Adults)) + ggtitle("Number of Adults for the City Hotel")
table(resort$Adults)
city$Children <- as.numeric(city$Children)
ggplot(data=city) + geom_histogram(mapping=aes(x=Children)) + ggtitle("Number of Children for the City Hotel")
ggplot(data=city) + geom_histogram(mapping=aes(x=Babies)) + ggtitle("Number of Babies for the City Hotel")
ggplot(data=resort) + geom_histogram(mapping=aes(x=Adults)) + ggtitle("Number of Adults for the Resort Hotel")
ggplot(data=resort) + geom_histogram(mapping=aes(x=Children)) + ggtitle("Number of Children for the Resort Hotel")
ggplot(data=resort) + geom_histogram(mapping=aes(x=Babies)) + ggtitle("Number of Babies for the Resort Hotel")
table(city$Babies)
table(city$Children)
table(city$Adults)


#NEW VARIABLES
city$party_size <- city$Adults +city$Children + city$Babies
resort$party_size <- resort$Adults +resort$Children + resort$Babies

city$party_type <- "Family"
city$party_type[city$party_size == 1] <- "Single"
city$party_type[city$Adults == 2 & city$party_size == 2] <- "Couple"
city$party_type <- as.factor(city$party_type)
resort$party_type <- "Family"
resort$party_type[resort$party_size == 1] <- "Single"
resort$party_type[resort$Adults == 2 & resort$party_size == 2] <- "Couple"
resort$party_type <- as.factor(resort$party_type)

s_city<-city[city$party_type=="Couple",]
prop.table(table(s_city$IsRepeatedGuest))
prop.table(table(s_city$IsCanceled))
prop.table(table(s_city$DepositType))

#Cancellation Analysis
prop.table(table(resort$IsCanceled))
prop.table(table(city$IsCanceled))
prop.table(table(city$IsCanceled, city$party_type), margin=2)
prop.table(table(resort$IsCanceled, resort$party_type), margin=2)
prop.table(table(city$IsCanceled, city$CustomerType), margin=2)
prop.table(table(resort$IsCanceled, resort$CustomerType), margin=2)

prop.table(table(city$IsCanceled, city$MarketSegment), margin=2)
prop.table(table(resort$IsCanceled, resort$MarketSegment), margin=2)

# Creating the season variable
city$`Arrival Date`<-strptime(city$`Arrival Date`,format="%Y-%m-%d")
city$month<-as.numeric(format(city$`Arrival Date`,"%m"))
city$season ="winter"
city$season[city$month>=6&city$month<=8]<-"summer"
city$season[city$month>=9&city$month<=11]<-"autumn"
city$season[city$month>=3&city$month<=5]<-"spring"
city$season<-factor(city$season,levels=c("summer","spring","winter","autumn"))

resort$`Arrival Date`<-strptime(resort$`Arrival Date`,format="%Y-%m-%d")
resort$month<-as.numeric(format(resort$`Arrival Date`,"%m"))
resort$season<-"winter"
resort$season[resort$month>=6&resort$month<=8]<-"summer"
resort$season[resort$month>=9&resort$month<=11]<-"autumn"
resort$season[resort$month>=3&resort$month<=5]<-"spring"
resort$season<-factor(resort$season,levels=c("summer","spring","winter","autumn"))
prop.table(table(city$IsCanceled, city$season), margin=2)
prop.table(table(resort$IsCanceled, resort$season), margin=2)

#REVENUE ANALYSIS
city$StayNights <- city$StaysInWeekendNights + city$StaysInWeekNights
city$AvgRevPStay <- city$StayNights*city$ADR

resort$StayNights <- resort$StaysInWeekendNights + resort$StaysInWeekNights
resort$AvgRevPStay <- resort$StayNights*resort$ADR

summary(city$AvgRevPStay)
summary(resort$AvgRevPStay)

ggplot(data=city) + geom_bar(aes(x=MarketSegment))
ggplot(data=resort) + geom_bar(aes(x=MarketSegment))

ggplot(data=city) + geom_boxplot(mapping=aes(x=party_type, y=AvgRevPStay, group=party_type))
ggplot(data=resort) + geom_boxplot(mapping=aes(x=party_type, y=AvgRevPStay, group=party_type))

ggplot(data=city) + geom_boxplot(mapping=aes(x=MarketSegment, y=AvgRevPStay, group=MarketSegment))
ggplot(data=resort) + geom_boxplot(mapping=aes(x=MarketSegment, y=AvgRevPStay, group=MarketSegment))

ggplot(data=city) + geom_boxplot(mapping=aes(x=CustomerType, y=AvgRevPStay, group=CustomerType))
ggplot(data=resort) + geom_boxplot(mapping=aes(x=CustomerType, y=AvgRevPStay, group=CustomerType))


###############################################################
#APRIORI ANALYSIS

city$BookingTime <- 4
city$BookingTime[city$LeadTime < 23] <- 1
city$BookingTime[city$LeadTime >= 23 & city$LeadTime < 74] <- 2
city$BookingTime[city$LeadTime >= 74 & city$LeadTime < 163] <- 3
city$BookingTime <- as.factor(city$BookingTime)

quantile(resort$LeadTime, probs=c(0.25, 0.5, 0.75))
resort$BookingTime <- 4
resort$BookingTime[resort$LeadTime < 10] <- 1
resort$BookingTime[resort$LeadTime >= 10 & resort$LeadTime < 57] <- 2
resort$BookingTime[resort$LeadTime >= 57 & resort$LeadTime < 155] <- 3
resort$BookingTime <- as.factor(resort$BookingTime)

city$AvgRevPS <- 4
city$AvgRevPS[city$AvgRevPStay < 160] <- 1
city$AvgRevPS[city$AvgRevPStay >= 160 & city$AvgRevPStay < 264] <- 2
city$AvgRevPS[city$AvgRevPStay >= 264 & city$AvgRevPStay < 401] <- 3
city$AvgRevPS <- as.factor(city$AvgRevPS)

quantile(resort$AvgRevPStay, probs=c(0.25, 0.5, 0.75))
resort$AvgRevPS <- 4
resort$AvgRevPS[resort$AvgRevPStay < 117] <- 1
resort$AvgRevPS[resort$AvgRevPStay >= 117 & resort$AvgRevPStay < 273] <- 2
resort$AvgRevPS[resort$AvgRevPStay >= 273 & resort$AvgRevPStay < 593] <- 3
resort$AvgRevPS <- as.factor(resort$AvgRevPS)


## Looking at cancellation using Apriori rules
city_new<-data.frame(party_type = as.factor(city$party_type),
                     ReservedRoomType = as.factor(city$ReservedRoomType),
                     Meal_type = as.factor(city$Meal),
                     MarketSegment = as.factor(city$MarketSegment),
                     IsRepeatedGuest = as.factor(city$IsRepeatedGuest),
                     DepositType = as.factor(city$DepositType),
                     CustomerType = as.factor(city$CustomerType),
                     DistributionChannel = as.factor(city$DistributionChannel),
                     AssignedRoomType = as.factor(city$AssignedRoomType),
                     Company = as.factor(city$Company),
                     Agent = as.factor(city$Agent),
                     Country = as.factor(city$Country),
                     IsCanceled = as.factor(city$IsCanceled),
                     PreviousCancellations = as.factor((city$PreviousCancellations>0)),
                     PreviousBookingNotCancelled = as.factor((city$PreviousBookingsNotCanceled>0)),
                     CarParkingRequired = as.factor((city$RequiredCarParkingSpaces>0)),
                     LeadTime = as.factor(city$BookingTime),
                     Waitlist = as.factor((city$DaysInWaitingList>0)),
                     BookingChanges = as.factor((city$BookingChanges>0)),
                     AverageRevenue = city$AvgRevPS
                     )
cityX <- as(city_new,"transactions")
itemFreq<- itemFrequency(cityX)
itemFreq<-sort(itemFreq)
itemFrequencyPlot(cityX, topN=20)
inspect(cityX[1:10])
rules <- apriori(cityX,
                 parameter=list(supp=0.2, conf=0.80), 
                 control=list(verbose=F),
                 appearance=list(default="lhs",rhs=("IsCanceled=1")))
inspect(rules)

##RESORT
resort_new<-data.frame(party_type = as.factor(resort$party_type),
                       ReservedRoomType = as.factor(resort$ReservedRoomType),
                       Meal_type = as.factor(resort$Meal),
                       MarketSegment = as.factor(resort$MarketSegment),
                       IsRepeatedGuest = as.factor(resort$IsRepeatedGuest),
                       DepositType = as.factor(resort$DepositType),
                       CustomerType = as.factor(resort$CustomerType),
                       DistributionChannel = as.factor(resort$DistributionChannel),
                       AssignedRoomType = as.factor(resort$AssignedRoomType),
                       Company = as.factor(resort$Company),
                       Agent = as.factor(resort$Agent),
                       Country = as.factor(resort$Country),
                       IsCanceled = as.factor(resort$IsCanceled),
                       PreviousCancellations = as.factor((resort$PreviousCancellations>0)),
                       PreviousBookingNotCancelled = as.factor((resort$PreviousBookingsNotCanceled>0)),
                       CarParkingRequired = as.factor((resort$RequiredCarParkingSpaces>0)),
                       LeadTime = as.factor(resort$BookingTime),
                       Waitlist = as.factor((resort$DaysInWaitingList>0)),
                       BookingChanges = as.factor((resort$BookingChanges>0)),
                       AverageRevenue = resort$AvgRevPS,
                       season = resort$season
)
resortX <- as(resort_new,"transactions")
itemFreq<- itemFrequency(resortX)
itemFreq<-sort(itemFreq)
itemFrequencyPlot(resortX, topN=20)
inspect(resortX[1:10])
rules <- apriori(resortX,
                 parameter=list(supp=0.2, conf=0.4), 
                 control=list(verbose=F),
                 appearance=list(default="lhs",rhs=("IsCanceled=1")))
inspect(rules)
####################################################################################

#####
#SVM
#####
# Generating a new data frame with the necessary variables

#CITY
city_svm <- data.frame(Cancel=city$IsCanceled,
                       LeadTime=city$LeadTime,
                       StaysWkn=city$StaysInWeekendNights,
                       StaysWk=city$StaysInWeekNights,
                       Adults=city$Adults,
                       Children=as.numeric(city$Children),
                       Babies=city$Babies,
                       RepeatedGuest=city$IsRepeatedGuest,
                       PrevCancellations=city$PreviousCancellations,
                       PrevBookingsNotCancelled=city$PreviousBookingsNotCanceled,
                       BookingChanges=city$BookingChanges,
                       DaysWaitingList=city$DaysInWaitingList,
                       ADR=city$ADR,
                       ReqParkingSpaces=city$RequiredCarParkingSpaces,
                       NumSpecialRequests=city$TotalOfSpecialRequests,
                       StayNights=city$StayNights,
                       AvgRevStay=city$AvgRevPStay
)
# converting our dependent varibale to factors
city_svm$Cancel = as.factor(city_svm$Cancel)
#getting rid of any NAs in the data
city_svm<- na.omit(city_svm)
#train-test split
library(caret);library(kernlab);
ctrain_L <- createDataPartition(y=city_svm$Cancel, p=.60, list=FALSE)
ctrain_Set <- city_svm[ctrain_L,]
ctest_Set<- city_svm[-ctrain_L,]
#train the svm model, with cost 5 and 3-fold cross -validation
#takes a little time(15 mins) to train
csvmOut <- ksvm(Cancel~., data=ctrain_Set, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
csvmOut
csvmPred <- predict(csvmOut, ctest_Set)
table(csvmPred, ctest_Set$Cancel)
confusionMatrix(table(csvmPred, ctest_Set$Cancel))

#RESORT
resort_svm <- data.frame(Cancel=resort$IsCanceled,
                         LeadTime=resort$LeadTime,
                         StaysWkn=resort$StaysInWeekendNights,
                         StaysWk=resort$StaysInWeekNights,
                         Adults=resort$Adults,
                         Children=as.numeric(resort$Children),
                         Babies=resort$Babies,
                         RepeatedGuest=resort$IsRepeatedGuest,
                         PrevCancellations=resort$PreviousCancellations,
                         PrevBookingsNotCancelled=resort$PreviousBookingsNotCanceled,
                         BookingChanges=resort$BookingChanges,
                         DaysWaitingList=resort$DaysInWaitingList,
                         ADR=resort$ADR,
                         ReqParkingSpaces=resort$RequiredCarParkingSpaces,
                         NumSpecialRequests=resort$TotalOfSpecialRequests,
                         StayNights=resort$StayNights,
                         AvgRevStay=resort$AvgRevPStay,
                         Month=resort$month
                         
)
resort_svm$Cancel = as.factor(resort_svm$Cancel)
#getting rid of any NAs in the data
resort_svm<- na.omit(resort_svm)
#train-test split
ctrain_L <- createDataPartition(y=resort_svm$Cancel, p=.60, list=FALSE)
ctrain_Set <- resort_svm[ctrain_L,]
ctest_Set<- resort_svm[-ctrain_L,]
#train the svm model, with cost 5 and 3-fold cross -validation
#takes a little time(15 mins) to train
csvmOut <- ksvm(Cancel~., data=ctrain_Set, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
csvmOut
csvmPred <- predict(csvmOut, ctest_Set)
table(csvmPred, ctest_Set$Cancel)
confusionMatrix(table(csvmPred, ctest_Set$Cancel))

#################################################
#Linear models in a separate R-file.
cityWeek<- read_excel("city-week pivot table.xlsx")
#ADR of city hotel
lmOutA <- lm(formula=aveADR~NoDeposit+NonRefund+BB+HB+Direct+Winter+roomtype,data=cityWeek)
summary(lmOutA)
#Cancellation rate of city hotel
lmOutB <- lm(formula=cancel~LeadTime+Transient+TransientParty+NoDeposit+NonRefund+BB,data=cityWeekC)
summary(lmOutB)
#ADR of resort hotel
resortWeek<- read_excel("resort-week pivot table.xlsx")
lmOutC <- lm(formula=aveADR~LeadTime+Summer+Winter+Contract+Group+Transient+TransientParty+adults+children+babies+BB+Groups+OfflineTATO+OnlineTA+DCCorporate+DCDirect+DCTATO,data=resortWeek)
summary(lmOutC)
#Cancellation rate of resort hotel
lmOutD <- lm(formula=cancel~NoDeposit+NonRefund,data=resortWeek)
summary(lmOutD)

