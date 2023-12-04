setwd("C:\\Research\\Dynamic_Discounting\\Data")

load(file = "scf_all.RData")
load(file = "transaction.raw1.RData")


# load library
source("C:\\Research\\Dynamic_Discounting\\R_files_BM/00_load_libraries.R")

# create a clean data frame
# subset only needed variables 
transactions.clean <- subset(transactions.raw.df, select =-c(Payment.to.supplier,Discount.charge,
                                                             Transaction,Supplier,Buyer))

# merge with scf_all dataframe
transactions.clean <- merge(transactions.clean, scf_all.df, by="dyad.orig.id")

# rename some variables 
transactions.clean<-plyr::rename(transactions.clean,c("Total.amount.of.invoice...."="full.amount",
                                                "Payment.to.supplier...."="payment",
                                                "Discount.charge...."="discountCharge",
                                                "Supplier.discount.rate..bps."="discountRate",
                                                "Discount.activation"="discountDate",
                                                "Due.date"="Invoice.due.date"))

rm(transactions.raw.df)

transactions.clean <- transactions.clean[!is.na(transactions.clean$BID),]

# work on dates variables 
transactions.clean$discountDate<-as.Date(as.character(transactions.clean$discountDate), "%d.%m.%Y")
transactions.clean$Invoice.upload.date<-as.Date(as.character(transactions.clean$Invoice.upload.date), "%d.%m.%Y")
transactions.clean$Invoice.due.date<-as.Date(as.character(transactions.clean$Invoice.due.date), "%d.%m.%Y")

# remove USD sign 
transactions.clean$full.amount <- gsub("[$,]", "", transactions.clean$full.amount)
transactions.clean$payment <- gsub("[$,]", "", transactions.clean$payment)
transactions.clean$discountCharge <- gsub("[$,]", "", transactions.clean$discountCharge)

# make values numeric
transactions.clean$full.amount <- as.numeric(transactions.clean$full.amount)
transactions.clean$payment <- as.numeric(transactions.clean$payment)
transactions.clean$discountCharge <- as.numeric(transactions.clean$discountCharge)

transactions.clean <-subset(transactions.clean,select=-c(discountRate))

# create a spend variable
transactions.clean$spend.year <- year(transactions.clean$Invoice.upload.date) 

# discounting data 
transactions.clean$discountDuration <- as.numeric(transactions.clean$Invoice.due.date-transactions.clean$discountDate-3) #3 because it takes 3 days for processing etc.
transactions.clean$discountDuration <- ifelse(transactions.clean$discountDuration<=0,1,transactions.clean$discountDuration)

transactions.clean$actual.discountRate <- 10000*with(transactions.clean,discountCharge * 365/(discountDuration*full.amount)) # BPS

# duration variables 
transactions.clean$duration <- as.numeric(transactions.clean$discountDate - transactions.clean$Invoice.upload.date)
transactions.clean[transactions.clean$duration==0,]$duration <- 1

transactions.clean$paymentTerms <- as.numeric(transactions.clean $Invoice.due.date - transactions.clean$Invoice.upload.date)

# create annual spend variable for 2012 and 2017 data only 

for(year in 2012:2017){
  annualSpend.df<-data.frame(tapply(transactions.clean[transactions.clean$spend.year==year,]$full.amount,transactions.clean[transactions.clean$spend.year==year,]$SID,sum))
  annualSpend.df$SID <- as.numeric(rownames(annualSpend.df))
  colnames(annualSpend.df)<-c("annualSpend","SID")
  annualSpend.df$annualSpend<-unname(annualSpend.df$annualSpend)
  annualSpend.df <- plyr::rename(annualSpend.df,c("annualSpend"=paste("annualSpend.",year,sep="")))
  transactions.clean<-plyr::join(transactions.clean,annualSpend.df,by=c("SID"))
}

transactions.clean <- transactions.clean[transactions.clean$spend.year!="2018",]


#######
#
# SCF data set
# 
########

dyads.df <- unique(subset(transactions.clean,select=c(
  "SID",
  "BID",
  "Supplier.industry",
  "Supplier.country",
  "Supplier.revenue",
  "Buyer.Country",
  "Buyer.Industry",
  "Buyer.COGS",
  "Buyer.revenue",
  "Buyer.creditRating",
  "annualSpend.2012",
  "annualSpend.2013",
  "annualSpend.2014",
  "annualSpend.2015",
  "annualSpend.2016",
  "annualSpend.2017"
)))

dyads.df$BID <- as.integer(dyads.df$BID)
dyads.df$SID <- as.integer(dyads.df$SID)
dyads.df$annualSpend.2012 <- unname(dyads.df$annualSpend.2012)
dyads.df$annualSpend.2013 <- unname(dyads.df$annualSpend.2013)
dyads.df$annualSpend.2014 <- unname(dyads.df$annualSpend.2014)
dyads.df$annualSpend.2015 <- unname(dyads.df$annualSpend.2015)
dyads.df$annualSpend.2016 <- unname(dyads.df$annualSpend.2016)
dyads.df$annualSpend.2017 <- unname(dyads.df$annualSpend.2017)

# remove suppliers with only annual spent in 2018
dyads.df$available.years <- 6-(is.na(dyads.df$annualSpend.2012)*1 + is.na(dyads.df$annualSpend.2013)*1+is.na(dyads.df$annualSpend.2014)*1+
                                 is.na(dyads.df$annualSpend.2015)*1+is.na(dyads.df$annualSpend.2016)*1+is.na(dyads.df$annualSpend.2017)*1)

dyads.df <- dyads.df[dyads.df$available.years > 0,]

na20 <- function(vec){
  return(ifelse(is.na(vec),0,vec))
}

dyads.df$annualSpend.mean <- (na20(dyads.df$annualSpend.2012) + na20(dyads.df$annualSpend.2013) + na20(dyads.df$annualSpend.2014) + na20(dyads.df$annualSpend.2015) + na20(dyads.df$annualSpend.2016) + na20(dyads.df$annualSpend.2017)) / dyads.df$available.years

dyads.df$Supplier.revenue <- ifelse(dyads.df$Supplier.revenue < dyads.df$annualSpend.mean, dyads.df$annualSpend.mean, dyads.df$Supplier.revenue)
dyads.df$annualSpend2revenue <- dyads.df$annualSpend.mean / dyads.df$Supplier.revenue
#dyads.df$annualSpend2revenue <- ifelse(dyads.df$annualSpend2revenue>1,1,dyads.df$annualSpend2revenue)

dyads.df <- dyads.df[!is.na(dyads.df$annualSpend2revenue),]

transactions.clean <- transactions.clean[transactions.clean$SID %in% unique(dyads.df$SID),]

buyers.df <- unique(subset(dyads.df,select=c(
  "BID",
  "Buyer.Country",
  "Buyer.Industry",
  "Buyer.COGS",
  "Buyer.revenue",
  "Buyer.creditRating"
)))

scf.clean <- dyads.df

save(transactions.clean,file="transactions.clean.RData")
save(buyers.df,file="buyers.RData")
save(scf.clean,file="scf.clean.RData")