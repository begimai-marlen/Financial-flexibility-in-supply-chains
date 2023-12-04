setwd("C:\\Research\\Dynamic_Discounting\\Data")

# clear workspace
rm(list = ls())

# load packages
source("C:\\Research\\Dynamic_Discounting\\R_files_BM/00_load_libraries.R")

# load data
load(file = "scf.clean.RData")
load(file = "transactions.clean.RData")


###############
#
#
#   Create long data set for repeated measures 
#   - days until next quarter
#   - Total number of invoices outstanding
#   - Total value of invoices outstanding
#   - binary dummy "end of quarter"


# use this only for testing purposes with random subset
set.seed(123)
(sample<-round(runif(n=5000,min=1,max=length(transactions.clean$SID))))

t.df <- subset(transactions.clean,select=c(
  #t.df <- subset(transactions.df[sample,],select=c(
  "BID",
  "spend.year",
  "SID",
  "discountDate",
  "Invoice.upload.date",
  "Invoice.due.date",
  "full.amount",
  "actual.discountRate",
  "duration",
  "Supplier.country",
  "Supplier.industry",
  "Supplier.revenue",
  "Supplier.revenue",
  "Buyer.COGS",
  "Buyer.revenue",
  "Buyer.Industry",
  "Buyer.Country"))

TID <- 1:length(t.df[,1])
t.df<-cbind(t.df, TID )
t.df$rep <- t.df$duration+1

t.long.df <-expandRows(t.df, "rep")

rep.vec<-c()
for(i in 1:length(t.df[,1])){
  rep.vec <- c(rep.vec,0:(t.df[i,"duration"]))
}

t.long.df$repNumber<-rep.vec

t.long.df$date <- t.long.df$Invoice.upload.date + t.long.df$repNumber

t.long.df<-plyr::rename(t.long.df,replace=c("repNumber"="days.waited"))

t.long.df$event<-FALSE
t.long.df$event<-ifelse(t.long.df$days.waited==t.long.df$duration,TRUE,t.long.df$event)

t.long.df$start <- t.long.df$days.waited+ 1  # becauase "days waited" formerly "repNumber" starts at t=0 but we want t=1 as start date for parametric models
t.long.df$stop <- t.long.df$days.waited + 2  # becauase "days waited" formerly "repNumber" starts at t=0 but we want t=1 as start date for parametric models

getQuarter <- function(date){
  month <- as.numeric(substr(as.character(date),start=6,stop=7))
  return(ceil((month-.5)/3))
}

t.long.df$quarter <- getQuarter(t.long.df$date)

quarters <- as.Date(paste("01",as.character(1),as.character(2019),sep="."), "%d.%m.%Y")
for(year in 2010:2018){
  for(i in c(1,4,7,10)){
    quarters <- append(quarters,as.Date(paste("01",as.character(i),as.character(year),sep="."), "%d.%m.%Y"))
  }
}

days.to.next.quarter <- function(d,quarters){
  m<-t(matrix(rep(quarters,length(d)),ncol=length(d))) - matrix(rep(d,length(quarters)),ncol=length(quarters))
  m[m<=0]<-Inf
  return(apply(m, 1, function(x) min(x)))
}

t.long.df$days.to.next.quarter<-days.to.next.quarter(t.long.df$date,quarters)
t.long.df$end.of.quarter <- ifelse(t.long.df$days.to.next.quarter<5,TRUE,FALSE)

t.long.df<-t.long.df[order(t.long.df$date),]

t.unique.df<-unique(subset(t.long.df,select=c(date, SID)))

t.unique.df$open.invoices.count <- NULL
t.unique.df$open.invoices.amount <- NULL
t.unique.df$dueDate.vec <- NULL

for(i in 1:length(t.unique.df[,1])){
  temp.df <- t.long.df[t.long.df$SID==t.unique.df[i,"SID"] & t.long.df$date==t.unique.df[i,"date"], ]
  t.unique.df[i,"open.invoices.count"] <- length(temp.df[,"full.amount"])
  t.unique.df[i,"open.invoices.amount"] <- sum(temp.df[,"full.amount"])
  t.unique.df[i,"dueDate.vec"] <- paste(as.numeric(sort(temp.df$Invoice.due.date)),collapse = ",")
}

get.invoice.sequence.pos<-function(dueDate.vec,invoice.date) {
  test.vec <-as.numeric(unlist(strsplit(dueDate.vec, split=",")))
  min(which(test.vec==as.numeric(invoice.date)))
}

t.long.df<-merge(t.long.df,t.unique.df, by=c("date","SID"))

t.long.df$open.invoices.count <- t.long.df$open.invoices.count -1
t.long.df$open.invoices.amount <- t.long.df$open.invoices.amount - t.long.df$full.amount

t.long.df$invoice.sequence.pos<- -1

for(i in 1:length(t.long.df[,1])){
  t.long.df[i,"invoice.sequence.pos"] = get.invoice.sequence.pos(t.long.df[i,"dueDate.vec"],t.long.df[i,"Invoice.due.date"])
}

t.long.df$invoice.next.in.line <- (t.long.df$invoice.sequence.pos==1)*1

t.long.df$soiat <- (t.long.df$event & t.long.df$open.invoices.count==0)*1

temp.df<-data.frame(tapply(t.long.df$soiat,t.long.df$TID,max))
temp.df$TID<-as.numeric(row.names(temp.df))
names(temp.df)<-c("single.open.invoice.at.discount","TID")

t.long.df<-subset(plyr::join(t.long.df,temp.df), select=-c(soiat))
t.long.df$single.open.invoice.at.discount<-as.numeric(t.long.df$single.open.invoice.at.discount)

t.long.df<-subset(t.long.df,select=-c(dueDate.vec))

save(t.long.df,file="longtransactions.RData")