setwd("C:\\Research\\Dynamic_Discounting\\Data")

load(file = "scf.clean.RData")
load(file = "transactions.clean.RData")
load(file = "buyers.RData")
load(file = "longtransactions.RData")

# load library
source("C:\\Research\\Dynamic_Discounting\\R_files_BM/00_load_libraries.R")
t.long.df1 <- t.long.df[t.long.df$spend.year <= 2017 & t.long.df$spend.year >= 2012,]

transactions.clean1 <- transactions.clean[transactions.clean$spend.year <= 2017 & transactions.clean$spend.year >= 2012,]

#plyr::count(transactions.clean1$BID)

# Descriptive statistics long table
min(transactions.clean1$discountCharge)
max(transactions.clean1$discountCharge)
mean(transactions.clean1$discountCharge)
sd(transactions.clean1$discountCharge)
count(transactions.clean1$discountCharge)

min(transactions.clean$payment)
max(transactions.clean$payment)
mean(transactions.clean$payment)
sd(transactions.clean$payment)
count(transactions.clean$payment)

min(transactions.clean$full.amount)
max(transactions.clean$full.amount)
mean(transactions.clean$full.amount)
sd(transactions.clean$full.amount)
count(transactions.clean$full.amount)

min(transactions.clean$duration)
max(transactions.clean$duration)
mean(transactions.clean$duration)
sd(transactions.clean$duration)
count(transactions.clean$duration)

min(transactions.clean$actual.discountRate)
max(transactions.clean$actual.discountRate)
mean(transactions.clean$actual.discountRate)
sd(transactions.clean$actual.discountRate)
count(transactions.clean$actual.discountRate)

min(transactions.clean$paymentTerms)
max(transactions.clean$paymentTerms)
mean(transactions.clean$paymentTerms)
sd(transactions.clean$paymentTerms)
count(transactions.clean$paymentTerms)

###
min(scf.clean$Supplier.revenue)
max(scf.clean$Supplier.revenue)
mean(scf.clean$Supplier.revenue)
sd(scf.clean$Supplier.revenue)
count(scf.clean$Supplier.revenue)

min(buyers.df$Buyer.revenue)
max(buyers.df$Buyer.revenue)
mean(buyers.df$Buyer.revenue)
sd(buyers.df$Buyer.revenue)
count(buyers.df$Buyer.revenue)

min(buyers.df$Buyer.COGS)
max(buyers.df$Buyer.COGS)
mean(buyers.df$Buyer.COGS)
sd(buyers.df$Buyer.COGS)
count(buyers.df$Buyer.COGS)

plyr::count(scf.clean$Supplier.industry)
plyr::count(buyers.df$Buyer.Industry)
plyr::count(scf.clean$Supplier.country)
plyr::count(buyers.df$Buyer.Country)
plyr::count(buyers.df$Buyer.creditRating)


min(t.long.df$days.to.next.quarter)
max(t.long.df$days.to.next.quarter)
mean(t.long.df$days.to.next.quarter)
sd(t.long.df$days.to.next.quarter)


### correlation table
df <- subset(t.long.df, select=c(Supplier.revenue, full.amount, actual.discountRate, duration, days.to.next.quarter))
cor_matrix <- cor(df)

rcorr(as.matrix(df))

corr_test <- corr.test(df)$p
corrplot(cor_matrix)


# Print the LaTeX table
latex_table <- xtable(cor_matrix)
print(latex_table)


### create a figure on the number of invoice and aggregate it on the monthly level
# use transactional data frame 
transactions.clean$TID <- 1
transactions.clean$month_year <- format(ymd(transactions.clean$discountDate), "%Y-%m")
#transactions.clean$month_year <- as.Date(paste0(transactions.clean$month_year, "-01"))

###
start_date <- ymd('2012-01-01')  # Specify the start date
end_date <- ymd('2017-12-31')    # Specify the end date

# Filter the data between the start and end dates
df <- transactions.clean[transactions.clean$discountDate >= start_date & transactions.clean$discountDate <= end_date,]

# number of invoices
result <- df %>% group_by(month_year) %>% dplyr::summarise(invoice.count = sum(TID))
result$date <- as.Date(paste0(result$month_year, "-01"), format = "%Y-%m-%d")


ggplot(data=result, aes(x= month_year, y=invoice.count, group=1)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = "2012-12", colour = "black") +
  geom_vline(xintercept = "2013-12", colour = "black") +
  geom_vline(xintercept = "2014-12", colour = "black") +
  geom_vline(xintercept = "2015-12", colour = "black") +
  geom_vline(xintercept = "2016-12", colour = "black") +
  geom_vline(xintercept = "2017-12", colour = "black") +
  geom_vline(aes(xintercept = "2012-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2012-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2012-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2013-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2013-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2013-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2014-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2014-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2014-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2015-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2015-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2015-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2016-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2016-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2016-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2017-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2017-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2017-09", colour = I("black")), linetype="dashed") +
  xlab("Time") +
  theme(axis.title.x = element_text(size = 25)) +
  ylab("Number of Discounted Invoices") +
  theme(axis.title.y = element_text(size = 25))


# number of full amount transaction 
result.2 <- df %>% group_by(month_year) %>% dplyr::summarise(invoice.amount = sum(full.amount))
result.2$date <- as.Date(paste0(result.2$month_year, "-01"), format = "%Y-%m-%d")

ggplot(data=result.2, aes(x=month_year, y=invoice.amount, group=1)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_line() +
 #geom_bar(stat="identity", color="black", fill="grey", just = 1) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = "2012-12", colour = "black") +
  geom_vline(xintercept = "2013-12", colour = "black") +
  geom_vline(xintercept = "2014-12", colour = "black") +
  geom_vline(xintercept = "2015-12", colour = "black") +
  geom_vline(xintercept = "2016-12", colour = "black") +
  geom_vline(xintercept = "2017-12", colour = "black") +
  geom_vline(aes(xintercept = "2012-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2012-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2012-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2013-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2013-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2013-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2014-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2014-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2014-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2015-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2015-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2015-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2016-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2016-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2016-09", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2017-03", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2017-06", colour = I("black")), linetype="dashed") +
  geom_vline(aes(xintercept = "2017-09", colour = I("black")), linetype="dashed") +
  xlab("Time") +
  theme(axis.title.x = element_text(size = 25)) +
  ylab("Volume of Discounted Invoices in USD") +
  theme(axis.title.y = element_text(size = 25)) 
  