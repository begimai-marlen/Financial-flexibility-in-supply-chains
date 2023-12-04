setwd("C:\\Research\\Dynamic_Discounting\\Data")

load(file = "scf.clean.RData")
load(file = "transactions.clean.RData")
load(file = "buyers.RData")
load(file = "longtransactions.RData")

# load library
source("C:\\Research\\Dynamic_Discounting\\R_files_BM/00_load_libraries.R")

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
result$year <- year(result$date)
result$month <- month(result$date)


ggplot(data=result, aes(x= interaction(month, year), y=invoice.count, group=year, fill=month_year)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "latex",size=18))+
  xlab("Time") +
  theme(axis.title.x = element_text(size = 20)) +
  ylab("Number of discounted invoices") +
  theme(axis.title.y = element_text(size = 20)) + 
  scale_x_discrete(NULL, guide = "axis_nested")  


# number of full amount transaction 
result.2 <- df %>% group_by(month_year) %>% dplyr::summarise(invoice.amount = sum(full.amount))
result.2$date <- as.Date(paste0(result.2$month_year, "-01"), format = "%Y-%m-%d")
result.2$year <- year(result.2$date)
result.2$month <- month(result.2$date)


ggplot(data=result.2, aes(x= interaction(month, year), y=invoice.amount, group=year, fill=month_year)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black",
                                     "gray", "grey", "black")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
  axis.text.x = element_text(family = "latex",size=18))+
  xlab("Time") +
  theme(axis.title.x = element_text(size = 20)) +
  ylab("Volume of discounted invoices in USD") +
  theme(axis.title.y = element_text(size = 20)) +
  scale_x_discrete(NULL, guide = "axis_nested")  
