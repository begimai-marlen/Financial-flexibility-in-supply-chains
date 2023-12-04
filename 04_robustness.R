setwd("C:\\Research\\Dynamic_Discounting\\Data")

load(file = "scf.clean.RData")
load(file = "transactions.clean.RData")
load(file = "longtransactions.RData")

# load library
source("C:\\Research\\Dynamic_Discounting\\R_files_BM/00_load_libraries.R")

t.long.df$mean.centered.log.full.amount <- scale(log(t.long.df$full.amount),center=T,scale=T)
t.long.df$mean.centered.scaled.amount <- scale(t.long.df$full.amount,center=T,scale=T)
t.long.df$mean.centered.discountRate <- scale(t.long.df$actual.discountRate,center=T,scale=T)
t.long.df$mean.centered.Supplier.revenue <- scale(t.long.df$Supplier.revenue,center=T,scale=T)
t.long.df$mean.centered.log.Supplier.revenue <- scale(log(t.long.df$Supplier.revenue),center=T,scale=T)
t.long.df$mean.centered.days.to.next.quarter <- scale((t.long.df$days.to.next.quarter),center=T,scale=F)

t.long.df$mean.days <- mean((t.long.df$days.to.next.quarter))


############################
#
#
#   PLOT -- counterfactual
#
#
###############################

summary.flex <- function(flexsurvreg.fm){
  stars.fn <- function(p){
    ifelse(p < 0.001,"***",
           ifelse(p<0.01,"**",
                  ifelse(p<0.05,"*",
                         ifelse(p<0.1,"+","")
                  )
           )
    ) %>% return
  }
  
  flexsurvreg.fm$res.t %>% 
    as.data.frame %>%
    tibble::rownames_to_column("coeficient") %>%
    select(c(coeficient,est,se)) %>%
    mutate(t=est/se) %>%
    mutate(p = (1-pnorm(abs(t)))*2) %>%
    mutate(sig = stars.fn(p)) %>%
    return
}

##### another graph
#### O days, 30 days, 60 days, 90 days

summary.flex(interaction.weibull <-flexsurvreg(Surv(start,stop,event) ~ Supplier.industry 
                                               + as.factor(spend.year)
                                               + mean.centered.discountRate
                                               + mean.centered.log.Supplier.revenue
                                               + mean.centered.log.full.amount
                                               + mean.centered.days.to.next.quarter
                                               + mean.centered.log.Supplier.revenue:mean.centered.days.to.next.quarter
                                               + mean.centered.log.full.amount:mean.centered.days.to.next.quarter,
                                               data=t.long.df,
                                               dist="weibull"))


days.to.next.quarter.0 <- attr(summary(interaction.weibull, type="survival"),"X") %>%
  as.data.frame() %>%
  mutate(mean.centered.days.to.next.quarter=-44.46472+0)

days.to.next.quarter.30 <- attr(summary(interaction.weibull, type="survival"),"X") %>%
  as.data.frame() %>%
  mutate(mean.centered.days.to.next.quarter=-44.46472+30)

days.to.next.quarter.60 <- attr(summary(interaction.weibull, type="survival"),"X") %>%
  as.data.frame() %>%
  mutate(mean.centered.days.to.next.quarter=-44.46472+60)

days.to.next.quarter.90 <- attr(summary(interaction.weibull, type="survival"),"X") %>%
  as.data.frame() %>%
  mutate(mean.centered.days.to.next.quarter=-44.46472+90)

labels.y <- c("0%", "25%","50%","75%","100%")

complement.fn <- function(x) (1-x)

model.0.days <- summary(interaction.weibull, X = days.to.next.quarter.0)[[1]] %>% 
  as.data.frame() %>%
  rename_with(.fn = ~ paste0("model.0.days.", .x), .cols=c(!time)) %>%
  mutate_at(vars(contains(".")), ~complement.fn(.))

model.30.days <- summary(interaction.weibull, X = days.to.next.quarter.30)[[1]] %>% 
  as.data.frame() %>%
  rename_with(.fn = ~ paste0("model.30.days.", .x), .cols=c(!time)) %>%
  mutate_at(vars(contains(".")), ~complement.fn(.))

model.60.days <- summary(interaction.weibull, X = days.to.next.quarter.60)[[1]] %>% 
  as.data.frame() %>%
  rename_with(.fn = ~ paste0("model.60.days.", .x), .cols=c(!time)) %>%
  mutate_at(vars(contains(".")), ~complement.fn(.))

model.90.days <- summary(interaction.weibull, X = days.to.next.quarter.90)[[1]] %>% 
  as.data.frame() %>%
  rename_with(.fn = ~ paste0("model.90.days.", .x), .cols=c(!time)) %>%
  mutate_at(vars(contains(".")), ~complement.fn(.))

all.data <- model.0.days %>%
  merge(.,model.30.days) %>%
  merge(.,model.60.days)%>%
  merge(.,model.90.days)


(full.model.graph <- ggplot(all.data, aes(time)) + 
    geom_hline(yintercept = 1, linetype="dashed", colour="grey") +
    geom_line(aes(y = model.0.days.est, colour = I("black"))) +
    geom_line(aes(y = model.0.days.lcl, colour = I("black")),linetype = "dashed") + 
    geom_line(aes(y = model.0.days.ucl, colour = I("black")),linetype = "dashed") + 
    geom_line(aes(y = model.30.days.est, colour = I("blue"))) + 
    geom_line(aes(y = model.30.days.lcl, colour = I("blue")),linetype = "dashed") + 
    geom_line(aes(y = model.30.days.ucl, colour = I("blue")),linetype = "dashed") + 
    geom_line(aes(y = model.60.days.est, colour = I("grey"))) + 
    geom_line(aes(y = model.60.days.lcl, colour = I("grey")),linetype = "dashed") + 
    geom_line(aes(y = model.60.days.ucl, colour = I("grey")),linetype = "dashed") + 
    geom_line(aes(y = model.90.days.est, colour = I("red"))) + 
    geom_line(aes(y = model.90.days.lcl, colour = I("red")),linetype = "dashed") + 
    geom_line(aes(y = model.90.days.ucl, colour = I("red")),linetype = "dashed") + 
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(plot.title    = element_text(family = "latex",size=20),
          plot.subtitle = element_text(family = "latex"),
          axis.title.x  = element_text(family = "latex",size=20),
          axis.title.y  = element_text(family = "latex",size=20),
          axis.text.x   = element_text(family = "latex",size=20),
          axis.text.y   = element_text(family = "latex",size=20))+
    geom_vline(aes(xintercept = 30, colour = I("black")), linetype="dashed") +
 #   ggtitle("Model with 0 (black line), 30 (blue line), 60(grey line), 90 (red line) days to the next quarter") +
    theme(plot.title = element_text(size = 25)) +
    scale_y_continuous(name="Percentage of invoices discounted", breaks=(0:4)/4,labels=labels.y)+
    theme(axis.title.y = element_text(size = 25)) +
    scale_x_continuous(name="Days since invoice release", breaks = c(30, 60, 90, 120, 150), labels = c(30, 60, 90, 120, 150))+
    theme(axis.title.x = element_text(size = 25)))


geom_segment(aes(x = 2, y = 0.9, xend = 2.6, yend = 0.72), arrow = arrow(angle=20,length = unit(0.25, "cm"),type="closed"),color="grey")+
  geom_label(
    label="long payment terms", 
    x=2.5,
    y=0.95,
    label.size = NA,
    color = "grey",
    family="latex",
    size=14
  ))

