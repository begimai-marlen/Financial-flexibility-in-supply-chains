setwd("C:\\Research\\Dynamic_Discounting\\Data")

load(file = "scf.clean.RData")
load(file = "transactions.clean.RData")
load(file = "longtransactions.RData")

# load library
source("C:\\Research\\Dynamic_Discounting\\R_files_BM/00_load_libraries.R")

# scale the full amount 
t.long.df$mean.centered.log.full.amount <- scale(log(t.long.df$full.amount),center=T,scale=T)
t.long.df$mean.centered.scaled.amount <- scale(t.long.df$full.amount,center=T,scale=T)
t.long.df$mean.centered.discountRate <- scale(t.long.df$actual.discountRate,center=T,scale=T)
t.long.df$mean.centered.Supplier.revenue <- scale(t.long.df$Supplier.revenue,center=T,scale=T)
t.long.df$mean.centered.log.Supplier.revenue <- scale(log(t.long.df$Supplier.revenue),center=T,scale=T)
t.long.df$mean.centered.days.to.next.quarter <- scale((t.long.df$days.to.next.quarter),center=T,scale=T)


t.long.df1 <- t.long.df$spend.year <= 2017 & t.long.df$spend.year >= 2012


############
###
# DIRECT EFFECT 
#
###
############


# Model 1: DIRECT effect
summary((direct.controls <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                      + as.factor(spend.year)
                                      + mean.centered.discountRate 
                                      + cluster(TID),
                                      data=t.long.df,
                                      method="efron")))

summary((direct.supplier.size <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                      + mean.centered.log.Supplier.revenue + as.factor(spend.year)
                                      + mean.centered.discountRate 
                                      + cluster(TID),
                                      data=t.long.df,
                                      method="efron")))

summary((direct.invoice.value <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                      + mean.centered.log.full.amount + as.factor(spend.year)
                                      + mean.centered.discountRate
                                      + cluster(TID),
                                      data=t.long.df,
                                      method="efron")))

summary((both <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                      + as.factor(spend.year)
                      + mean.centered.discountRate 
                      + mean.centered.log.Supplier.revenue
                      + mean.centered.log.full.amount 
                      + cluster(TID),
                      data=t.long.df,
                      method="efron")))


summary((days <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                              + as.factor(spend.year)
                              + mean.centered.discountRate 
                              + mean.centered.log.Supplier.revenue
                              + mean.centered.log.full.amount 
                              + mean.centered.days.to.next.quarter
                              + cluster(TID),
                              data=t.long.df,
                              method="efron")))

###
direct.effects <- list(direct.controls, 
                       direct.supplier.size,
                       direct.invoice.value,
                       both,
                       days)

modelsummary(direct.effects, 
             estimate = "{estimate}{stars}",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

modelsummary(direct.effects, 
             estimate = "{estimate}{stars}",
             output = "latex",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")


# Model 3: interaction effects
summary((interaction.1 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                     + as.factor(spend.year) 
                     + mean.centered.discountRate
                     + mean.centered.log.Supplier.revenue*mean.centered.days.to.next.quarter
                     + cluster(TID),
                     data=t.long.df,
                     method="efron")))

summary((interaction.2 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                             + as.factor(spend.year) 
                             + mean.centered.discountRate
                             + mean.centered.log.full.amount*mean.centered.days.to.next.quarter
                             + cluster(TID),
                             data=t.long.df,
                             method="efron")))


summary((interaction.all <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                               + as.factor(spend.year) 
                               + mean.centered.discountRate
                               + mean.centered.log.Supplier.revenue*mean.centered.days.to.next.quarter
                               + mean.centered.log.full.amount*mean.centered.days.to.next.quarter
                               + cluster(TID),
                               data=t.long.df,
                               method="efron")))

main.model <- list(
  interaction.1,
  interaction.2,
  interaction.all
)

modelsummary(main.model, 
             estimate = "{estimate}{stars}",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

modelsummary(main.model, 
             estimate = "{estimate}{stars}",
             output = "latex",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

#####
#
# Weibull Model 
#
#### 

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

# Model 1: controls
summary.flex(control.weibull <-flexsurvreg(Surv(start,stop,event) ~ Supplier.industry 
                           + mean.centered.log.Supplier.revenue 
                           + as.factor(spend.year)
                           + mean.centered.log.full.amount
                           + mean.centered.discountRate,
                           data=t.long.df,
                           dist="weibull"))


# Model 2: controls + main effects
summary.flex(main.effect.weibull <-flexsurvreg(Surv(start,stop,event) ~ Supplier.industry 
                     + mean.centered.log.Supplier.revenue + as.factor(spend.year)
                     + mean.centered.discountRate
                     + mean.centered.log.full.amount 
                     + mean.centered.days.to.next.quarter,
                     data=t.long.df,
                     dist="weibull"))

# Model 3: interaction effects
summary.flex(interaction.weibull.1 <-flexsurvreg(Surv(start,stop,event) ~ Supplier.industry 
                                               + as.factor(spend.year)
                                               + mean.centered.discountRate
                                               + mean.centered.log.Supplier.revenue
                                               + mean.centered.days.to.next.quarter
                                               + mean.centered.log.Supplier.revenue:mean.centered.days.to.next.quarter,
                                               data=t.long.df,
                                               dist="weibull"))
# Model 3: interaction effects
summary.flex(interaction.weibull.2 <-flexsurvreg(Surv(start,stop,event) ~ Supplier.industry 
                                               + as.factor(spend.year)
                                               + mean.centered.discountRate
                                               + mean.centered.log.full.amount
                                               + mean.centered.days.to.next.quarter
                                               + mean.centered.log.full.amount:mean.centered.days.to.next.quarter,
                                               data=t.long.df,
                                               dist="weibull"))

# Model 3: interaction effects
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


aft.model <- list(
  control.weibull,
  main.effect.weibull,
  interaction.weibull.1,
  interaction.weibull.2,
  interaction.weibull
)

modelsummary(aft.model, 
             estimate = "{estimate}{stars}",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

modelsummary(aft.model, 
             estimate = "{estimate}{stars}",
             output = "latex",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

#
#
# ROBUSTNESS TESTS
#####

#####
#
# SUBSET OF INVOICES DISCOUNTED >=3 days
# 
#####

#
# Model 1: only controls
summary((controls.robust.3 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                           + mean.centered.log.Supplier.revenue + as.factor(spend.year)
                           + mean.centered.discountRate
                           + mean.centered.log.full.amount
                           + cluster(TID),
                           data=t.long.df[t.long.df$duration >= 3,],
                           method="efron")))

# Model 2: controls + main effects
summary((main.effect.robust.3 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                              + mean.centered.log.Supplier.revenue + as.factor(spend.year) 
                              + mean.centered.log.full.amount
                              + mean.centered.discountRate
                              + mean.centered.days.to.next.quarter
                              + cluster(TID),
                              data=t.long.df[t.long.df$duration >= 3,],
                              method="efron")))


# Model 3: interaction effects
summary((interaction.1.robust.3 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                      + as.factor(spend.year) 
                                      + mean.centered.discountRate
                                      + mean.centered.log.Supplier.revenue*mean.centered.days.to.next.quarter
                                      + cluster(TID),
                                      data=t.long.df[t.long.df$duration >= 3,],
                                      method="efron")))
# Model 3: interaction effects
summary((interaction.2.robust.3 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                        + as.factor(spend.year) 
                                        + mean.centered.discountRate
                                        + mean.centered.log.full.amount*mean.centered.days.to.next.quarter
                                        + cluster(TID),
                                        data=t.long.df[t.long.df$duration >= 3,],
                                        method="efron")))

# Model 3: interaction effects
summary((interaction.all.robust.3 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                      + as.factor(spend.year) 
                                      + mean.centered.discountRate
                                      + mean.centered.log.Supplier.revenue*mean.centered.days.to.next.quarter
                                      + mean.centered.log.full.amount*mean.centered.days.to.next.quarter
                                      + cluster(TID),
                                      data=t.long.df[t.long.df$duration >= 3,],
                                      method="efron")))

robustness.3.days <- list(controls.robust.3,
                          main.effect.robust.3,
                          interaction.1.robust.3,
                          interaction.2.robust.3,
                          interaction.all.robust.3)

modelsummary(robustness.3.days, 
             estimate = "{estimate}{stars}",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

modelsummary(robustness.3.days, 
             estimate = "{estimate}{stars}",
             output = "latex",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

########
##
##
# Supplier outliers
##
##
########


# Model 1: only controls
summary((controls.outliers <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                  + mean.centered.log.Supplier.revenue + as.factor(spend.year)
                  + mean.centered.discountRate
                  + mean.centered.log.full.amount
                  + cluster(TID),
                  data=t.long.df[t.long.df$mean.centered.log.Supplier.revenue <= 2 & t.long.df$mean.centered.log.Supplier.revenue >= -2,],
                  method="efron")))

# Model 2: controls + main effects
summary((main.effect.outliers <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                     + mean.centered.log.Supplier.revenue + as.factor(spend.year) 
                     + mean.centered.discountRate
                     + mean.centered.days.to.next.quarter + mean.centered.log.full.amount
                     + cluster(TID),
                     data=t.long.df[t.long.df$mean.centered.log.Supplier.revenue <= 2 & t.long.df$mean.centered.log.Supplier.revenue >= -2,],
                     method="efron")))

# Model 3: interaction effects
summary((interaction.outliers.1 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                     + as.factor(spend.year) 
                     + mean.centered.discountRate
                     + mean.centered.log.Supplier.revenue*mean.centered.days.to.next.quarter
                     + cluster(TID),
                     data=t.long.df[t.long.df$mean.centered.log.Supplier.revenue <= 2 & t.long.df$mean.centered.log.Supplier.revenue >= -2,],
                     method="efron")))

# Model 3: interaction effects
summary((interaction.outliers.2 <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                      + as.factor(spend.year) 
                                      + mean.centered.discountRate
                                      + mean.centered.log.full.amount*mean.centered.days.to.next.quarter
                                      + cluster(TID),
                                      data=t.long.df[t.long.df$mean.centered.log.Supplier.revenue <= 2 & t.long.df$mean.centered.log.Supplier.revenue >= -2,],
                                      method="efron")))

# Model 3: interaction effects
summary((interaction.outliers.all <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                      + as.factor(spend.year) 
                                      + mean.centered.discountRate
                                      + mean.centered.log.full.amount*mean.centered.days.to.next.quarter                     
                                      + mean.centered.log.Supplier.revenue*mean.centered.days.to.next.quarter
                                      + cluster(TID),
                                      data=t.long.df[t.long.df$mean.centered.log.Supplier.revenue <= 2 & t.long.df$mean.centered.log.Supplier.revenue >= -2,],
                                      method="efron")))


robustness.outliers <- list(controls.outliers,
                            main.effect.outliers,
                            interaction.outliers.1,
                            interaction.outliers.2,
                            interaction.outliers.all)

modelsummary(robustness.outliers, 
             estimate = "{estimate}{stars}",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

modelsummary(robustness.outliers, 
             estimate = "{estimate}{stars}",
             output = "latex",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

###
#
# FIXED EFFECT MODEL

# Model 1: only controls
summary((controls.fe <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                          + mean.centered.log.Supplier.revenue + as.factor(spend.year)
                          + mean.centered.log.full.amount
                          + mean.centered.discountRate
                          + cluster(SID),
                          data=t.long.df,
                          method="efron")))


# Model 2: controls + main effects
summary((main.effect.fe <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                             + mean.centered.log.Supplier.revenue + as.factor(spend.year) 
                             + mean.centered.log.full.amount 
                             + mean.centered.discountRate
                             + mean.centered.days.to.next.quarter
                             + cluster(SID),
                             data=t.long.df,
                             method="efron")))

# Model 3: interaction effects
summary((interaction.1.fe <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                               + as.factor(spend.year) 
                               + mean.centered.discountRate
                               + mean.centered.log.Supplier.revenue*mean.centered.days.to.next.quarter
                               + cluster(SID),
                               data=t.long.df,
                               method="efron")))

summary((interaction.2.fe <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                               + as.factor(spend.year) 
                               + mean.centered.discountRate
                               + mean.centered.log.full.amount*mean.centered.days.to.next.quarter
                               + cluster(SID),
                               data=t.long.df,
                               method="efron")))


summary((interaction.all.fe <-coxph(Surv(start,stop,event) ~ Supplier.industry 
                                 + as.factor(spend.year) 
                                 + mean.centered.discountRate
                                 + mean.centered.log.Supplier.revenue*mean.centered.days.to.next.quarter
                                 + mean.centered.log.full.amount*mean.centered.days.to.next.quarter
                                 + cluster(SID),
                                 data=t.long.df,
                                 method="efron")))

fixed.effects <- list(controls.fe,
                      main.effect.fe,
                      interaction.1.fe,
                      interaction.2.fe,
                      interaction.all.fe)

modelsummary(fixed.effects, 
             estimate = "{estimate}{stars}",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")

modelsummary(fixed.effects, 
             estimate = "{estimate}{stars}",
             output = "latex",
             gof_omit = "R2 Marg.|R2 Cond|BIC|ICC|RMSE")




