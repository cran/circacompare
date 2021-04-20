## ---- include=F---------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5,
  fig.height = 3.5
)
set.seed(42)


## ---- message=F, warning=F----------------------------------------------------
library(circacompare)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(42)
data_single <- make_data(k1=0, alpha1=0, phi1=0, noise_sd=1)[c("time", "measure")]
head(data_single)

## -----------------------------------------------------------------------------
set.seed(42)
data_grouped <- make_data(phi1=6, noise_sd=1)
head(data_grouped)
tail(data_grouped)

## -----------------------------------------------------------------------------
result <- circa_single(x=data_single, col_time="time", col_outcome="measure", period=24)

result

## -----------------------------------------------------------------------------
result2 <- circacompare(x=data_grouped, col_time="time", col_group="group", col_outcome="measure")

result2

## -----------------------------------------------------------------------------
set.seed(42)
phi1_in <- 3
mixed_data <- function(n){
  counter <- 1
  for(i in 1:n){
    x <- make_data(k1=0, alpha1=5, phi1=rnorm(1, phi1_in, 1), hours=72, noise_sd = 2)
    x$id <- counter
    counter <- counter + 1
    if(i==1){res <- x}else{res <- rbind(res, x)}
  }
  return(res)
}
df <- mixed_data(20)
out <- circacompare_mixed(
  x = df,
  col_time = "time",
  col_group = "group",
  col_outcome = "measure",
  col_id = "id",
  control=list(grouped_params=c("alpha", "phi"), random_params=c("phi1")),
  period=24
)

ggplot(data=df[df$id %in% c(1:6),], aes(time, measure)) + 
  geom_point(aes(col=group)) + 
  geom_smooth(aes(group=interaction(as.factor(id), group)), span=0.3)+
  facet_wrap(~id)


## -----------------------------------------------------------------------------
out$plot

summary <- as.data.frame(circacompare:::extract_model_coefs(out$fit))
summary$`95% CI ll` <- summary$estimate - summary$std_error*1.96
summary$`95% CI ul` <- summary$estimate + summary$std_error*1.96

summary

## -----------------------------------------------------------------------------
set.seed(42)
tau_in <- runif(1, 8, 20)
alpha_decay1_in <- runif(1, 0.02, 0.05)

df <- make_data(k1=0, alpha1=10, phi1=0, seed=42, hours=120, noise_sd=2)
df$time <- df$time/24*tau_in

# Note that when decay is estimated, it is on a scale of time in hours. There is no relation decay rate and the period.
df$measure[df$group=="g2"] <- df$measure[df$group=="g2"]*exp(-alpha_decay1_in*df$time[df$group=="g2"])

out_alpha_decay <-
  circacompare(x=df, "time", "group", "measure", period=NA,
  control=list(
    main_params=c("k", "alpha", "phi", "tau"),
    decay_params=c("alpha"),
    grouped_params=c("alpha", "alpha_decay")
  ))

out_alpha_decay$plot

summary <- as.data.frame(circacompare:::extract_model_coefs(out_alpha_decay$fit))
summary$`95% CI ll` <- summary$estimate - summary$std_error*1.96
summary$`95% CI ul` <- summary$estimate + summary$std_error*1.96
summary$p_value <- NULL
summary


## -----------------------------------------------------------------------------
cat("Real period: ", tau_in, "\n",
    "Real alpha_decay: ", alpha_decay1_in, sep="")

