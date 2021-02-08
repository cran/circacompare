## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5,
  fig.height = 3.5
)
library(circacompare)
set.seed(42)
data_single <- make_data(k1=0, alpha1=0, phi1=0)[c("time", "measure")]
data_grouped <- make_data(phi1=6)

## -----------------------------------------------------------------------------
head(data_single)

## -----------------------------------------------------------------------------
head(data_grouped)
tail(data_grouped)

## -----------------------------------------------------------------------------
result <- circa_single(x=data_single, col_time="time", col_outcome="measure", period=24)

result

## -----------------------------------------------------------------------------
result2 <- circacompare(x=data_grouped, col_time="time", col_group="group", col_outcome="measure")

result2

## -----------------------------------------------------------------------------
nls_model <- result2[[3]]

confint(nls_model)

