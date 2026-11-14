## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6.5,
  fig.height = 4.2,
  fig.pos = "H",
  dpi = 120
)
library(CEACT)

## ----data---------------------------------------------------------------------
data("trial_cea")
trial <- trial_cea

head(trial)

## ----observed-----------------------------------------------------------------
observed <- cea(cost + qaly ~ group, data = trial, ref = "control")
summary(observed)

## ----bootstrap----------------------------------------------------------------
set.seed(42)
boot_res <- booticer(cost + qaly ~ group, data = trial, ref = "control",
                      R = 1000, ci.type = "perc")
summary(boot_res)

## ----plane, fig.cap="Cost-effectiveness plane from stratified non-parametric bootstrap replicates. The red line is the willingness-to-pay threshold."----
plotceplane(boot_res, k = 20000)

## ----ceac-table---------------------------------------------------------------
ceac_tbl <- computenmbceac(
  boot_res,
  wtp_range = seq(0, 50000, 5000)
)

ceac_tbl

## ----ceac-plot, fig.cap="Cost-effectiveness acceptability curve. The curve gives the bootstrap probability that treatment is cost-effective at each willingness-to-pay threshold."----
plotceac(ceac_tbl)

## ----dsa----------------------------------------------------------------------
dsa_effect <- dsaicer(
  cost + qaly ~ group,
  data = trial,
  param = "qaly",
  range = seq(0.50, 0.70, 0.025),
  ref = "control",
  metric = "INMB",
  k = 20000
)

dsa_effect

## ----one-way-dsa-plot, fig.cap="One-way deterministic sensitivity analysis varying treatment-arm effect.", fig.height=3.6----
plotdsa(dsa_effect, metric = "INMB")

