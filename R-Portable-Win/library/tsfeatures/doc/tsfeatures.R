## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  fig.align = "center"
)
library(tsfeatures)

## ----cran-installation, eval = FALSE------------------------------------------
#  install.packages("tsfeatures")

## ----gh-installation, eval = FALSE--------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("robjhyndman/tsfeatures")

## -----------------------------------------------------------------------------
mylist <- list(sunspot.year, WWWusage, AirPassengers, USAccDeaths)
tsfeatures(mylist)

## -----------------------------------------------------------------------------
# Function from outside of tsfeatures package being used
is.monthly <- function(x){
  frequency(x) == 12
}
tsfeatures(mylist, features = "is.monthly")

## -----------------------------------------------------------------------------
acf_features(AirPassengers)

## -----------------------------------------------------------------------------
arch_stat(AirPassengers)

## -----------------------------------------------------------------------------
autocorr_features(AirPassengers)

## -----------------------------------------------------------------------------
str(binarize_mean(AirPassengers))

## -----------------------------------------------------------------------------
comp <- compengine(AirPassengers)
knitr::kable(comp)

## -----------------------------------------------------------------------------
crossing_points(AirPassengers)

## -----------------------------------------------------------------------------
dist_features(AirPassengers)

## -----------------------------------------------------------------------------
entropy(AirPassengers)

## -----------------------------------------------------------------------------
firstzero_ac(AirPassengers)

## -----------------------------------------------------------------------------
flat_spots(AirPassengers)

## -----------------------------------------------------------------------------
heterogeneity(AirPassengers)

## -----------------------------------------------------------------------------
holt_parameters(AirPassengers)
hw_parameters(AirPassengers)

## -----------------------------------------------------------------------------
hurst(AirPassengers)

## -----------------------------------------------------------------------------
stability(AirPassengers)
lumpiness(AirPassengers)

## -----------------------------------------------------------------------------
max_level_shift(AirPassengers)
max_var_shift(AirPassengers)
max_kl_shift(AirPassengers)

## -----------------------------------------------------------------------------
nonlinearity(AirPassengers)

## -----------------------------------------------------------------------------
pacf_features(AirPassengers)

## -----------------------------------------------------------------------------
pred_features(AirPassengers)

## -----------------------------------------------------------------------------
sampenc(AirPassengers, M = 5, r = 0.3)

## -----------------------------------------------------------------------------
scal_features(AirPassengers)

## -----------------------------------------------------------------------------
station_features(AirPassengers)

## -----------------------------------------------------------------------------
stl_features(AirPassengers)

## -----------------------------------------------------------------------------
unitroot_kpss(AirPassengers)
unitroot_pp(AirPassengers)

## -----------------------------------------------------------------------------
zero_proportion(AirPassengers)

## ----yahoo, message=FALSE-----------------------------------------------------
library(tsfeatures)
library(dplyr)

yahoo <- yahoo_data()

## ----hwl, eval=FALSE----------------------------------------------------------
#  hwl <- bind_cols(
#           tsfeatures(yahoo,
#             c("acf_features","entropy","lumpiness",
#               "flat_spots","crossing_points")),
#           tsfeatures(yahoo,"stl_features", s.window='periodic', robust=TRUE),
#           tsfeatures(yahoo, "max_kl_shift", width=48),
#           tsfeatures(yahoo,
#             c("mean","var"), scale=FALSE, na.rm=TRUE),
#           tsfeatures(yahoo,
#             c("max_level_shift","max_var_shift"), trim=TRUE)) %>%
#    select(mean, var, x_acf1, trend, linearity, curvature,
#           seasonal_strength, peak, trough,
#           entropy, lumpiness, spike, max_level_shift, max_var_shift, flat_spots,
#           crossing_points, max_kl_shift, time_kl_shift)

## ----hwlsave, eval=FALSE, echo=FALSE------------------------------------------
#  # Now store the computed results for later use
#  save(hwl, file="../extra-data/hwl.rda")

## ----hwlquick, include=FALSE--------------------------------------------------
# This replicates the above but uses pre-stored data to speed things up
tmp <- tempfile()
utils::download.file("https://github.com/robjhyndman/tsfeatures/raw/master/extra-data/hwl.rda", tmp)
load(tmp)

## ----yahoographics------------------------------------------------------------
# 2-d Feature space
library(ggplot2)
hwl_pca <- hwl %>%
  na.omit() %>%
  prcomp(scale=TRUE)
hwl_pca$x %>%
  as_tibble() %>%
  ggplot(aes(x=PC1, y=PC2)) +
    geom_point()

## ----ijf2017, message=FALSE---------------------------------------------------
library(tsfeatures)
library(dplyr)
library(tidyr)
library(forecast)

M3data <- purrr::map(Mcomp::M3,
  function(x) {
      tspx <- tsp(x$x)
      ts(c(x$x,x$xx), start=tspx[1], frequency=tspx[3])
  })
khs_stl <- function(x,...) {
  lambda <- BoxCox.lambda(x, lower=0, upper=1, method='loglik')
  y <- BoxCox(x, lambda)
  c(stl_features(y, s.window='periodic', robust=TRUE, ...), lambda=lambda)
}

## ----khs, eval=FALSE----------------------------------------------------------
#  khs <- bind_cols(
#    tsfeatures(M3data, c("frequency", "entropy")),
#    tsfeatures(M3data, "khs_stl", scale=FALSE)) %>%
#    select(frequency, entropy, trend, seasonal_strength, e_acf1, lambda) %>%
#    replace_na(list(seasonal_strength=0)) %>%
#    rename(
#      Frequency = frequency,
#      Entropy = entropy,
#      Trend = trend,
#      Season = seasonal_strength,
#      ACF1 = e_acf1,
#      Lambda = lambda) %>%
#    mutate(Period = as.factor(Frequency))

## ----khssave, eval=FALSE, echo=FALSE------------------------------------------
#  # Now store the computed results for later use
#  save(khs, file="../extra-data/khs.rda")

## ----khsquick, include=FALSE--------------------------------------------------
# This replicates the above but uses pre-stored data to speed things up
tmp <- tempfile()
utils::download.file("https://github.com/robjhyndman/tsfeatures/raw/master/extra-data/khs.rda", tmp)
load(tmp)

## ----ijf2017graphs, message=FALSE---------------------------------------------
# Fig 1 of paper
khs %>%
  select(Period, Entropy, Trend, Season, ACF1, Lambda) %>%
  GGally::ggpairs()

# 2-d Feature space (Top of Fig 2)
khs_pca <- khs %>%
  select(-Period) %>%
  prcomp(scale=TRUE)
khs_pca$x %>%
  as_tibble() %>%
  bind_cols(Period=khs$Period) %>%
  ggplot(aes(x=PC1, y=PC2)) +
    geom_point(aes(col=Period))

