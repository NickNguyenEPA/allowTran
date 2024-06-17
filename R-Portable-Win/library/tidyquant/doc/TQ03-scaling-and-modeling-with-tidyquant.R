## ---- echo = FALSE, message = FALSE, warning = FALSE--------------------------
knitr::opts_chunk$set(message = FALSE,
                      warning = FALSE,
                      fig.width = 8, 
                      fig.height = 4.5,
                      fig.align = 'center',
                      out.width='95%', 
                      dpi = 200)
# devtools::load_all() # Travis CI fails on load_all()

## -----------------------------------------------------------------------------
# Loads tidyquant, lubridate, xts, quantmod, TTR, and PerformanceAnalytics
library(tidyverse)
library(tidyquant)  

## -----------------------------------------------------------------------------
c("AAPL", "GOOG", "FB") %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")

## -----------------------------------------------------------------------------
stock_list <- tibble(stocks = c("AAPL", "JPM", "CVX"),
                     industry = c("Technology", "Financial", "Energy"))
stock_list

## -----------------------------------------------------------------------------
stock_list %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")

## -----------------------------------------------------------------------------
tq_index("DOW")

## ---- eval=FALSE--------------------------------------------------------------
#  tq_exchange("NYSE")

## -----------------------------------------------------------------------------
tq_index("DOW") %>%
    slice(1:3) %>%
    tq_get(get = "stock.prices")

## -----------------------------------------------------------------------------
data("FANG")

FANG

## -----------------------------------------------------------------------------
FANG_returns_yearly <- FANG %>%
    group_by(symbol) %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 period     = "yearly", 
                 col_rename = "yearly.returns") 

## -----------------------------------------------------------------------------
FANG_returns_yearly %>%
    ggplot(aes(x = year(date), y = yearly.returns, fill = symbol)) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(title = "FANG: Annual Returns", 
         subtitle = "Mutating at scale is quick and easy!",
         y = "Returns", x = "", color = "") +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() +
    theme_tq() +
    scale_fill_tq()

## -----------------------------------------------------------------------------
AAPL <- tq_get("AAPL", from = "2007-01-01", to = "2016-12-31")
AAPL

## -----------------------------------------------------------------------------
get_annual_returns <- function(stock.returns) {
    stock.returns %>%
        tq_transmute(select     = adjusted, 
                     mutate_fun = periodReturn, 
                     type       = "log", 
                     period     = "yearly")
}

## -----------------------------------------------------------------------------
AAPL_annual_log_returns <- get_annual_returns(AAPL)
AAPL_annual_log_returns

## -----------------------------------------------------------------------------
AAPL_annual_log_returns %>%
    ggplot(aes(x = year(date), y = yearly.returns)) + 
    geom_hline(yintercept = 0, color = palette_light()[[1]]) +
    geom_point(size = 2, color = palette_light()[[3]]) +
    geom_line(size = 1, color = palette_light()[[3]]) + 
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "AAPL: Visualizing Trends in Annual Returns",
         x = "", y = "Annual Returns", color = "") +
    theme_tq()

## -----------------------------------------------------------------------------
mod <- lm(yearly.returns ~ year(date), data = AAPL_annual_log_returns)
mod

## -----------------------------------------------------------------------------
library(broom)
tidy(mod)

## -----------------------------------------------------------------------------
get_model <- function(stock_data) {
    annual_returns <- get_annual_returns(stock_data)
    mod <- lm(yearly.returns ~ year(date), data = annual_returns)
    tidy(mod)
}

## -----------------------------------------------------------------------------
get_model(AAPL)

## -----------------------------------------------------------------------------
set.seed(10)
stocks_tbl <- tq_index("SP500") %>%
    sample_n(5) 
stocks_tbl

## -----------------------------------------------------------------------------
stocks_model_stats <- stocks_tbl %>%
    select(symbol, company) %>%
    tq_get(from = "2007-01-01", to = "2016-12-31") %>%
    
    # Nest 
    group_by(symbol, company) %>%
    nest() %>%
    
    # Apply the get_model() function to the new "nested" data column
    mutate(model = map(data, get_model)) %>%
    
    # Unnest and collect slope
    unnest(model) %>%
    filter(term == "year(date)") %>%
    arrange(desc(estimate)) %>%
    select(-term)

stocks_model_stats

## -----------------------------------------------------------------------------
tq_get("XYZ", "stock.prices")

## ---- warning = TRUE----------------------------------------------------------
c("AAPL", "GOOG", "BAD APPLE") %>%
    tq_get(get = "stock.prices", complete_cases = TRUE)

## ---- warning = TRUE----------------------------------------------------------
c("AAPL", "GOOG", "BAD APPLE") %>%
    tq_get(get = "stock.prices", complete_cases = FALSE)

