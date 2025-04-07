library(fpp3)
library(here)
library(stargazer)
library(dplyr)
library(readr)
library(lubridate)
library(readxl)
library(GGally)

here()

dow_chem       <- read_csv(here("./data/Dow Jones Chemicals Historical Data.csv"))
chemical_trade <- read_excel(here("./data/SeriesReport-202504071000-V.xlsx"), sheet = "CIDR", range = "A8:B405")

# Convert the datetime columns to date
chemical_trade$Date <- yearmonth(chemical_trade$Period, format="%m%Y")
dow_chem$Date <- yearmonth(dow_chem$Date, format="%m%d%Y")

# rescale columns
chemical_trade$Value <- chemical_trade$Value / 1000

dow_chem <- dow_chem |> mutate(
  Price = Price / 100,
  Open  = Open  / 100,
  High  = High  / 100,
  Low   = Low   / 100,
  Vol   = as.numeric(substr(`Vol.`, 1, 6)) / 100
) |> select(
  Date, Price, Open, High, Low, Vol
)

# Filter date range
chemical_trade <- chemical_trade %>%
  filter(Date >= yearmonth("2006 Jan") & Date <= yearmonth("2012 Dec"))

# add indicator for Great Recession period
chemical_trade <- chemical_trade |> mutate(
  great_recession = ifelse(Date >= yearmonth("2007 Dec") & Date <= yearmonth("2009 Jun"), 1, 0)
)

# Join datasets together
chemical_trade <- chemical_trade |> 
  left_join(dow_chem, by = "Date")

# Convert data to tsibble
data <- as_tsibble(chemical_trade, index = Date)

data <- data |> mutate(
  diff = difference(Value, lag = 1),
  diff_price_dow = difference(Price, lag = 1),
  diff_open_dow  = difference(Open, lag = 1),
  diff_high_dow  = difference(High, lag = 1),
  diff_low_dow   = difference(Low, lag = 1),
  diff_vol_dow   = difference(Vol, lag = 1),
)

# Split the data 
train <- data |> slice(1:72)
test  <- data |> slice(73:84)

# Create variable train_test which is "train" for observations 1:48 and "test" else
data <- data |> mutate(
  train_test = if_else(
    row_number() <= 72
    , if_else(data$great_recession == 1, "train_great_recession", "train")
    , "test")
)

# Plot over time, colored by train_test
ggplot(data, aes(x = Date, y = Value)) +
  geom_line(color="grey70") +
  geom_point(size=3, aes(color = train_test)) +
  labs(title = "U.S. Chemicals and Allied Products Wholesale Trade",
       y = "Sales and Inventories ($ billions)",
       x = "",
       caption = "Source: U.S. Census Bureau.") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
  ) +
  guides(colour = guide_legend(title = ""))
ggsave(here("./analysis/output/graphs/train_test.png"), width = 8, height = 5)

# First diff plot over time, colored by train_test
ggplot(data, aes(x = Date, y = diff)) +
  geom_line(color="grey70") +
  geom_point(size=3, aes(color = train_test)) +
  labs(title = "U.S. Chemicals and Allied Products Wholesale Trade", subtitle = "First Difference",
       y = "Change in Sales and Inventories ($ billions)",
       x = "",
       caption = "Source: U.S. Census Bureau.") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
  ) +
  guides(colour = guide_legend(title = ""))
ggsave(here("./analysis/output/graphs/train_test_diff.png"), width = 8, height = 5)


# ACF 
data |> ACF(Value) |>
  autoplot()
data |> ACF(diff) |>
  autoplot()

# Finding best ARIMAX model
arima_test <- train |>
  model(
    arimax0  = ARIMA(Value, ic = "aic"),
    arimax01 = ARIMA(Value, ic = "aic"),
    arimax1  = ARIMA(Value ~ great_recession + Vol, ic = "aic"),
    arimax2  = ARIMA(Value ~ great_recession + Vol + Price, ic = "aic"),
    arimax3  = ARIMA(Value ~ great_recession + Vol + Price + Open, ic = "aic"),
    arimax4  = ARIMA(Value ~ great_recession + Vol + Price + Open + High, ic = "aic"),
    arimax5  = ARIMA(Value ~ great_recession + Vol + Price + Open + High + Low, ic = "aic"),
    arimax6  = ARIMA(Value ~ great_recession + Vol + lag(Vol, 1), ic = "aic"),
    arimax7  = ARIMA(Value ~ great_recession + Vol + lag(Vol, 1) + lag(Vol, 2), ic = "aic"),
    arimax8  = ARIMA(Value ~ great_recession + Vol + lag(Vol, 1) + lag(Vol, 2) + lag(Vol, 3), ic = "aic"),
    arimax9  = ARIMA(Value ~ great_recession + Vol + lag(Vol, 1) + lag(Vol, 2) + lag(Vol, 3)+ lag(Vol, 4), ic = "aic"),
  )
glance(arima_test)
# arimax9 has the lowest AIC, but its AICc is the same as simpler models.
# For simplicity, I use arimax1.

arima_test |> select(arimax1) |> report() 
# ARIMA(4,0,0)(0,0,1)

# Fit models
my_models <- train |>
  model(
    ets    = ETS(Value ~ trend("A") + season("A"), ic = "aic"),
    arima  = ARIMA(Value, ic = "aic"),
    arimax = ARIMA(Value ~ great_recession + Vol, ic = "aic")
    ) |>
  mutate(ensemble = (ets+arima+arimax)/3)

my_forecasts <- my_models |>
  forecast(new_data = test)


# Plot the model predictions on the testing data
my_forecasts |>
  autoplot(test, level = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
  ) + 
  labs(
    y = "Sales and Inventories ($ billions)",
    title = "U.S. Chemicals and Allied Products Wholesale Trade",
    x="",
    caption = "Source: U.S. Census Bureau.") +
  guides(colour = guide_legend(title = ""))
ggsave(here("./analysis/output/graphs/forecasts.png"), width = 8, height = 5)


# IS and OOS accuracy metrics
is_accuracy  <- my_models |> accuracy()
oos_accuracy <- accuracy(my_forecasts, test)
combined_accuracy <- rbind(is_accuracy, oos_accuracy) |> arrange(desc(.type), RMSE)

d <- as.matrix(mutate_if(
  combined_accuracy, is.numeric, ~round(., 2)
))
stargazer(d, out = here("./analysis/output/graphs/Accuracy Table.tex"), type = "latex")

stargazer(d, type = "text")





# Fit models (diff)
train2 <- train |> filter(Date > yearmonth("2006 Jan"))

my_models <- train2 |>
  model(
    ets    = ETS(diff ~ trend("A") + season("A"), ic = "aic"),
    arima  = ARIMA(diff, ic = "aic"),
    arimax = ARIMA(diff ~ great_recession + Vol, ic = "aic")
  ) |>
  mutate(ensemble = (ets+arima+arimax)/3)

my_forecasts <- my_models |>
  forecast(new_data = test)


# Plot the model predictions on the testing data
my_forecasts |>
  autoplot(test, level = NULL) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1)
  ) + 
  labs(
    y = "Change in Sales and Inventories ($ billions)",
    title = "U.S. Chemicals and Allied Products Wholesale Trade", subtitle= "First Difference",
    x="",
    caption = "Source: U.S. Census Bureau.") +
  guides(colour = guide_legend(title = ""))
ggsave(here("./analysis/output/graphs/forecasts_diff.png"), width = 8, height = 5)


# IS and OOS accuracy metrics
is_accuracy  <- my_models |> accuracy()
oos_accuracy <- accuracy(my_forecasts, test)
combined_accuracy <- rbind(is_accuracy, oos_accuracy) |> arrange(desc(.type), RMSE)

d <- as.matrix(mutate_if(
  combined_accuracy, is.numeric, ~round(., 2)
))
stargazer(d, out = here("./analysis/output/graphs/Accuracy Table_diff.tex"), type = "latex")

stargazer(d, type = "text")

