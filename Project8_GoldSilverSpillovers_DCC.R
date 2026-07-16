# ============================================================
# Project 8: Volatility Spillovers — COMEX to PMEX
# Gold & Silver Markets (US to Pakistan)
# Author: Ahmer | GitHub: ahmer-econ | Date: July 2026
# ============================================================

# Install packages (only runs if not already installed)
if (!require(quantmod)) install.packages("quantmod")
if (!require(rugarch)) install.packages("rugarch")
if (!require(rmgarch)) install.packages("rmgarch")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(FinTS)) install.packages("FinTS")
if (!require(tseries)) install.packages("tseries")

# Load packages
library(quantmod)
library(rugarch)
library(rmgarch)
library(tidyverse)
library(FinTS)
library(tseries)
# ============================================================
# STEP 2: Download Price Data from Yahoo Finance
# ============================================================

# Set date range
start_date <- "2015-01-02"
end_date   <- "2024-12-31"

# Download raw price series
getSymbols("GC=F",  src = "yahoo", from = start_date, to = end_date)
# ============================================================
# STEP 3: Merge All Series and Handle Missing Values
# ============================================================

# Merge all four series into one xts object
prices <- merge(gold_usd, silv_usd, gold_pkr, silv_pkr)

# Rename columns cleanly
colnames(prices) <- c("Gold_USD", "Silver_USD", "Gold_PKR", "Silver_PKR")

# Remove any rows where ANY series has NA
prices <- na.omit(prices)

# Check dimensions and date range
cat("Observations after cleaning:", nrow(prices), "\n")
cat("Date range:", as.character(index(prices)[1]), 
    "to", as.character(index(prices)[nrow(prices)]), "\n")

# Preview
head(prices)
tail(prices)
getSymbols("SI=F",  src = "yahoo", from = start_date, to = end_date)
getSymbols("PKR=X", src = "yahoo", from = start_date, to = end_date)

# Extract adjusted closing prices
gold_usd <- Ad(`GC=F`)
silv_usd <- Ad(`SI=F`)
usdpkr   <- Ad(`PKR=X`)

# Construct Pakistani prices (USD price × exchange rate)
gold_pkr <- gold_usd * usdpkr
silv_pkr <- silv_usd * usdpkr

# Preview
head(gold_usd)
head(gold_pkr)
# ============================================================
# STEP 3: Merge All Series and Handle Missing Values
# ============================================================

# Merge all four series into one xts object
prices <- merge(gold_usd, silv_usd, gold_pkr, silv_pkr)

# Rename columns cleanly
colnames(prices) <- c("Gold_USD", "Silver_USD", "Gold_PKR", "Silver_PKR")

# Remove any rows where ANY series has NA
prices <- na.omit(prices)

# Check dimensions and date range
cat("Observations after cleaning:", nrow(prices), "\n")
cat("Date range:", as.character(index(prices)[1]), 
    "to", as.character(index(prices)[nrow(prices)]), "\n")

# Preview
head(prices)
tail(prices)
# ============================================================
# STEP 4: Compute Log Returns
# ============================================================

returns <- diff(log(prices)) * 100  # multiply by 100 for percentage returns
returns <- na.omit(returns)

# Check
cat("Return observations:", nrow(returns), "\n")
cat("Date range:", as.character(index(returns)[1]),
    "to", as.character(index(returns)[nrow(returns)]), "\n")

# Summary statistics
summary(returns)
# ============================================================
# STEP 5: Diagnose Extreme Values in PKR Returns
# ============================================================

# Find dates where Gold_PKR return exceeds +/- 50%
extreme_dates <- index(returns)[abs(returns$Gold_PKR) > 50]
cat("Dates with extreme Gold_PKR returns:\n")
print(extreme_dates)

# Show the actual return values on those dates
print(returns[extreme_dates, ])

# Also check the raw PKR exchange rate on those dates
print(usdpkr[extreme_dates, ])
# ============================================================
# STEP 6: Remove Bad Data Points and Recheck
# ============================================================

# Remove the two corrupted dates from prices object
bad_dates <- as.Date(c("2015-05-04", "2015-05-05"))
prices_clean <- prices[!index(prices) %in% bad_dates, ]

# Recompute returns on clean prices
returns <- diff(log(prices_clean)) * 100
returns <- na.omit(returns)

# Verify
cat("Clean return observations:", nrow(returns), "\n")
cat("Date range:", as.character(index(returns)[1]),
    "to", as.character(index(returns)[nrow(returns)]), "\n")
# ============================================================
# STEP 8: ADF Unit Root Tests on Returns
# ============================================================

adf_results <- sapply(as.data.frame(returns), function(x) {
  test <- adf.test(x)
  c(ADF_Statistic = round(test$statistic, 4),
    p_value       = round(test$p.value, 4))
})

print(adf_results)
# ============================================================
# STEP 9: ARCH Effects Test (Engle's LM Test)
# ============================================================

arch_results <- sapply(as.data.frame(returns), function(x) {
  test <- ArchTest(x, lags = 10)
  c(Chi_Square = round(test$statistic, 4),
    p_value    = round(test$p.value, 4))
})

print(arch_results)
# ============================================================
# STEP 10: Univariate GARCH(1,1) Specifications
# ============================================================

# Define GARCH(1,1) specification with Student-t errors
garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"  # Student-t for fat tails
)

# Fit to each series
fit_gold_usd  <- ugarchfit(garch_spec, data = returns$Gold_USD)
fit_silv_usd  <- ugarchfit(garch_spec, data = returns$Silver_USD)
fit_gold_pkr  <- ugarchfit(garch_spec, data = returns$Gold_PKR)
fit_silv_pkr  <- ugarchfit(garch_spec, data = returns$Silver_PKR)

# Quick summary of each
cat("=== Gold USD ===\n");  print(fit_gold_usd@fit$matcoef)
cat("=== Silver USD ===\n"); print(fit_silv_usd@fit$matcoef)
cat("=== Gold PKR ===\n");  print(fit_gold_pkr@fit$matcoef)
cat("=== Silver PKR ===\n"); print(fit_silv_pkr@fit$matcoef)

summary(returns)
# ============================================================
# STEP 7: Descriptive Statistics
# ============================================================

library(moments)  # for skewness and kurtosis

desc_stats <- function(x) {
  c(Mean     = mean(x),
    Std.Dev  = sd(x),
    Min      = min(x),
    Max      = max(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x))
}

stats_table <- sapply(as.data.frame(returns), desc_stats)
print(round(stats_table, 4))
install.packages("moments")
library(moments)

desc_stats <- function(x) {
  c(Mean     = mean(x),
    Std.Dev  = sd(x),
    Min      = min(x),
    Max      = max(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x))
}

stats_table <- sapply(as.data.frame(returns), desc_stats)
print(round(stats_table, 4))
install.packages("moments")
library(moments)

desc_stats <- function(x) {
  c(Mean     = mean(x),
    Std.Dev  = sd(x),
    Min      = min(x),
    Max      = max(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x))
}

stats_table <- sapply(as.data.frame(returns), desc_stats)
print(round(stats_table, 4))
# ============================================================
# STEP 7: Descriptive Statistics
# ============================================================

library(moments)  # for skewness and kurtosis

desc_stats <- function(x) {
  c(Mean     = mean(x),
    Std.Dev  = sd(x),
    Min      = min(x),
    Max      = max(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x))
}

stats_table <- sapply(as.data.frame(returns), desc_stats)
print(round(stats_table, 4))
# ============================================================
# STEP 7: Descriptive Statistics
# ============================================================

library(moments)  # for skewness and kurtosis

desc_stats <- function(x) {
  c(Mean     = mean(x),
    Std.Dev  = sd(x),
    Min      = min(x),
    Max      = max(x),
    Skewness = skewness(x),
    Kurtosis = kurtosis(x))
}

stats_table <- sapply(as.data.frame(returns), desc_stats)
print(round(stats_table, 4))
# ============================================================
# STEP 11: DCC-GARCH Estimation
# ============================================================

# Stack the four univariate specs into a multivariate spec
dcc_spec <- dccspec(
  uspec = multispec(replicate(4, garch_spec)),
  dccOrder = c(1, 1),
  distribution = "mvt"  # multivariate Student-t
)

# Fit DCC model
dcc_fit <- dccfit(dcc_spec, data = returns)

# Print results
print(dcc_fit)
# ============================================================
# STEP 12: Extract Dynamic Conditional Correlations
# ============================================================

# Extract time-varying correlation matrices
dcc_corr <- rcor(dcc_fit)

# The output is a 4x4xT array
# Extract the six unique pairs
dates <- index(returns)

cor_gold_usd_gold_pkr  <- dcc_corr[1, 3, ]  # COMEX Gold vs Pakistan Gold
cor_gold_usd_silv_pkr  <- dcc_corr[1, 4, ]  # COMEX Gold vs Pakistan Silver
cor_silv_usd_gold_pkr  <- dcc_corr[2, 3, ]  # COMEX Silver vs Pakistan Gold
cor_silv_usd_silv_pkr  <- dcc_corr[2, 4, ]  # COMEX Silver vs Pakistan Silver
cor_gold_usd_silv_usd  <- dcc_corr[1, 2, ]  # COMEX Gold vs COMEX Silver
cor_gold_pkr_silv_pkr  <- dcc_corr[3, 4, ]  # Pakistan Gold vs Pakistan Silver

# Summary of mean correlations
cat("Mean Dynamic Correlations:\n")
cat("COMEX Gold - Pakistan Gold:   ", round(mean(cor_gold_usd_gold_pkr), 4), "\n")
cat("COMEX Gold - Pakistan Silver: ", round(mean(cor_gold_usd_silv_pkr), 4), "\n")
cat("COMEX Silver - Pakistan Gold: ", round(mean(cor_silv_usd_gold_pkr), 4), "\n")
cat("COMEX Silver - Pakistan Silver:", round(mean(cor_silv_usd_silv_pkr), 4), "\n")
cat("COMEX Gold - COMEX Silver:    ", round(mean(cor_gold_usd_silv_usd), 4), "\n")
cat("Pakistan Gold - Pakistan Silver:", round(mean(cor_gold_pkr_silv_pkr), 4), "\n")
# ============================================================
# STEP 13: Plot Dynamic Conditional Correlations
# ============================================================

par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))

plot(dates, cor_gold_usd_gold_pkr, type = "l", col = "darkgoldenrod",
     main = "COMEX Gold vs Pakistan Gold", ylab = "DCC", xlab = "")
abline(h = mean(cor_gold_usd_gold_pkr), col = "red", lty = 2)

plot(dates, cor_silv_usd_silv_pkr, type = "l", col = "steelblue",
     main = "COMEX Silver vs Pakistan Silver", ylab = "DCC", xlab = "")
abline(h = mean(cor_silv_usd_silv_pkr), col = "red", lty = 2)

plot(dates, cor_gold_usd_silv_usd, type = "l", col = "darkgreen",
     main = "COMEX Gold vs COMEX Silver", ylab = "DCC", xlab = "")
abline(h = mean(cor_gold_usd_silv_usd), col = "red", lty = 2)

plot(dates, cor_gold_pkr_silv_pkr, type = "l", col = "purple",
     main = "Pakistan Gold vs Pakistan Silver", ylab = "DCC", xlab = "")
abline(h = mean(cor_gold_pkr_silv_pkr), col = "red", lty = 2)

par(mfrow = c(1, 1))
# ============================================================
# STEP 14: Save DCC Correlation Plot
# ============================================================

png("D:/Documents/APPLIED ECONOMETRICS WORK/Gold Silver Spillovers/DCC_Correlations.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))

plot(dates, cor_gold_usd_gold_pkr, type = "l", col = "darkgoldenrod",
     main = "COMEX Gold vs Pakistan Gold", ylab = "DCC", xlab = "")
abline(h = mean(cor_gold_usd_gold_pkr), col = "red", lty = 2)

plot(dates, cor_silv_usd_silv_pkr, type = "l", col = "steelblue",
     main = "COMEX Silver vs Pakistan Silver", ylab = "DCC", xlab = "")
abline(h = mean(cor_silv_usd_silv_pkr), col = "red", lty = 2)

plot(dates, cor_gold_usd_silv_usd, type = "l", col = "darkgreen",
     main = "COMEX Gold vs COMEX Silver", ylab = "DCC", xlab = "")
abline(h = mean(cor_gold_usd_silv_usd), col = "red", lty = 2)

plot(dates, cor_gold_pkr_silv_pkr, type = "l", col = "purple",
     main = "Pakistan Gold vs Pakistan Silver", ylab = "DCC", xlab = "")
abline(h = mean(cor_gold_pkr_silv_pkr), col = "red", lty = 2)

par(mfrow = c(1, 1))
dev.off()

cat("Plot saved.\n")
# ============================================================
# STEP 15: Extract Conditional Volatilities
# ============================================================

# Extract time-varying conditional variances (sigma) from DCC fit
vol <- sigma(dcc_fit)
colnames(vol) <- c("Gold_USD", "Silver_USD", "Gold_PKR", "Silver_PKR")

# Summary of mean volatilities
cat("Mean Conditional Volatilities (%):\n")
print(round(colMeans(vol), 4))

# Plot all four volatility series
png("D:/Documents/APPLIED ECONOMETRICS WORK/Gold Silver Spillovers/Conditional_Volatilities.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))

plot(dates, vol[, "Gold_USD"], type = "l", col = "darkgoldenrod",
     main = "COMEX Gold Volatility", ylab = "Conditional SD (%)", xlab = "")

plot(dates, vol[, "Silver_USD"], type = "l", col = "steelblue",
     main = "COMEX Silver Volatility", ylab = "Conditional SD (%)", xlab = "")

plot(dates, vol[, "Gold_PKR"], type = "l", col = "darkgreen",
     main = "Pakistan Gold Volatility", ylab = "Conditional SD (%)", xlab = "")

plot(dates, vol[, "Silver_PKR"], type = "l", col = "purple",
     main = "Pakistan Silver Volatility", ylab = "Conditional SD (%)", xlab = "")

par(mfrow = c(1, 1))
dev.off()

cat("Volatility plot saved.\n")
print(round(colMeans(vol), 4))
# ============================================================
# STEP 16: Granger Causality Tests on Squared Returns
# ============================================================

library(lmtest)

sq_returns <- returns^2  # squared returns as volatility proxy

# US Gold → Pakistan Gold
gc1 <- grangertest(sq_returns$Gold_PKR ~ sq_returns$Gold_USD, order = 5)
cat("COMEX Gold → Pakistan Gold:\n"); print(gc1)

# Pakistan Gold → US Gold (reverse)
gc2 <- grangertest(sq_returns$Gold_USD ~ sq_returns$Gold_PKR, order = 5)
cat("Pakistan Gold → COMEX Gold:\n"); print(gc2)

# US Silver → Pakistan Silver
gc3 <- grangertest(sq_returns$Silver_PKR ~ sq_returns$Silver_USD, order = 5)
cat("COMEX Silver → Pakistan Silver:\n"); print(gc3)

# Pakistan Silver → US Silver (reverse)
gc4 <- grangertest(sq_returns$Silver_USD ~ sq_returns$Silver_PKR, order = 5)
cat("Pakistan Silver → COMEX Silver:\n"); print(gc4)
# ============================================================
# END OF ESTIMATION
# Script: Project8_GoldSilverSpillovers_DCC.R
# All outputs saved to Gold Silver Spillovers folder
# ============================================================