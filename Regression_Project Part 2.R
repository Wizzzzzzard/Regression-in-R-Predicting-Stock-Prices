library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(leaps)
library(MASS)


# load the London Stock Exchange data set
load("C:/Users/elija/OneDrive - University of Strathclyde/Data Analytics in R/Regression Project/project_data.RData")

# print first rows of the London Stock Exchange data
head(lse)

## a(i)
# Calculate the sample correlation coefficient between the closing price of each stock and that of
# BAE Systems and use a single plot to summarise these.

lse = subset(lse, select = -c(Date,Weekday))

lse <- lse[c("BA","STJ","RB", "MIN", "PRU", "CCH", "EXPN", "VOD", "GVC", "AHT", "CPG", "CCL", "RR",
             "DLG", "TUI", "LLOY", "RMV", "SSE", "SDR", "SMT", "EZJ", "NMC", "BATS", "SPX", "TSCO",
             "CNA", "RTO", "ANTO","Year","Month")]

lse.lm <- lm(BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
             + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
             + CNA + RTO + ANTO + Year + Month, data = lse)
summary(lse.lm)

variables = c("STJ","RB", "MIN", "PRU", "CCH", "EXPN", "VOD", "GVC", "AHT", "CPG", "CCL", "RR",
              "DLG", "TUI", "LLOY", "RMV", "SSE", "SDR", "SMT", "EZJ", "NMC", "BATS", "SPX", "TSCO",
              "CNA", "RTO", "ANTO","Year","Month")

best_subset <- leaps(x=lse[,2:30],y=lse$BA,
                     nbest=10,method="adjr2",
                     names=variables)

best_subset <- data.frame(Size= best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
                          best_subset$which, row.names = NULL)

plot(best_subset$Size, best_subset$AdjR2,
     ylab = "Adjusted R-squared",
     xlab = "Number of variables (including intercept)")

# Pick Best from 19, 20, 21, as mostly leveled off after this
best_subset %>% filter(Size==19)
# Best = STJ, RB, MIN, PRU, CCH, EXPN, VOD, GVC, AHT, CPG, TUI, SSE, SDR, NMC, TSCO, CNA, RTO, Year
size19 <- lm(BA ~ STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+TUI+SSE+SDR+NMC+TSCO+CNA+RTO+Year, data=lse)

best_subset %>% filter(Size==20)
# Best = STJ, RB, MIN, PRU, CCH, EXPN, VOD, GVC, AHT, CPG, DLG, SSE, SDR, SMT, NMC, TSCO, CNA, RTO, Year
size20 <- lm(BA ~ STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+DLG+SSE+SDR+SMT+NMC+TSCO+CNA+RTO+Year, data=lse)

best_subset %>% filter(Size==21)
# Best = STJ, RB, MIN, PRU, CCH, EXPN, VOD, GVC, AHT, CPG, DLG, RMV, SSE, SDR, SMT, NMC, TSCO, CNA, RTO, Year
size21 <- lm(BA ~ STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+DLG+RMV+SSE+SDR+SMT+NMC+TSCO+CNA+RTO+Year, data=lse)

## Forward Testing
lse_0 <- lm(BA ~ 1, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# STJ has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# DLG has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# BATS has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# VOD has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# RTO has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# GVC has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# CPG has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# SPX has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# SSE has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# PRU has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# AHT has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# RB has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# MIN has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# RMV has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# Year has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# EXPN has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# TSCO has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# SDR has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# NMC has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# CCH has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC+CCH, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# SMT has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC+CCH+SMT, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# ANTO has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC+CCH+SMT+ANTO, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# TUI has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC+CCH+SMT+ANTO+TUI, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# CNA has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC+CCH+SMT+ANTO+TUI+CNA, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# LLOY has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC+CCH+SMT+ANTO+TUI+CNA+LLOY, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# Month has smallest p-value and largest F value so add to next step
lse_0 <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC+CCH+SMT+ANTO+TUI+CNA+LLOY+Month, data = lse)
add1(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year)
# Final model as none of the remaining variables have a p-value of less than 0.05
lse.forward <- lm(BA ~ STJ+DLG+BATS+VOD+RTO+GVC+CPG+SPX+SSE+PRU+AHT+RB+MIN+RMV+Year+EXPN+TSCO+SDR+NMC+CCH+SMT+ANTO+TUI+CNA+LLOY+Month, data = lse)


## Backwards Testing
lse_0 <- lm(BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
            + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
            + CNA + RTO + ANTO + Month + Year, data = lse)
drop1(lse_0, test = "F",
      scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
      + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
      + CNA + RTO + ANTO  + Month + Year)
# EZJ has largest p-value so drop
lse_0 <- lm(BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
            + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
            + CNA + RTO + ANTO + Month + Year, data = lse)
drop1(lse_0, test = "F",
      scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
      + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
      + CNA + RTO + ANTO  + Month + Year)
# CCL has largest p-value so drop
lse_0 <- lm(BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + RR
            + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
            + CNA + RTO + ANTO + Month + Year, data = lse)
drop1(lse_0, test = "F",
      scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + RR
      + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
      + CNA + RTO + ANTO  + Month + Year)
# RR has largest p-value so drop
lse_0 <- lm(BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG
            + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
            + CNA + RTO + ANTO + Month + Year, data = lse)
drop1(lse_0, test = "F",
      scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG
      + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
      + CNA + RTO + ANTO + Month + Year)
# ANTO has largest p-value so drop
lse_0 <- lm(BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG
            + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
            + CNA + RTO + Month + Year, data = lse)
drop1(lse_0, test = "F",
      scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG
      + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
      + CNA + RTO + Month + Year)
# Final model as all remaining variables have a p-value below 0.1 and dropping Year
# has no major effect on other variables significance 

lse.backward <- lm(BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG
                   + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + NMC + BATS + SPX + TSCO
                   + CNA + RTO + Month + Year, data = lse)

# Stepwise Selection
lse_0 <- lm(BA ~ 1, data = lse)
step(lse_0, test = "F",
     scope = ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
     + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
     + CNA + RTO + ANTO + Month + Year, direction="both")

lse.stepswise <- lm(BA ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + 
                      SPX + SSE + PRU + AHT + RB + MIN + RMV + Year + EXPN + TSCO + 
                      SDR + CCH + NMC + SMT + ANTO + TUI + LLOY + CNA + Month, data = lse)

# Select stepswise model as it maintains the Year with a lower p-value than backwards model
# and is also smaller than the forwards model, striking a good balance between the two

# Compare sizes 19, 20, 21 models and stepswise model
CP_PRESS <- function(model, sigma_full){
  res <- resid(model)
  hat_mod <- hatvalues(model)
  CP <- sum(res^2)/sigma_full + 2*length(coef(model)) - length(res)
  PRESS <- sum(res^2/(1-hat_mod)^2)
  list(Cp=CP, PRESS=PRESS)
}

full.mod <- lm(BA ~ STJ + RB + MIN + PRU + CCH + EXPN + VOD + GVC + AHT + CPG + CCL + RR
               + DLG + TUI + LLOY + RMV + SSE + SDR + SMT + EZJ + NMC + BATS + SPX + TSCO
               + CNA + RTO + ANTO + Year + Month, data = lse)
sigma_q <- summary(full.mod)$sigma^2

size19_stat <- CP_PRESS(size19, sigma_q)
size20_stat <- CP_PRESS(size20, sigma_q)
size21_stat <- CP_PRESS(size21, sigma_q)
step_stat <- CP_PRESS(lse.stepswise, sigma_q)
back_stat <- CP_PRESS(lse.backward, sigma_q)
front_stat <- CP_PRESS(lse.forward,sigma_q)

print(size19_stat)
print(size20_stat)
print(size21_stat)
print(step_stat)
print(back_stat)
print(front_stat)

adjr19 <- summary(size19)$adj.r.squared
adjr20 <- summary(size20)$adj.r.squared
adjr21 <- summary(size21)$adj.r.squared
adjrstep <- summary(lse.stepswise)$adj.r.squared
adjrback <- summary(lse.backward)$adj.r.squared
adjrfront <- summary(lse.forward)$adj.r.squared

data.frame(Name = c("Size 19","Size 20", "Size 21","Stepwise","Backwards","Forwards"),
           Adjr2 = c(adjr19,adjr20,adjr21,adjrstep,adjrback,adjrfront),
           Cp = c(size19_stat$Cp,size20_stat$Cp,size21_stat$Cp,step_stat$Cp,back_stat$Cp,front_stat$Cp),
           PRESS = c(size19_stat$PRESS,size20_stat$PRESS,size21_stat$PRESS,step_stat$PRESS,back_stat$PRESS,front_stat$PRESS))

summary(lse.forward)
summary(lse.stepswise)

# print the estimated coefficients
coef(lse.stepswise)

# store the fitted values from the full model
fit <- lse.stepswise$fitted.values
print(fit)

# Having fitted a model and extracted the fitted values,
# now produce scatter plots of the studentised residuals against the fitted values,
# and for each of the independent variables.

# this divides the plotting area into a 2 by 2 matrix of panels
par(mfrow = c(2, 2))
# Plot studentised residuals against fitted values.
plot(fit, rstudent(lse.stepswise))
title(main = "Residuals vs Fitted Values")
# Plot studentised residuals against STJ.
plot(lse$STJ, rstudent(lse.stepswise))
title(main = "Residuals vs STJ")
# Plot studentised residuals against DLG.
plot(lse$DLG, rstudent(lse.stepswise))
title(main = "Residuals vs DLG")
# Plot studentised residuals against BATS.
plot(lse$BATS, rstudent(lse.stepswise))
title(main = "Residuals vs BATS")

par(mfrow = c(2, 2))
# Plot studentised residuals against VOD
plot(lse$VOD, rstudent(lse.stepswise))
title(main = "Residuals vs VOD")
# Plot studentised residuals against RTO
plot(lse$RTO, rstudent(lse.stepswise))
title(main = "Residuals vs RTO")
# Plot studentised residuals against GVC
plot(lse$GVC, rstudent(lse.stepswise))
title(main = "Residuals vs GVC")
# Plot studentised residuals against CPG
plot(lse$CPG, rstudent(lse.stepswise))
title(main = "Residuals vs CPG")

par(mfrow = c(2, 2))
# Plot studentised residuals against SPX
plot(lse$SPX, rstudent(lse.stepswise))
title(main = "Residuals vs SPX")
# Plot studentised residuals against SSE
plot(lse$SSE, rstudent(lse.stepswise))
title(main = "Residuals vs SSE")
# Plot studentised residuals against PRU
plot(lse$PRU, rstudent(lse.stepswise))
title(main = "Residuals vs PRU")
# Plot studentised residuals against AHT
plot(lse$AHT, rstudent(lse.stepswise))
title(main = "Residuals vs AHT")

par(mfrow = c(2, 2))
# Plot studentised residuals against RB
plot(lse$RB, rstudent(lse.stepswise))
title(main = "Residuals vs RB")
# Plot studentised residuals against MIN
plot(lse$MIN, rstudent(lse.stepswise))
title(main = "Residuals vs MIN")
# Plot studentised residuals against RMV
plot(lse$RMV, rstudent(lse.stepswise))
title(main = "Residuals vs RMV")
# Plot studentised residuals against Year
plot(lse$Year, rstudent(lse.stepswise))
title(main = "Residuals vs Year")

par(mfrow = c(2, 2))
# Plot studentised residuals against EXPN
plot(lse$EXPN, rstudent(lse.stepswise))
title(main = "Residuals vs EXPN")
# Plot studentised residuals against TSCO
plot(lse$TSCO, rstudent(lse.stepswise))
title(main = "Residuals vs TSCO")
# Plot studentised residuals against SDR
plot(lse$SDR, rstudent(lse.stepswise))
title(main = "Residuals vs SDR")
# Plot studentised residuals against CCH
plot(lse$CCH, rstudent(lse.stepswise))
title(main = "Residuals vs CCH")

par(mfrow = c(2, 2))
# Plot studentised residuals against NMC
plot(lse$NMC, rstudent(lse.stepswise))
title(main = "Residuals vs NMC")
# Plot studentised residuals against SMT
plot(lse$SMT, rstudent(lse.stepswise))
title(main = "Residuals vs SMT")
# Plot studentised residuals against ANTO
plot(lse$ANTO, rstudent(lse.lm))
title(main = "Residuals vs ANTO")
# Plot studentised residuals against TUI
plot(lse$TUI, rstudent(lse.lm))
title(main = "Residuals vs TUI")

par(mfrow = c(2, 2))
# Plot studentised residuals against LLOY
plot(lse$LLOY, rstudent(lse.stepswise))
title(main = "Residuals vs LLOY")
# Plot studentised residuals against CNA
plot(lse$CNA, rstudent(lse.stepswise))
title(main = "Residuals vs CNA")
# Plot studentised residuals against Month
plot(lse$Month, rstudent(lse.stepswise))
title(main = "Residuals vs Month")


# Draw an overall Residuals against fitted values
par(mfrow = c(1, 1))
plot(lse.stepswise, which = 1)

# It's clear from these plots that although some of the variables have a constant error
# variance, there are a substantial amount of variables for which this is not true. However
# the overall model seems to have errors with close to constant variance.

# Independence of errors:
# if the residuals are independent, there should be no weaving/curved/nonrandom patterns visible in any of the scatterplots. 
# There does appear to be some unusual clustering and curving patterns in some of the residual plots, 
# which should be investigated further in order to confirm that the errors are independent

# Draw a QQ-plot
par(mfrow = c(1, 1))
plot(lse.stepswise, which = 2)

# The QQ plot suggest that the errors are either normally distributed with a few outliers,
# or that there is a slight left skew present. In order to check this a transformation of the
# data could be carried out and a new Q-Q plot produced afterwards

# Although the boxcox command will find a single, likelihood maximising power transformation,
# this may be contained within a wide confidence interval. However due to the extremely dense
# plots produced it is difficult to make out curvature in the data so this method is still
# worth trying

boxcox(BA ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SPX + SSE + PRU
       + AHT + RB + MIN + RMV + Year + EXPN + TSCO + SDR + CCH + NMC
       + SMT + ANTO + TUI + LLOY + CNA + Month, data = lse)

# 0.5 falls near the centre of the confidence interval therefore will try sqrt of data.

par(mfrow = c(1, 1))

# fit sqrt model
sqrt.mod1 <- lm(sqrt(BA) ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SPX + SSE + PRU
                + AHT + RB + MIN + RMV + Year + EXPN + TSCO + SDR + CCH + NMC
                + SMT + ANTO + TUI + LLOY + CNA + Month, data = lse)
# produce residual plots
plot(sqrt.mod1, which = c(1,2))
title(main = "Sqrt Model")

summary(sqrt.mod1)

# ANTO is no longer significant in sqrt model so will remove it and refit the model

# fit sqrt model
sqrt.mod1 <- lm(sqrt(BA) ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SPX + SSE + PRU
                + AHT + RB + MIN + RMV + Year + EXPN + TSCO + SDR + CCH + NMC
                + SMT + TUI + LLOY + CNA + Month, data = lse)
# produce residual plots
plot(sqrt.mod1, which = c(1,2))
title(main = "Sqrt Model")

summary(sqrt.mod1)

# These transformations seemed to have little effect on the model, suggesting either that
# there is a problem with the variables selected for the model or for skewness in the
# initial data itself. As the data is not based off a natural or random process, but rather from a human
# driven process (i.e. the Stock Exchange) with patterns behind it, it seems likely that it may
# be the latter. In this case it seems that a polynomial model would fit better than the current model
# to the problem. However this is beyond my ability at this time to implement

# I ended up changing my mind about this after doing some reading up on the subject and so decided to
# continue trying to improve the model

### Check multicollinearity and remove values with high relation to each other compared to
### the BA column

lse_new = subset(lse, select = c(BA, STJ, DLG, BATS, VOD, RTO, GVC, CPG, SPX, SSE, PRU, AHT,
                             RB, MIN, RMV, Year, EXPN, TSCO, SDR, CCH, NMC, SMT, TUI, LLOY, CNA, Month))

cor(lse_new$BA,lse_new$GVC)

# Flag any variables with 0.80 or greater correlation and test models without them

# VOD and SSE 0.897
# VOD and AHT -0.8297
# VOD and MIN 0.875
# VOD and Year -0.876
# VOD and NMC -0.814
# VOD and SMT -0.803
# VOD and CNA 0.871
# VOD and BA = -0.644

# RTO and SPX 0.842
# RTO and BA = 0.171

# GVC and AHT 0.828
# GVC and RMV -0.821
# GVC and Year 0.824
# GVC and NMC 0.853
# GVC and SMT 0.84
# GVC and BA = 0.577

# SPX and RTO = 0.842
# SPX and SMT = 0.854
# SPX and BA = 0.158

# SSE and VOD 0.897
# SSE and MIN 0.875
# SSE and Year 0.889
# SSE and NMC -0.867
# SSE and SMT -0.818
# SSE and CNA 0.940
# SSE and BA = -0.514

# PRU and SDR 0.844
# PRU and BA = 0.207

# AHT and MIN -0.812
# AHT and Year 0.818
# AHT and NMC 0.841
# AHT and SMT 0.854
# AHT and BA = 0.581

# MIN and VOD 0.875
# MIN and SSE 0.875
# MIN and AHT -0.812
# MIN and Year -0.866
# MIN and NMC -0.847
# MIN and SMT -0.803
# MIN and CNA 0.876
# MIN and BA = -0.516

# RMV and GVC -0.821
# RMV and Year -0.840
# RMV and CCH -0.806
# RMV and BA = -0.645

# Year and VOD -0.876 
# Year and GVC 0.824
# Year and SSE -0.889
# Year and AHT 0.818
# Year and MIN -0.866
# Year and RMV -0.840
# Year and CCH 0.842
# Year and NMC 0.895
# Year and SMT 0.830
# Year and CNA -0.921
# Year and BA = -0.645

# EXPN and SDR -0.800
# EXPN and BA = -0.061

# SDR and PRU 0.845
# SDR and EXPN -0.800
# SDR and BA = 0.134

# CCH and RMV -0.806
# CCH and Year 0.842
# CCH and NMC 0.870
# CCH and SMT 0.835
# CCH and CNA -0.819
# CCH and BA = 0.497

# NMC and VOD -0.814
# NMC and GVC 0.853
# NMC and SSE -0.867
# NMC and AHT 0.841
# NMC and MIN -0.847
# NMC and Year 0.895
# NMC and CCH 0.870
# NMC and SMT 0.937
# NMC and CNA -0.852
# NMC and BA = 0.414

# SMT and VOD -0.803
# SMT and GVC 0.840
# SMT and SPX 0.854
# SMT and SSE -0.818
# SMT and AHT 0.854
# SMT and MIN -0.803
# SMT and Year 0.830
# SMT and CCH 0.835
# SMT and NMC 0.937
# SMT and BA = 0.381

# CNA and VOD 0.871
# CNA and SSE 0.940
# CNA and MIN 0.876
# CNA and Year -0.921
# CNA and CCH -0.819
# CNA and NMC -0.852
# CNA and BA = -0.659

# Drop EXPN as lower than SDR
# DROP SPX as lower than RTO
# Drop SDR as lower than PRU

mod1 <- lm(BA ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SPX + SSE + PRU
            + AHT + RB + MIN + RMV + Year + EXPN + TSCO + SDR + CCH + NMC
            + SMT + TUI + LLOY + CNA + Month, data = lse)
mod2 <- lm(BA ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SSE + PRU
             + AHT + RB + MIN + RMV + Year + TSCO + CCH + NMC
             + SMT + TUI + LLOY + CNA + Month, data = lse)

summary(mod1)
summary(mod2)

# Removing some of the highly correlated values has reduced significance of other values
mod <- lm(BA ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SSE + PRU
           + AHT + RB + MIN + RMV + TSCO + CCH + NMC
           + SMT + TUI + LLOY + CNA + Month, data = lse)
summary(mod)

plot(mod, which = c(1,2), ask = F)

# QQ-plot has improved so will try a transformation of the data

boxcox(BA ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SSE + PRU
       + AHT + RB + MIN + RMV + TSCO + CCH + NMC
       + SMT + TUI + LLOY + CNA + Month, data = lse)

# 0 and 0.5 lie inside confidence interval so will try sqrt and log models

par(mfrow = c(2, 2))

# fit log model
log.mod <- lm(log(BA) ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SSE + PRU
               + AHT + RB + MIN + RMV + TSCO + CCH + NMC
               + SMT + TUI + LLOY + CNA + Month, data = lse)

# produce residual plots
plot(log.mod, which = c(1,2))
title(main = "Log Model")

# fit sqrt model
sqrt.mod <- lm(sqrt(BA) ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SSE + PRU
                + AHT + RB + MIN + RMV + TSCO + CCH + NMC
                + SMT + TUI + LLOY + CNA + Month, data = lse)

# produce residual plots
plot(sqrt.mod, which = c(1,2))
title(main = "Sqrt Model")

# SQRT model has improved normality but still skewed at one end

# Doublecheck for multicolinearity again

lse_new = subset(lse, select = c(BA, STJ, DLG, BATS, VOD, RTO, GVC, CPG, SSE, PRU, AHT,
                                 RB, MIN, RMV, TSCO, CCH, NMC, SMT, TUI, LLOY, CNA, Month))

cor(lse_new,lse_new$BA)

# High Corrs include VOD, GVC, SSE, AHT, MIN, CCH, NMC, SMT
# Corrs with BA: -0.644, 0.577, -0.514, 0.581, -0.516, 0.497, 0.414, 0.381

# Try remodelling without some more

# Removing some of the highly correlated values has reduced significance of other values
NMCmod <- lm(BA ~ STJ + DLG + BATS + RTO + CPG + PRU
          + RB + RMV + TSCO
          + TUI + LLOY + CNA + Month, data = lse)
summary(NMCmod)

plot(NMCmod, which = c(1,2), ask = F)

# QQ-plot has improved so will try a transformation of the data (also drop BATS)

boxcox(BA ~ STJ + DLG + BATS + RTO + CPG + PRU
       + RB + RMV + TSCO + TUI + LLOY + CNA + Month, data = lse)

# 0 lie inside confidence interval so will try log model

# fit log model
log.mod <- lm(log(BA) ~ STJ + DLG + BATS + RTO + CPG + PRU
              + RB + RMV + TSCO + TUI + LLOY + CNA + Month, data = lse)

# produce residual plots
plot(log.mod, which = c(1,2))
title(main = "Log Model")

summary(log.mod)

# Drop RB
log.mod <- lm(log(BA) ~ STJ + DLG + BATS + RTO + CPG + PRU
              + RMV + TSCO + TUI + LLOY + CNA + Month, data = lse)

# produce residual plots
plot(log.mod, which = c(1,2))
title(main = "Log Model")

summary(log.mod)

# Drop TUI
log.mod <- lm(log(BA) ~ STJ + DLG + BATS + RTO + CPG + PRU
              + RMV + TSCO + LLOY + CNA + Month, data = lse)

# produce residual plots
plot(log.mod)
title(main = "Log Model")

summary(log.mod)

plot(BA~Month, data=lse)
# Remove Month from model as seems to have no real correlation

plot(BA~DLG, data=lse)
# Increase power of x or y

plot(BA~CPG, data=lse)
# Decrease x, increase y - can decrease CPG by squaring as it's a decimal

lse$CPG1 <- (lse$CPG)**2

lm1 <- lm(log(BA) ~ STJ + DLG + BATS + RTO + CPG1 + PRU + RMV + TSCO + LLOY + CNA, data = lse)
lm2 <- lm(log(BA) ~ STJ + DLG + BATS + RTO + CPG + PRU + RMV + TSCO + LLOY + CNA, data = lse)

summary(lm1)
summary(lm2)

# residual assumption checking plots
par(mfrow = c(2, 2))
plot(lm1, which = c(1,2), ask = F)
plot(lm2, which = c(1,2), ask = F)

par(mfrow = c(2, 2))
plot(lm1$residuals ~ lse$CPG1)
plot(lm2$residuals ~ lse$CPG)

log.table <- subset(lse, select = c(BA,STJ,DLG,BATS,RTO,CPG,PRU,RMV,TSCO,LLOY,CNA))

log.mod <- lm(log(BA) ~ STJ + DLG + BATS + RTO + CPG + PRU
              + RMV + TSCO + LLOY + CNA, data = lse)


# Calculate degrees of freedom
dfR <- nrow(log.table) - length(coef(log.mod)) #dfT - dfM (n)
dfM <- length(coef(log.mod))-1 # n - p - 1
dfT <- nrow(log.table) - 1 # n-1

# Calculate sum of squares
RSS <- sum(resid(log.mod)^2) #SSresidual
TSS <- sum((log.table$BA - mean(log.table$BA))^2) #SStotal
MSS <- TSS - RSS #SSregression

# Calculate Mean Squares
MSR <- RSS/dfR #SSregression/p
MSM <- MSS/dfM #SSresidual/(n-p-1)

# Calculate F statistic
F <- MSM/MSR

# Create ANOVA Table
ANOVA <- data.frame("ID" = c("Regression", "Residual", "Total"),
                    "Degrees of Freedom" = c(dfM, dfR, dfT),
                    "Sum of Squares" = c(MSS, RSS, TSS),
                    "Mean Square" = c( MSM, MSR, ""),
                    "F Statistic" = c(F,"",""))

qf(0.95, 11,995)
pf(F, 11, 995, lower.tail=FALSE)

size19_stat <- CP_PRESS(size19, sigma_q)
size20_stat <- CP_PRESS(size20, sigma_q)
size21_stat <- CP_PRESS(size21, sigma_q)
step_stat <- CP_PRESS(lse.stepswise, sigma_q)
nt_sqrt_stat <- CP_PRESS(sqrt.mod1, sigma_q)
NMC_stat <- CP_PRESS(NMCmod, sigma_q)
NMC_log_stat <- CP_PRESS(log.mod, sigma_q)

print(size19_stat)
print(size20_stat)
print(size21_stat)
print(step_stat)
print(nt_sqrt_stat)
print(NMC_stat)
print(NMC_log_stat)

adjr19 <- summary(size19)$adj.r.squared
adjr20 <- summary(size20)$adj.r.squared
adjr21 <- summary(size21)$adj.r.squared
adjrsum <- summary(lse.stepswise)$adj.r.squared
adjrntss <- summary(sqrt.mod1)$adj.r.squared
adjrNMC <- summary(NMCmod)$adj.r.squared
adjr_logNMC <- summary(log.mod)$adj.r.squared

data.frame(Name = c("Size 19","Size 20", "Size 21","Stepwise","MC SQRT","NMC Model","NMC Log Model"),
           Adjr2 = c(adjr19,adjr20,adjr21,adjrsum,adjrntss,adjrNMC,adjr_logNMC),
           Cp = c(size19_stat$Cp,size20_stat$Cp,size21_stat$Cp,step_stat$Cp,nt_sqrt_stat$Cp,NMC_stat$Cp,NMC_log_stat$Cp),
           PRESS = c(size19_stat$PRESS,size20_stat$PRESS,size21_stat$PRESS,step_stat$PRESS,nt_sqrt_stat$PRESS,NMC_stat$PRESS,NMC_log_stat$PRESS))

# Attempted transformation of DLG and CPG but as there are some negative values these created errors
# so have discarded 