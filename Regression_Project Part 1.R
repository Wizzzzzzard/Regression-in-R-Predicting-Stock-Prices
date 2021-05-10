### Rerun everything with Year factored back in

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(leaps)

# load the London Stock Exchange data set
load("C:/Users/elija/OneDrive - University of Strathclyde/Data Analytics in R/Regression Project/project_data.RData")

# print first rows of the London Stock Exchange data
head(lse)

# check for N/A or Inf values
apply(lse, 2, function(x) any(is.na(x) | is.infinite(x)))

# plot time chart of stock prices to check for any trends
plot(BA~Date, data=lse)

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

correlations = c(cor(lse$BA,lse$STJ),cor(lse$BA,lse$RB),cor(lse$BA,lse$MIN),
                 cor(lse$BA,lse$PRU),cor(lse$BA,lse$CCH),cor(lse$BA,lse$EXPN),
                 cor(lse$BA,lse$VOD),cor(lse$BA,lse$GVC),cor(lse$BA,lse$AHT),
                 cor(lse$BA,lse$CPG),cor(lse$BA,lse$CCL),cor(lse$BA,lse$RR),
                 cor(lse$BA,lse$DLG),cor(lse$BA,lse$TUI),cor(lse$BA,lse$LLOY),
                 cor(lse$BA,lse$RMV),cor(lse$BA,lse$SSE),cor(lse$BA,lse$SDR),
                 cor(lse$BA,lse$SMT),cor(lse$BA,lse$EZJ),cor(lse$BA,lse$NMC),
                 cor(lse$BA,lse$BATS),cor(lse$BA,lse$SPX),cor(lse$BA,lse$TSCO),
                 cor(lse$BA,lse$CNA),cor(lse$BA,lse$RTO),cor(lse$BA,lse$ANTO),
                 cor(lse$BA,lse$Year),cor(lse$BA,lse$Month))

coefficients <- data.frame(Base_Variable = rep("BA", 29),
           Comparison_Variable = variables,
           R = correlations,
           R_squared = correlations**2)

coefficients %>%
  ggplot() +
  geom_col(aes(Comparison_Variable,R,fill=Comparison_Variable)) +
  ylab("R") +
  scale_fill_discrete(name="Company Name") +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank())

# Difficult to interpret as some values strongly positive and some strongly negative
# So plotted R^2 instead

coefficients %>%
  ggplot() +
  geom_col(aes(Comparison_Variable,R_squared,fill=Comparison_Variable)) +
  ylab("R-squared") +
  scale_fill_discrete(name="Company Name") +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank())

## (ii)
# Find the 5 companies whose stock prices have the strongest correlation (in absolute value) with
# BAE System's

coefficients %>%
  arrange(desc(R_squared)) -> coefficients

head(coefficients,6)
# Top 5 most correlated companies are STJ, CNA, EZJ, RMV and VOD
# i.e St James's Place plc, Centrica plc, EasyJet plc, Rightmove plc and Vodafone Group plc

## b(i)
# Fit a linear regression for BAE Systems's stock price based on the five companies found in (a)(ii)
top5 = subset(lse, select = c(BA,STJ,CNA,EZJ,RMV,VOD))

pairs(top5)

top5 %>%
  pivot_longer(cols=c("STJ","CNA","EZJ", "RMV", "VOD"), names_to="Company_Name", values_to="Closing_Prices") %>%
  ggplot(aes(Company_Name,Closing_Prices)) +
  geom_boxplot()

top5_mod <- lm(BA~STJ+CNA+EZJ+RMV+VOD, data=top5)

ggplot(top5, aes(BA, STJ)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="BA Closing Prices", y="STJ Closing Prices",
       title="Plot of BA Closing Prices against STJ Closing Prices") -> p1
ggplot(top5, aes(BA, CNA)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="BA Closing Prices", y="CNA Closing Prices",
       title="Plot of BA Closing Prices against CNA Closing Prices") -> p2
ggplot(top5, aes(BA, EZJ)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="BA Closing Prices", y="EZJ Closing Prices",
       title="Plot of BA Closing Prices against EZJ Closing Prices") -> p3
ggplot(top5, aes(BA, RMV)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="BA Closing Prices", y="RMV Closing Prices",
       title="Plot of BA Closing Prices against RMV Closing Prices") -> p4
ggplot(top5, aes(BA, VOD)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="BA Closing Prices", y="VOD Closing Prices",
       title="Plot of BA Closing Prices against VOD Closing Prices") -> p5

(p1+p2)/(p3+p4+p5)

# Write down the ANOVA table for this linear regression

summary(top5_mod)

# Calculate degrees of freedom
dfR <- nrow(top5) - length(coef(top5_mod)) #dfT - dfM (n)
dfM <- length(coef(top5_mod))-1 # n - p - 1
dfT <- nrow(top5) - 1 # n-1

# Calculate sum of squares
RSS <- sum(resid(top5_mod)^2) #SSresidual
TSS <- sum((top5$BA - mean(top5$BA))^2) #SStotal
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

## (ii)
# Comment on the goodness of fit of the model
qf(0.95, 5, 1001)
pf(F, 5, 1001, lower.tail=FALSE)

# The observed F (389.518) is large in comparison to the critical value obtained from qf()(2.223)
# and the p-value is very small (6.804e-232) so can reject H0, which indicates that there is a linear association
# between BA's closing prices and at least one of STJ's, CNA's, EZJ's, RMV's and VOD's closing prices.

## (iii)
# Provide an interpretation of the model coefficients

# The model is as follows:
#   BA = β0 + β1(STJ) + β2(CNA) + β3(EZJ) + β4(RMV) + β5(VOD) + errors
#   BA = 562.877 + 52.718(STJ) + 8.021(CNA) - 16.019(EZJ) - 5.81(RMV) - 20.039(VOD) + errors

# Null Hypothesis (H0) the coefficients are all equal to 0
# Alternative Hypothesis (Hi) at least one coefficient is not equal to 0

summary(top5_mod)

top5names <- c("STJ","CNA","EZJ","RMV","VOD")
top5coeffs <- c(52.718,8.021,-16.019,-5.810,-20.039)

# Hypothesis test of the slope for STJ at the 5% significance level
# The p-value for STJ is shown as < 2e-16. This is less than 0.05 hence can reject
# the null hypothesis and conclude that there is a linear association between STJ and
# BA closing prices that is significant at the 5% level

# Hypothesis test of the slope for CNA at the 1% significance level
# The p-value for CNA is shown as 0.0104. This is less than 0.05 hence can reject
# the null hypothesis and conclude that there is a linear association between CNA and
# BA closing prices that is significant at the 5% level

# Hypothesis test of the slope for EZJ at the 1% significance level
# The p-value for EZJ is shown as < 2e-16. This is less than 0.05 hence can reject
# the null hypothesis and conclude that there is a linear association between EZJ and
# BA closing prices that is significant at the 5% level

# Hypothesis test of the slope for RMV at the 1% significance level
# The p-value for RMV is shown as 0.0256. This is less than 0.05 hence can reject
# the null hypothesis and conclude that there is a linear association between RMV and
# BA closing prices that is significant at the 5% level

# Hypothesis test of the slope for VOD at the 1% significance level
# The p-value for VOD is shown as < 4.62e-13. This is less than 0.05 hence can reject
# the null hypothesis and conclude that there is a linear association between VOD and
# BA closing prices that is significant at the 5% level

confint.lm(top5_mod)

# This indicates that the average increase in BA closing prices is highly likely to be between 46.170615
# and 59.2648403 for every increase of one in the closing price of STJ. (Least significant)

# This indicates that the average increase in BA closing prices is highly likely to be between 1.892567
# and 14.1493742 for every increase of one in the closing price of CNA.(2nd most significant)

# This indicates that the average decrease in BA closing prices is highly likely to be between -19.740284
# and -12.2975838 for every increase of one in the closing price of EZJ.

# This indicates that the average decrease in BA closing prices is highly likely to be between -10.911287
# and -0.7095529 for every increase of one in the closing price of RMV. (Most significant)

# This indicates that the average decrease in BA closing prices is highly likely to be between -25.400837 
# and -14.6767780 for every increase of one in the closing price of VOD. (2nd most significant)

# Carrying out a confidence interval check for the value of BA closing prices based on values from
# the first column of the top5 table to see if it passes the sanity check

# conf int: on average what would a tree with height x and girth y have as a volume?
# Takes into account sample variation

predict(top5_mod,data.frame(STJ = (-0.966),CNA=(2.44),EZJ=(2.17),RMV=(2.00),VOD=(1.59)),interval="confidence")

# This gave a value of 453.2791 with an UL of 459.0606 and LL of 447.2791, which the actual value of 456.7 fits
# comfortably inside. This is a nice tight interval too, with a +/- of a little over 1% of the fitted variable

predict(top5_mod,data.frame(STJ = (-0.966),CNA=(2.44),EZJ=(2.17),RMV=(2.00),VOD=(1.59)),interval="prediction")

# Using the prediction interval the model was also able to roughly fit the true value fairly closely, however the 
# prediction interval is much larger, with an interval of +/- over 70 (or about 15% of the fitted variable)
 
# This suggests that the model is capable of predicting the value of closing prices for a given sample quite accurately.
# However when trying to predict the closing price for a specific day it is far less useful

## (iv)
# How do the estimated coefficients compare to the correlations from part (a)(i)? Investigate any
# differences and describe the potential issues with this model.

top5corrs <- c(cor(top5$BA,top5$STJ),cor(top5$BA,top5$CNA),cor(top5$BA,top5$EZJ),cor(top5$BA,top5$RMV),cor(top5$BA,top5$VOD))

data.frame("Name" = top5names,
           "Correlations" = top5corrs,
           "Coefficients" = top5coeffs)

# In terms of correlations STJ,CNA,EZJ,RMV and VOD all have roughly equal effect on the closing price of BA, either in a positive
# or negative direction, with the magnitude between the highest and lowest correlation having a difference of only 0.043. However
# When we then compare this to the coefficients, three of the stocks have a much higher effect on the calculated BA closing price
# with STJ having a whopping coefficient of 52.718, which is over twice the magnitude of the second highest coefficient, -20.039
# for VOD closing prices. This is also a strange comparison, as VOD had the lowest correlation with BA yet it has the second largest
# effect on the calculated BA price. These factors, plus the strangely diminished effect the second most correlated value has
# on the BA equation suggest that there are other variables effecting the data that are not present in the model in it's current
# form

plot(top5_mod)

# From the Residuals vs Fitted Values chart there seems to be an equal spread of variables,
# which suggests that the model does have close to constant error variance

# The QQ plot suggests that the model is skewed in one direction (i.e the errors are not normally distributed),
# as both the upper and lower tails fall below the line - this could possibly be reduced by using a 
# transformation to negate the outliers seen in the Residuals vs Fitted values chart

## (d)
# Using an appropriate variable selection technique and any transformations of the independent variables,
# build an improved model for BAE Systems’s daily closing price.

# Begin by using leaps and bounds to whittle down the number of models to choose from
# Leaps and Bounds semi-auto model selection

summary(lse.lm)

best_subset <- leaps(x=lse[,2:30],y=lse$BA,
                     nbest=5,method="adjr2",
                     names=variables)

best_subset <- data.frame(Size= best_subset$size, AdjR2 = round(best_subset$adjr2, 3),
           best_subset$which, row.names = NULL)

plot(best_subset$Size, best_subset$AdjR2,
     ylab = "Adjusted R-squared",
     xlab = "Number of variables (including intercept)")

# Pick Best from 19, 20, 21, as mostly leveled off after this
best_subset %>% filter(Size==19)
# Best = STJ, RB, MIN, PRU, CCH, EXPN, VOD, GVC, AHT, CPG, TUI, SSE, SDR, NMC, TSCO, CNA, RTO, Year
size14 <- lm(BA ~ STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+TUI+SSE+SDR+NMC+TSCO+CNA+RTO+Year, data=lse)

best_subset %>% filter(Size==20)
# Best = STJ, RB, MIN, PRU, CCH, EXPN, VOD, GVC, AHT, CPG, DLG, SSE, SDR, SMT, NMC, TSCO, CNA, RTO, Year
size15 <- lm(BA ~ STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+DLG+SSE+SDR+SMT+NMC+TSCO+CNA+RTO+Year, data=lse)

best_subset %>% filter(Size==21)
# Best = STJ, RB, MIN, PRU, CCH, EXPN, VOD, GVC, AHT, CPG, DLG, RMV, SSE, SDR, SMT, NMC, TSCO, CNA, RTO, Year
size16 <- lm(BA ~ STJ+RB+MIN+PRU+CCH+EXPN+VOD+GVC+AHT+CPG+DLG+RMV+SSE+SDR+SMT+NMC+TSCO+CNA+RTO+Year, data=lse)
