predict_BAE <- function(lse, newdata){
  # Carry out any transformations prior to fitting you model
  # Add transformed variables to both lse and newdata. E.g.:
  lse$BA.log <- log(lse$BA)
  newdata$BA.log <- log(newdata$BA)
  
  # this is the part that fits your linear model
  BAE.lm <- lm(sqrt(BA) ~ STJ + DLG + BATS + VOD + RTO + GVC + CPG + SPX + SSE + PRU
            + AHT + RB + MIN + RMV + Year + EXPN + TSCO + SDR + CCH + NMC
            + SMT + TUI + LLOY + CNA + Month, data = lse)
  
  # this is the part that produces predictions using your linear model
  predictions <- predict(BAE.lm, newdata = newdata)
  predictions <- predictions**2
  return(predictions)
}