predict_BAE <- function(lse, newdata){
  # Carry out any transformations prior to fitting you model
  # Add transformed variables to both lse and newdata. E.g.:
  
  # this is the part that fits your linear model
  BAE.lm <- lm(log(BA) ~ STJ + DLG + BATS + RTO + CPG + PRU
               + RMV + TSCO + LLOY + CNA, data = lse)
  
  # this is the part that produces predictions using your linear model
  predictions <- predict(BAE.lm, newdata = newdata)
  predictions <- exp(predictions)
  return(predictions)
}