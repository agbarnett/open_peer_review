# vif_matrix.R
# calculate VIF for glm where the x-variables are a matrix
# code adapted from car::vif
# September 2025

vif_matrix = function(in_model){
  V = vcov(in_model)
  V = V[-1,-1] # remove intercept
  terms = names(in_model$coefficients)[-1] # no intercept
  terms = str_remove(terms, '^x_selected')
  n.terms = length(terms)
  R <- cov2cor(V)
  detR <- det(R) # determinant
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:n.terms) {
      result[term, 1] <- det(as.matrix(R[term, term])) * det(as.matrix(R[-term, -term]))/detR
      result[term, 2] <- length(term)
  }
  if (all(result[, 2] == 1)) 
        result <- result[, 1]
       else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  return(result)
}

# checking the matrix version
check = FALSE
if(check == TRUE){
  library(car)
  vif(lm(prestige ~ income + education, data=Duncan))
  # now check with matrix version
  y = Duncan$prestige
  x = select(Duncan, income, education)%>% as.matrix()
  mmodel = glm(y ~ x)
  vif_matrix(mmodel)
}

