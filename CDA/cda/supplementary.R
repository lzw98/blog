## categorical data

# binomial
dbinom(0,10,0.2)

#cdf with given parameters(cumulative density function)
n <- 10
prob_matrix <- sapply(c(0.2,0.5,0.8), function(p) pbinom(0:n,n,p))
#sapply(x,fun,options)
#where x is our dataframe(or matrix) and fun is an arbitrary function,typical functions contains mean,sd
dimnames(prob_matrix) <- list(0:n,c("P=0.2","P=0.5","P=0.8"))
xtable::xtable(prob_matrix,align = "cccc",digits = 3)


