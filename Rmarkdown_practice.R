

MonteCarlo <- function(n, t)
{ # function MonteCarlo
  normalVariables <- rnorm(n, 0, 1)
  signalVector <- pmax(sign(normalVariables - t), rep(0, n))
  countLargerThanT <- sum(signalVector)
  return((n - countLargerThanT) / n)
} # end function MonteCarlo

RealValue <- function(t)
{ # function RealValue
  return(pnorm(t, 0, 1))
} # end function RealValue