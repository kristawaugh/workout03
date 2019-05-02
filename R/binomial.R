library(devtools)

# 1.1 Private Checker Functions... 

# private auxillary function to test if an input prob is a valid probability value
check_prob <- function(prob){
  if(prob > 0 || prob > 1){
    return(TRUE)
  }
  else{
    stop("invalid prob value, must be between 0 and 1")
  }
}

# private auxillary function to test if an input trials is a valid value for number of trials
check_trials <- function(trials){
  if(trials > 0 & trials == round(trials)){
    return(TRUE)
  }
  else{
    stop("invalid trials value")
  }
}

# private auxillary function to test if an input success is a valid value for number of successes
check_success <- function(success, trials)
  for (i in success) 
    if (i < trials || i > 0) {
      return(TRUE)
    }
  else{
    stop("invalid success value")
}

# 1.2 Private Auxillary Functions 

# private auxillary function to compute mean of a binomial distribution 
aux_mean <- function(trials, prob){
  mean <- trials * prob
  return(mean)
}

# private auxillary function to compute variance of a binomial distribution
aux_variance <- function(trials, prob){
  variance <- trials * prob * (1 - prob)
  return(variance)
}

# private auxillary function to compute mode of a binomial distribution
aux_mode <- function(trials, prob) {
  mode <- floor(trials * prob + prob)
  return(mode)
}

# private auxillary function to compute skewness of a binomial distribution
aux_skewness <- function(trials, prob){
  skewness <- (1 - 2 * prob) / sqrt(trials * prob * (1 - prob))
  return(skewness)
}

# private auxillary function to compute kurtosis of a binomial distribution
aux_kurtosis <- function(trials, prob){
  kurtosis <-  (1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob))
  return(kurtosis)
}

# 1.3 Function bin_choose()
#' @title  bin_choose()
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n the number of trials
#' @param k the number of successes
#' @return the number of combinations
#' @export


bin_choose <- function(n,k) {
  if (n > k) {
    choose <- factorial(n) / (factorial(k) * factorial(n - k))
    return(choose)
}
 else
stop("k cannot be greater than n")
}

#1.4 Function bin_probability
#' @title  bin_probability()
#' @description calculates the probability of k successes in n trials, given the probability of success 
#' @param n the number of trials
#' @param k the number of successes
#' @argument check_trials()
#' @argument check_prob()
#' @argument check_success()
#' @return the number of combinations
#' @export

bin_probability <- function(trials, prob, success){
    if (check_trials(trials) == FALSE) {
      stop("invalid trials value")
    }
    else if (check_prob(prob) == FALSE) {
      stop("invalid probability value")
    }
    else if (check_success(success, trials) == FALSE) {
      stop("invalid success value")
    }
    else {
      probability <- bin_choose (trials, success) * (prob ^ success) * (1 - prob) ^ (trials - success)
      return(probability)
    }
bin_probability(5, .5, 2)
}

#1.5 Function bin_distribution 

bin_distribution <- function(trials, prob)
  if(bin_probability(trials, prob) == FALSE) {
    stop("incorrect")
  }
  else
    return(data.frame, c("bindis", "data.frame"))
}
