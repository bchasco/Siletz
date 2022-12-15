#Indexes
minY <- 1998
maxY <- 2021
n_life_stages <- 3

#Life history parameters are:
#egg survival 
e_surv <- 0.9
#juvenile survival
j_surv <- 0.9
#adult survival
a_surv <- 0.9

#process errors/ hiearchical parameters
e_proc <- 2
j_proc <- 2
a_proc <- 2

#Observation error
e_obs <- 0.1
j_obs <- 0.1
a_obs <- 0.1


#Life history models
f_eggs <- function(adults, e_surv){
  return(adults*e_surv)
}

f_juv <- function(eggs,j_suv){
  return(eggs*j_surv)
}

f_adults <- function(juv,a_surv){
  return(juv * a_surv)
}

#The are the conditions you need to progagate the model
init_cond <- c(eggs = 1e6, juv  = 1e6*j_surv, adults = 1e6*j_surv*a_surv) 

#This is where you will store all of the observations
obs <- data.frame(t(init_cond))
proc <- data.frame(t(init_cond))

surv <- data.frame(t(c(e_surv = e_surv,
                     j_surv = j_surv,
                     a_surv = a_surv)))


for(y in 2:(length(minY:maxY)+1)){
  
  #Work through the life history
  surv[y,'e_surv'] <- plogis(qlogis(e_surv) + rnorm(1,0,e_proc)) 
  #This is an example of a year effect for egg survival
  #Where the logit of the survival is increase ~10% a year
  # surv[y,'e_surv'] <- plogis(qlogis(e_surv) * rnorm(1,y*0.1,e_proc)) 
  surv[y,'j_surv'] <- plogis(qlogis(j_surv) + rnorm(1,0,j_proc))
  surv[y,'a_surv'] <- plogis(qlogis(a_surv) + rnorm(1,0,a_proc))
    
  #The true process that actually exists in nature
  proc[y,'eggs'] <- f_eggs(obs[y-1,'adults'] , surv[y,'e_surv'])
  proc[y,'juv'] <- f_juv(obs[y-1,'eggs'] , surv[y,'j_surv'])
  proc[y,'adults'] <- f_juv(obs[y-1,'juv'] , surv[y,'a_surv'])

  #This is what you are observing with some observation (log-normal) error
  obs[y,'eggs'] <- proc[y,'eggs'] * exp(rnorm(1,0-e_obs^2/2,e_obs))
  obs[y,'juv'] <- proc[y,'juv'] * exp(rnorm(1,0-j_obs^2/2,j_obs))
  obs[y,'adults'] <- proc[y,'adults']  * exp(rnorm(1,0-a_obs^2/2,a_obs))
  
}


library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Read data
## The data generation code is in bpa-code.txt, available at
## http://www.vogelwarte.ch/de/projekte/publikationen/bpa/complete-code-and-data-files-of-the-book.html
# stan_data <- read_rdump("GLMM_Poisson.data.R")
stan_data <- list( n = nrow(obs),
                   y = obs[,1])
## Initial values
inits <- function() list(theta_log = runif(1, log(1e5), log(1e6)),
                         sigma_log = runif(1, 5, 6))

## Parameters monitored
params <- c("theta_log", "sigma_log")

# MCMC settings
ni <- 3000
nt <- 10
nb <- 1000
nc <- 4

## Call Stan from R
out <- stan("./code/siletz_model.stan", data = stan_data,
            init = inits, pars = params,
            chains = nc, iter = ni, warmup = nb, thin = nt,
            seed = 1,
            open_progress = FALSE)


