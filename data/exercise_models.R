

library (brms)
library (bmmb)
options (contrasts = c('contr.sum','contr.sum'))

data (exp_ex)

# create our dependent variables
exp_ex$Female = ifelse(exp_ex$G == 'f', 1, 0)

# new dependent variable
exp_ex$y = cbind(b = as.numeric(exp_ex$C=='b'),
                   g = as.numeric(exp_ex$C=='g'),
                   m = as.numeric(exp_ex$C=='m'),
                   w = as.numeric(exp_ex$C=='w'))

# variable representing the size (n) of each observation. They are all 1.
exp_ex$size = 1


# center vtl
exp_ex$vtl_original = exp_ex$vtl
exp_ex$vtl = exp_ex$vtl - mean (exp_ex$vtl)

# center and scale f0
exp_ex$f0_original = exp_ex$f0 
exp_ex$f0 = exp_ex$f0 - mean(exp_ex$f0)
exp_ex$f0 = exp_ex$f0 / 100

# new dependent variable: Size Group
SG = 0
SG[exp_ex$C=='g'] = 1
SG[exp_ex$C=='b'] = 1
SG[exp_ex$C=='w'] = 2
SG[exp_ex$C=='m'] = 3
exp_ex$SG = SG




### Chapter 3

exp_ex
men = exp_ex[exp_ex$C_v=='m',]

model_priors =  
  brms::brm (height ~ 1, data = men, chains = 4, cores = 4,
             warmup = 1000, iter = 3500, thin = 2,
             prior = c(brms::set_prior("normal(176, 15)", class = "Intercept"),
                       brms::set_prior("normal(0, 15)", class = "sigma")))

model_priors = brms::add_criterion(model_priors, "loo")

# saveRDS (model_priors, '3_model_priors_ex.RDS')

