

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
men = exp_ex[exp_ex$C_v=='m',]

model_priors_ex =  
  brms::brm (height ~ 1, data = men, chains = 4, cores = 4,
             warmup = 1000, iter = 3500, thin = 2,
             prior = c(brms::set_prior("normal(176, 15)", class = "Intercept"),
                       brms::set_prior("normal(0, 15)", class = "sigma")))

model_priors_ex = brms::add_criterion(model_priors_ex, "loo")

# saveRDS (model_priors_ex, '3_model_priors_ex.RDS')


### Chapter 4
men = exp_ex[exp_ex$C_v=='m',]

model_multilevel_L_S_ex =  brms::brm (
  height ~ 1 + (1|L) + (1|S), data = men, chains = 4, cores = 4,
  warmup = 1000, iter = 3500, thin = 2,
  prior = c(brms::set_prior("normal(176, 15)", class = "Intercept"),
            brms::set_prior("normal(0, 15)", class = "sd"),
            brms::set_prior("normal(0, 15)", class = "sigma")))

model_multilevel_L_S_ex = brms::add_criterion(model_multilevel_L_S_ex, "loo")

# saveRDS (model_multilevel_L_S_ex, '../models/4_model_multilevel_L_S_ex.RDS')



### Chapter 5

options (contrasts = c('contr.sum','contr.sum'))
notmen = exp_ex[exp_ex$C_v!='m' & exp_ex$C!='m',]

model_sum_coding_t_ex =  brms::brm (
  height ~ A + (1|L) + (1|S), data = notmen, chains = 4, 
  cores = 4, warmup = 1000, iter = 3500, thin = 2, family="student",
  prior = c(brms::set_prior("student_t(3, 156, 12)", class = "Intercept"),
            brms::set_prior("student_t(3, 0, 12)", class = "b"),
            brms::set_prior("student_t(3, 0, 12)", class = "sd"),
            brms::set_prior("gamma(2, 0.1)", class = "nu"),
            brms::set_prior("student_t(3, 0, 12)", class = "sigma")))

model_sum_coding_t_ex = brms::add_criterion(model_sum_coding_t_ex, "loo")

# saveRDS (model_sum_coding_t_ex, '../models/5_model_sum_coding_t_ex.RDS')



### Chapter 6

options (contrasts = c('contr.sum','contr.sum'))
notmen = exp_ex[exp_ex$C_v!='m' & exp_ex$C!='m',]

priors = c(brms::set_prior("student_t(3,156, 12)", class = "Intercept"),
           brms::set_prior("student_t(3,0, 12)", class = "b"),
           brms::set_prior("student_t(3,0, 12)", class = "sd"),
           brms::set_prior("lkj_corr_cholesky (2)", class = "cor"), 
           brms::set_prior("gamma(2, 0.1)", class = "nu"),
           brms::set_prior("student_t(3,0, 12)", class = "sigma"))

model_re_t_ex =  
  brms::brm (height ~ A + (A|L) + (1|S), data = notmen, chains = 4, 
             cores = 4, warmup = 1000, iter = 5000, thin = 4, 
             prior = priors, family = "student")

model_re_t_ex = brms::add_criterion(model_re_t_ex, "loo")

# saveRDS (model_re_t_ex, '../models/6_model_re_t_ex.RDS')


### Chapter 7

priors = c(brms::set_prior("student_t(3,156, 12)", class = "Intercept"),
           brms::set_prior("student_t(3,0, 12)", class = "b"),
           brms::set_prior("student_t(3,0, 12)", class = "sd"),
           brms::set_prior("lkj_corr_cholesky (2)", class = "cor"), 
           brms::set_prior("gamma(2, 0.1)", class = "nu"),
           brms::set_prior("student_t(3,0, 12)", class = "sigma"))

model_interaction_ex =  
  brms::brm (height ~ A + G + A:G + (A + G + A:G|L) + (1|S), 
             data = exp_ex, chains = 4, cores = 4, warmup = 1000, 
             iter = 5000, thin = 4, prior = priors, family = "student")

model_interaction_ex = brms::add_criterion(model_interaction_ex, "loo")

# saveRDS (model_interaction_ex, '../models/7_model_interaction_ex.RDS')

