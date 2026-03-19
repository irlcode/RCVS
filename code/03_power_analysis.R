message("10. Replicating power analysis estimates...")
# https://r-packages.io/packages/epiR/epi.prev
# https://cran.r-project.org/web/packages/epiR/epiR.pdf

# pos — a vector listing the count of positive test results for each population.
# tested — a vector listing the count of subjects tested for each population.
# se — test sensitivity (0 - 1). se can either be a single number or a vector of the same length as pos. See the examples, below, for details.
# sp — test specificity (0 - 1). sp can either be a single number or a vector of the same length as pos. See the examples, below, for details.
# method — a character string indicating the confidence interval calculation method to use. Options are "c-p" (Cloppper-Pearson), "sterne" (Sterne), "blaker" (Blaker) and "wilson" (Wilson).
# units — multiplier for the prevalence estimates.
# conf.level — magnitude of the returned confidence interval. Must be a single number between 0 and 1.

# Load function from 
# Reiczigel, Földi and Ózsvári (2010) Exact confidence limits for 
# prevalence of a disease with an imperfect diagnostic test, 
# Epidemiology and infection, 138: 1674-1678.
# http://www2.univet.hu/users/jreiczig/CI4prevSeSp/R_function_ci4prev.txt
source("code/aux_code/R_function_ci4prev.r")

# from DS
compute_sampling_error <- function(sample_size, prevalence, se, sp) {
  
  res <- ci4prev(n = sample_size, poz = ceil(sample_size*prevalence), se = se, sp = sp, method = "wi", dec = 9)
  sampling_error <- (max(res)-min(res))/2
  sampling_error
  
}

# Test parameters
victimization_prevalence <- 0.23 # Close to mean(rcvs_2021$resp_is_crime_victim)
type1_error <- 0.5 # close to rcvs_2021[!is.na(crime_type), mean(crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление")) ]
type2_error <- 0.05 # assumed false negatives

# 2024
# Panel — 3.12
round(compute_sampling_error(sample_size = 3456, prevalence = victimization_prevalence, se = type1_error, sp = (1 - type2_error))*100,2)
# Cross-section — 1.72
round(compute_sampling_error(sample_size = 11323, prevalence = victimization_prevalence, se = type1_error, sp = (1 - type2_error))*100,2)
# Pooled — 1.51
round(compute_sampling_error(sample_size = 11323+3456, prevalence = victimization_prevalence, se = type1_error, sp = (1 - type2_error))*100,2)

# 2018 — 1.41
round(compute_sampling_error(sample_size = 16818, prevalence = victimization_prevalence, se = type1_error, sp = (1 - type2_error))*100,2)
# 2021 — 1.53
round(compute_sampling_error(sample_size = 14431, prevalence = victimization_prevalence, se = type1_error, sp = (1 - type2_error))*100,2)

