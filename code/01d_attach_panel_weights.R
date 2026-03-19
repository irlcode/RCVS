message("5. Attach panel weights...")

# Since successful contact can be influenced by numerous factors we adopted an approach from epidemiology (Bärnighausen, 2011), 
# accounting for the individual characteristics of interviewers, the call dates, and the respondents' regions of residence. 
# We estimated a logit model where the dependent variable equals 1 if the interviewer successfully reached a potential respondent 
# and 0 otherwise. Independent variables included the date of the call, the respondent's region, and the interviewer fixed effects. 

# The second stage of the analysis involved estimating the probability of a respondent's cooperation, 
# provided that contact had been established. In this model, the dependent variable was the successful completion of the interview, 
# while the independent variables included the respondents' socio-demographic characteristics 
# reported during the previous 2021 wave (gender, marital status, age, education, employment status, rural residence, and household income). 

# The final panel weight was calculated as the product of the two components attrition_weight: 
# the inverse probability of initial contact contact_weight and the inverse probability of cooperation interview_weight. 
# This approach allows for a more granular adjustment for non-random attrition, 
# ensuring that the longitudinal sample remains representative of the original population. 
# To prevent excessive variance in the estimates, all weights exceeding 5 were trimmed to this value

# Load prepared result of script: 3_weighting_and_power_analysis/2_compute_weights_for_cohort.r
load("data/auxdata/computed_panel_weights_13mar26.rdata")

# Attaching weight decomposition to the panel
panel[panel_weights, c("crosssection2021_weight", "interview_weight", "contact_weight", "attrition_weight") := .(crosssection2021_weight, interview_weight, contact_weight, attrition_weight), on = .(year, ID)]