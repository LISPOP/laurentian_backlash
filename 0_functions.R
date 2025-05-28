

#### FUNCTIONS FOR THE LAURENTIAN PROJECT ####

##########################################################################
### FUNCTION:     RMSPEtest                                              #
### AUTHOR:       Michael Kistner                                        #
### PURPOSE:      Function takes as input a panel dataframe prepared     #
###               use with the dataprep function in the Synth package,   #
###               as well as all necessary arguments for the function.   #
###               Returns a dataframe of RMSPE ratios for control and    #
###               treated units, from which a p-value can be calculated. #
### Dependencies: Synth, Tidyverse                                       #
##########################################################################

require(Synth)
require(tidyverse)

RMSPETest <- function(df, dependent, predictors, unit_numerics, unit_names,
                      time_var, treatment_id, controls_id, 
                      time_predictors_prior, time_optimize_ssr){ 
  
  # Combine control IDs and treatment IDs into vector
  unit_vector <- c(controls_id, treatment_id)
  
  # Create dataframe to contain RMSPE ratio for each unit 
  ratio_df <- data.frame(unit_vector,
                         rep(NA, length(unit_vector))) %>%
    setNames(c("Control_Unit", "RMSPE_Ratio"))
  
  # For each unit, fit synthetic control model
  for(unit in unit_vector){
    tryCatch({
      # Specify placebo control and treatment units for each iteration
      placebo_controls <- unit_vector[unit_vector != unit]
      placebo_treatment <- unit
      
      # Prepare data to fit synth model
      synthin <- dataprep(foo = as.data.frame(df),
                          predictors = predictors,
                          predictors.op = "mean",
                          dependent = dependent,
                          unit.variable = unit_numerics,
                          unit.names.variable = unit_names,
                          time.variable = time_var,
                          treatment.identifier = placebo_treatment,
                          controls.identifier = placebo_controls,
                          time.predictors.prior = time_predictors_prior,
                          time.optimize.ssr = time_optimize_ssr)
      
      # Fit synth model
      synthout <- synth(synthin)
      
      # Calculate the synthetic control's outcome for each time period using calculated
      # weights
      outcome_df <- select(df, unit_names, time_var, dependent) %>%
        setNames(c("unit.names", "time.variable", "dependent.variable")) %>% 
        left_join(synth.tab(synthout, synthin)$tab.w, .) %>%
        mutate(weighted_outcome = w.weights * dependent.variable) %>%
        group_by(time.variable) %>%
        summarize(weighted_outcome = sum(weighted_outcome))
      
      # Extract outcome in vector
      placebo_outcome <- df[dependent][df[unit_numerics] == placebo_treatment]
      
      # Extract time period in vector
      time.variable <- unique(df[time_var])
      
      # Add to outcome df and calculate squared difference
      outcome_df <- data.frame(time.variable, placebo_outcome) %>%
        setNames(c("time.variable", "placebo_outcome")) %>%
        left_join(outcome_df, .) %>%
        mutate(squared_difference = (weighted_outcome - placebo_outcome)^2)
      
      # Calculate the root mean squared error for pre and post time periods 
      preRMSPE <- sqrt(mean(outcome_df[outcome_df$time.variable == time_optimize_ssr, ]$squared_difference))
      postRMSPE <- sqrt(mean(outcome_df[outcome_df$time.variable == 2022, ]$squared_difference))
      
      # Calculate the RMSPE ratio
      ratio <- postRMSPE / preRMSPE
      
      # Add to dataframe containing results for each iteration
      ratio_df["RMSPE_Ratio"][ratio_df["Control_Unit"] == unit] <- ratio
    }, error=function(e){print(paste("Error in computations for control unit: ",
                                     unit, sep = ""))}) # If model cannot be fit, return error.
  }
  
  # Return results
  return(ratio_df)
}


df = subset(pc, Year > 2017)
dependent = "Percent"
predictors=c("francophones_pct", "mining_pct", "income", "phds_pct",  "first_nations_pct", "density")
unit_numerics = "ED_ID"
unit_names = "District"
time_var = "Year"
treatment_id = 103
controls_id = northern_controls
time_predictors_prior = 2022
time_optimize_ssr = 2018

unit_vector <- c(controls_id, treatment_id)

# Create dataframe to contain RMSPE ratio for each unit 
ratio_df <- data.frame(unit_vector,
                       rep(NA, length(unit_vector))) %>%
  setNames(c("Control_Unit", "RMSPE_Ratio"))

# For each unit, fit synthetic control model
for(unit in unit_vector){
  tryCatch({
    # Specify placebo control and treatment units for each iteration
    placebo_controls <- unit_vector[unit_vector != 105]
    placebo_treatment <- 105
    
    # Prepare data to fit synth model
    synthin <- dataprep(foo = as.data.frame(df),
                        predictors = predictors,
                        predictors.op = "mean",
                        dependent = dependent,
                        unit.variable = unit_numerics,
                        unit.names.variable = unit_names,
                        time.variable = time_var,
                        treatment.identifier = placebo_treatment,
                        controls.identifier = placebo_controls,
                        time.predictors.prior = time_predictors_prior,
                        time.optimize.ssr = time_optimize_ssr)
    
    # Fit synth model
    synthout <- synth(synthin)
    
    # Calculate the synthetic control's outcome for each time period using calculated
    # weights
    outcome_df <- select(df, unit_names, time_var, dependent) %>%
      setNames(c("unit.names", "time.variable", "dependent.variable")) %>% 
      left_join(synth.tab(synthout, synthin)$tab.w, .) %>%
      mutate(weighted_outcome = w.weights * dependent.variable) %>%
      group_by(time.variable) %>%
      summarize(weighted_outcome = sum(weighted_outcome))
    
    # Extract outcome in vector
    placebo_outcome <- df[dependent][df[unit_numerics] == placebo_treatment]
    
    # Extract time period in vector
    time.variable <- unique(df[time_var])
    
    # Add to outcome df and calculate squared difference
    outcome_df <- data.frame(time.variable, placebo_outcome) %>%
      setNames(c("time.variable", "placebo_outcome")) %>%
      left_join(outcome_df, .) %>%
      mutate(squared_difference = (weighted_outcome - placebo_outcome)^2)
    
    # Calculate the root mean squared error for pre and post time periods 
    preRMSPE <- sqrt(mean(outcome_df[outcome_df$time.variable == time_optimize_ssr, ]$squared_difference))
    postRMSPE <- sqrt(mean(outcome_df[outcome_df$time.variable == 2022, ]))
    
    # Calculate the RMSPE ratio
    ratio <- postRMSPE / preRMSPE
    
    # Add to dataframe containing results for each iteration
    ratio_df["RMSPE_Ratio"][ratio_df["Control_Unit"] == unit] <- ratio
  }, error=function(e){print(paste("Error in computations for control unit: ",
                                   unit, sep = ""))}) # If model cannot be fit, return error.
}
