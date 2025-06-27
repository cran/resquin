## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(resquin)
nep_resp_styles <- resp_styles(
  x = nep,
  scale_min = 1, # minimum response option
  scale_max = 5, # maximum response option
  min_valid_responses = 1) # default, excludes respondents with any missing value

summary(nep_resp_styles)

## -----------------------------------------------------------------------------
first_flagging <- flag_resp(nep_resp_styles,
                            ARS > 0.8)

summary(first_flagging)

## -----------------------------------------------------------------------------
nep_resp_patterns <- resp_patterns(nep)
nep_resp_patterns_resp_styles <- cbind(nep_resp_styles,nep_resp_patterns[,-1])

second_flagging <- flag_resp(nep_resp_patterns_resp_styles,
                             ARS > 0.8,
                             longest_string_length >= 8)
summary(second_flagging)

## -----------------------------------------------------------------------------
flag_resp(nep_resp_patterns_resp_styles,
          ARS > 0.8,
          longest_string_length >= 8,
          ARS > 0.8 | longest_string_length >= 8) |> 
  summary()

## -----------------------------------------------------------------------------
random_vector <- sample(c(F,T),1000,replace = T)
random_vector[is.na(nep_resp_styles$ARS)] <- NA # Add missing data as in the other data frames

# example three contains response indicator values per respondent
external_indicator_data <- cbind(
  nep_resp_patterns_resp_styles,
  new_indicator = random_vector)

flag_resp(external_indicator_data,
          ARS > 0.8,
          longest_string_length >= 8,
          new_indicator == T) |> 
  summary()

## -----------------------------------------------------------------------------
flag_df <- flag_resp(
  nep_resp_patterns_resp_styles,
  ARS > 0.8,
  longest_string_length >= 8,
  ARS > 0.8 | longest_string_length >= 8) 

flag_df

## -----------------------------------------------------------------------------
# Exclude the 33 flagged respondents with ARS > 0.8
nep[!flag_df$`ARS > 0.8`,] |> 
  na.omit() #exclude respondents with missing values

## -----------------------------------------------------------------------------
# Extract only the 33 flagged respondents with ARS 0.8
nep[flag_df$`ARS > 0.8`,] |> 
  na.omit()

