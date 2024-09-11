## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# A test data set with three items and ten respondents
testdata <- data.frame(
  var_a = c(1,4,3,5,3,2,3,1,3,NA),
  var_b = c(2,5,2,3,4,1,NA,2,NA,NA),
  var_c = c(1,2,3,NA,3,4,4,5,NA,NA))

testdata

## -----------------------------------------------------------------------------
library(resquin)
# Calculating response style indicators for all respondents with no missing values
results_response_styles <- resp_styles(
  x = testdata,
  scale_min = 1,
  scale_max = 5,
  min_valid_responses = 1, # Excludes respondents with less than 100% valid responses
  normalize = T)  # Presents results in percent of all responses

round(results_response_styles,2)

## -----------------------------------------------------------------------------
# Calulating response distribution indicators for all respondents with no missing values
results_resp_distributions <- resp_distributions(
  x = testdata,
  min_valid_responses = 1) # Excludes respondents with less than 100% valid responses

round(results_resp_distributions,2)

