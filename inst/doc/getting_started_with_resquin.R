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
results_response_styles |>
  summary()

## -----------------------------------------------------------------------------
results_response_styles |> 
  print()

## -----------------------------------------------------------------------------
# Calulating response distribution indicators for all respondents with no missing values
results_resp_distributions <- resp_distributions(
  x = testdata,
  min_valid_responses = 1) # Excludes respondents with less than 100% valid responses

round(results_resp_distributions,2)

## -----------------------------------------------------------------------------
results_resp_nondifferentiation <- resp_nondifferentiation(
  x = testdata,
  min_valid_responses = 1) # Excludes respondents with less than 100% valid responses

round(results_resp_nondifferentiation,2)

## -----------------------------------------------------------------------------
results_resp_patterns <- resp_patterns(testdata)
round(results_resp_patterns,2)

## -----------------------------------------------------------------------------
# Single defined pattern
defined_patterns_single <- resp_patterns(
  x = testdata,
  defined_patterns = c(1,2,3)
)
defined_patterns_single

# Multiple defined patterns
defined_patterns_multiple <- resp_patterns(
  x = testdata,
  defined_patterns = list( # wrap multiple pattern vectors into a list
    c(1,2,3),
    c(2,3,4),
    c(4,3,2),
    c(3,2,1)
  )
)

defined_patterns_multiple

# defined patterns are returned as a single list column.
# One way to make the data accessible is by using tidyr::unnest_wider()
# This way, each column represents the count of one defined pattern

defined_patterns_multiple |> 
  tidyr::unnest_wider(defined_patterns)

## -----------------------------------------------------------------------------
arbitrary_patterns_length_2 <- resp_patterns(
  x = testdata,
  arbitrary_patterns = 2
)

arbitrary_patterns_length_2

# You can also request the detection of patterns of multiple 
# lengths, in this case 2 and 3
arbitrary_patterns_length_2_3 <- resp_patterns(
  x = testdata,
  arbitrary_patterns = c(2,3)
)
arbitrary_patterns_length_2_3

## -----------------------------------------------------------------------------
# Default. Integer ids.
resp_distributions(testdata)

# No id column
resp_distributions(testdata,id = F)

# Custom id vectors
custom_ids <- letters[1:nrow(testdata)]
resp_distributions(testdata,id = custom_ids)

