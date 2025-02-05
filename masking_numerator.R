# About this script -------------------------------------------------------
# Author: Zhaonan Fang
# Date created: 03/02/2025
# Purpose: Define function for masking numerators for demographic data


########## rules for numerators (not denominators) ###########################################
########## rule 1: if any values in (0, max), begin masking, else return original vector #####
########## rule 2: if only 1 value is masked, mask the second smallest (all occurrences) #####

# from smallest to largest, for each value, 
# if the value is not 1 or 2 (that is, not in range (0,3)), do not mask.
# else, mask the current value and count the current total number of masked values.
# when finished, if the total number of masked values <2, mask 2nd smallest.
### this may be too complex, we can use some functions e.g., sum(), unique() to save time.


### define function
mask_numerators <- function(vec, maxNum = 3, maskMessage = 'masked') { # change maxnum to any num you want (n.b. vec has to be numeric)
  # 1. mask any values within the range (0,3)
  vec_masked <- ifelse(vec > 0 & vec < maxNum, maskMessage, vec)
  
  # count how many values were masked currently
  num_masked <- sum(vec_masked == maskMessage)
  
  # 2. if no values within range (0,3), return the original vector 
  if (num_masked == 0) {
    return(as.character(vec)) # as character to be merged with other vectors in the future
  }
  
  # 3. if only 1 value was masked, mask all occurrences of the second smallest
  if (num_masked == 1) {
    # get unique values excluding 0
    unique_sorted <- unique(sort(vec)) # sort and get all unique values
    unique_sorted <- unique_sorted[unique_sorted != 0] # excluding 0
    
    # get the second smallest values (excluding 0)
    second_smallest <- unique_sorted[2] 
    vec_masked[vec == second_smallest] <- maskMessage # mask all occurrences (use vec not vec_masked as the latter is a vector of characters)
  }
  return(vec_masked)
}

### examples
# example_vec <- c(4, 6, 2, 4, 0)
# masked_vec <- mask_numerators(example_vec, 3)
# 
# example_vec <- c(4, 6, 2, 4, 1)
# masked_vec <- mask_numerators(example_vec, 3)
# 
# example_vec <- c(4, 6, 2, 4, 3)
# masked_vec <- mask_numerators(example_vec, 3)
# 
# example_vec <- c(4, 6, 3, 4, 3)
# masked_vec <- mask_numerators(example_vec, 3)
# masked_vec <- mask_numerators(example_vec, 5)
# 
# 
# 
# # set example dataset
# set.seed(1234) # ensures reproducibility
# 
# example <- tibble(
#   pdu = sample(8, size = 100, replace = TRUE), # uniform distribution
#   gender = sample(c("boy", "girl", "non-binary", "prefer not to say"), size = 100, replace = TRUE) # uniform distribution
# )
# 
# example <- example |>
#   dplyr::group_by(pdu, gender) |>
#   dplyr::summarise(numerator = n()) |> # normal distribution
#   dplyr::mutate(proportion = sprintf("%.1f%%", 100 * numerator / sum(numerator)),
#                 numerator_masked = mask_numerators(numerator, 3),
#                 proportion_masked = ifelse(numerator_masked == 'masked', 'masked', proportion))  |>
#   tidyr::pivot_wider(names_from = gender, values_from = c(numerator, numerator_masked, proportion, proportion_masked))

