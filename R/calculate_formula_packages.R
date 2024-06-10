# The objective of this script is to document the methodology on how to
# calculate the amount of baby formula package units a baby needs during its
# first two years of life.

# To estimate the amount of baby formula powder units (900gr), the following
# information is needed:
# - formula powder per bottle/serving in grams
# - recommended amount of servings/bottles per day
# - how the previous two vary as the baby grows

# based on löwenzahn organics baby formula instructions
# https://bit.ly/42bNli8

calculate_formula_packages <- function(){
  # define grams of formula baby needs per period of life
  formula_grams <- c(
    8.8, # weeks 1-2
    13.2, # weeks 3-4
    17.6, # weeks 5-8
    22, # weeks 9-16
    26.4, # weeks 17-20 (5 months)
    26.4 # weeks 21-24 (6+ months) 
  )
  
  # define bottles of formula baby needs per period of life
  bottles_day <- c(
    7, # weeks 1-2
    5, # weeks 3-4
    5, # weeks 5-8
    5, # weeks 9-16
    4, # weeks 17-20 (5 months)
    2 # weeks 21-24 (6+ months)
  )
  
  # define feeding times per period of life
  weeks <- c(
    2, # weeks 1-2
    2, # weeks 3-4
    4, # weeks 5-8
    8, # weeks 9-16
    4, # weeks 17-20 (5 months)
    72 # weeks 21-24 (6+ months)
  )
  
  arguments <- list(formula_grams, bottles_day, weeks)
  
  #This function calculates the total amount of baby formula needed in grams
  #over a specified period.
  calculate_baby_formula_needed <-
    function(formula_grams_p_bottle,
             bottles_p_day,
             feeding_weeks) {
      feeding_days <- feeding_weeks * 7
      used_formula <-
        formula_grams_p_bottle * bottles_p_day * feeding_days
      
      return(used_formula)
    }
  
  # function calculates packages of baby formula needed
  calculate_baby_formula_packages <- function(formula_grams, package_gr = 900){
    return(formula_grams / package_gr)
  }
  
  # calculate total baby formula one baby needs during two years
  total_formula <- 
    pmap(arguments, calculate_baby_formula_needed) %>% 
    flatten_dbl() %>% 
    sum()
  
  # calculate total baby formula packages needed for two years
  formula_packages <-
    calculate_baby_formula_packages(formula_grams = total_formula,
                                    package_gr = 900)
  
  return(formula_packages)
}





