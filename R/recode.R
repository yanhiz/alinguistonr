#' Standardized constrast coding for categorical predictors
#'
#' This functions takes a vector variable and automatically creates a standardized contrast coding for it.
#' The standardization is done by centering and reduction.
#' Once the variable is recoded, it can be directly used in a model formula.
#' Please note that the function only allow for one reference level for all contrasts (a vs. b, a vs. c).
#' It does not allow sequential constrats (a vs. b, b vs. a).
#' You can change the reference level with the `ref` argument.
#' Default is the first level in alphabetical order.
#'
#' @param var The vector variable to be recoded
#' @param ref The reference level of the constrats. Default is the first level in alphabetical order.
#' @param center Default `TRUE`. Center the variable around its mean.
#' @param reduce Default `TRUE`. Reduce the variable by its standard deviation.
#' @examples
#' # You can use base R syntax to recode the variable.
#' data$my_variable <- recode(data$my_variable,ref='my_ref')
#' # You can also use tidyverse `mutate()` function.
#' data <- data %>%
#'   mutate(my_variable1 = recode(my_variable1, ref='my_ref1'),
#'          my_variable2 = recode(my_variable2, ref='my_ref2'))
#' # Fast recoding for multiple variables can be achieved combining `recode()` with `across()`
#' data <- data %>%
#'   mutate(across(c('my_variable1','my_variable2'),~recode(.x)))

recode <- function(var,ref=levels(factor(var))[1],center=T,reduce=F){
  factor <- relevel(factor(var),ref=ref)
  n <- length(levels(factor))
  names <- levels(factor)
  contrast <- contr.treatment(n)
  colnames(contrast) <- str_c('_',names[2:length(names)])
  for (level in names[2:length(names)]){
    mean <- mean(ifelse(factor==level,1,0))
    sd <- sd(ifelse(factor==level,1,0))
    if (center == T & reduce == T){contrast[,str_c('_',level)] <- (contrast[,str_c('_',level)]-mean)/sd}
    if (center == T & reduce == F){contrast[,str_c('_',level)] <- (contrast[,str_c('_',level)]-mean)}
    if (center == F & reduce == F){contrast[,str_c('_',level)] <- (contrast[,str_c('_',level)])}
  }
  contrasts(factor) <- contrast
  factor
}
