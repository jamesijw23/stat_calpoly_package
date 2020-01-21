#' summarize_1_quantitative
#' @param radian @name ss 1 quantitative @description
#' @return value
#' @export



summarize_one_quant_variable = function(df,x_variable){
  vec = dplyr::pull(df, x_variable)
  vec = psych::describe(vec) 
  vec = dplyr::select(vec,n,mean,sd,min,max)
  # vec = knitr::kable(vec) 
  # vec = kableExtra::kable_styling(vec,position = 'center', full_width = F,
  #                                 bootstrap_options ='bordered',font_size = 20)
  return(vec)
}
