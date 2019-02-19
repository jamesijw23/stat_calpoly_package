#' summarize_1_quantitative
#' @param radian @name ss 1 quantitative @description
#' @return value
#' @export



summarize_one_quant_variable = function(df,x_variable){
  vec = dplyr::select(df, x_variable)
  vec = describe(vec)
  return(vec)
}
