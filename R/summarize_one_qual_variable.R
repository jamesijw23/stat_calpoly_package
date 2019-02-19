#' summarize_1_qualitative
#' @param radian @name ss 1 qualitative @description
#' @return value
#' @export


summarize_one_qual_variable = function(df,x_variable){
  vec = dplyr::select(df,x_variable)
  vec = data.frame(table(vec) )
  return(vec)
}
