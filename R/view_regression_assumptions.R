#' view_regression_assumptions
#' @param radian @name ss slr @description
#' @return value
#' @export




view_regression_assumptions = function(model){
  par(mfrow = c(1, 2))
  reg_plot = plot(model)
  
  return(reg_plot)
}
