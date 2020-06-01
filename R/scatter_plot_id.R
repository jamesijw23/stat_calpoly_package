#' scatter_plot_id
#' @param radian @name ss scatterplot @description
#' @return value
#' @export


scatter_plot_id = function(df,
                           quantitative_expl_variable,
                           categorical_expl_variable,
                           response_variable,
                           model_type = "dvm"){
  ## Specify The Variables
  outcome <- response_variable
  variables <- c(quantitative_expl_variable, 
                 categorical_expl_variable)
  
  ## Determine Type Model
  if(model_type == "dvm"){
    m_formula <- as.formula(
      paste(outcome, 
            paste(variables, collapse = " + "), 
            sep = " ~ "))
    plot_title = paste0("Dummy Variable for ",outcome)
  } else if(model_type == "ivm"){
    m_formula <- as.formula(
      paste(outcome, 
            paste(variables, collapse = " * "), 
            sep = " ~ "))
    plot_title = paste0("Interaction Variable for ",outcome)
  }
  
  ## Determine the Number of Colors
  number_levels = length(unique(df[,categorical_expl_variable]))
  
  if(number_levels == 2){
    number_colors = c("black", "red")
  } else if(number_levels == 3){
    number_colors = c("black", "red","blue")
  }
  
  ## Run in the model
  test_model = lm(m_formula, data = df)
  
  ## Create Predicted Values
  mod1_df <-cbind(df,predict(test_model,interval="confidence"))
  
  ## Plot Figure
  ggplot2::ggplot(mod1_df, ggplot2::aes_string(x=quantitative_expl_variable,
                                               y=response_variable,
                                               color=categorical_expl_variable)) + 
    ggplot2::theme_bw() + 
    ggplot2::geom_point() +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab(quantitative_expl_variable) +
    ggplot2::scale_color_manual(values=number_colors) +
    ggplot2::ylab(response_variable) +
    ggplot2::geom_line(ggplot2::aes(y=fit)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                      size = 15, 
                                                      face = "bold"))
  
}

