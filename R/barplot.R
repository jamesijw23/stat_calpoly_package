#' Graph bar_plot
#' @param radian @name bar_plot @description easy way to bar plot
#' @return value
#' @export


bar_plot = function(df,x_variable,y_variable,bar_color = "black",main_title,type,color2){


  if(type == 1){
    df[, x_variable] <- as.factor(df[, x_variable])
    p1 = ggplot2::ggplot(df, ggplot2::aes_string(x=x_variable))+
      ggplot2::geom_bar(stat="count", width=0.7, fill=bar_color)+
      ggplot2::theme_bw() +
      ggplot2::ggtitle(main_title) +
      ggplot2::xlab(x_variable) +
      ggplot2::ylab('Frequncy') +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold"))

  } else if (type == 2){

    df2 = data.frame(table(df[, x_variable],df[, y_variable]))
    df3 = df2[df2$Var1 != '',]
    df3 = df3[df3$Var2 != '',]

    p1 = ggplot2::ggplot(df3, ggplot2::aes(x=Var1,y = Freq, fill=Var2 ))+
      ggplot2::geom_bar(stat="identity", position=ggplot2::position_dodge())+
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", fill = y_variable) +
      ggplot2::ggtitle(main_title) +
      ggplot2::xlab(x_variable) +
      ggplot2::ylab('Frequncy') +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size = 15, face = "bold")) +
      ggplot2::scale_fill_brewer(palette=paste0("Set",color2,sep=''))
  }
  return(p1)
}
