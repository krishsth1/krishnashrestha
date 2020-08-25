#' it apply
#' df data frame
#' col is colour to line plot

#' @export





P_L<- function(df,col){


  data=df
  library(ggplot2)
  plot1<-ggplot(data) +
    aes(x = L, y = TP) +
    geom_line(size = 0.64, colour =col) +
    labs(title = "Labour Unit VS Total Production",
         caption = "Cobb-Douglas Production Function") +
    geom_point()+
    scale_x_continuous(breaks = L) +
    xlab("Labour Unit")+
    ylab("Total Production")
  return(plot1)
}
