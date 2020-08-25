#' it apply
#' df data frame
#' col : colour to line plot character

#' @export



P_L_M<-function(df,col="red"){
library(ggplot2)
library(patchwork)
library(dplyr)
data<-df
data<- data %>% mutate(LM=c(0,(diff(L)/2)+L[-length(L)]))
data<- data %>% mutate(int=case_when(
  TR > 56~"Profit",
  round(TR,0) == 56 ~"Balance",
  TRUE~"Loss"
))
plot2<-ggplot(data) +
  aes(x = L, y = AP) +
  geom_line(size = 0.64, colour =col) +
  labs(title = "Labour Unit VS Average Production",
       caption = "Cobb-Douglas Production Function") +
  geom_point(colour="black")+
  scale_x_continuous(breaks = L) +
  xlab("Labour Unit")+
  ylab("Average Production")

plot3<-data %>% filter(LM!=0) %>%
  ggplot() +
  aes(x = LM, y = MP) +
  geom_line(size = 0.64,aes(colour =int)) +
  labs(title = "Labour Unit VS Marginal Production",
       caption = "Cobb-Douglas Production Function") +
  geom_point(aes(colour=int))+
  scale_x_continuous(breaks = data$LM) +
  theme(legend.position="none")+xlab("Labour Unit")+
  ylab("Marginal Production")

#plotly::ggplotly(plot1)
return(plot2/plot3)



}
