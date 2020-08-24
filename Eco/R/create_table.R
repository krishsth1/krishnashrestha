#' it apply
#' L vector of labour unit
#' A
#' K fixed capital
#' @export



create_table<-  function(L,A,K)
  {


  library(dplyr)


  TP= A* sqrt(K) *sqrt(L)

  data = data.frame(L,TP)
  data <- data %>% mutate(AP= TP/L)
  data<- data %>% mutate(MP= c(0,diff(TP)))
  data<- data %>% mutate(LM=c(0,(diff(L)/2)+L[-length(L)]))

  data <- data %>% mutate(TR=MP*4)
  data<- data %>% mutate(int=case_when(
    TR > 56~"Profit",
    round(TR,0) == 56 ~"Balance",
    TRUE~"Loss"
  ))
  table1<-data %>% select(L,TP,MP,AP,TR)


  return(table1)
}

