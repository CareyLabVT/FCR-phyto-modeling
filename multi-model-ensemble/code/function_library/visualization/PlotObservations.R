#Plot observations
#Author: Mary Lofton
#Date: 01JUN23


PlotObservations <- function(observations, pred_only){
  
  if(pred_only == TRUE){
    observations <- observations %>%
      filter(year(Date) == 2022)
  }
  
  p <- ggplot(data = observations, aes(x = Date, y = Chla_ugL))+
    geom_point(size = 1) + 
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          plot.title = element_text(hjust = 0.98, vjust = -8, face = "bold",
                                    size = 16))+
    scale_x_date(date_labels = "%b")
  
  if(pred_only == TRUE){
    p <- p + ggtitle("2022")
  } 

  return(p)
}
