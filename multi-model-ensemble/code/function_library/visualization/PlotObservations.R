#Plot observations
#Author: Mary Lofton
#Date: 01JUN23


PlotObservations <- function(observations, pred_only, focal_dates, forecast_horizon){
  
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
    scale_x_date(date_labels = "%b")+
    ylim(NA,max(observations$Chla_ugL)+3)
  
  if(pred_only == TRUE){
    p <- p + ggtitle("2022")
  } 
  
  if(!is.null(focal_dates)){
    
    for(f in 1:length(focal_dates))
    p <- p +
      geom_rect(xmin = as.Date(focal_dates[f]) - (forecast_horizon + 1),
                xmax = as.Date(focal_dates[f]) + 1,
                ymin = 5, ymax = max(observations$Chla_ugL) + 1,
                fill = NA, color = "#B85233",
                linetype = 8)
  }

  return(p)
}
