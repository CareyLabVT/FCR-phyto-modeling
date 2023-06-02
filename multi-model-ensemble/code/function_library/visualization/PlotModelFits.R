#Plot observations
#Author: Mary Lofton
#Date: 01JUN23


PlotModelFits <- function(observations, predictions, model_ids){
  
    #limit to year of prediction
    observations <- observations %>%
      filter(!year(Date) == 2022) %>%
      add_column(variable = "observed")
    
    #limit to models you want
    predictions <- predictions %>%
      filter(model_id %in% model_ids) %>%
      mutate(model_id = factor(model_id, levels = model_ids))
    
  p <- ggplot()+
    geom_line(data = predictions, aes(x = datetime, y = prediction, group = model_id, color = model_id))+
    geom_point(data = observations, aes(x = Date, y = Chla_ugL,
                                        group = variable, fill = variable), size = 1) + 
    xlab("")+
    ylab(expression(paste("Chlorophyll-a (",mu,g,~L^-1,")")))+
    theme_bw()+
    theme(axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          plot.title = element_text(hjust = 0.98, vjust = -8, face = "bold",
                                    size = 16),
          legend.title = element_text(face = "bold"))+
    scale_color_manual(name = "Model ID", values = c("#71BFB9","#B85233","#F2EC67","#56B4E9"))+
    scale_fill_discrete(name = "")
    

  return(p)
}
