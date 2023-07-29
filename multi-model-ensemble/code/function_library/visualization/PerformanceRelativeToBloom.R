# Title: Performance relative to bloom
# Author: Mary Lofton
# Date: 17MAY23

pacman::p_load(cowplot)

#Function to render plot of forecast performance relative to an event date
#'@param data data frame with (at a minimum) the following columns:
#'horizon: the forecast horizon
#'depth: depth of the forecast in meters
#'a column whose name is the forecast scoring metric you wish to plot (e.g., "crps")
#'optionally, a column of group ids (e.g., model or team ids, different depths you want to plot, etc.)
#'@param ylims optionally, y axis limits for plotting, provided as c(min, max)
#'@param variable_name character string of target variable, eg., "temperature"
#'@param max_horizon_past maximum days prior to event that you would like to plot scores for as a *negative* number (e.g., -35)
#'@param score character string of the column name containing the score you would like to plot (e.g., "crps")
#'@param group_id character string of the column name containing the variable you would like to group by in plot (e.g., "model_id" or "depth")
#'@param focal_dates character string of event date(s) (e.g., date of turnover)
#'@param data_plot plot observational data leading up to event?

PerformanceRelativeToBloom <- function(observations,
                                       model_output,
                                       variable_name,
                                       max_horizon_past,
                                       score,
                                       focal_dates,
                                       data_plot){

  for(d in 1:length(focal_dates)){
    
  #set plot dates
  plot_dates <- seq.Date(from = as.Date(focal_dates[d])-(-1*max_horizon_past), to = as.Date(focal_dates[d]), by = "day")
  
  #reformat observations
  pred_dates <- data.frame(Date = plot_dates) %>%
    left_join(., observations, by = "Date") %>%
    rename(datetime = Date)
  
  #focal chl-a
  focal_chla <- observations %>%
    filter(Date == focal_dates[d]) %>%
    pull(Chla_ugL)
  
  #reformat model output
  output <- model_output %>% 
    filter(reference_datetime %in% plot_dates & datetime == focal_dates[d]) %>%
    group_by(model_type, model_id, reference_datetime) %>%
    mutate(horizon = datetime - reference_datetime) %>%
    ungroup() %>%
    separate(horizon, c("horizon"), sep = " ") %>%
    left_join(., pred_dates, by = "datetime") %>%
    group_by(model_type, model_id, horizon) %>%
    summarize(rmse = sqrt(mean((focal_chla - prediction)^2, na.rm = TRUE))) %>%
    filter(!horizon == 0) %>%
    mutate(horizon = as.numeric(horizon)) %>%
    arrange(model_type, model_id, horizon)
  
  plot_data_temp <- output %>%
    mutate(horizon_past = -horizon) %>%
    filter(horizon_past >= max_horizon_past) %>%
    rename(score = any_of(score)) %>%
    mutate(model_type = factor(model_type, levels = c("null","statistical","process","machine learning"))) %>%
    mutate(model_id = factor(model_id, levels = c("DOY","historical mean","persistence","ARIMA","ETS","TSLM","prophet","OptimumMonod","OptimumSteele","OptimumMonodNP","OptimumSteeleNP","LSTM","XGBoost")))
  
  
  if(d == 1){
    plot_data <- plot_data_temp
  } else {
    plot_data <- bind_rows(plot_data, plot_data_temp)
  }
  
  } #end for loop
  
  plot_data <- plot_data %>%
    select(-horizon) %>%
    mutate(horizon_past = as.double(horizon_past)) %>%
    group_by(model_type, model_id, horizon_past) %>%
    summarize(score = mean(score, na.rm = TRUE))

  #a bunch of if statements for y axis label
  if(score == "crps"){
    ylab1 = "CRPS "
    if(variable_name == "temperature"){
      ylab = paste0(ylab1,"(°C)")
    }
  } else if(score == "logs"){
    ylab = "Ignorance score"
  } else if (score == "rmse"){
    if(variable_name == "chlorophyll-a"){
      ylab = expression(paste("RMSE (",mu,g,~L^-1,")"))
    }
  }

  p <- ggplot(data = plot_data, aes(x = horizon_past, y = score, group = model_id, linetype = model_id, color = model_type)) +
    geom_line(linewidth = 1) +
    xlim(max_horizon_past,0) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
    annotate("text", x = -4.25, y = max(plot_data$score)+1, 
             label = "date of chl-a peak", fontface = 2)+
    labs(x = "Days prior to chl-a peak", y = ylab, title = paste0("Predictions for day of chl-a peak")) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold", hjust = 1),
          legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1),
          legend.key.width = unit(2,"cm"))+
    guides(color = guide_legend(order = 1)) +
    scale_linetype_manual(name = "Model ID", values = c("solid", "dashed", "dotted", "solid", "dashed", "dotted","dotdash", "solid", "dashed", "dotted", "dotdash","solid", "dashed"))+
    scale_color_manual(name = "Model type", values = c("#71BFB9","#B85233","#E69F00","#0072B2"))
      
  if(data_plot == TRUE){
  p1 <- ggplot(data = pred_dates, aes(x = datetime, y = Chla_ugL)) +
    geom_point() +
    geom_vline(xintercept = as.Date(focal_date), linetype = "dashed") +
    annotate("text", x = as.Date(focal_date) - 7.2, y = max(pred_dates$Chla_ugL)-1, 
             label = "date of maximum chl-a")+
    labs(x = "", y = expression(paste("Chlorophyll-a (",mu,g,~L^-1,")"))) +
    theme_classic() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, face = "bold", hjust = 1),
          legend.title = element_text(face = "bold"),
          panel.background = element_rect(color = "black", linewidth = 1))+
    guides(color = guide_legend(order = 1))+
    ggtitle(paste0("Observations prior to ",focal_date))
  
  p2 <- plot_grid(p, p1, align = "v", nrow = 2, axis = "lr", rel_heights = c(2,1.5))

  return(p2)
  } else {
    return(p)
  }

}
