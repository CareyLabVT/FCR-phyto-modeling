#Fit DOY model for chl-a
#Author: Mary Lofton
#Date: 28FEB23

#Purpose: fit ARIMA model for chla from 2018-2021

pacman::p_load(fable, moments, parsnip, tidymodels, xgboost, DiagrammeR)
tidymodels_prefer()
set.seed(100)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L
#'@param cal_dates list of two dates (yyyy-mm-dd) for start and
#'stop of calibration/fit period

fit_XGBoost <- function(data, cal_dates){
  
  #assign model fit start and stop dates
  start_cal <- date(cal_dates[1])
  stop_cal <- date(cal_dates[2])
  
  #assign target and predictors
  dates <- as_tibble(data) %>%
    filter(Date >= start_cal & Date <= stop_cal) %>%
    select(Date)
  df <- as_tibble(data) %>%
    filter(Date >= start_cal & Date <= stop_cal) %>%
    select(AirTemp_C,Shortwave_Wm2,Windspeed_ms,Inflow_cms, WaterTemp_C ,LightAttenuation_Kd, DIN_ugL, SRP_ugL, Chla_ugL) 
  
  #set recipe
  xgboost_recipe <- df |> 
    recipe(Chla_ugL ~ . )
  
  #assign folds
  folds <- vfold_cv(df, v = 10)
  
  #specify XGBoost from parsnip package
  xgboost_mod <- 
    boost_tree(mtry = tune(), min_n = tune(), tree_depth = tune(), learn_rate = tune(),
               loss_reduction = tune(), sample_size = tune(), trees = 1000) %>%
    set_engine("xgboost", num.threads = parallel::detectCores()) |> 
    set_mode("regression")
  
  #specify XGBoost workflow 
  xgboost_wflow <- 
    workflow() |> 
    add_model(xgboost_mod) |> 
    add_recipe(xgboost_recipe)
  
  #tune XGBoost hyperparameters
  xgboost_resample_fit <- 
    xgboost_wflow |>  
    tune_grid(resamples = folds,
              grid = 25,
              control = control_grid(save_pred = TRUE),
              metrics = metric_set(mae))
  
  #select best hyperparameters
  xgboost_resample_fit %>% 
    collect_metrics() |> 
    arrange(mean)
  
  best_hyperparameters <- xgboost_resample_fit %>%
    select_best("mae")
  
  #update workflow
  final_workflow <- 
    xgboost_wflow %>% 
    finalize_workflow(best_hyperparameters)
  
  #train model
  xgboost_fit <- final_workflow |> 
    fit(data = df)
  
  #plot model fit
  fitted_values <- predict(xgboost_fit, df) %>%
    bind_cols(dates)
  df <- df %>%
    bind_cols(dates)

  XGBoost_plot <- ggplot()+
    xlab("")+
    ylab("Chla (ug/L)")+
    geom_point(data = df, aes(x = Date, y = Chla_ugL, fill = "obs"))+
    geom_line(data = fitted_values, aes(x = Date, y = .pred, color = "XGBoost"))+
    labs(color = NULL, fill = NULL)+
    theme_classic()

  #build output df
  df.out <- data.frame(model_id = "XGBoost",
                       datetime = dates$Date,
                       variable = "chlorophyll-a",
                       prediction = fitted_values$.pred)

  
  #return output + model with best fit + plot
  return(list(out = df.out, XGBoost = xgboost_fit, plot = XGBoost_plot))
}
