#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Modified to cater for task at hand by Zsombor Hegedus

# CHAPTER 18
# CH18 Forecasting daily ticket sales for a swimming pool 
# using swim data
# version 0.9 2020-08-31
#########################################################################################


#
###########################################################

# Clear memory -------------------------------------------------------
rm(list=ls())

# Import libraries ---------------------------------------------------
library(ranger)
library(xgboost)
library(tidyverse)
library(stargazer)
library(Hmisc)
library(timeDate)
library(lubridate)
library(caret)
library(prophet)
library(viridis)


#####################################
# Creating time features  ----------
#####################################
data_in <- 'C:/Users/T450s/Desktop/programming/git/forecast_ticket_volume/'

#import data
daily_agg<-read.csv(file = paste(data_in,"data/swim_work.csv",sep="")) %>% 
  mutate(date = as.Date(date))

# dow: 1=Monday, weekend: Sat and Sun.
daily_agg <- daily_agg %>%
  mutate(year = year(date),
         quarter = quarter(date),
         month = factor(month(date)),
         day = day(date)) %>%
  mutate(dow = factor(lubridate::wday(date, week_start = getOption("lubridate.week.start", 1)))) %>%
  mutate(weekend = factor(as.integer(dow %in% c(6,7))))



daily_agg <- daily_agg %>% 
  mutate(school_off = ((day>15 & month==5 & day <=30) | (month==6 |  month==7) |
                         (day<15 & month==8) | (day>20 & month==12) ))

daily_agg <- daily_agg %>% 
  mutate(trend = c(1:dim(daily_agg)[1]))

summary(daily_agg$QUANTITY)



# Get holiday calendar ----------------------------------

holidays <-  as.Date(holidayNYSE(2010:2017))
  
daily_agg <- daily_agg %>% 
  mutate(isHoliday = ifelse(date %in% holidays,1,0))

Hmisc::describe(daily_agg)

# Define vars for analysis ----------------------------------

daily_agg <- 
  daily_agg %>% 
  group_by(month) %>% 
  mutate(q_month = mean(QUANTITY)) %>% 
  ungroup()

hist(daily_agg$q_month) # skewed with right tail

daily_agg <- daily_agg %>% 
  mutate(QUANTITY2 = ifelse(QUANTITY<1, 1, QUANTITY)) %>% 
  mutate(q_ln = log(QUANTITY2))

daily_agg <- 
  daily_agg %>% 
  group_by(month, dow) %>% 
  mutate(tickets = mean(QUANTITY),
         tickets_ln = mean(q_ln)) %>% 
  ungroup()

# named date vars for graphs
mydays <- c("Mon","Tue","Wed",
            "Thu","Fri","Sat",
            "Sun")
daily_agg$dow_abb   <-factor(   mydays[daily_agg$dow],  levels=mydays)
daily_agg$month_abb <-factor(month.abb[daily_agg$month],levels=month.abb)

# get first and second deltas 

daily_agg <- 
    daily_agg %>% mutate(
    d1_q_ln  = q_ln/lag(q_ln,1) ,
    d2_q_ln = q_ln/lag(q_ln,2),
    d1_q_ln = ifelse(is.na(d1_q_ln) | is.infinite(d1_q_ln),0,d1_q_ln),
    d2_q_ln = ifelse(is.na(d2_q_ln) | is.infinite(d2_q_ln),0,d2_q_ln)
) 


################################
# Descriptive graphs ----------
#################################


g1 <-ggplot(data=daily_agg[daily_agg$year==2015,], aes(x=date, y=QUANTITY)) +
  geom_line(size=0.4, color='navyblue') +
  scale_x_date(breaks = as.Date(c("2015-01-01","2015-04-01","2015-07-01","2015-10-01","2016-01-01")),
               labels = scales::date_format("%d%b%Y"),
               date_minor_breaks = "1 month" ) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  scale_color_discrete(name = "")+ theme_minimal()
g1


# seasonality is strong in the summer time, and there are interim spikes in within weeks as well

g2<-ggplot(data=daily_agg[(daily_agg$year>=2010) & (daily_agg$year<=2014),], aes(x=date, y=QUANTITY)) +
  geom_line(size=0.2, color='navyblue') +
  theme_minimal() +
  scale_x_date(breaks = as.Date(c("2010-01-01","2011-01-01","2012-01-01","2013-01-01","2014-01-01","2015-01-01")),
               labels = date_format("%d%b%Y"),
               minor_breaks = "3 months") +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  scale_color_discrete(name = "")
g2

#spikes around summer time are consistent over the 5 years 


g3<-ggplot(data=daily_agg, aes(x=month_abb, y=QUANTITY)) +
  theme_minimal() +
  labs( x = "Date (month)", y="Daily ticket sales" ) +
  geom_boxplot(color='navyblue',outlier.color = 'black', outlier.alpha = 0.6, outlier.size = 0.4)
g3


g4<-ggplot(data=daily_agg, aes(x=dow_abb, y=QUANTITY)) +
  theme_minimal() +
  labs( x = "Day of the week", y="Daily ticket sales" ) +
  geom_boxplot(color='navyblue',outlier.color = 'black', outlier.alpha = 0.6, outlier.size = 0.4)
  #geom_boxplot(color=color[1], outlier.shape = NA)
g4

# to check for interactions, look at the heatmap
swim_heatmap <- 
  ggplot(daily_agg, aes(x = dow_abb, y = month_abb, fill = tickets)) +
  geom_tile(colour = "white") +
  labs(x = 'Day of the week', y = 'Month ') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "D") +
  theme_minimal() +
  theme(legend.position = "right",
    legend.text = element_text(size=6),
    legend.title =element_text(size=6)
    )
swim_heatmap

# not in book
swim_heatmap_log <-
  ggplot(daily_agg, aes(x = dow_abb, y = month_abb, fill = tickets_ln)) +
  geom_tile(colour = "white") +
  labs(x = 'Day of week', y = 'Month ') +
  scale_fill_viridis(alpha = 0.7, begin = 1, end = 0.2, direction = 1, option = "D") +
  theme_minimal()  
swim_heatmap_log

#####################################
# PREDICTION  ----------
#####################################


#############################
# Create train/houldout data
#############################

# Last year of data
data_holdout<- daily_agg %>%
  filter(year==2016)

# Rest of data for training
data_train <- daily_agg %>%
  filter(year<2016)

# Prepare for cross-validation
data_train <- data_train %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.integer(rowname))

test_index_list <- data_train %>% 
  split(f = factor(data_train$year)) %>% 
  lapply(FUN = function(x){x$rowname})

train_index_list <- test_index_list %>% 
  lapply(FUN = function(x){setdiff(data_train$rowname, x)})

  
train_control <- trainControl(
  method = "cv",
  index = train_index_list, #index of train data for each fold
  # indexOut = index of test data for each fold, complement of index by default
  # indexFinal = index of data to use to train final model, whole train data by default
  savePredictions = TRUE
)

# Fit models ---------------------------------------------------------

#Model 1 linear trend + monthly seasonality
model1 <- as.formula(QUANTITY ~ 1 + trend + month)
reg1 <- train(
  model1,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 2 linear trend + monthly seasonality + days of week seasonality 
model2 <- as.formula(QUANTITY ~ 1 + trend + month + dow)
reg2 <- train(
  model2,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 3 linear trend + monthly seasonality + days of week  seasonality + holidays 
model3 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday)
reg3 <- train(
  model3,
  method = "lm",
  data = data_train,
  trControl = train_control
)


#Model 4 linear trend + monthly seasonality + days of week  seasonality + holidays + sch*dow
model4 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday + school_off*dow)
reg4 <- train(
  model4,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 5 linear trend + monthly seasonality + days of week  seasonality + holidays + interactions
model5 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday + school_off*dow + weekend*month)
reg5 <- train(
  model5,
  method = "lm",
  data = data_train,
  trControl = train_control
)

#Model 6 linear trend + monthly seasonality + days of week  seasonality + holidays + interactions + deltas
model6 <- as.formula(QUANTITY ~ 1 + trend + month + dow + isHoliday + school_off*dow + weekend*month + d1_q_ln + d2_q_ln)
reg6 <- train(
    model6,
    method = "lm",
    data = data_train,
    trControl = train_control
)


#Model 7 =  multiplicative trend and seasonality (ie take logs, predict log values and transform back with correction term)
model7 <- as.formula(q_ln ~ 1 + trend + month + dow + isHoliday + school_off*dow + d1_q_ln + d2_q_ln)
reg7 <- train(
  model7,
  method = "lm",
  data = data_train,
  trControl = train_control
)


stargazer(reg2$finalModel, reg3$finalModel, reg4$finalModel, reg5$finalModel, 
          out=paste(output,"Ch18_swim_tsregs.txt",sep=""), type = "text", digits=2)
stargazer(reg6$finalModel, 
          out=paste(output,"Ch18_swim_tsregs2.txt",sep=""), type = "text", digits=2)

# Get CV RMSE ----------------------------------------------

model_names <- c("reg1","reg2","reg3","reg4","reg5", 'reg6')
rmse_CV <- c()

for (i in model_names) {
  rmse_CV[i]  <- get(i)$results$RMSE
}
rmse_CV

#had to cheat and use train error on full train set because could not obtain CV fold train errors
corrb <- mean((reg7$finalModel$residuals)^2)
rmse_CV["reg7"] <- reg7$pred %>% 
  mutate(pred = exp(pred  + corrb/2)) %>% 
  group_by(Resample) %>% 
  summarise(rmse = RMSE(pred, exp(obs))) %>% 
  as.data.frame() %>% 
  summarise(mean(rmse)) %>% 
  as.numeric()

rmse_CV
# Use prophet prediction -------------------------------------------
# add CV into prophet
# can be done with prophet: https://facebook.github.io/prophet/docs/diagnostics.html
# done but this is a different cross-validation as for the other models as it must be time-series like

# prophet -  multiplicative option -- tried but produced much worse results (~34. RMSE)


model_prophet <- prophet( fit=F, 
                          seasonality.mode = "additive", 
                          yearly.seasonality = "auto",
                          weekly.seasonality = "auto",
                          growth = "linear",
                          daily.seasonality=TRUE)

model_prophet <-  add_country_holidays(model_prophet, "US")
model_prophet <- fit.prophet(model_prophet, df= data.frame(ds = data_train$date,
                                                           y = data_train$QUANTITY ))

cv_pred <- cross_validation(model_prophet, initial = 365, period = 365, horizon = 365, units = 'days')
rmse_prophet_cv <- performance_metrics(cv_pred, rolling_window = 1)$rmse
rmse_prophet_cv

####
# Random Forest
####

predictors <- c('trend', 'month', 'dow' ,'isHoliday', 'weekend', 'school_off' ,'d1_q_ln' ,'d2_q_ln')

sqrt(length(predictors))


tune_grid <- expand.grid(
    .mtry = c( 4, 5),
    .splitrule = "variance",
    .min.node.size = c(3,4, 5)
)

set.seed(7)
system.time({
    rf_model <- train(
        formula(paste0("QUANTITY ~", paste0(predictors, collapse = " + "))),
        data = data_train,
        method = "ranger",
        trControl = train_control,
        tuneGrid = tune_grid,
        importance = "impurity"
    )
})

rf_model$results

####
# XGBoost
####

gbm_grid <-  expand.grid(nrounds=c(350),
                         max_depth = c(2,3, 4),
                         eta = c(0.03,0.05, 0.06),
                         gamma = c(0.01),
                         colsample_bytree = c(0.5),
                         subsample = c(0.75),
                         min_child_weight = c(0))


set.seed(7)
xgb_model <- train(
    formula(paste0("QUANTITY ~", paste0(predictors, collapse = " + "))),
    method = "xgbTree",
    #metric = 'NormalizedGini',
    data = data_train,
    tuneGrid = gbm_grid,
    trControl = train_control
)

xgb_model$results

# Check model results

models <- list(
    'OLS1' = reg1, 
    'OLS2' = reg2,
    'OLS3' = reg3,
    'OLS4' = reg4,
    'OLS5' = reg5,
    'OLS6' = reg6,
    'Random Forest' = rf_model,
    'XGBoost'= xgb_model)

results <- resamples(models) %>%  summary()
results

#Based on CV RMSEs Random forest is to be used 

###########################x
# Evaluate best model on holdout set --------------------------------------------
###########################

# Out of curiosity, checking RMSEs on holdout set 

result_holdout <- map(models, ~{
    RMSE(predict(.x, newdata = data_holdout), data_holdout[["QUANTITY"]])
}) %>% unlist() %>% as.data.frame() %>%
    rename("Holdout RMSE" = ".")
result_holdout

result_list_cv = list()
for (i in 1:length(results$models)) {
    result_list[[results$models[i]]] <- results$statistics$RMSE[results$models[i],'Mean']
}
col1 <-names(result_list)
col2  <- result_list %>% unlist
summary_df <- cbind(col1,col2,result_holdout) %>% as.data.frame()

colnames(summary_df) <- c('Models','CV RMSE', 'Holdout RMSE')

# Best holdout RMSE: 

data_holdout <- data_holdout %>% 
  mutate(y_hat = predict(rf_model, newdata = .))

rmse_holdout_best <- RMSE(data_holdout$QUANTITY, data_holdout$y_hat)
rmse_holdout_best

###########################x
# Plot best predictions --------------------------------------------
###########################x

#graph relative RMSE (on holdout) per month 
rmse_monthly <- data_holdout %>% 
  mutate(month = factor(format(date,"%b"), 
                        levels= unique(format(sort(.$date),"%b")), 
                        ordered=TRUE)) %>% 
  group_by(month) %>% 
  summarise(
    RMSE = RMSE(QUANTITY, y_hat),
    RMSE_norm= RMSE(QUANTITY, y_hat)/mean(QUANTITY)
            ) 

g_predictions_rmse<- ggplot(rmse_monthly, aes(x = month, y = RMSE_norm)) +
  geom_col(bg='navyblue', color='navyblue') +
  labs( x = "Date (month)", y="RMSE (normalized by monthly sales)" ) +
    theme_minimal() 
g_predictions_rmse



# predictions are relatively bad in Jan, May, Dec, but quite good in Jun, July

g_predictions<-
  ggplot(data=data_holdout, aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  scale_y_continuous(expand = c(0,0))+
  scale_x_date(expand=c(0,0), breaks = as.Date(c("2016-01-01","2016-03-01","2016-05-01","2016-07-01","2016-09-01","2016-11-01", "2017-01-01")),
               labels = date_format("%d%b%Y"),
               date_minor_breaks = "1 month" )+
  scale_color_manual(values=c('navyblue','purple4'), name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_minimal() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-07-15"), y = 50, label = "Predicted", color=color[2], size=3)+
  #annotate("text", x = as.Date("2016-09-01"), y = 125, label = "Actual", color=color[1], size=3)
  theme(legend.position=c(0.7,0.8),
      legend.direction = "horizontal",
      legend.text = element_text(size = 6),
      legend.key.width = unit(.8, "cm"),
      legend.key.height = unit(.3, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.8))
         )
g_predictions
#save_fig("ch18_swim_predictions", output, "large")


# Preds for Jan 
g_predictions_jan <- ggplot(data=data_holdout %>% filter(month==1), aes(x=date, y=QUANTITY)) +
  geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
  geom_line(aes(y=y_hat, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
  geom_ribbon(aes(ymin=QUANTITY,ymax=y_hat), fill='purple', alpha=0.3) +
  #scale_y_continuous(expand = c(0.01,0.01), limits = c(0,150))+
  scale_x_date(expand=c(0.01,0.01), breaks = as.Date(c("2016-01-01","2016-01-08","2016-01-15","2016-01-22","2016-01-29")),
               limits = as.Date(c("2016-01-01","2016-01-31")),
               labels = date_format("%d%b")) +
  scale_color_manual(values=c('navyblue','purple4'), name="")+
  scale_size_manual(name="", values=c(0.4,0.7))+
  #scale_linetype_manual(name = "", values=c("solid", "solid")) +
  scale_linetype_manual(name = "", values=c("solid", "twodash")) +
  labs( x = "Date (day)", y="Daily ticket sales" ) +
  theme_minimal() +
  #theme(legend.position = "none") +
  #annotate("text", x = as.Date("2016-08-04"), y = 55, label = "Actual", color=color[2], size=2)+
  #annotate("text", x = as.Date("2016-08-17"), y = 115, label = "Predicted", color=color[1], size=2)
  theme(legend.position=c(0.7,0.8),
        legend.direction = "horizontal",
        legend.text = element_text(size = 4),
        legend.key.width = unit(.8, "cm"),
        legend.key.height = unit(.2, "cm")) + 
  guides(linetype = guide_legend(override.aes = list(size = 0.6))
  )


g_predictions_may <- ggplot(data=data_holdout %>% filter(month==5), aes(x=date, y=QUANTITY)) +
    geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
    geom_line(aes(y=y_hat, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
    geom_ribbon(aes(ymin=QUANTITY,ymax=y_hat), fill='purple', alpha=0.3) +
    #scale_y_continuous(expand = c(0.01,0.01), limits = c(0,150))+
    scale_x_date(expand=c(0.01,0.01), breaks = as.Date(c("2016-05-01","2016-05-08","2016-05-15","2016-05-22","2016-05-29")),
                 limits = as.Date(c("2016-05-01","2016-05-31")),
                 labels = date_format("%d%b")) +
    scale_color_manual(values=c('navyblue','purple4'), name="")+
    scale_size_manual(name="", values=c(0.4,0.7))+
    #scale_linetype_manual(name = "", values=c("solid", "solid")) +
    scale_linetype_manual(name = "", values=c("solid", "twodash")) +
    labs( x = "Date (day)", y="Daily ticket sales" ) +
    theme_minimal() +
    #theme(legend.position = "none") +
    #annotate("text", x = as.Date("2016-08-04"), y = 55, label = "Actual", color=color[2], size=2)+
    #annotate("text", x = as.Date("2016-08-17"), y = 115, label = "Predicted", color=color[1], size=2)
    theme(legend.position=c(0.7,0.8),
          legend.direction = "horizontal",
          legend.text = element_text(size = 4),
          legend.key.width = unit(.8, "cm"),
          legend.key.height = unit(.2, "cm")) + 
    guides(linetype = guide_legend(override.aes = list(size = 0.6))
    )

g_predictions_jun <- ggplot(data=data_holdout %>% filter(month==6), aes(x=date, y=QUANTITY)) +
    geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
    geom_line(aes(y=y_hat, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
    geom_ribbon(aes(ymin=QUANTITY,ymax=y_hat), fill='purple', alpha=0.3) +
    #scale_y_continuous(expand = c(0.01,0.01), limits = c(0,150))+
    scale_x_date(expand=c(0.01,0.01), breaks = as.Date(c("2016-06-01","2016-06-08","2016-06-15","2016-06-22","2016-06-29")),
                 limits = as.Date(c("2016-06-01","2016-06-31")),
                 labels = date_format("%d%b")) +
    scale_color_manual(values=c('navyblue','purple4'), name="")+
    scale_size_manual(name="", values=c(0.4,0.7))+
    #scale_linetype_manual(name = "", values=c("solid", "solid")) +
    scale_linetype_manual(name = "", values=c("solid", "twodash")) +
    labs( x = "Date (day)", y="Daily ticket sales" ) +
    theme_minimal() +
    #theme(legend.position = "none") +
    #annotate("text", x = as.Date("2016-08-04"), y = 55, label = "Actual", color=color[2], size=2)+
    #annotate("text", x = as.Date("2016-08-17"), y = 115, label = "Predicted", color=color[1], size=2)
    theme(legend.position=c(0.7,0.8),
          legend.direction = "horizontal",
          legend.text = element_text(size = 4),
          legend.key.width = unit(.8, "cm"),
          legend.key.height = unit(.2, "cm")) + 
    guides(linetype = guide_legend(override.aes = list(size = 0.6))
    )

g_predictions_jul <- ggplot(data=data_holdout %>% filter(month==7), aes(x=date, y=QUANTITY)) +
    geom_line(aes(size="Actual", colour="Actual", linetype = "Actual") ) +
    geom_line(aes(y=y_hat, size="Predicted" ,colour="Predicted",  linetype= "Predicted")) +
    geom_ribbon(aes(ymin=QUANTITY,ymax=y_hat), fill='purple', alpha=0.3) +
    #scale_y_continuous(expand = c(0.01,0.01), limits = c(0,150))+
    scale_x_date(expand=c(0.01,0.01), breaks = as.Date(c("2016-07-01","2016-07-08","2016-07-15","2016-07-22","2016-07-29")),
                 limits = as.Date(c("2016-07-01","2016-07-31")),
                 labels = date_format("%d%b")) +
    scale_color_manual(values=c('navyblue','purple4'), name="")+
    scale_size_manual(name="", values=c(0.4,0.7))+
    #scale_linetype_manual(name = "", values=c("solid", "solid")) +
    scale_linetype_manual(name = "", values=c("solid", "twodash")) +
    labs( x = "Date (day)", y="Daily ticket sales" ) +
    theme_minimal() +
    #theme(legend.position = "none") +
    #annotate("text", x = as.Date("2016-08-04"), y = 55, label = "Actual", color=color[2], size=2)+
    #annotate("text", x = as.Date("2016-08-17"), y = 115, label = "Predicted", color=color[1], size=2)
    theme(legend.position=c(0.7,0.8),
          legend.direction = "horizontal",
          legend.text = element_text(size = 4),
          legend.key.width = unit(.8, "c
                                  m"),
          legend.key.height = unit(.2, "cm")) + 
    guides(linetype = guide_legend(override.aes = list(size = 0.6))
    )

g_predictions_jan
g_predictions_may
g_predictions_jun
g_predictions_jul


# Variable importance

# variable importance data frame
rf_model_var_imp <- importance(rf_model$finalModel)/1000
rf_model_var_imp_df <-
    data.frame(varname = names(rf_model_var_imp),imp = rf_model_var_imp) %>%
    arrange(desc(imp)) %>%
    mutate(imp_percentage = imp/sum(imp))
rf_model_var_imp_df <- rf_model_var_imp_df
# Grouped variable importance

rf_model_var_imp_df <-
rf_model_var_imp_df %>% mutate(
    group = ifelse(grepl('month',varname ), 'month',
                   ifelse( grepl('dow',varname ), 'dow',varname))
)

rf_model_var_imp_grouped <- rf_model_var_imp_df %>%  group_by(group) %>% dplyr::summarise(group_imp_sum = sum(imp_percentage)) %>%  arrange(desc(group_imp_sum))


# variable importance plot - top 10 only
rf_model_var_imp_grouped_plot <-
    ggplot(rf_model_var_imp_grouped, aes(x=reorder(group, group_imp_sum), y=group_imp_sum)) +
    geom_point(color="navyblue", size=1) +
    geom_segment(aes(x=group,xend=group,y=0,yend=group_imp_sum), color="navyblue", size=0.7) +
    ylab("Importance (Percent)") +   xlab("Variable Name") +
    coord_flip() +
    # expand=c(0,0),
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_bw() +
    theme(axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
          axis.title.x = element_text(size=8), axis.title.y = element_text(size=8))
rf_model_var_imp_grouped_plot
