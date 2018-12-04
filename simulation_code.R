# LIBRARIES/PRELIMINARIES ----
library(tidyverse)
library(survival)
library(lubridate)
set.seed(100)

lag_functions <- setNames(c(paste("(", paste("dplyr::lag(., ", 1:7, ")", collapse = ' + '), ") / 7"),
                            paste("(", paste("dplyr::lag(., ", 1:28, ")", collapse = ' + '), ") / 28")), 
                          c("acute", "chronic"))

# SIMULATE DATA ----

dates <- seq(from = as.Date('2017-07-01'), to = as.Date('2018-06-30'), by = 'days')

session_load <- rep(0, 365)
session_load[dates >= as.Date('2017-08-01') & dates <= as.Date('2017-08-21')] <- 60 + sample(0:40, size = 21, replace = TRUE) # Training Camp
session_load[dates >= as.Date('2017-08-22') & dates <= as.Date('2018-05-31') & lubridate::wday(dates) == 7] <- 70 # Game days
session_load[dates >= as.Date('2017-08-22') & dates <= as.Date('2018-05-31') & lubridate::wday(dates) %in% c(2:6)] <- sample(0:40, size = 203, replace = TRUE) # Practice days
session_load[week(dates) %in% c(38, 51, 5, 6, 13)] <- 0 # International Breaks

load_data <- data.frame(
  dates,
  session_load,
  stringsAsFactors = FALSE
)  %>%
  mutate_at(vars(session_load), funs_(lag_functions))


# PLOTS ----

# * Load over time ----
ggplot(load_data, aes(x = dates, y = session_load)) + 
  geom_point() + 
  geom_line(aes(x = dates, y = acute), col = 'red') + 
  geom_line(aes(x = dates, y = chronic), col = 'blue') + 
  xlab("Date") + 
  ylab("Player Load")

# * Load density ----

load_data %>%
  mutate(acute_chronic_ratio = acute / chronic,
         ac_flag = ifelse(acute_chronic_ratio < .8 | acute_chronic_ratio > 1.3, 1, 0),
         ac_flag = as.factor(ac_flag)) %>% 
  filter(!is.na(ac_flag)) %>%
  mutate(ac_flag = forcats::fct_recode(ac_flag, 
                                       'Flag' = '1',
                                       'No Flag' = '0')) %>%
  filter(session_load > 0) %>%
  ggplot( aes(x = session_load, group = ac_flag, color = ac_flag)) + 
  geom_density(alpha = .2) + 
  labs(y = 'Density', 
       x = 'Percent of Max Load') + 
  guides(color=guide_legend(title="AC Flag"))


# SIMULATE 1000 SEASONS ----

player_data_complete = NULL

for(i in 1:1000){ 

  # * Simulate injuries ----  
  player_data <- load_data %>%
    mutate(injury = rbinom(n = length(dates), size = 1, prob = session_load / 500)) %>% # Tune for baseline injury rate
    mutate_at(vars(session_load), funs_(lag_functions)) %>% 
    mutate(acute_chronic_ratio = acute / chronic,
           ac_flag = ifelse(acute_chronic_ratio < .8 | acute_chronic_ratio > 1.3, 1, 0)) %>%
    filter(session_load > 0)
  
  player_data_complete <- rbind(player_data_complete, player_data) # bad practice -- don't do this...
  
}

# * Test of proportions ----
prop.test(rbind(table(player_data_complete$injury[player_data_complete$ac_flag == 1]), 
                  table(player_data_complete$injury[player_data_complete$ac_flag == 0])))

# * Logistic reg w/ and w/o session_load ----
summary(glm(injury ~ ac_flag, 
            data = player_data_complete, 
            family = 'binomial')) # AC is significant!
summary(glm(injury ~ ac_flag + session_load + I(session_load^2) + I(session_load^3), 
            data = player_data_complete, 
            family = 'binomial')) # But not if we account for session load!


# * Cox model ----

augmented_data <- player_data_complete %>%
  mutate(inj_time = ifelse(injury == 1, session_load * runif(nrow(player_data_complete)), session_load)) 

cox_model <- coxph(Surv(inj_time, injury) ~ ac_flag, augmented_data) 
summary(cox_model) # AC flag not significant






