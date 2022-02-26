library(tidyverse)
library(knitr)
library(broom)
library(leaps)
library(rms)
library(Sleuth3)


sat_scores <- Sleuth3::case1201 
full_model <- lm(SAT ~ Takers + Income + Years + Public + Expend + Rank , data = sat_scores)
tidy(full_model)

model_select <- regsubsets(SAT ~ Takers + Income + Years + Public + Expend + 
                             Rank , data = sat_scores, method = "backward")

select_summary <- summary(model_select)

select_summary$obj

select_summary$adjr2

select_summary$

model_select_aic <- step(full_model, direction = "backward")
tidy(model_select_aic)

sat_scores <- augment(model_select_aic, sat_scores)

sat_scores$obs_num <- seq.int(nrow(sat_scores))

head(sat_scores, 5)

p <- ggplot(sat_scores, aes(x=obs_num, y=.hat)) + geom_point() + geom_hline(yintercept=0.24) + xlab("Observation Number") +
  ylab("Observation Leverage") + ggtitle("Leverage of Observations")
p

high_leverage_states <- filter(sat_scores, .hat > 0.24)
head(high_leverage_states)

p <- ggplot(sat_scores, aes(x=.fitted, y=.std.resid)) + geom_point() + geom_hline(yintercept=2) + geom_hline(yintercept=-2) + xlab("Prediction") + ylab("Standard Residuals") + ggtitle("Standardized Residuals vs. Predicted Values")
p

out_of_bouds_std_resid <- filter(sat_scores, .std.resid < -2 | .std.resid > 2)
head(out_of_bouds_std_resid)

p <- ggplot(sat_scores, aes(x=obs_num, y=.cooksd)) + geom_point() + geom_hline(yintercept=1) + xlab("Observation Number") + ylab("Cook's Distance") + ggtitle("Cook's Distance of Each Observation")
p

out_of_bounds_cooksd <- filter(sat_scores, .cooksd > 1)
head(out_of_bounds_cooksd)