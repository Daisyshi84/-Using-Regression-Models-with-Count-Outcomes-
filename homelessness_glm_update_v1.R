
######################################################################
#   Assessment of what's driving homelessnesss in these big citites
###################################################################### 

#read data and load packages;
data<- read.csv("what_is_driving_homelessness.csv")
setwd("C:/Users/SSE6/Downloads")
library(ggplot2);library(dplyr);library(plotly);library(tidyverse);  library(psych); library(biostat3);library(AER);
library(gtsummary);library(kableExtra);

set.seed(1)
#Conducting a Poisson regression will allow you to see which predictor variables (if any) have a statistically significant effect on the response variable.
summary(data)

# randomly assign new groups based on the median value of rent,wage,pop,income and to see if the variance across the groups
new_data<-data %>% select(-metro_id) %>% 
  mutate(rent=ifelse(median_rent> median(median_rent),"High Rent","Low Rent"),
        wage=ifelse(median_wage> median(median_wage),"High Wage","Low Wage"),
household_income=ifelse(median_household_income>median(median_household_income),"High household_income","Low household_income"),
pop=ifelse(population> quantile(population,0.75),"High","Low"))
        
#check out variance between groups;
new_data %>% 
  filter(pop=="High") %>% 
  select(pit_homelessness_2019) %>% 
  var() / new_data %>% 
  filter(pop=="Low") %>% 
  select(pit_homelessness_2019) %>% 
  var()

new_data %>% 
  filter(household_income=="High household_income") %>% 
  select(pit_homelessness_2019) %>% 
  var() / new_data %>% 
  filter(household_income=="Low household_income") %>% 
  select(pit_homelessness_2019) %>% 
  var()


#summary table
new_data %>% select(-region_name) %>% 
  tbl_summary(by=rent)  %>% 
  add_p()


#check distribution and overall summary for homelessness
describeBy(data$pit_homelessness_2019)
hist(data$pit_homelessness_2019)

#The variance is much greater than the mean, which suggests that we will have over-dispersion in the model.
data %>% 
  summarize(mean=mean(pit_homelessness_2019),var=var(pit_homelessness_2019))


#get top 10 cities for homelessness
data1 <- data %>%
  group_by(region_name) %>%
  summarise(homneless_mean = mean(pit_homelessness_2019)) %>% 
  arrange(desc(homneless_mean)) %>% 
  top_n(10)
 

# plot the top 10 cities
 data1 %>%
  mutate(region_name = fct_reorder(region_name, homneless_mean)) %>%
  ggplot(aes(x=region_name, y = homneless_mean)) +
  geom_bar(stat="identity", fill="#A68060", alpha=.6, width=.8) +
  coord_flip() +
  xlab(" ") + ylab("Figure1: Homelessness Count") +
  theme_bw()  
 
#fit the poisson model
 p_model <-glm(pit_homelessness_2019 ~ offset(log(population)) + median_rent + median_wage + 
            pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income+
            unempoyment_rate + pct_hosp_admits_medicaid + pct_snap_participation, 
            family = poisson(link = "log"), data =  new_data )
#view model output
summary(p_model)
 
#Over dispersion means the assumptions of the model are not met, hence we cannot trust its output;
dispersiontest(model)


#fit negative binomial regression model
summary(nb_model <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent + median_wage + 
                     pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income+
                     unempoyment_rate + pct_hosp_admits_medicaid + pct_snap_participation, 
                    data =  new_data ))
par(mfrow = c(1, 2))    
#Residual plot for Poisson regression
p_res <- resid(p_model)
plot(fitted(p_model), p_res, col='steelblue', pch=16,
     xlab='Predicted Offers', ylab='Standardized Residuals', main='Poisson')
abline(0,0)

#Residual plot for negative binomial regression 
nb_res <- resid(nb_model)
plot(fitted(nb_model), nb_res, col='steelblue', pch=16,
     xlab='Predicted Offers', ylab='Standardized Residuals', main='Negative Binomial')
abline(0,0)


pchisq(2 * (logLik(nb_model) - logLik(p_model)), df = 1, lower.tail = FALSE)

 
summary(nb1<- glm.nb(pit_homelessness_2019 ~ median_rent , 
                     data =  new_data ))
par(mfrow = c(1, 2))    
#Residual plot for Poisson regression
p_res <- resid(nb1)
plot(fitted(nb1), p_res, col='steelblue', pch=16,
     xlab='Predicted Offers', ylab='Standardized Residuals', main='median_rent')
abline(0,0)
summary(nb2<- glm.nb(pit_homelessness_2019 ~ unempoyment_rate , 
                     data =  new_data ))
     
#Residual plot for Poisson regression
p_res <- resid(nb2)
plot(fitted(nb2), p_res, col='steelblue', pch=16,
     xlab='Predicted Offers', ylab='Standardized Residuals', main='unempoyment_rate')
abline(0,0)


 
# define the NB models to compare 
cand.models <- list( ) 
cand.models[[1]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent + median_wage + 
                             pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income+
                             unempoyment_rate + pct_hosp_admits_medicaid + pct_snap_participation, 
                           data =  new_data ) 
cand.models[[2]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent + median_wage + 
                             pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income+
                             unempoyment_rate + pct_hosp_admits_medicaid, 
                           data =  new_data ) 
cand.models[[3]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent + median_wage + 
                             pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income+
                             unempoyment_rate, 
                           data =  new_data ) 
cand.models[[4]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent + median_wage + 
                             pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income, 
                           data =  new_data) 
cand.models[[5]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent + median_wage + 
                             pct_below_avg_wage,
                           data =  new_data) 
cand.models[[6]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent + unempoyment_rate ,
                           data =  new_data) 
cand.models[[7]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent,
                           data =  new_data) 
cand.models[[8]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) ,
                           data =  new_data) 
cand.models[[9]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + median_rent + log(unempoyment_rate) ,
                           data =  new_data) 
cand.models[[10]] <- glm.nb(pit_homelessness_2019 ~ offset(log(population)) + log(median_rent) + log(unempoyment_rate) ,
                           data =  new_data) 
 
# name the models 
model.names <- c("1", "2", "3", "4", "5", "6", "7", "8","9","10")
                 
names(cand.models) <- model.names 
# calculate and combine AIC, AIC weights, and BIC 
results <- data.frame(models = model.names) 
results$bic.val <- unlist(lapply(cand.models, BIC)) 
results$bic.rank <- rank(results$bic.val) 
results$aic.val <- unlist(lapply(cand.models, AIC)) 
results$aic.delta <- results$aic.val-min(results$aic.val) 

results$aic.likelihood <- exp(-0.5* results$aic.delta) 
results$aic.weight <- results$aic.likelihood/sum(results$aic.likelihood) 
# sort models by AIC weight 
results <- results[rev(order(results[, "aic.weight"])),] 
results$cum.aic.weight <- cumsum(results[, "aic.weight"])


#Results for Final Count Regression Model (Negative Binomial)
results


final_model<-glm.nb(pit_homelessness_2019 ~ offset(log(population)) + log(median_rent) + log(unempoyment_rate) ,
                    data =  new_data) 

#model 6 appears best across all model compassion

summary(final_model)
coef(summary(final_model))

confint(final_model)
rm(table)
table<-cbind(estimate = coef(summary(final_model)),
          confint(final_model))

kable(table, escape=F, align=c("l",rep("r",8))) %>%
  kable_styling(full_width = F,position="left")

 
exp(1.6838*log(1.1))
exp(1.4067*log(1.05))
exp(1.4067*log(1.1))

