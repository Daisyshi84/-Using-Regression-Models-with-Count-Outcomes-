
######################################################################
#
#   Assessment of what's driving homelessnesss in these big citites
#   
###################################################################### 

#load ggplot2 package
setwd("C:/Users/SSE6/Downloads")
library(ggplot2);library(dplyr);library(plotly);library(tidyverse);  library(psych); library(biostat3);library(AER);
library(gtsummary);

data<- read.csv("what_is_driving_homelessness.csv")

set.seed(1)
#Conducting a Poisson regression will allow you to see which predictor variables (if any) have a statistically significant effect on the response variable.
summary(data)

 
new_data<-data %>% select(-metro_id) %>% 
  mutate(rent=ifelse(median_rent> median(median_rent),"High Rent","Low Rent"),
        wage=ifelse(median_wage> median(median_wage),"High Wage","Low Wage"),
household_income=ifelse(median_household_income>median(median_household_income),"High household_income","Low household_income"),
pop=ifelse(population> quantile(population,0.75),"High","Low"))
        
#check out variance between groups
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




new_data %>% select(-region_name) %>% 
  tbl_summary(by = rent) %>% 
  add_p()
  


#check distribution 
hist(data$pit_homelessness_2019)

#The variance is much greater than the mean, which suggests that we will have over-dispersion in the model.
mean(data$pit_homelessness_2019)
var(data$pit_homelessness_2019)


describeBy(data$pit_homelessness_2019)

data1 <- data %>%
  group_by(region_name) %>%
  summarise(homneless_mean = mean(pit_homelessness_2019)) %>% 
  arrange(desc(homneless_mean)) %>% 
  top_n(10)
 
 

  
# load the library
library(forcats)

# Reorder following the value of another column:
 data1 %>%
  mutate(region_name = fct_reorder(region_name, homneless_mean)) %>%
  ggplot( aes(x=region_name, y = homneless_mean)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Homeless By Region") + ylab("Homeless Mean Value ") +
  theme_bw() +  
  title(main="XXX") 
 
 


#fit the model
model <-glm(pit_homelessness_2019~ population + median_rent + median_wage + 
            pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income+
            unempoyment_rate + pct_hosp_admits_medicaid +pct_snap_participation, 
            family = poisson(link = "log"), data =  new_data )

#view model output
summary(model)
 
#Overdispersion means the assumptions of the model are not met, hence we cannot trust its output;
dispersiontest(model)

# fit quasipoisson model; 
summary(model2 <- glm(pit_homelessness_2019 ~ population + median_rent + median_wage + 
                        pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income+
                        unempoyment_rate + pct_hosp_admits_medicaid +pct_snap_participation, family=quasipoisson, data=new_data))
#fit a reduced model;
summary(model3 <- glm(pit_homelessness_2019 ~  population + median_rent +median_household_income, family=quasipoisson, data=new_data))


# fit quasipoisson model; 
summary(model4 <- glm(pit_homelessness_2019 ~   median_rent + median_wage + 
                        pct_below_avg_wage + pct_below_half_of_avg_wage +median_household_income+
                        unempoyment_rate + pct_hosp_admits_medicaid +pct_snap_participation, family=quasipoisson, data=new_data))
 

#compare the results;#bases on the result, model2 appears best approach;
anova(model1,model2) 
anova(model2,model3 , test = "F")
anova(model2,model4 , test = "F")
 
#get the coefficients and calculate the exp
exp(model4$coefficients)
 
 


#Collecting more Demographic information include age, sex, education,and race etc for future analysis.
