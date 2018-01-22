library(tidyverse)
library(ggplot2)
library(broom)
library(dplyr)
library(psych)
library(pipeliner)
library(forcats)
library(stringr)
library(stargazer)
library(foreign)
library(pipeR)
install.packages(mosaic)
library(mosaic)
library(tigerstats)
install.packages("mlogit")
library(mlogit)
install.packages("pipeR")
install.packages("lm.beta")
library(lm.beta)
install.packages("ggfortify")
library(ggfortify)
getwd()
setwd("/Users/afework/Desktop/Assignment_3")
spss_orphan_children_data <- read.spss("spss_orphan_children_data.sav", to.data.frame = TRUE, use.value.labels = TRUE,
                                       use.missings = TRUE)
str(spss_orphan_children_data)
read.spss(spss_orphan_children_data)
head(spss_orphan_children_data)

new_data <- spss_orphan_children_data %>% 
  select(age, sex, grade, parentalStatus, missedParent, currentLiving, PRtotal, EMtotal, SAtotal, PGtotal, PLtotal, PWBtotal)
    
summary(spss_orphan_children_data)
class(sex)
class(parentalStatus)
class(age)

attach(spss_orphan_children_data)
attach(Non_orphan)

Non_orphan = new_data[1 : 120, ]
head(Non_orphan)
orphan = new_data[121 : 240, ]



### filter of orhan and non orphan children. 
Non_orphan <- 
spss_orphan_children_data %>% 
  filter(parentalStatus == "yes")


Non_orphan <- filter(spss_orphan_children_data, parentalStatus == "yes")
Orphan_children <- filter(spss_orphan_children_data, parentalStatus == "no")


# Exploratory Data Analysis
### bar_plot of parental status  


new_data %>%
  ggplot() +
  aes(x=parentalStatus, fill = sex) +
  geom_bar()

### bar_plot of currentliving place of orphan 


ggplot(data = orphan) +
  geom_bar(mapping = aes(x = currentLiving, fill = sex))


###  looking the distribution of PWBtotal by parentalStatus using geom_boxplot

ggplot(data = new_data, mapping = aes(x = parentalStatus, y = PWBtotal)) +
  geom_boxplot(fill = "blue", color = "red", outlier.shape = 23) + theme_grey()

### looking the distribution of PWBtotal by current living place 

ggplot(data = orphan, mapping = aes(x = currentLiving, y = PWBtotal)) +
  geom_boxplot()


ggplot(data = new_data) +
  geom_boxplot(mapping = aes(x = reorder(currentLiving, PWBtotal, FUN = median), y = PWBtotal))

#### looking the difference of pyschological wellbeing in terms of sex 

ggplot(data = new_data) +
  geom_boxplot(mapping = aes(x = sex, y = PWBtotal))

#### Frequency polygons plot 
ggplot(data = new_data, mapping = aes(x = grade, colour = currentLiving)) +
  geom_freqpoly(binwidth = 0.1)


### histogram plot 
ggplot(spss_orphan_children_data, aes( x = PWBtotal, colour = currentLiving)) + 
  geom_histogram ()

ggplot(spss_orphan_children_data, aes(x = (parentalStatus))) +
  geom_histogram()

plot(sex, PWBtotal, main = "scatterplot")


## Simple linear regression
#### Create simple linear regression of psychological wellbeing on children age.

model1 <- lm(PWBtotal ~ age, new_data)
summary(model1)
coef(model1)

new_data %>% 
  lm(PWBtotal ~ age, data = .) %>% 
  summary()


# Plot the linear regression

new_data %>% 
  ggplot() +
  aes(y = PWBtotal, x = age) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# tidy() puts returns the model summary in a neat data frame format
tidy(model1)

# Augments adds
augment(model1, new_data)

# Glimpse returns important model performance metrics.
glance(model1)

# To get the standardized coefficients (scale free)

model1_std <- lm(scale(PWBtotal) ~ scale(age), data = new_data)
summary(model1_std)

# The slope of age on the correlation between psychological wellbeing of children and age

cor(new_data$PWBtotal, new_data$age)

## Residual diagnostics

new_data %>% 
  augment(lm(PWBtotal ~ age, data = .), .) %>% 
  ggplot() +
  aes(.resid) +
  geom_histogram(bins = 5)


# This plots shows the total variance of the outcome variable (summary of the blue lines)
new_data %>% 
  mutate(mean_PWBtotal = mean(PWBtotal)) %>% 
  ggplot() +
  aes(y = PWBtotal, x = age) +
  geom_hline(aes(yintercept = mean_PWBtotal), size = 1.5) +
  geom_segment(aes(xend = age, yend = mean_PWBtotal), linetype = "dashed", color = "blue", size = 1.2) +
  geom_point(size = 3) +
  ggtitle("Difference between the grand mean (baseline model)\nand the observed values", subtitle = "Total variance") +
  theme(plot.title=element_text(size = 18, face = "bold"),
        plot.subtitle=element_text(size = 16))


# Improvement of the fit by using the model, compared to only using the mean
# This plots shows the total variance of the outcome variable (summary of the blue lines)
orphan %>% 
  augment(lm(PWBtotal ~ age, data = .), .) %>% 
  mutate(mean_PWBtotal = mean(PWBtotal)) %>% 
  ggplot() +
  aes(y = PWBtotal, x = age) +
  geom_hline(aes(yintercept = mean_PWBtotal), size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, size = 1.5, color = "black") +
  geom_segment(aes(xend = age, yend = mean_PWBtotal, y = .fitted), linetype = "dashed", color = "purple", size = 1.2) +
  geom_point(size = 2) +
  ggtitle("Difference between the model\nand the baseline model (grand mean)", subtitle = "Model improvement") +
  theme(plot.title=element_text(size = 18, face = "bold"),
        plot.subtitle=element_text(size = 16))



# The Shapiro-Wilks test

new_data %>% 
  augment(lm(PWBtotal ~ age, data = .), .) %>% 
  pull(.resid) %>% 
  shapiro.test(.)

#### prediction
predict(model1, data.frame("age" = 18))


### simple linear regression for orphan and non orphan separately 

model2 <- lm(PWBtotal ~ grade, new_data)
summary(model2)
coef(model2)

### Plot the linear regression
ggplot(new_data, aes(x = grade, y = PWBtotal)) + 
  geom_point(color = 'blue', alpha = 0.5) +
  geom_smooth(method = 'lm', color = 'red')

lm_orphan <- lm(PWBtotal ~ currentLiving, data = orphan)
summary(lm_orphan)


###### MULTIPLE REGRESSION

lm1 <- lm(PWBtotal ~ age + parentalStatus , data = new_data)
summary(lm1)
tidy(lm1)


lm2 <- lm(PWBtotal ~ parentalStatus * age, data = new_data)
summary(lm2)
tidy(lm2)

lm3 <- lm(PWBtotal ~ parentalStatus : age, data = new_data)
summary(lm3)
tidy(lm3)

lm4 <- lm(PWBtotal ~ parentalStatus : age + fct_relabel(type, "grade"), data = new_data)

# To get the table in the console, use the type = "text" argument.
stargazer(lm1, lm2, title = "Results", align = TRUE, type = "text")


##### Model selection
glance(lm1)
glance(lm2)
glance(lm3)

#### Compare the logLik models using the anova() function.
anova(lm1, lm2, lm3)


#### Create standardized versions from all objects
lm1_std <- lm.beta(lm1)
lm2_std <- lm.beta(lm2)
lm3_std <- lm.beta(lm3)



results_table_html <-
  stargazer(lm1_std,
            lm2_std,
            lm3_std,
            coef = list(lm1_std$standardized.coefficients,
                        lm2_std$standardized.coefficients,
                        lm3_std$standardized.coefficients),
            title = "Model comparison",
            dep.var.labels = "psychological wellbeing",
            align = TRUE,
            ci = TRUE,
            df = TRUE,
            digits = 2,
            type = "html")


ggplot(new_data, aes(x = grade, y = PWBtotal)) + 
  geom_point(color = 'blue', alpha = 0.5) +
  geom_smooth(method = 'lm', color = 'red')


##### Fit logistic binomial regression
orphan_fit <- glm(sex ~ parentalStatus, family = "binomial", data = new_data)
summary(orphan_fit)
tidy(orphan_fit)
glance(orphan_fit)

# To get the odds ratio, use the exp() function on the coefficients
exp(orphan_fit$coefficients)
# Calculate confidence intervals for the ORs
exp(confint(orphan_fit))



## correlation 

plot(new_data$age, new_data$PWBtotal, new_data$sex) 

pairs(new_data)
cor(new_data)
