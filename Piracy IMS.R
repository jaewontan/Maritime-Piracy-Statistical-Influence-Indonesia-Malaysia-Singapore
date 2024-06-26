#Piracy Spatial Regression Analysis

library(dplyr)
library(spdep)
library(spatialreg)
library(car)
library(leaflet)
library(sf)
library(sp)
library(readxl)
library(ggplot2)
library(tmap)
library(flextable)
library(stargazer)
library(corrr)
library(corrplot)
library(ggcorrplot)
library(tidyverse)  # data wrangling
library(broom)  # tidy regression output
library(mosaic)

# Load the data
merged_data <- read_xlsx(file.choose())

# Select only 'Year' and 'Country' columns from original data frame
merged_data <- dplyr::select(merged_data, country, Year, pirateattacks, Capture_fisheries_production, 
                             unemployment_rate, state_fragility, 
                             mobile_cellular_subscriptions_per_100_people,
                             total_merchandise_imports_exports)

# Remove NA values from the merged data
merged_data <- merged_data %>%
  na.omit()

# Filter for selected countries in the "country" column
merged_data <- merged_data[merged_data$country %in% c("SGP", "IDN", "MYS"),]

mean(merged_data$pirateattacks)
var(merged_data$pirateattacks)

#Done because log of zero is inf
merged_data$pirateattacks <- merged_data$pirateattacks + 1

summary(merged_data)

library(vtable)
st(merged_data)





library(moments)

# Calculate skewness, kurtosis, and Jarque-Bera test
skewness_kurtosis_test <- function(data, variables) {
  results <- data.frame(variable = character(), skewness = numeric(), kurtosis = numeric(), 
                        jarque_bera = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
  for (var in variables) {
    results <- rbind(results, data.frame(
      variable = var,
      skewness = skewness(data[[var]]),
      kurtosis = kurtosis(data[[var]]),
      jarque_bera = jarque.test(data[[var]])$statistic,
      p_value = jarque.test(data[[var]])$p.value
    ))
  }
  return(results)
}
#Log transformation of the skewed data:
merged_data$pirateattackslog <- log(merged_data$pirateattacks)

mean(merged_data$pirateattackslog)
var(merged_data$pirateattackslog)

summary(merged_data)

library(vtable)
st(merged_data)


# Select the variables of interest
variables <- c("pirateattacks", "pirateattackslog","Capture_fisheries_production", "unemployment_rate",
               "state_fragility", "mobile_cellular_subscriptions_per_100_people",
               "total_merchandise_imports_exports")

# Calculate the results
results <- skewness_kurtosis_test(merged_data, variables)

# Print the results as a table
print(results)


results %>% 
  as_flextable()

st(results)

shapiro.test(merged_data$pirateattackslog)

# loading the required package
library("dgof")
ks.test(merged_data$pirateattackslog, "pnorm")


#calculate skewness
#A zero means no skewness at all (normal distribution)
#A negative value means the distribution is negatively skewed
#A positive value means the distribution is positively skewed
library(moments)
skewness(merged_data$pirateattacks)
kurtosis(merged_data$pirateattacks)
jarque.test(merged_data$pirateattacks)

library(ggpubr)

# Distribution of Pirate attacks variable
ggdensity(merged_data, x = "pirateattacks", fill = "lightgray", title = "Pirate Attacks") +
  scale_x_continuous(limits = c(0, 150)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

skewness(merged_data$pirateattacks, na.rm = TRUE)




# Distribution of Pirate Attacks variable logged
ggdensity(merged_data, x = "pirateattackslog", fill = "lightgray", title = "Pirate Attacks Logged") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")


#calculate skewness
#A zero means no skewness at all (normal distribution)
#A negative value means the distribution is negatively skewed
#A positive value means the distribution is positively skewed
skewness(merged_data$pirateattackslog, na.rm = TRUE)

merged_data %>%
  ggplot() +
  geom_histogram(aes(x=pirateattackslog), binwidth = 0.2) +
  xlab("Piracy Attacks")

# Add a new column named pirateattackzscore with z-scores of pirateattacks
merged_data <- mutate(merged_data, pirateattackzscore = (pirateattacks - mean(pirateattacks)) / sd(pirateattacks))

# Distribution of Pirate Attacks variable z-score
ggdensity(merged_data, x = "pirateattackzscore", fill = "lightgray", title = "Pirate Attacks Z-scores") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#calculate skewness
#A zero means no skewness at all (normal distribution)
#A negative value means the distribution is negatively skewed
#A positive value means the distribution is positively skewed
skewness(merged_data$pirateattackzscore, na.rm = TRUE)

merged_data %>%
  ggplot() +
  geom_histogram(aes(x=pirateattackzscore), binwidth = 0.1) +
  xlab("Piracy Attacks")

# Simple Histogram for Pirate Attacks
hist(merged_data$pirateattacks)

# Simple Histogram for Logged data
hist(merged_data$pirateattackslog)

# Simple Histogram for Z scores
hist(merged_data$pirateattackzscore)

# Coloured Histogram with Different Number of Bins
hist(merged_data$pirateattackslog, breaks=50, col="grey")

# Add a Normal Curve
x <- merged_data$pirateattackslog
h<-hist(x, breaks=10, col="grey", xlab="Pirate Attacks Logged",
        main="Pirate Attacks Logged Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)

lines(xfit, yfit, col="blue", lwd=2)

# Kernel Density Plot
d <- density(merged_data$pirateattackslog) # returns the density data
plot(d) # plots the results

# Filled Density Plot
d <- density(merged_data$pirateattackslog)
plot(d, main="Kernel Density of Piracy Attacks Logged")
polygon(d, col="grey", border="blue")


# Compare  distributions by country
library(sm)
attach(merged_data)

# create value labels
country.f <- factor(country, levels= c("IDN","MYS","SGP"),
                    labels = c("Indonesia","Malaysia","Singapore"))

# plot densities
sm.density.compare(pirateattackslog, country, xlab="Pirate Attacks")
title(main="Pirate Attacks by Country")

# add legend via mouse click
colfill<-c(2:(2+length(levels(country.f))))
legend(locator(1), levels(country.f), fill=colfill)



# Check for multicollinearity
formula_step1 <- pirateattackslog ~  Capture_fisheries_production + unemployment_rate + 
  state_fragility + mobile_cellular_subscriptions_per_100_people + total_merchandise_imports_exports
vif_values_step1 <- vif(lm(formula_step1, data = merged_data))
print(vif_values_step1)


library(MASS)
#standardize the data
#std_data <- merged_data %>% mutate_at(c('total_fisheries_per_ton','unemployment_rate', 'state_fragility', 'Capture_fisheries_production', 'total_military', 'GDP_per_capita_current', 'population'), ~(scale(.) %>% as.vector))

formula_vif <- pirateattacks ~ Capture_fisheries_production + unemployment_rate + 
  state_fragility + mobile_cellular_subscriptions_per_100_people + total_merchandise_imports_exports
vif_values <- vif(lm(formula_vif, data = merged_data))
print(vif_values)

formula_vif2 <- pirateattacks ~ unemployment_rate + state_fragility + Capture_fisheries_production
vif_values2 <- vif(lm(formula_vif2, data = merged_data))
print(vif_values2)

# Simplify the model by removing variables with high VIF
formula_simple <- pirateattackslog ~  Capture_fisheries_production + unemployment_rate + 
  state_fragility + mobile_cellular_subscriptions_per_100_people + total_merchandise_imports_exports

library(car)
library(jtools)
library(ggplot2)
library(broom.mixed)
library(interactions)

# Multiple Linear Regression
fit.ols.multiple <- lm(pirateattackslog ~  Capture_fisheries_production + unemployment_rate +
                         state_fragility + mobile_cellular_subscriptions_per_100_people +
                         total_merchandise_imports_exports, data = merged_data)
summary(fit.ols.multiple)

library(jtools)
library(modelsummary)
library(pixiedust)
summ(fit.ols.multiple)

fit.ols.multiple %>% 
  as_flextable()

dust(fit.ols.multiple)
modelsummary(as)

dust(fit.ols.multiple) %>%
  sprinkle(col = 2, round = 10) %>%
  sprinkle(col = 3, round = 10) %>%
  sprinkle(col = 4, round = 3) %>%
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_colnames(term = "Variable",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "T-statistic",
                    p.value = "P-value") %>%
kable() %>%
kable_styling()

library(strengejacke)
tab_model(fit.ols.multiple)

anova(fit.ols.multiple)

plot( x = fit.ols.multiple, pch = 19)

# Multiple Regression Plot
#load car package
library(car)

#produce added variable plots for multiple regression 
avPlots(fit.ols.multiple)

# Simple Linear Regression
fit.ols.simple <- lm(pirateattackslog ~ Capture_fisheries_production, data = merged_data)
summary(fit.ols.simple)

fit.ols.simple2 <- lm(pirateattacks ~ Capture_fisheries_production, data = merged_data)
summary(fit.ols.simple2)

fit.ols.simple %>% 
  as_flextable()

# Simple Regression Plot
#load car package
library(car)

#produce added variable plots for simple regression 
avPlots(fit.ols.simple2)


#First Simple regression plot for pirateattackslog ~ Capture_fisheries_production
simple_plot <- lm(pirateattackslog ~ Capture_fisheries_production, data = merged_data)
summary(simple_plot)
simple_plot %>% 
  as_flextable()
plot(pirateattackslog ~ Capture_fisheries_production, data = merged_data)
abline(simple_plot)

#First multiple regression plot for pirateattackslog ~ Capture_fisheries_production
sp1 <- lm(pirateattackslog ~ Capture_fisheries_production, data = merged_data)
summary(sp1)
sp1 %>% 
  as_flextable()
plot(pirateattackslog ~ Capture_fisheries_production, data = merged_data)
abline(sp1)


library(ggplot2)

ggplot(merged_data, aes(x = Capture_fisheries_production, y = pirateattackslog )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplotRegression <- function (simple_plot) {
  
  require(ggplot2)
  
  ggplot(simple_plot$model, aes_string(x = names(simple_plot$model)[2], y = names(simple_plot$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(simple_plot)$adj.r.squared, 5),
                       "Intercept =",signif(simple_plot$coef[[1]],5 ),
                       " Slope =",signif(simple_plot$coef[[2]], 5),
                       " P =",signif(summary(simple_plot)$coef[2,4], 5)))
}

ggplotRegression(lm(pirateattackslog ~ Capture_fisheries_production, data = merged_data))


#Second Simple regression plot for pirateattackslog ~ state_fragility
sp2 <- lm(pirateattackslog ~ state_fragility, data = merged_data)
summary(sp2)
sp2 %>% 
  as_flextable()
plot(pirateattackslog ~ state_fragility, data = merged_data)
abline(sp2)

library(ggplot2)

ggplot(merged_data, aes(x = state_fragility, y = pirateattackslog )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplotRegression2 <- function (sp2) {
  
  require(ggplot2)
  
  ggplot(sp2$model, aes_string(x = names(sp2$model)[2], y = names(sp2$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(sp2)$adj.r.squared, 5),
                       "Intercept =",signif(sp2$coef[[1]],5 ),
                       " Slope =",signif(sp2$coef[[2]], 5),
                       " P =",signif(summary(sp2)$coef[2,4], 5)))
}

ggplotRegression2(lm(pirateattackslog ~ state_fragility, data = merged_data))


#Third Simple regression plot for pirateattackslog ~ total_merchandise_imports_exports
sp3 <- lm(pirateattackslog ~ total_merchandise_imports_exports, data = merged_data)
summary(sp3)
sp3 %>% 
  as_flextable()
plot(pirateattackslog ~ total_merchandise_imports_exports, data = merged_data)
abline(sp3)

library(ggplot2)

ggplot(merged_data, aes(x = total_merchandise_imports_exports, y = pirateattackslog )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplotRegression3 <- function (sp3) {
  
  require(ggplot2)
  
  ggplot(sp3$model, aes_string(x = names(sp3$model)[2], y = names(sp3$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(sp3)$adj.r.squared, 5),
                       "Intercept =",signif(sp3$coef[[1]],5 ),
                       " Slope =",signif(sp3$coef[[2]], 5),
                       " P =",signif(summary(sp3)$coef[2,4], 5)))
}

ggplotRegression3(lm(pirateattackslog ~ total_merchandise_imports_exports, data = merged_data))



#Fourth Simple regression plot for pirateattackslog ~ unemployment_rate
library(tidyverse)
library(caret)
sp4 <- lm(pirateattackslog ~ unemployment_rate, data = merged_data)
summary(sp4)
sp4 %>% 
  as_flextable()
plot(pirateattackslog ~ unemployment_rate, data = merged_data)
abline(sp4)

library(ggplot2)

ggplot(merged_data, aes(x = unemployment_rate, y = pirateattackslog )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplotRegression4 <- function (sp4) {
  
  require(ggplot2)
  
  ggplot(sp4$model, aes_string(x = names(sp4$model)[2], y = names(sp4$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(sp4)$adj.r.squared, 5),
                       "Intercept =",signif(sp4$coef[[1]],5 ),
                       " Slope =",signif(sp4$coef[[2]], 5),
                       " P =",signif(summary(sp4)$coef[2,4], 5)))
}

ggplotRegression4(lm(pirateattackslog ~ unemployment_rate, data = merged_data))



#Fifth Simple regression plot for pirateattackslog ~ mobile_cellular_subscriptions_per_100_people
library(tidyverse)
library(caret)
sp5 <- lm(pirateattackslog ~ mobile_cellular_subscriptions_per_100_people, data = merged_data)
summary(sp5)
sp5 %>% 
  as_flextable()
plot(pirateattackslog ~ mobile_cellular_subscriptions_per_100_people, data = merged_data)
abline(sp5)

library(ggplot2)

ggplot(merged_data, aes(x = mobile_cellular_subscriptions_per_100_people, y = pirateattackslog )) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

ggplotRegression5 <- function (sp5) {
  
  require(ggplot2)
  
  ggplot(sp5$model, aes_string(x = names(sp5$model)[2], y = names(sp5$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(sp5)$adj.r.squared, 5),
                       "Intercept =",signif(sp5$coef[[1]],5 ),
                       " Slope =",signif(sp5$coef[[2]], 5),
                       " P =",signif(summary(sp5)$coef[2,4], 5)))
}

ggplotRegression5(lm(pirateattackslog ~ mobile_cellular_subscriptions_per_100_people, data = merged_data))



# Print models side by side
library(stargazer)
stargazer(sp1, sp2, sp3, sp4, sp5, type = "text",
          title="Title: Regression Results")

stargazer(sp1, sp2, sp3, sp4, sp5, type="html", out="test.html", out.header=TRUE)



#Diagnostics ########
#Assess the residuals from the regression model. Checking to see if it breaks any of the OLS assumptions particularly that errors are nornally distrubuted
ggplot() + 
  geom_histogram(mapping = aes(x=resid(fit.ols.multiple))) +
  xlab("OLS residuals")

# Plots the quantile of the residuals against the expected quantile of the standard normal distribution
qqPlot(fit.ols.multiple)

#Check that errors are not heteroskedastic - that is the variance of residuals are constant
plot(resid(fit.ols.multiple))

merged_data <- merged_data %>%
  mutate(olsresid = resid(fit.ols.multiple))



#Piracy Regression Analysis Poisson et al

library(dplyr)
library(spdep)
library(spatialreg)
library(car)
library(leaflet)
library(sf)
library(sp)
library(readxl)
library(ggplot2)
library(tmap)
library(flextable)
library(stargazer)
library(corrr)
library(corrplot)
library(ggcorrplot)
library(tidyverse)  # data wrangling
library(broom)  # tidy regression output
library(mosaic)

# Load the data
merged_data <- read_xlsx("/Users/jevondixon/Desktop/Dissertation/Dataset/Piracy_Prof.xlsx")

# Select only 'Year' and 'Country' columns from original data frame
merged_data <- dplyr::select(merged_data, country, Year, pirateattacks, Capture_fisheries_production, 
                             unemployment_rate, state_fragility, 
                             mobile_cellular_subscriptions_per_100_people,
                             total_merchandise_imports_exports)

# Remove NA values from the merged data
merged_data <- merged_data %>%
  na.omit()

# Filter for selected countries in the "country" column
merged_data <- merged_data[merged_data$country %in% c("SGP", "IDN", "MYS"),]

#Poisson Regression
# Fit the Poisson distribution to the data
poisson_fit <- fitdistr(merged_data$pirateattacks, "poisson")

# Extract the estimated lambda
lambda <- poisson_fit$estimate[1]

# Print the estimated lambda
cat("Estimated lambda:", lambda)

# model poisson regression using glm()
poisson.model <- glm(pirateattacks ~  Capture_fisheries_production + unemployment_rate +
                       state_fragility + mobile_cellular_subscriptions_per_100_people + 
                       total_merchandise_imports_exports, merged_data, family = poisson(link = "log"))
poisson.model

poisson.model %>% 
  as_flextable()

summary(poisson.model)
summ(poisson.model)
poisson.model

library(pixiedust)

dust(poisson.model) %>%
  sprinkle(col = 2, round = 10) %>%
  sprinkle(col = 3, round = 10) %>%
  sprinkle(col = 4, round = 3) %>%
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_colnames(term = "Variable",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "T-statistic",
                    p.value = "P-value") %>%
  kable() %>%
  kable_styling()


# Poisson Added Variables Regression Plot
#load car package
library(car)
library(MASS)
library(jtools)
#produce added variable plots
avPlots(poisson.model)

#over-dispersion or under-dispersion
# If the Residual Deviance is greater than the degrees of freedom, then over-dispersion exists. 
#This means that the estimates are correct, but the standard errors (standard deviation) are wrong and unaccounted for by the model.

#Quasipoisson
poisson.model2 <- glm(pirateattacks ~  Capture_fisheries_production + unemployment_rate + 
                        state_fragility + mobile_cellular_subscriptions_per_100_people + 
                        total_merchandise_imports_exports, merged_data, family = quasipoisson(link = "log"))
poisson.model2

poisson.model2 %>% 
  as_flextable()

summary(poisson.model2)
summ(poisson.model2)

# QuasiPoisson Added Variables Regression Plot
#load car package
library(car)

#produce added variable plots
avPlots(poisson.model2)

library(vcd)
Ord_plot(merged_data$pirateattacks, tol=0.1)

distplot(merged_data$pirateattacks, type='poisson')

distplot(merged_data$pirateattacks, type="nbinom")

plot(poisson.model2)

summary(poisson.model2)

library(pixiedust)

dust(poisson.model2) %>%
  sprinkle(col = 2, round = 10) %>%
  sprinkle(col = 3, round = 10) %>%
  sprinkle(col = 4, round = 3) %>%
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_colnames(term = "Variable",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "T-statistic",
                    p.value = "P-value") %>%
  kable() %>%
  kable_styling()



#Negative binomial regression
library(MASS)
library(jtools)
negb <- glm.nb(pirateattacks ~  Capture_fisheries_production + unemployment_rate + 
                 state_fragility + mobile_cellular_subscriptions_per_100_people + 
                 total_merchandise_imports_exports, data = merged_data) #negative binomial
negb
summary(negb)
summ(negb)

negb %>% 
  as_flextable()


library(pixiedust)

dust(negb) %>%
  sprinkle(col = 2, round = 10) %>%
  sprinkle(col = 3, round = 10) %>%
  sprinkle(col = 4, round = 3) %>%
  sprinkle(col = 5, fn = quote(pvalString(value))) %>%
  sprinkle_colnames(term = "Variable",
                    estimate = "Estimate",
                    std.error = "SE",
                    statistic = "T-statistic",
                    p.value = "P-value") %>%
  kable() %>%
  kable_styling()





# NegB Added Variables Regression Plot
#load car package
library(car)

#produce added variable plots
avPlots(negb)

library(sjstats)

# Fit the negative binomial model
negb <- glm.nb(pirateattacks ~ Capture_fisheries_production + unemployment_rate + 
                 state_fragility + mobile_cellular_subscriptions_per_100_people + 
                 total_merchandise_imports_exports, data = merged_data)

# Calculate pseudo R-squared using sjstats
performance::r2(negb)


#Get back multiple ols model

merged_data$pirateattackslog <- log(merged_data$pirateattacks + 1)

# Multiple Linear Regression
fit.ols.multiplelog <- lm(pirateattackslog ~  Capture_fisheries_production + unemployment_rate + 
                            state_fragility + mobile_cellular_subscriptions_per_100_people + 
                            total_merchandise_imports_exports, data = merged_data)
summary(fit.ols.multiplelog)
summ(fit.ols.multiplelog)

library(strengejacke)
tab_model(fit.ols.multiplelog,poisson.model, poisson.model2, negb)
# Print models side by side
library(stargazer)
stargazer(fit.ols.multiplelog, poisson.model, poisson.model2, negb, type = "text",digits = 10,
          title="Title: Regression Results")

stargazer(fit.ols.multiplelog, poisson.model, poisson.model2, negb, type="html", digits = 10, out="test.html", out.header=TRUE)

#One way of deciding which model is appropriate is to examine the fit statistic Akaike Information Criterion (AIC)
#which is a index of sorts to indicate how close the model is to reality
#A lower value indicates a better fitting model.
AIC(fit.ols.multiple)
AIC(poisson.model)
AIC(poisson.model2)
AIC(negb)

AICs<-c(AIC(fit.ols.multiple),AIC(poisson.model), AIC(poisson.model2), AIC(negb))
labels<-c("OLS", "Poisson Model","Quasi-Poisson Model", "Negative Binomial Model" )

flextable(data.frame(Models=labels, AIC=round(AICs, 2)))


