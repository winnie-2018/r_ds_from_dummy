# preparation for reading the csv file ----

# get directory
getwd()

# set path variable
path <- "/home/winequality-white.csv"

# read CSV file
white_wine <- read.csv(path, header=TRUE, sep=";")
white_wine

# get a list of column names
features <- colnames(white_wine)
features

# show first 5 rows
head(white_wine, 5)

# rename the columns by replacing '.' to '_' ----
colnames(white_wine)[1] <- "fixed_acidity"
colnames(white_wine)[2] <- "volatile_acidity"
colnames(white_wine)[3] <- "citric_acid"
colnames(white_wine)[4] <- "residual_sugar"
colnames(white_wine)[5] <- "chlorides"
colnames(white_wine)[6] <- "free_sulfur_dioxide"
colnames(white_wine)[7] <- "total_sulfur_dioxide"
colnames(white_wine)[8] <- "density"
colnames(white_wine)[9] <- "pH"
colnames(white_wine)[10] <- "sulphates"
colnames(white_wine)[11] <- "alcohol"
colnames(white_wine)[12] <- "quality"

fixed_acidity <- white_wine$fixed_acidity
volatile_acidity <- white_wine$volatile_acidity
citric_acid <- white_wine$citric_acid
residual_sugar <- white_wine$residual_sugar
chlorides <- white_wine$chlorides
free_sulfur_dioxide <- white_wine$free_sulfur_dioxide
total_sulfur_dioxide <- white_wine$total_sulfur_dioxide
density <- white_wine$density
pH <- white_wine$pH
sulphates <- white_wine$sulphates
alcohol <- white_wine$alcohol
quality <- white_wine$quality

# descriptive statistic section ----
# see the summary of each column
summary(white_wine)

# get range, variance, standard deviation, IQR
now <- alcohol
var(now)
sd(now)
max(now) - min(now)
IQR(now)

# histogram, boxplots ----------------------------------

# fixed_acidity
# Plot histogram and label text
h <- hist(fixed_acidity,
          main="Fixed Acidity Count",
          xlab="Fixed Acidity (g/L)",
          ylab="Count",
          ylim = c(0, 2500), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=fixed_acidity, 
  xlab = "Fixed Acidity (g/L)",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=fivenum(fixed_acidity), labels =fivenum(fixed_acidity), y=1.25)
text(x=boxplot.stats(fixed_acidity)$stats, labels =boxplot.stats(fixed_acidity)$stats, y=1.25)

# volatile_acidity
# Plot histogram and label text
h <- hist(volatile_acidity,
          main="Volatile Acidity Count",
          xlab="Volatile Acidity (g/L)",
          ylab="Count",
          ylim = c(0, 2500), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=volatile_acidity, 
  xlab = "Volatile Acidity (g/L)",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=fivenum(volatile_acidity), labels =fivenum(volatile_acidity), y=1.25)
text(x=boxplot.stats(volatile_acidity)$stats, labels =boxplot.stats(volatile_acidity)$stats, y=1.25)

# citric_acid
# Plot histogram and label text
h <- hist(citric_acid,
          main="Citric Acid Count",
          xlab="Citric Acid (g/L)",
          ylab="Count",
          ylim = c(0, 2000), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=citric_acid, 
  xlab = "Citric Acid (g/L)",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=fivenum(citric_acid), labels =fivenum(citric_acid), y=1.25)
text(x=boxplot.stats(citric_acid)$stats, labels = boxplot.stats(citric_acid)$stats, y=1.25)

# residual_sugar
# Plot histogram and label text
h <- hist(residual_sugar,
          main="Residual Sugar Count",
          xlab="Residual Sugar (g/L)",
          ylab="Count",
          ylim = c(0, 2600), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=residual_sugar, 
  xlab = "Residual Sugar (g/L)",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=fivenum(residual_sugar), labels =fivenum(residual_sugar), y=1.25)
text(x=boxplot.stats(residual_sugar)$stats, labels = boxplot.stats(residual_sugar)$stats, y=1.25)

# chlorides
# Plot bar chart and label text
ch_tab <- table(chlorides)
ch_bar <- barplot(
  ch_tab, 
  xlab="Chlorides (mg/L)", 
  ylab="Count", 
  col="#69b3a2",
  main = "Chlorides count")

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=chlorides, 
  xlab = "Chlorides (mg/L)",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=fivenum(chlorides), labels =fivenum(chlorides), y=-1.25)
text(x=boxplot.stats(chlorides)$stats, labels = boxplot.stats(chlorides)$stats, y=1.25)

# free_sulfur_dioxide
# Plot histogram and label text
h <- hist(free_sulfur_dioxide,
          main="Free SO2 Count",
          xlab="Free SO2 (mg/L)",
          ylab="Count",
          ylim = c(0, 2500), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=free_sulfur_dioxide, 
  xlab = "Free SO2 (mg/L)",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=boxplot.stats(free_sulfur_dioxide)$stats, labels = boxplot.stats(free_sulfur_dioxide)$stats, y=1.25)

# total_sulfur_dioxide
# Plot histogram and label text
h <- hist(total_sulfur_dioxide,
          main="Total SO2 Count",
          xlab="Total SO2 (mg/L)",
          ylab="Count",
          ylim = c(0, 3000), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=total_sulfur_dioxide, 
  xlab = "Total SO2 (mg/L)",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=quantile(total_sulfur_dioxide), labels =quantile(total_sulfur_dioxide), y=1.25)
text(x=boxplot.stats(total_sulfur_dioxide)$stats, labels = boxplot.stats(total_sulfur_dioxide)$stats, y=1.25)

# density
# Plot histogram and label text
h <- hist(density,
          main="Density Count",
          xlab="Density (g/cm3)",
          ylab="Count",
          ylim = c(0, 3000), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=density, 
  xlab = "Density (g/cm3)",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=quantile(density), labels =quantile(round(density,2)), y=1.25)
text(x=boxplot.stats(density)$stats, labels = round(boxplot.stats(density)$stats, 2), y=1.25)

# pH
# Plot histogram and label text
h <- hist(pH,
          main="pH value Count",
          xlab="pH",
          ylab="Count",
          ylim = c(0, 1500), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
abline(v = mean(pH), col = "red", lwd = 2)

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=pH, 
  xlab = "pH",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=quantile(pH), labels =quantile(pH), y=1.25)
text(x=boxplot.stats(pH)$stats, labels = round(boxplot.stats(pH)$stats, 2), y=1.25)

# Sulphates
# Plot histogram and label text
h <- hist(sulphates,
          main="Sulphates Content count",
          xlab="Sulphates content (mg/L)",
          ylab="Count",
          ylim = c(0, 1200), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=sulphates, 
  xlab = "Sulphates (mg/L)",
  data=white_wine, 
  horizontal = TRUE)
text(x=quantile(sulphates), labels =quantile(round(sulphates,2)), y=1.25)
text(x=boxplot.stats(sulphates)$stats, labels = round(boxplot.stats(sulphates)$stats, 2), y=1.25)

# Alcohol
# Plot histogram and label text
h <- hist(alcohol,
          main="Alcohol Percentage Count",
          xlab="Alcohol (%)",
          ylab="Count",
          ylim = c(0, 1200), 
          col="darkmagenta",
          freq=TRUE)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=alcohol, 
  xlab = "Alcohol (%)",
  data=white_wine, 
  horizontal = TRUE,
  main = "Alcohol",
  axes = TRUE)
text(x=fivenum(alcohol), labels =fivenum(alcohol), y=1.25)

# Quality
# Plot barchart and label text
quality_tab <- table(quality)
quality_bar <- barplot(
  quality_tab, 
  ylim = c(0, 2500), 
  xlab="Quality", 
  ylab="Count", 
  col="#69b3a2",
  main = "Quality count")

text(x = quality_bar, 
  y = quality_tab + 0.5, 
  labels = quality_tab, 
  adj=c(0.5, -0.5))

# Plot boxplot and label left/right whiskers, median Q1/Q3
boxplot(
  x=quality, 
  xlab = "Quality",
  data=white_wine, 
  horizontal = TRUE,
  axes = TRUE)
text(x=quantile(quality), labels =quantile(quality), y=1.25)
text(x=boxplot.stats(quality)$stats, labels = round(boxplot.stats(quality)$stats, 2), y=1.25)

# ggplot: heat map --------
# Load the required packages
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(lattice)
library(reshape2)

# correlation matrix: figures
cor_matrix <- round(cor(white_wine),2) 
melted_corr_mat <- melt(cor_matrix)

# create a basic correlation heatmap using ggplot
ggplot(
  data <- melted_corr_mat, aes(x=Var1, y=Var2, fill=value) +
    geom_tile(color="white") +
    labs(x="", y="", title="Heatmap of features for wine quality") +
    theme(axis.text.x = element_text(angle=45,hjust=1)) +
    geom_text(aes(label=value),color="white", size=3)
)

# scatter plot -----------------
# alcohol-quality
smoothScatter(alcohol, 
              quality, 
              main="Quality vs Alcohol",
              xlab="Alcohol (%)", 
              ylab="Quality",)
abline(lm(quality ~ alcohol, data = white_wine), col = "orange",lwd = 3)

# Quality-chlorides
smoothScatter(chlorides, 
              quality, 
              main="Quality vs Chlorides",
              xlab="Chlorides (mg/L)", 
              ylab="Quality",)
abline(lm(quality ~ chlorides, data = white_wine), col = "orange",lwd = 3)

# residual sugar - density
smoothScatter(
  residual_sugar, 
  density, 
  main = "Density vs Residual Sugar",
  xlab="Residual Sugar (g/L)", 
  ylab="Density (g/cm3)")
abline(lm(density ~ residual_sugar, 
       data = white_wine), 
       col = "orange", 
       lwd=2)

# models --------------
# z test - subset data -------

# low alcohol subset
low_pct <- white_wine[alcohol < 12, "quality"]

# high alcohol subset
high_pct <- white_wine[white_wine$alcohol >= 12,"quality"]  

# z test: no normalisation ----
# Low alcohol %
var_low<-var(low_pct) # variance of low alcohol
mean_low<-mean(low_pct) # mean of low alcohol

hist_low <- hist(low_pct, # Plot histogram
     main="Group: Alcohol < 12%", 
     xlab = "Quality",
     freq=TRUE) 

box_low <- boxplot(low_pct, # Plot boxplot
        horizontal=TRUE,
        xlab = "Quality") 

# High alcohol %
var_high<-var(high_pct) # variance of high alcohol
mean_high<-mean(high_pct) # mean of high alcohol

hist_high <- hist(high_pct, # Plot histogram
     main="Group: Alcohol >= 12%", 
     xlab = "Quality")

box_high <- boxplot(high_pct, # Plot boxplot
        horizontal=TRUE,  
        xlab = "Quality") 
text(x=fivenum(high_pct), labels =fivenum(high_pct), y=1.25) # values of quantiles

shapiro.test(low_pct) # Shapiro-Wilk test to check normality

# Calculate z-score for data with no normalisation
z_score <-(mean_high-mean_low)/sqrt((var_low/length(low_pct))+(var_high/length(high_pct)))
z_score

# Calculate p-value 
p_val <- 2*pnorm(q=z_score, lower.tail=FALSE)
p_val

# z test: after normalisation ----
# Low alcohol %
a_less_12 <- log(low_pct) # log transform low alcohol group
mean_less_12 <- mean(a_less_12) # mean of logged low alcohol group
var_less_12 <- var(a_less_12) # variance of logged low alcohol group

hist_log_low <- hist(a_less_12, # Plot histogram
                  main="Group: Alcohol < 12%", 
                  xlab = "log(quality)")

box_log_low <- boxplot(a_less_12, # Plot boxplot
                    horizontal=TRUE,  
                    xlab = "log(quality)")
shapiro.test(a_less_12) # Shapiro-Wilk test to check normality

# high alcohol %
a_more_12 <- log(high_pct) # log transform high alcohol group
mean_more_12 <- mean(a_more_12) # mean of logged high alcohol group
var_more_12 <- var(a_more_12) # variance of logged high alcohol group

hist_log_high <- hist(a_more_12, # Plot histogram
                     main="Group: Alcohol >= 12%", 
                     xlab = "log(quality)")
box_log_high <- boxplot(a_more_12, # Plot boxplot
                       horizontal=TRUE,  
                       xlab = "log(quality)") 
shapiro.test(a_more_12) # Shapiro-Wilk test to check normality

# Calculate z-score for data with after normalisation
z_log_score <-(mean(a_more_12)-mean(a_less_12))/sqrt((var(a_more_12)/length(a_more_12))+(var(a_less_12)/length(a_less_12)))
z_log_score

# Calculate p-value
p_val_log <- 2*pnorm(q=z, lower.tail=FALSE)
p_val_log

# Pearson correlation analysis ----

# log(residual_sugar): Box plot to see the pattern
boxplot(log(residual_sugar), horizontal=TRUE, 
        xlab="log(residual_sugar)",
        ylab= "Count",
        main="Normalised Residual Sugar")
text(x=boxplot.stats(log(residual_sugar))$stats, 
     labels = round(boxplot.stats(log(residual_sugar))$stats, 2), 
     y=1.25)

# log(density): Box plot to see the pattern
boxplot(log(density), horizontal=TRUE, 
        xlab="log(density)",
        ylab= "Count",
        main="Normalised Density")
text(x=boxplot.stats(log(density))$stats, 
     labels = round(boxplot.stats(log(density))$stats, 2), 
     y=1.25)

# Scatter plot of normalised residual_sugar vs density
plot(x=log(residual_sugar), log(density),
     main="log(density) VS log(residual_sugar)",
     pch='.',
     col="blue")
abline(fit <- lm(log(density) ~ log(residual_sugar), data = white_wine), 
       col = "red", 
       lwd=2)

# get the intercept and gradient
coef(fit)
# (Intercept) log(residual_sugar) 
# -0.009648201         0.002466801 
# log(density) = 0.002466801*log(residual_sugar)-0.009648201  

# Correlation test for normalised
cor.test(log(residual_sugar), 
         log(density), 
         method = "pearson")

# Scatter plot of residual_sugar vs density
plot(y=density, x=residual_sugar, pch='.')
abline(fit<-lm(density ~ residual_sugar, data = white_wine), 
       col = "blue", 
       lwd=2)

# get the intercept and gradient
coef(fit)
# (Intercept) residual_sugar 
# 0.9908653878   0.0004947244 
# density = 0.0004947244*residual_sugar + 0.9908653878

# correlation test
cor.test(density, 
         residual_sugar,
         method = "pearson")

ggplot(white_wine, aes(x = variable1, y = variable2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter plot of Variable 1 vs Variable 2",
       x = "Variable 1",
       y = "Variable 2")

# Multiple Linear regression ----
library(car) # use car library

# build model for quality vs alcohol, density, chlorides
model <- lm(quality ~ alcohol + density + chlorides, data = white_wine)
model
summary(model)  

# Variance Inflation Factor (VIF): check multicollinearity
vif(model)

# coefficients of variables
coef(model)
# quality = 0.34311688*alcohol + 23.6708667*density -2.3823*chlorides -21.1502

# confidence interval of model at 95% for all predictors
confint(model)

# Added-Variable Plots: Plot interaction between variables
avPlots(model)
abline(model, 
       col = "blue", 
       lwd=2)

