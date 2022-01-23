library(tidyverse)
library(ggplot2)
library(modelsummary)
library(fixest)
library(caret)
library(gridExtra)


############################# Data Extraction, Cleaning and Munging ################################################

# Loading the data
dataset <- read_csv(url('https://raw.githubusercontent.com/CheemaHaaris/DA3/main/Assignment1/data/morg-2014-emp.csv?token=GHSAT0AAAAAABQS6MPX3ZY76CICUOB5R2WSYPQHFJQ'))


# Subsetting the data for the relevant occupations - financial specialists 

df <- subset(dataset,dataset$occ2012 == 0800 |dataset$occ2012 == 0810 |dataset$occ2012 == 0820 | dataset$occ2012 == 0830| dataset$occ2012 == 0840| dataset$occ2012 == 0850| dataset$occ2012 == 0860| dataset$occ2012 == 0900| dataset$occ2012 == 0910| dataset$occ2012 == 0930| dataset$occ2012 == 0940| dataset$occ2012 == 0950)


# Selecting the relevant variables for analysis 

df <- df[, c(5:9, 11:13)]


##### Factorising the categorical vars and checking observations in each category

df$grade92 <- as.factor(df$grade92)

datasummary( grade92 ~ N + Percent(), data = df) 

# Maintain the relevant education levels and discarding the rest - 39, 40, 42, 43, 44
# 39 = High school graduate, diploma
# 40 = Some college but no degree
# 42 = Associate degree -- academic program
# 43 = Bachelors
# 44 = Masters

df <- filter(df, grade92 == 39 | grade92 == 40 | grade92 == 42 | grade92 == 43| grade92 == 44)

df$race <- as.factor(df$race)

datasummary( race ~ N + Percent(), data = df) 

              
# race = 1,2,4
# 1 = white
# 2 = black
# 4 = asian 

df <- filter(df, race == 1 | race == 2 | race == 4)

df$sex <- as.factor(df$sex)

datasummary( sex ~ N + Percent(), data = df) 



df$marital <- as.factor(df$marital)

datasummary( marital ~ N + Percent(), data = df)

# club 1,2,3 &  4,5,6,7 respectively in new current marital status var


### Data Refactoring ###

# Target Var - Earning per hour

df <- df %>% 
  mutate( w = earnwke/uhours ) %>%
  mutate( lnw = log(w) )

df <- filter(df, w < 500 & w > 0)


# Education Level

df <- df %>% 
  mutate( lower_BA = ifelse( grade92 == 39 | grade92 == 40 | grade92 == 42, 1,0),
          BA = ifelse( grade92 == 43, 1, 0),
          MA = ifelse( grade92 == 44, 1, 0))

# Race

df <- df %>% 
  mutate( white = ifelse( race == 1, 1, 0),
          black = ifelse( race == 2, 1, 0),
          asian = ifelse( race == 4, 1, 0))


# Sex 

df$sex <- ifelse(df$sex == 1, 0, 1)

# Creating a new var female for the gender binary var

df <- df %>% mutate(female = as.numeric(sex == 1))


# Marital Status (Current)

df <- df %>% mutate( current_marital = ifelse( marital == 1 | marital == 2 | marital == 3, 1, 0))


# Age

# quadratic, cubic
df<- df %>%
  mutate(agesq = age^2,
         agecu = age^3)


# Now that we have finalised our variables, we can check for missing values

table(is.na(df))

# Data summary

datasummary( (`Earnings per hour` = w) + (`Lower than Bachelors Degree` = lower_BA) + (`Bachelors Degree` = BA) +
                (`Masters Degree` = MA) + (`Whites` = white) + (`Blacks` = black) + (`Asians` = asian) +
                (`Age` = age) + (`Female` = female) + (`Current Marital Status` = current_marital)  ~
               Mean + Median + Min + Max + P25 + P75 + N , data = df )


##################################### Exploratory Data Analysis ####################################################


### Checking variable distributions 
## Target Variable


p1 <- ggplot(df, aes(x = w)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), boundary=0,
                 fill = 'navyblue', color = 'white', size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  coord_cartesian(xlim = c(0, 120)) +
  labs(x = "Earning per hour",y = "Percent", title = "Distribution of Earnings per hour")+
  theme_bw() +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0.01,0.01),breaks = seq(0,120, 30))


# is taking log an option
#ggplot(df, aes(x = lnw)) +  geom_histogram(aes(y = (..count..)/sum(..count..)), boundary=0, fill = 'navyblue', color = 'white', size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) + labs(x = "Log of Earning per hour",y = "Percent")+ theme_bw() + expand_limits(x = 0.01, y = 0.01) + scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) 


### Predictor Variables

# Education Level

# Distribution of variable - degree
 p2 <- ggplot( data = df, aes( x = factor(grade92)) )  +
  geom_bar(aes(y = (..count..)/sum(..count..)), color='white',fill='navyblue', alpha=0.8, show.legend = F) +
  theme_bw() +
  labs( x = NULL, y = NULL,
        title = 'Distribution of Education Level') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, hjust = 0.5 ) ) +
   scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
   scale_x_discrete(labels = c("High School/Diploma", "College","Associate degree","Bachelors", "Masters" ))

 
 

 
 # 39 = High school graduate, diploma
 # 40 = Some college but no degree
 # 42 = Associate degree -- academic program
 # 43 = Bachelors
 # 44 = Masters
# Race
 
 p3 <- ggplot( data = df, aes( x = race) )  +
   geom_bar(aes(y = (..count..)/sum(..count..)), color='white',fill='navyblue', alpha=0.8, show.legend = F) +
   theme_bw() +
   labs( x = NULL, y = NULL,
         title = 'Distribution of Race') +
   theme( panel.grid.minor.x = element_blank(), 
          plot.title = element_text( size = 12, hjust = 0.5 ) ) +
   scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
    scale_x_discrete(labels = c("Whites", "Blacks","Asians" ))
 
 

# Sex
 
 p4 <- ggplot( data = df, aes( x = factor(female) ))  +
   geom_bar(aes(y = (..count..)/sum(..count..)), color='white',fill='navyblue', alpha=0.8, show.legend = F) +
   theme_bw() +
   labs( x = NULL, y = NULL,
         title = 'Gender-wise Distribution') +
   theme( panel.grid.minor.x = element_blank(), 
          plot.title = element_text( size = 12, hjust = 0.5 ) ) +
   scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
   scale_x_discrete(labels = c("Male", "Female"))
 
 

# Age
 
 p5 <- ggplot(df, aes(x = age)) +
   geom_histogram(aes(y = (..count..)/sum(..count..)), boundary=0,
                  fill = 'navyblue', color = 'white', size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
   #coord_cartesian(xlim = c(0, 500)) +
   labs(x = NULL,y = NULL,
        title = 'Age Distribution')+
   theme_bw() +
   scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
   scale_x_continuous(expand = c(0.01,0.01),breaks = seq(15,65, 10))

# Marital
 
 p6 <- ggplot( data = df, aes( x = factor(current_marital) ))  +
   geom_bar(aes(y = (..count..)/sum(..count..)), color='white',fill='navyblue', alpha=0.8, show.legend = F) +
   theme_bw() +
   labs( x = NULL, y = NULL,
         title = 'Marital Status Distribution') +
   theme( panel.grid.minor.x = element_blank(), 
          plot.title = element_text( size = 12, hjust = 0.5 ) ) +
   scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
   scale_x_discrete( labels=  c("Married","Not Married"))

 ################################## Lowess Comparisons for functional form ###########################################
 

 # Age
 p7 <- ggplot(data = df, aes(x = age, y = w )) +
   geom_point( color = 'blue', size = 1.5,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
   geom_smooth(method="loess", se=F, colour='red', size=1, span=0.9) +
   labs(x = "Age (years)",y = "Earning per hour") +
   theme_bw() +
   expand_limits(x = 0.01, y = 0.01) +
   scale_y_continuous(expand = c(0.01,0.01),limits = c(0,125), breaks = seq(0,125, 50)) +
   scale_x_continuous(expand = c(0.01,0.01),limits = c(15,65), breaks = seq(15,65, 10))
 
 # Testing age as a quadratic and cubic
 
 p8 <- ggplot(data = df, aes(x = age, y = w )) +
    geom_smooth( aes(colour='red'), method="loess", formula = y ~ x,se=F, size=1) +
    geom_smooth( aes(colour='black'), method="lm", formula = y ~ poly(x,2) , se=F, size=1) +
    geom_point( aes( y = w ) , color = 'blue', size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
    labs(x = "Age (years)",y = "Earning per hour") +
    scale_color_manual(name="", values=c('red','black'),labels=c("Lowess in age","Quadratic in age")) +
    theme_bw() +
    scale_x_continuous(limits = c(15,65), breaks = seq(15,65, 10)) +
    scale_y_continuous(limits = c(0,120), breaks = seq(0,120, 30)) +
    theme(legend.position = c(0.5,0.8),
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.box.background = element_rect(color = "white"))
 
 p9 <- ggplot(data = df, aes(x = age, y = w )) +
    geom_smooth( aes(colour='red'), method="loess", formula = y ~ x,se=F, size=1) +
    geom_smooth( aes(colour='black'), method="lm", formula = y ~ poly(x,3) , se=F, size=1) +
    geom_point( aes( y = w ) , color = 'blue', size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
    labs(x = "Age (years)",y = "Earning per hour") +
    scale_color_manual(name="", values=c('red','black'),labels=c("Lowess in age","Cubic in age")) +
    theme_bw() +
   # scale_x_continuous(limits = c(15,65), breaks = seq(15,65, 10)) +
    #scale_y_continuous(limits = c(0,120), breaks = seq(0,120, 30)) +
    theme(legend.position = c(0.5,0.8),
          legend.direction = "horizontal",
          legend.background = element_blank(),
          legend.box.background = element_rect(color = "white"))
 
 ############################################# Regressions ##########################################################
 
 
 # Defining formulas for regressions
 
 model1 <- as.formula( w ~ age + agesq)
 model2 <- as.formula( w ~ age + agesq + female + lower_BA + MA)
 model3 <- as.formula( w ~ age + agesq + female + lower_BA + MA + black + asian + current_marital)
 model4 <- as.formula( w ~ age + agesq + agecu + female + lower_BA + MA + black + asian + current_marital + female*lower_BA + female*MA + female*age)
 
 
 # Running OLS
 
 reg1 <- feols(model1, data = df, vcov = 'hetero')
 reg2 <- feols(model2, data = df, vcov = 'hetero')
 reg3 <- feols(model3, data = df, vcov = 'hetero')
 reg4 <- feols(model4, data = df, vcov = 'hetero')


 # evaluation of the models: using all the sample RMSE and BIC
 
 fitstat_register("k", function(x){length( x$coefficients ) - 1}, "No. Variables")
 etable( reg1 , reg2 , reg3 , reg4 , fitstat = c('aic','bic','rmse','r2','n','k') ) 

 
 # Cross validation 
 
 # Simple k-fold cross validation setup:
 # 1) Used method for estimating the model: "lm" - linear model (y_hat = b0+b1*x1+b2*x2 + ...)
 # 2) set number of folds to use (must be less than the no. observations)
 k <- 5
 
 # We use the 'train' function which allows many type of model training -> use cross-validation
 set.seed(3167)
 cv1 <- train(model1, df, method = "lm", trControl = trainControl(method = "cv", number = k))
 
 set.seed(3167)
 cv2 <- train(model2, df, method = "lm", trControl = trainControl(method = "cv", number = k))
 
 set.seed(3167)
 cv3 <- train(model3, df, method = "lm", trControl = trainControl(method = "cv", number = k))
 
 set.seed(3167)
 cv4 <- train(model4, df, method = "lm", trControl = trainControl(method = "cv", number = k))


 
 # Calculate RMSE for each fold and the average RMSE as well
 cv <- c("cv1", "cv2", "cv3", "cv4")
 rmse_cv <- c()
 
 for(i in 1:length(cv)){
    rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                           get(cv[i])$resample[[1]][2]^2 +
                           get(cv[i])$resample[[1]][3]^2 +
                           get(cv[i])$resample[[1]][4]^2 +
                           get(cv[i])$resample[[1]][5]^2)/5)
 }
 
 
 # summarize results 
 cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                      rbind(cv1$resample[1], rmse_cv[1]),
                      rbind(cv2$resample[1], rmse_cv[2]),
                      rbind(cv3$resample[1], rmse_cv[3]),
                      rbind(cv4$resample[1], rmse_cv[4])
 )
 
 colnames(cv_mat)<-c("Resample","Model1", "Model2", "Model3", "Model4")
 cv_mat  


 # Show model complexity and out-of-sample RMSE performance
 m_comp <- c()
 models <- c("reg1", "reg2", "reg3", "reg4")
 for( i in 1 : length(cv) ){
   m_comp[ i ] <- length( get( models[i] )$coefficient  - 1 ) 
 }
 
 m_comp <- tibble( model = models , 
                   complexity = m_comp,
                   RMSE = rmse_cv )

p10 <- ggplot( m_comp , aes( x = complexity , y = RMSE ) ) +
   geom_point(color='red',size=2) +
   geom_line(color='blue',size=0.5)+
   labs(x='Number of explanatory variables',y='Averaged RMSE on test samples',
        title='Prediction performance and model compexity') +
   theme_bw() 


 gridExtra::grid.arrange()
 
 
 
 
 
 
 
 
 
 
 
 