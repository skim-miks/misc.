#### Project code

# read in data
movies <- read.csv("MovieData.csv",header=TRUE)
movies <- movies[!is.na(movies$movie_running_time),]
nrow(movies)
str(movies)
# summary statistics for quantitative variables
x <- summary(movies$movie_running_time)
y <- summary(movies$movie_financial_summary_production_budget)
z <- summary(movies$movie_production_year)
x.data <- as.data.frame(cbind("min"=x[1],"first"=x[2],"median"=x[3],"third"=x[4],"max"=x[5]))
y.data <- as.data.frame(cbind("min"=y[1],"first"=y[2],"median"=y[3],"third"=y[4],"max"=y[5]))
z.data <- as.data.frame(cbind("min"=z[1],"first"=z[2],"median"=z[3],"third"=z[4],"max"=z[5]))
#install package for table one to get tables
install.packages("tableone")
library(tableone)
?tableone
#create tables with five number summary
table1 <- CreateTableOne(data=x.data)
table2 <- CreateTableOne(data=y.data)
table3 <- CreateTableOne(data=z.data)
table1
table2
table3
#create model with predicted variable and covariates
model <- lm(movie_financial_summary_domestic_box_office~movie_production_year+movie_financial_summary_production_budget+movie_running_time,data=movies)
red.model <- lm(movie_financial_summary_domestic_box_office~1,data=movies)
anova(red.model,model)
summary(model)
pairs(movies)
coefficients(model)
plot(rstudent(model),fitted(model))
plot(model)
