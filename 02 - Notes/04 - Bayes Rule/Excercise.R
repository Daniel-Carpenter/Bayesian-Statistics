# Tables
# probability
# Conditional probability.

library(s20x)
library(RColorBrewer) # Colors in R

data("course.df")
head(course.df)
tab = with(course.df, table(Degree, Gender))
tab2 = addmargins(tab)
tab2
windows()
barplot(tab, beside=TRUE, 
        col = brewer.pal(5, "Pastel1"),
        legend = TRUE,
        ylab = 'Frequency',
        main = 'Frequency of M/F Students by Degree Type')


# Make a prop table

## Show the original Frequency Table
tab2

## Create a Relative Proportion Table using tab
tab3 = round(prop.table(tab), 3)
tab3

## Add Row and Column Sums
addmargins(tab3)

## Calculation for testing

userPopulation = 0.005
truePositive   = 0.99
trueNegative   = 0.99
posNotUser      = 1 - trueNegative


(truePositive * userPopulation) / (trueNegative * userPopulation + posNotUser * 0.995)

0.99*0.005/(0.99*0.005+0.01*0.995)


