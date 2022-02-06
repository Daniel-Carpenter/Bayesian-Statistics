# Tables
# probability
# Conditional probability.

library(s20x)
data("course.df")
head(course.df)
tab = with(course.df, table(Degree, Gender))
tab2 = addmargins(tab)
tab2
windows()
barplot(tab, beside=TRUE, col = rainbow(4), legend = TRUE)

# Make a prop table
# Use tab

tab3 = prop.table(tab)
tab3
tab2

addmargins(tab3)

## Calculation for testing
0.99*0.005/(0.99*0.005+0.01*0.995)


