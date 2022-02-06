# ggplot
# graphical formalisms

# Data

ddt = read.csv("DDT.csv")
class(ddt)
library(ggplot2)
if(!require(ggplot2)) install.packages("ggplot2")

# univariate

# Categorical

univ = ggplot(ddt, aes(x = RIVER)) + geom_bar()
windows() #quartz() for mac

univ

# Quantitative
univ2 = ggplot(ddt, aes(x = LENGTH)) + 
  geom_histogram(bins = 20)
print(univ2)

# Both Qual and Quant

qq = ggplot(ddt, aes(x = SPECIES, y = LENGTH, fill = SPECIES )) + 
  geom_boxplot()
qq

# Both Quantitative

qnqn = ggplot(ddt, aes(x = WEIGHT, y = LENGTH)) + 
  geom_point()
qnqn


# Both Qualitative
#ggplot(df, aes(Fruit, ..count..)) + geom_bar(aes(fill = Bug), position = "dodge")
ququ = ggplot(ddt, aes(x = RIVER, y = ..count..)) +
  geom_bar(aes(fill = SPECIES), position = "dodge")
ququ

# 2 Quantitative 1 Qual

q21 = ggplot(ddt, aes(x = WEIGHT, y = LENGTH, color = SPECIES)) +
  geom_point()
q21

facets = ggplot(ddt, aes(x=WEIGHT, y=LENGTH)) + geom_point() +
  facet_wrap(~SPECIES)
facets


facets2 = ggplot(ddt, aes(x=WEIGHT, y=LENGTH)) + geom_point() +
  facet_wrap(~SPECIES) + geom_smooth(method = "loess")
facets2


