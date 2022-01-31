# example of a function

myfun = function(x) 
{
  library(ggplot2)
  y = x^2
  # new window
  dev.new(noRStudioGD = TRUE)
  
  plot(y~x)
  
  df = data.frame(x = x, y = y)
  
  dev.new(noRStudioGD = TRUE)
  
  g = ggplot(df, aes(x = x, y = y, col = I("red"))) 
  g = g + geom_line(lwd = 3) + ggtitle("Wayne's cool plot")
  print(g)
  
  
  write.csv(df, "df.csv")
  
  list(x = x, y = y, df = df)
  
}

obj = myfun(1:100)
