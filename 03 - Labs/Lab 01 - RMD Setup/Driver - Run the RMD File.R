library(rmarkdown)

# Render all the formats above and port to output directory
render("Lab1.Rmd", "all",
       output_dir = "Output")