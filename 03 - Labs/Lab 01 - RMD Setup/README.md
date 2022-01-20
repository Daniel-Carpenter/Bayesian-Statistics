## Helpful `R Markdown` Cheat sheets and Links

* [Lesson 1 for R Markdown](http://rmarkdown.rstudio.com/lesson-1.html)
* [R Markdown Cheat sheet (v2.0)](https://github.com/rstudio/cheatsheets/blob/main/rmarkdown-2.0.pdf)
* [R Markdown Reference Guide](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf?_ga=2.209992917.1171562397.1642623613-1554842467.1639495227)
* [Other R Markdown Cheat sheets](https://rmarkdown.rstudio.com/lesson-15.html)

------------------------------------------------------------------------

## Examples of `LaTex` Formulae with R

* [Note helpful LaTex repository
    here](https://www.overleaf.com/learn/latex/Learn_LaTeX_in_30_minutes#!#Adding_math_to_LaTeX)

<br>

#### Example `a`:

$$
(x + a)^n = 
\\sum\_{k=0}^{n}
  \\begin{pmatrix}
    n\\\\
    k
  \\end{pmatrix}
x^ka^{n-k}
$$

<br>

#### Example `b`:

$$
(1 + x)^n = 1 
+ \\frac{nx}{1!} + \\frac{n(n-1)x^2}{n!} + \\dotsm
$$

<br>

#### Example `c`:

$$
f(x) = a\_{0} + 
\\sum\_{n = 1}^{\\infty}
  (a\_{n}\\cos\\frac{n \\pi x}{L} + b\_{n}\\sin\\frac{n \\pi x}{L})
$$

<br>

#### Example `d`:

$$
e^x = 1 
+ \\frac{x}{1!}
+ \\frac{x^2}{2!}
+ \\frac{x^3}{3!}
+ \\dotsm, -\\infty &lt; x &lt; \\infty
$$

------------------------------------------------------------------------

## Histogram of *μ*

    library(rstan)

    ## Loading required package: StanHeaders

    ## Loading required package: ggplot2

    ## rstan (Version 2.21.3, GitRev: 2e1f913d3ca3)

    ## For execution on a local, multicore CPU with excess RAM we recommend calling
    ## options(mc.cores = parallel::detectCores()).
    ## To avoid recompilation of unchanged Stan programs, we recommend calling
    ## rstan_options(auto_write = TRUE)

    ## Do not specify '-march=native' in 'LOCAL_CPPFLAGS' or a Makevars file

    library(Intro2R)

    # Suggested to do this with multiple cores
    options(mc.cores = parallel::detectCores())
    rstan_options(auto_write = TRUE)

    # Read in the ddt csv file
    ddt <- read.csv("Input_Files/DDT.csv")

    # Create model using rstan
    basic_data <- list(y=ddt$LENGTH, N=length(ddt$LENGTH))
    fit <- stan(file = "Input_Files/basic.stan",
                model_name = "basic",
                data = basic_data,
                chains = 3,
                warmup = 1000,
                cores = 3,
                iter = 5000,
                pars = c("mu")
                )

    # ---

    library(ggplot2)
    library(rstanarm)

    ## Loading required package: Rcpp

    ## This is rstanarm version 2.21.1

    ## - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!

    ## - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.

    ## - For execution on a local, multicore CPU with excess RAM we recommend calling

    ##   options(mc.cores = parallel::detectCores())

    ## 
    ## Attaching package: 'rstanarm'

    ## The following object is masked from 'package:rstan':
    ## 
    ##     loo

    library(bayesplot)

    ## This is bayesplot version 1.8.1

    ## - Online documentation and vignettes at mc-stan.org/bayesplot

    ## - bayesplot theme set to bayesplot::theme_default()

    ##    * Does _not_ affect other ggplot2 plots

    ##    * See ?bayesplot_theme_set for details on theme setting

    # Create a blue histogram of mu
    color_scheme_set("blue")
    mcmc_hist(fit,pars = c("mu"))

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Lab1_files/figure-markdown_strict/histogram-1.png)

------------------------------------------------------------------------

## Assessment for Bayesian Statistics

<table>
<thead>
<tr class="header">
<th>Group</th>
<th>Weight</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Assignments</td>
<td>40%</td>
</tr>
<tr class="even">
<td>Midterm Exam</td>
<td>10%</td>
</tr>
<tr class="odd">
<td>Lab Exercises</td>
<td>10%</td>
</tr>
<tr class="even">
<td>Chapter Quizzes</td>
<td>10%</td>
</tr>
<tr class="odd">
<td>Final Exam</td>
<td>30%</td>
</tr>
<tr class="even">
<td><strong>Total</strong></td>
<td><strong>100%</strong></td>
</tr>
</tbody>
</table>
