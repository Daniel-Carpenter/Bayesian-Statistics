# GLM - Generalized Linear Model

## Overview of Choices for `Link` and `Noise Distribution`, based on data types
<img src = 'GLM Link and Noise Distribution Summary.png'> <br>

<br>

## Overview of `Data Types`
|Type|Description|Central Tendancy Measure|Has Absolute Zero?|Example|
|-|-|-|-|-|
|Interval   (*Metric*)|Space between twp vars |Can compute mean, median, mode, sd, etc.    |FALSE|time or temp   |
Dichotomous | Success or Failure
|Nominal (name) |  Characteristic; descriptive labels   |Mean|NA|Male or Female|
|Oridinal (ordered)|  Ordered variables |Use median or mode, **not mean**   |NA|Happy, Neautral, or Sad   |
|Ratio      |Order and exact values |Mode, median, mean, sd, etc.|TRUE|Height, weight, duration   |