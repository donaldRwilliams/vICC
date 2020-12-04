
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vICC: Varying Intraclass Correlation Coefficients

[![Build
Status](https://travis-ci.org/donaldRwilliams/BGGM.svg?branch=master)](https://travis-ci.org/donaldRwilliams/BGGM)

The goal of vICC is to compute varying intraclass correlation
coefficients (ICC) in a one-way random effects model (i.e., a random
intercepts only model). Often computing the ICC is the first step when
fitting a mixed-effects (a.k.a., hierarchical, multilevel, etc.) model
that results in *merely* one value that is assumed to apply to each
group (e.g., person, school). The underlying assumption is a common
within-group variance, whereas in **vICC** a random-effects model is
fitted to the residual variance, thereby permitting group-level ICCs.
When subjects are the grouping variable, this is akin to investigating
individual differences in the ICC.

## Measurement Reliability

The methodology in **vICC** was introduced in Williams, Martin, and Rast
(2019). The context was measurement reliability in a cognitive
inhibition task. To this end, **vICC** provides ICC(1), that is
correlation for any two observations from the same group, and ICC(2),
this is average score reliability. Both ICC(1) and ICC(2) are
reliability indices.

## Installation

You can install the released version of vICC from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("vICC")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("donaldRwilliams/vICC")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(vICC)
#> Package 'vICC' version 1.0.0
#> Type 'citation("vICC")' for citing this R package.
## basic example code
```

<div id="refs" class="references">

<div id="ref-williams2019putting">

Williams, Donald R, Stephen R Martin, and Philippe Rast. 2019. “Putting
the Individual into Reliability: Bayesian Testing of Homogeneous
Within-Person Variance in Hierarchical Models.”

</div>

</div>
