
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vICC: Varying Intraclass Correlation Coefficients

[![Build
Status](https://travis-ci.org/donaldRwilliams/BGGM.svg?branch=master)](https://travis-ci.org/donaldRwilliams/BGGM)

The goal of vICC is to compute varying intraclass correlation
coefficients (ICC) in a one-way random effects model (i.e., a random
intercepts only model). Often computing an ICC is the first step when
fitting a mixed-effects (a.k.a., hierarchical, multilevel, etc.) model
that results in *merely* one value that is assumed to apply to each
group (e.g., person, school, etc.). The underlying assumption is a
common within-group variance, whereas, in **vICC**, a random-effects
model is fitted to the residual variance, thereby permitting group-level
ICCs. When subjects are the grouping variable, this is akin to
investigating individual differences.

## Measurement Reliability

The methodology in **vICC** was introduced in Williams, Martin, and Rast
(2019). The context was measurement reliability in a cognitive
inhibition task. To this end, **vICC** provides ICC(1), that is the
correlation for any two observations from the same group, and ICC(2),
that is average score reliability. Both ICC(1) and ICC(2) are
reliability indices.

## Available Models

The following are implemented in **vICC**:

1.  `pick_group`:
    
    This model has a spike and slab on the random intercepts for the
    within-group variance. This provides posterior inclusion
    probabilities (PIP) that each group (e.g., person) does not belong
    to the common within-group variance model.

2.  `pick_tau`:
    
    This model has a spike and slab on the random effects standard
    deviation in the scale model which captures between-group
    variability in the within-group variances. This provides a PIP that
    there is variation in the within-group variances. In the context of
    reliability, a large PIP indicates that measurment invariance does
    not hold, given there are group-level differences in so-called
    measurment error.

3.  `pick_none`:
    
    This model also provides group-specific reliability, but there is no
    spike and slab formulation.

4.  `customary`:
    
    This is the standard random intercept model that assumes a common
    within-group variance.

Note that options 1 and 2 provide Bayesian model averaged estimates for
the ICCs.

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
