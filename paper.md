---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'vICC: Varying Intraclass Correlation Coefficients in R'
tags:
- intraclass correlation coefficients
- mixed-effects model
- spike-and-slab
- random effects
- reliability
authors:
  - name: Donald R. Williams
    affiliation: 1 # (Multiple affiliations must be quoted)
affiliations:
 - name: Department of Psychology, University of California, Davis
   index: 1
citation_author: Williams
date: 08 December 2020
year: 2020
bibliography: inst/REFERENCES.bib
---

# Summary
In mixed-effects (a.k.a, hierarchical or multilevel) models, intraclass
correlation coefficients (ICC) are commonly computed, with applications spanning 
from characterizing group-level homogeneity (X) to measurment reliabilty (X). While 
there are a wide spectrum of applications, an underlying assumption is that the 
variance components used in their computation are fixed and non-varying. Consider
the case of ICC(1), that is,

$$
\frac{\sigma_2_b}{\sigma_2_b + \sigma_2_w}
$$

which is computed from a one-way random effects model.

# Statement of Need
