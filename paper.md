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


\begin{align}
\text{ICC(1)} = \frac{\sigma^2_b}{\sigma^2_b + \sigma^2_w}
\end{align}


where $\sigma^2_b$ is the between-group variance and $\sigma^2_w$ the within-group variance. In
a one-way random effects model, $\sigma^2_w$ is essentially the $average$ within-group variance. 
However, if there are group-level differences in $\sigma^2_w$, this implies that there also
group-level variation in the ICC. The methodolgy in R package **vICC** was specifically designed
to quantify variation in ICC(1) by allowing $\sigma^2_w$ to vary. This can be used to 
indentying groups that are more (or less) homogenous, as well as which groups are adequately 
described by the customary ICC




# Statement of Need
