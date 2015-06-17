# adan
ADAN, a robust method for assessing the reliability of drug property predictions

## References

Carrió P, Pinto M, Ecker G, Sanz F, Pastor M. Applicability Domain Analysis (ADAN): A Robust Method for Assessing the Reliability of Drug Property Predictions. J Chem Inf Model 2014; 54: 1500–1511. http://dx.doi.org/10.1021/ci500172z

## Installation
```
library(devtools)
install_github("pauca/adan/adan")

library(adan)
vignette("adan")
```

## Example
```
adan.model <- 
  adan.build (train.md = train.md, # Molecular descriptors of train set
              train.a  = train.a,  # Actual values of train set
              train.p  = train.p,  # Predicted values of train set
              scale.md = FALSE
             )


adan.output <- 
  adan.test ( adan.model = adan.model, 
              query.md = test.md, # Molecular descriptors of test set
              query.p  = test.p   # Actual values of train  set
            )
```

