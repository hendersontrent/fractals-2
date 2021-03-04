# fractals-2
Builds analysis for an upcoming paper that aims to extend the findings of our previous work at https://doi.org/10.1098/rsif.2020.0334

*This README was last updated 04/03/2021*

## Workflow and code pipeline

The project builds off a modular and logical code base. The `setup.R` script loads all the necessary packages and dependencies to run the project and also establishes folders such as `data/` and `output/` programmatically. The analytical code is intended to be run in the following order:

1. `priors.R` - loads in and preprocesses data and runs an uninformative Bayesian regression model on the data collected as part of [Fractals 1.0](https://github.com/hendersontrent/jeff-paper) and saves the posteriors to be used as informative priors for the current Fractals 2.0 work
2. `posteriors.R` - loads in and preprocesses data and runs a Bayesian regression model using the extracted priors from `priors.R` to produce better posterior estimates for the dataset at hand. The specificity of priors is important at these low sample sizes as the likelihood is unlikely to dominate the prior in posterior generation.

Any additional code is likely a standalone piece.
