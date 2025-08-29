# robust.prioritizr: Spatial Conservation Planning Under Uncertainty

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
[![Coverage-Status](https://img.shields.io/codecov/c/github/frankiecho/roubust.prioritizr?label=Coverage)](https://app.codecov.io/gh/frankiecho/roubust.prioritizr/branch/main)

`robust.prioritizr` is an R package that extends the powerful `prioritizr` ecosystem to enable users to conduct planning for an uncertain future. It allows users to generate conservation plans that are robust to uncertainties in input data, such as those arising from climate change projections, species distribution models, or measurement errors.

Systematic conservation planning relies on optimization algorithms to identify the best areas to protect. However, the data used in this process—such as species habitat suitability-is often uncertain. `robust.prioritizr` provides the tools to overcome this challenge. It helps you create a portfolio of conservation sites that are most likely to achieve their targets across a range of possible future scenarios or data realizations.

Instead of relying on a single "best guess" for your data, you can provide multiple plausible realizations. For example, you can include species habitat projections from several different climate models and time periods. The package then finds a solution that meets your targets across all (or a specified proportion) of these scenarios, resulting in a more resilient and reliable conservation plan.

## Installation

You can install the development version of `robust.prioritizr` from GitHub:

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("frankiecho/robust.prioritizr")
```

## Usage Example

Here is a minimal example demonstrating how to build a robust conservation plan.

The key difference from a standard `prioritizr` workflow is the use of **groups**. Because a single feature (e.g., a species) is now represented by multiple data layers (each being a different scenario or "realization"), the `groups` argument tells the function which layers belong to the same feature.

```r
library(prioritizr)
library(robust.prioritizr)
library(terra)

# 1. Create some data
# Planning units with a uniform cost
pu_raster <- rast(matrix(rnorm(100), nrow = 10, ncol = 10))

# Create two "realizations" for a single species.
# Imagine these are habitat suitability maps from two different climate models.
# In realization 1, the habitat is in the upper half.
feature_1a <- pu_raster
values(feature_1a)[1:50] <- 1
values(feature_1a)[51:100] <- 0

# In realization 2, the habitat is in the lower half.
feature_1b <- pu_raster
values(feature_1b)[1:50] <- 0
values(feature_1b)[51:100] <- 1

# Stack the features into a single multi-layer SpatRaster
sim_features <- c(feature_1a, feature_1b)
names(sim_features) <- c("feature_1_scenario_A", "feature_1_scenario_B")

# 2. Define feature groups
# Both layers belong to the same feature, "feature_1"
feature_groups <- c("feature_1", "feature_1")

# 3. Build and solve the robust problem
# We want to protect 3 units of habitat for "feature_1",
# ensuring this target is met across BOTH scenarios.
p <- problem(pu_raster, sim_features) %>%
  add_robust_min_set_objective() %>%
  add_absolute_targets(30) %>%
  # Tell the problem how to group the feature layers
  add_constant_robust_constraints(groups = feature_groups) %>%
  add_default_solver(verbose = FALSE)

s <- solve(p)

# 4. Plot the solution
# The solution selects cells from both the upper and lower halves
# to ensure the target of 3 is met regardless of which scenario unfolds.
plot(s, main = "Robust Solution")
```

## Getting Help

If you have questions or suggestions, please post an issue on the [GitHub repository](https://github.com/frankiecho/robust.prioritizr/issues).