# Add robust minimum shortfall objective

Add an objective to a conservation planning problem that minimizes the
representation shortfalls for each feature group using the worst case
shortfall or a quantile of the shortfall distribution, whilst ensuring
that the total cost of the solution does not exceed a budget.

## Usage

``` r
add_robust_min_shortfall_objective(x, budget)
```

## Arguments

- x:

  [`prioritizr::problem()`](https://prioritizr.net/reference/problem.html)
  object.

- budget:

  `numeric` value specifying the maximum expenditure of the
  prioritization. For problems with multiple zones, the argument to
  `budget` can be (i) a single `numeric` value to specify a single
  budget for the entire solution or (ii) a `numeric` vector to specify a
  separate budget for each management zone.

## Value

An updated
[`prioritizr::problem()`](https://prioritizr.net/reference/problem.html)
object with the objective added to it.

## Details

The robust minimum shortfall objective seeks to find the set of planning
units that minimizes the representation shortfall for each feature
group, subject to a budget. In particular, a target shortfall reflects
difference between the target for a feature and the amount held by a
candidate solution, expressed as a proportion of the target. These
target shortfalls are then calculated for each of the features
associated with a feature group, and a representation shortfall is used
describe how well all the features associated with a particular feature
group are represented by a candidate solution. Thus this objective aims
to get as close as possible to reducing the representation shortfalls
shortfalls to zero, by getting as close as possible to reaching all of
the targets for the features associated with each of the feature groups.
In the robust minimum shortfall formulation, the algorithm attempts to
minimize the maximum shortfall within the feature group. In particular,
the chance constraint programming method (Charnes and Cooper 1959) is
used to formulate the optimization problem as a mixed integer linear
programming problem. With this method, the confidence level parameter
(i.e., specified per `conf_level` with
[`add_constant_robust_constraints()`](https://frankiecho.github.io/robust.prioritizr/reference/add_constant_robust_constraints.md)
or
[`add_variable_robust_constraints()`](https://frankiecho.github.io/robust.prioritizr/reference/add_variable_robust_constraints.md))
describes the quantile of the target shortfalls associated with the
feature group that should be minimized during optimization. For example,
if `conf_level = 1` for a feature group, then the 100th quantile is used
and this means that – after calculating the target shortfalls for each
feature associated with the feature group – the largest target shortfall
for the associated features is used to calculate the representation
shortfall for the feature group. If `conf_level = 0.5` for a feature
group, then the 50th quantile is used and this means that the median
target shortfall for the features associated with the group is used to
represent the representation shortfall for the feature group.

## Mathematical formulation

This objective can be expressed mathematically for a set of planning
units (\\I\\ indexed by \\i\\), a set of feature groups (\\J\\ indexed
by \\j\\), and a set of features associated with each feature group
(\\K\\ indexed by \\k\\). Let \\c_i\\ denote the cost of planning unit
\\i\\, \\R\_{ijk}\\ the amount of feature \\k\\ associated with planning
unit \\i\\ for feature group \\j\\, \\T\_{jk}\\ the target for each
feature \\k\\ in each feature group \\j\\, \\W\_{jk}\\ the weight for
each feature \\k\\ in each feature group \\j\\, and \\\alpha\\ the
confidence level for uncertainty (specified per `conf_level` with
[`add_constant_robust_constraints()`](https://frankiecho.github.io/robust.prioritizr/reference/add_constant_robust_constraints.md)
or
[`add_variable_robust_constraints()`](https://frankiecho.github.io/robust.prioritizr/reference/add_variable_robust_constraints.md)).
Additionally, to describe the decision variables, let \\x_i\\ denote the
status of the planning unit \\i\\ (e.g., specifying whether planning
unit \\i\\ has been selected or not with binary values), \\v\_{jk}\\ the
target shortfall for each feature \\k\\ associated with each feature
group \\j\\, and \\y_j\\ the representation shortfall for for each
feature group \\j\\. Given this terminology, the robust minimum
shortfall formulation of the reserve selection problem is formulated as
follows.

\$\$ \mathit{Minimize} \space \sum\_{j = 1}^{J} y_j \times
\frac{\sum\_{k = 1}^{K} W\_{jk}}{K} \\ \mathit{subject \space to} \\
\sum\_{i = 1}^{I} x_i c_i \leq B \\ \Pr\_ k\\\sum\_{i = 1}^{I} ( x_i
\times R\_{ijk} ) + ( T\_{jk} \times v\_{jk} ) \geq T\_{jk} \\ \geq
\alpha \quad \forall j \in J \\ y_j \geq v\_{jk} \quad \forall j \in J,
k \in K \\ 0 \leq y_j \leq 1 \quad \forall j \in J \$\$

Here, the objective function (first equation) is to minimize the
weighted sum of the representation shortfalls for each feature group. In
particular, the representation shortfall for a given feature group is
weighted according to the average weight value of the features
associated with the feature group. The budget constraints (second
equation) ensure that the solution does not exceed the budget. The
probabilistic constraints (third equation) specify that only some of the
target shortfall variables (i.e., \\v\_{jk}\\) associated with each
feature group are used to calculate the representation shortfall for
each feature group, and the subset of target shortfall variables that
are used is based on the confidence level (i.e., \\\alpha\\). For
example, if \\\alpha=1\\, then all of the target shortfall variables
associated with each feature group must be used for the calculations.
Alternatively, if \\\alpha=0.5\\, then only enough of the target
shortfall variables are required for the calculations to achieve a 50%
chance of correctly calculating the target shortfall variables for a
given feature group. The representation shortfall constraints (fourth
equation) ensure that the representation shortfall variable for each
feature group must be greater than or equal to the target shortfall
variables of the features associated with the feature group. In
combination with the other constraints, this means that the
representation shortfall variable for a given feature group is
calculated as the largest value of a subset of the target shortfall
variables for the features associated with the feature group, and this
particular subset is based on the confidence level. Thus if \\\alpha\\
is closer to a value of 1, then the representation shortfall variable
for each feature group is calculated with a greater degree of certainty
and, in turn, the optimization process seeks a solution that is more
robust to uncertainty. Since the probabilistic constraints are
non-linear, an approximation method is used to linearize them so that
the optimization problem can be solved with mixed integer programming
exact algorithm solvers.

The chance constraint programming method is used to linearize the
probabilistic constraints (Charnes and Cooper 1959). To describe this
method, let \\M\_{jk}\\ denote a binary auxiliary variable for each
feature \\k\\ associated with feature group \\j\\. Also \\K_j\\ denote a
pre-computed value describing the number of features associated with
each feature group \\j\\. Given this terminology, the method involves
replacing the probabilistic constraints with the following linear
constraints.

\$\$ \sum\_{i = 1}^{I} (x_i \times R\_{ijk}) + (T\_{jk} \times y_j) +
(T\_{jk} \times M\_{jk}) \geq T\_{jk} \quad \forall \space j \in J,
\space k \in K \\ \sum\_{k = 1}^{K_j} \frac{M\_{jk}}{K_j} \leq 1 -
\alpha \quad \forall \space j \in J\\ M\_{jk} \in \\0, 1\\ \$\$

Here, the solution calculates the representation shortfall variable for
a given feature group based on a particular subset of the target
shortfalls for the associated features. Specifically, this subset based
on a particular number of the smallest target shortfall variables based
on \\\alpha\\. For example, if a feature group is associated with 40
features and \\\alpha=0.75\\, then the representation shortfall for the
feature group is calculated by identifying which 10 of these 40 features
have the largest target shortfall variables, and the shortfall variable
used in the optimization process is the minimum of these large shortfall
values. As such, the chance constraint programming method provides an
intuitive approximation of the probabilistic constraints.

## References

Charnes A & Cooper WW (1959) Chance-constrained programming. *Management
Science*, 6(1), 73–79.

## See also

Other functions for adding robust objectives:
[`add_robust_min_set_objective()`](https://frankiecho.github.io/robust.prioritizr/reference/add_robust_min_set_objective.md)

## Examples

``` r
# Load packages
library(prioritizr)
library(terra)

# Get planning unit data
pu <- get_sim_pu_raster()

# Get feature data
features <- get_sim_features()

# Define the feature groups,
# Here, we will assign the first 2 features to the group A, and
# the remaining features to the group B
groups <- c(rep("A", 2), rep("B", nlyr(features) - 2))

# Build problem with budget calculated as 30% total cost
p <-
  problem(pu, features) %>%
  add_robust_min_shortfall_objective(
    budget = terra::global(pu, "sum", na.rm = TRUE)[[1]] * 0.3
  ) %>%
  add_constant_robust_constraints(groups = groups, conf_level = 0.4) %>%
  add_binary_decisions() %>%
  add_relative_targets(0.3) %>%
  add_default_solver(verbose = FALSE)

# Solve the problem
soln <- solve(p)
#> ℹ  The targets for these groups are transformed based on the `mean()` target
#>   value.

# Plot the solution
plot(soln)
```
