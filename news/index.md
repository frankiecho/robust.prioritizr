# Changelog

## robust.prioritizr 1.1.0

- Fixed a matrix index mismatch issue in the robust CVaR constraint that
  caused infeasibility errors for problems. Problem can be replicated
  when there are zeros in the feature data that can cause the matrix
  size and inferred index to be calculated incorrectly

## robust.prioritizr 1.0.3

CRAN release: 2026-03-17

- Fix a small typo in the DESCRIPTION file

## robust.prioritizr 1.0.2

CRAN release: 2026-03-03

- Use `@examplesIf` instead of `\dontrun{}` for examples

## robust.prioritizr 1.0.1

- Update and re-render documentation based on CRAN volunteer comments

## robust.prioritizr 1.0.0

## robust.prioritizr 0.1.0.0

- First stable release

## robust.prioritizr 0.0.0.1

- Initial development version.
