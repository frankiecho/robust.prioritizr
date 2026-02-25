## Resubmission

This is a resubmission. In this version I have:

* I have replaced `\dontrun{}` in my examples with an `@examplesIf` statement such that the example runs conditionally on a user's environment if the code is run outside of a test environment. The reason why `@examplesIf` is more suitable than `\donttest{}` in this context is because the examples involves solving intensive mathematical problems that could take more than 5 seconds to run on CRAN in some variants of the check process, depending on hardware. The `@examplesIf` statement uses a custom function in the package to check whether or not the code is run in a CMD CHECK environment.

Dear CRAN volunteers,

Thank you for reviewing this submission. This is the first time that the robust.prioritizr package has been submitted to CRAN. Although CRAN checks have flagged some words in the DESCRIPTION as possibly misspelled words, I confirm that these are false positives and the words are spelled correctly. Additionally, please note that several examples use the `\dontrun{}` commands to ensure that the package checks complete within a short period of time.

Cheers,

Frankie Cho

## R CMD check results

0 errors | 0 warnings | 1 note

* Possibly misspelled words in DESCRIPTION:
  Charnes (28:45)
  Rockafellar (30:6)
  Uryasev (30:20)
  prioritizations (14:18, 19:18, 21:52, 25:54, 27:33)
  prioritizr (18:19)

  **I confirm these words are spelled correctly. In particular, "prioritizations" is a term of art used in systematic conservation planning, prioritizr is the name of an R package available on CRAN, and "Charnes", "Rockafellar", and "Uryasev" are author names listed in citations.**

## Test environments

* [Ubuntu 24.04, R-release](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* [Ubuntu 24.04, R-devel](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* [Ubuntu 24.04, R-old-release](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* [MacOS 15 Arm64, R-release](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* [Windows Server 2025, R-release](hhttps://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

The package has no reverse dependencies on CRAN.
