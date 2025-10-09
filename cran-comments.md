Dear CRAN volunteers,

Thank you for reviewing this submission. This is the first time that the robust.prioritizr package has been submitted to CRAN. Although CRAN checks have flagged some words in the DESCRIPTION as possibly misspelled words, I confirm that these are false positives and the words are spelled correctly. Additionally, please note that several of the examples of use `\dontrun{}` to ensure that the package checks do not take too to complete.

Cheers,

Frankie Cho

## R CMD check results

0 errors | 0 warnings | 1 note

* Possibly misspelled words in DESCRIPTION:
    Charnes (24:58)
    prioritizations (15:51, 21:60, 23:39)
    Prioritizations (17:58)
    Rockafellar (25:6)
    Uryasev (25:20)

  **I confirm these words are spelled correctly. In particular, "prioritizatons" is a term of art used in systematic conservation planning, and "Charnes", "Rockafellar", and "Uryasev" are author names for citations.**

## Test environments

* [Ubuntu 24.04, R-release](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* [Ubuntu 24.04, R-devel](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* [Ubuntu 24.04, R-old-release](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* [MacOS 15 Arm64, R-release](https://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* [Windows Server 2025, R-release](hhttps://github.com/frankiecho/robust.prioritizr/actions/workflows/R-CMD-check.yaml)
* Windows Server 2008 (x64), R-devel (win-builder)

## Downstream dependencies

The package has no reverse dependencies on CRAN.
