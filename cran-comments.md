Dear CRAN volunteers,

In this version I have:
* Updated the package documentation to ensure compliance with the "ropensci-review-tools/autotest" package
* Added new "Software Review Roclets" (SRR) tags ("ropensci-review-tools/srr") to prepare the package for a review process in rOpenSci. 
* Updated citation keys to a preprint manuscript of the package.

Thank you.

Best regards,
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
