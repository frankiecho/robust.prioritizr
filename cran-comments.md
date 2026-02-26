Thank you for reviewing this submission. In this new version, I have addressed the previous comments from CRAN volunteers (please see below).

Cheers,

Frankie Cho

## Previous comments from CRAN volunteers

* \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user. That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a warning for the user. Does not seem necessary. Please replace \dontrun with \donttest. Please unwrap the examples if they are executable in < 5 sec, or replace dontrun{} with \donttest{}. For more details: <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>

  **I have updated the examples to remove the `\dontrun{}` commands. Since these examples may take longer than 5 seconds to complete, I have updated them to run conditionally when not executed during package checks (i.e., with the @examplesIf roxygen tag that implements the `if()` approach described in the cookbook).**

* still no man/ folder and no documentation. Please omit the @noRd-keywords in some of your 'roxygen2' sections of your .R-files to render .Rd-files. Ideally, you also provide an @example tag to those functions.

  **The package contains a man folder and documentation. After contacting the CRAN volunteer, they confirmed that this issue was due to a problem with downloading the package tarball and not the package submission.**

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
