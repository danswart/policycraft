# Superintendent overlay playground

This is an isolated copy of `inst/app`. Changes here do **not** affect the
package's working `launch_longitudinal()` app.

The experiment adds prominent double-headed term-span arrows and enlarged
superintendent labels to the Run Chart,
Line Chart, Untrended Expectation Chart, Trended Expectation Chart, and Cohort
Chart. Each visible arrow is clipped to the filtered chart range and labeled
with the superintendent's name and stated term.

From the package root, launch it with:

```r
source("playground/superintendent-overlays/launch_playground.R")
```

The term boundaries live in `app/global.R` as
`SCUC_SUPERINTENDENT_TERMS`. The 2001 and 2010 transitions are represented as
January 1 boundaries because only years were supplied. The exact supplied
appointment dates are used for Dr. Ealy (February 3, 2020) and Mrs. Meloni
(February 1, 2024).
