# Contributing to policycraft

Contributions should help analysts examine data in time order and explain what
they find.

## Before proposing a change

1. Open an issue describing the policy-analysis need and intended users.
2. Explain the analytical assumptions, limitations, and failure modes.
3. Distinguish calculations from interpretations and recommendations.
4. Avoid language implying that a chart proves causation.

## Development workflow

```r
devtools::document()
devtools::test()
devtools::check()
pkgdown::build_site()
```

New exported functions require Roxygen documentation, examples, and tests. New
analytical rules require a citation or an explicit methodological rationale.
Changes to the Shiny app should include a smoke-test update and browser review.

Please keep pull requests focused and describe any user-visible behavior change.

## Writing

Use plain language in documentation, app labels, code comments, and release
notes. Say what the code does and why it matters. Avoid corporate slogans,
buzzwords, sales language, and inflated claims. Keep Dan's wording when adding
to text he has revised; change it only when the task calls for that change.
