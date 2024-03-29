# circacompare 0.2.0

## Improvements

* create website for circacompare with pkgdown.

* support per-sample weights in `circacompare()`, `circa_single()`, `circa_single_mixed()` and `circacompare_mixed()`

* better error messages - also makes the shiny app more verbose for users

* allow the user to use `suppress_all` argument to show/hide messages during model fitting across all circacompare functions.

* allow user to specify use of linear model (non-mixed) for fitting models to each group when using circacompare_mixed(). (Faster computation!)

# circacompare 0.1.1

## Improvements

* User-definable parameters for all functions. Notably: period can be estimated rather than always user-defined, rhythmic characteristics can be selectively shared between groups, all parameters can have additional decay terms and those decay terms can have group-effects to determine differences in decay between groups.

* More informative summary tables from all models.

## Bug fixes

* More sensible default values for random effects.

* `nlme` is used for estimating presence of rhythmicity of each group when `circacompare_mixed()` is used, rather than `nls`.
