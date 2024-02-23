# Building sector model with heterogeuous renovation and construction of
    the stock

R package **brick**, version **0.2.0**

[![CRAN status](https://www.r-pkg.org/badges/version/brick)](https://cran.r-project.org/package=brick)  [![R build status](https://github.com/pik-piam/brick/workflows/check/badge.svg)](https://github.com/pik-piam/brick/actions) [![codecov](https://codecov.io/gh/pik-piam/brick/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/brick) 

## Purpose and Functionality

This building stock model represents residential and commercial
    buildings at customisable regional and temporal resolution. The building
    stock is quantified in floor area and distinguished by building type 
    (SFH/MFH) and location (rural/urban). In each building category,
    construction cohorts are tracked explicitly. This allows to characterise
    buildings specifically for each of subset of buildings. The evolution of the
    building stock follows from the flows of constructed, renovated and
    demolished buildings and is optimised under cost minimisation with a benefit
    for heterogeneity in the choice of construction and renovation alternatives.
    This benefit captures heterogeneity in the preferences of the agents and
    the building structure.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("brick")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Robin Hasse <robin.hasse@pik-potsdam.de>.

## Citation

To cite package **brick** in publications use:

Hasse R, Rosemann R (2024). _brick: Building sector model with heterogeuous renovation and construction of the stock_. R package version 0.2.0, <URL: https://github.com/pik-piam/brick>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {brick: Building sector model with heterogeuous renovation and construction of
the stock},
  author = {Robin Hasse and Ricarda Rosemann},
  year = {2024},
  note = {R package version 0.2.0},
  url = {https://github.com/pik-piam/brick},
}
```
