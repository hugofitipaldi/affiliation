
# affiliarion <a href='https://github.com/hugofitipaldi/affiliation'><img src='man/figures/hex_aff.png' width="15%" align="right"  /></a>

<!-- badges: start - -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9000-blue.svg)](https://github.com/hugofitipaldi/affiliation)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/github/last-commit/hugofitipaldi/affiliation.svg)](https://github.com/hugofitipaldi/affiliation/commits/main)

<!-- badges: end -->

## Installation

Development version of the package can be installed from Github with:

``` r
install.packages("remotes")
remotes::install_github("hugofitipaldi/affiliation")
```

## Country of affiliation

``` r
affiliation::get_affiliations(PMID = "30237159")
#>   author_lastname author_firstname                       country_of_affiliation
#> 1       Fitipaldi             Hugo                              Sweden_NA_NA_NA
#> 2        McCarthy           Mark I          United Kingdom_United Kingdom_NA_NA
#> 3          Florez           Jose C United States_United States_United States_NA
#> 4          Franks           Paul W   Sweden_United Kingdom_United States_Sweden
#>                                                                                                                                                                                                                                                                                                                                                                                                                              affiliation_freetext
#> 1                                                                                                                                                                                                                                                                             Genetic and Molecular Epidemiology Unit, Department of Clinical Sciences Malmö, Lund University Diabetes Centre, Skåne University Hospital, Malmö, Sweden._NA_NA_NA
#> 2                                                                                                                                                                                                                                                                 Oxford Centre for Diabetes, Endocrinology and Metabolism, University of Oxford, Oxford, U.K._Wellcome Trust Centre for Human Genetics, University of Oxford, Oxford, U.K._NA_NA
#> 3                                                                                                                                                                                           Diabetes Unit and Center for Genomic Medicine, Massachusetts General Hospital, Boston, MA._Programs in Metabolism and Medical and Population Genetics, Broad Institute, Cambridge, MA._Department of Medicine, Harvard Medical School, Boston, MA._NA
#> 4 Genetic and Molecular Epidemiology Unit, Department of Clinical Sciences Malmö, Lund University Diabetes Centre, Skåne University Hospital, Malmö, Sweden paul.franks@med.lu.se._Oxford Centre for Diabetes, Endocrinology and Metabolism, University of Oxford, Oxford, U.K._Department of Nutrition, Harvard T.H. Chan School of Public Health, Boston, MA._Department of Public Health and Clinical Medicine, Umeå University, Umeå, Sweden.
```
