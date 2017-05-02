## ethnicolor: Impute Race and Ethnicity Based on Name

[![Build Status](https://travis-ci.org/soodoku/ethnicolor.svg?branch=master)](https://travis-ci.org/soodoku/ethnicolor)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ethnicolor)](https://cran.r-project.org/package=ethnicolor)
![](http://cranlogs.r-pkg.org/badges/grand-total/ethnicolor)

### Data

The data are from:

* [The 2000 Census](https://github.com/soodoku/ethnicolor/tree/master/data-raw/census)
* [The 2010 Census](https://github.com/soodoku/ethnicolor/tree/master/data-raw/census)
* The Florida Voter Registration File from February 2017. Request access to the data [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UBIG3F) or request your own copy from the Florida Secretary of State. 
* [Wikipedia Data](https://github.com/soodoku/ethnicolor/tree/master/data-raw/wiki)

### Included Models for Imputing Race

* `census_ln` will simply append data from the 2000 and 2010 census. For some mild processing that underlies it, see [here](https://github.com/soodoku/ethnicolor/tree/master/data-raw/census).
* `pred_census_ln` exposes a model based off Florida Voter Registration Last Name Data. For details, see [here]()
* `pred_fl_reg_ln` exposes a model built using Florida Voter Registration data. Utilizes only the surname. For details, see [here](https://github.com/soodoku/ethnicolor/tree/master/data-raw/fl_voter_reg)
* `pred_fl_reg_ln` exposes a model built using Florida Voter Registration data. Utilizes the first name, middle name (if there), last name, and suffix (if there). For details, see [here](https://github.com/soodoku/ethnicolor/tree/master/data-raw/fl_voter_reg)

### Installation

To get the current development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("soodoku/ethnicolor")
```

### Related Information

For Python package that provides roughly the same functionality, see [ethnicolr](https://github.com/soodoku/ethnicolr).

### License

Scripts are released under the [MIT License](https://opensource.org/licenses/MIT).

### Contributor Code of Conduct

The project welcomes contributions from everyone! In fact, it depends on it. To maintain this welcoming atmosphere, and to collaborate in a fun and productive way, we expect contributors to the project to abide by the [Contributor Code of Conduct](http://contributor-covenant.org/version/1/0/0/).