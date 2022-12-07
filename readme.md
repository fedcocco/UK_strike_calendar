# strike-dates

This is the R project for extracting strike dates from the dataset downloaded from [strikemap.co.uk](https://www.strikemap.co.uk/). This task required heavily tailoring the data processing to the actual dataset. There were too many corner-cases to develop a reproducible solution in the short time available. **This code will not generalise to a future dataset.**

## Setup

Clone the project from GitHub, or alternatively just download the zip.

```zsh
git clone https://github.com/ft-interactive/strike-dates
```

Navigate into the project directory, open `strike-dates.Rproj`, and restore the environment. 

To do this, first make sure you have `renv` installed.

```r
install.packages("renv")
```

Then activate `renv`.

```r
renv::activate()
```

Then install the packages for the project with `renv::restore()`.

```r
renv::restore()
```

