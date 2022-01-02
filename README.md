# SC1_SM1_grp
Group project for SC1/SM1

## Installation
This package can be installed using devtools and the command
```{r}
devtools::install_github("hsansford1/higgsboson")
```
## Data
To load the data, use the function `data` with either `training` or `test` as input, i.e.
```{r}
data(training)
data(test)
```
Both datasets are very large with 30 feature columns, a weight column and a label column. The training data has 250,000 rows and the test data has 550,000 rows.  
