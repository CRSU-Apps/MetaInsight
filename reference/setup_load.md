# Load data

Load data from a spreadsheet or a default dataset and assess the data
for validity. This checks the column names for required columns and
balanced wide format numbered columns. Data can be in either a long or
wide format; long data has one row per study arm whereas wide data has
one row per study. For continuous outcomes, long data should contain the
columns: `Study` - an identifier, e.g. author and year, `T` - treatment,
`N` - number of participants, `Mean` - mean value of the outcome, `SD` -
standard deviation of the outcome. Wide data for continuous outcomes
should contain: `Study`, `N.1`, `N.2`, `Mean.1`, `Mean.2`, `SD.1`,
`SD.2` where the number refers to the arm of the study and extra columns
should be added depending on the number of arms. For binary outcomes,
long data should contain: `Study`, `T`, `N` (as for continuous data) and
`R` - the number of participants with the outcome of interest. Wide data
for binary outcomes should follow the same convention: `Study`, `T.1`,
`T.2`, `R.1`, `R.2`, `N.1`, `N.2`. Additionally, a `covar.<name>` column
can be added to all formats containing covariate data where `<name>`
should be replaced with the name of the covariate. For long data,
covariate values must be equal for every study arm. Risk of bias data
can also be included with all columns containing values ranging from 1
(low risk) to 3 (high risk): `rob` for the overall risk of bias,
`indirectness` for indirectness and `rob.<name>` for up to ten
individual components.

## Usage

``` r
setup_load(data_path = NULL, outcome, logger = NULL)
```

## Arguments

- data_path:

  character. Path to the file (either a `.csv` or `.xlsx`) to be loaded
  or if `NULL` load the default data

- outcome:

  character. Outcome type for the dataset. Either `binary` or
  `continuous`.

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

## Value

List containing:

- is_data_valid:

  logical. Whether the data is valid

- is_data_uploaded:

  logical. Whether the data is uploaded

- data:

  dataframe. The data that was uploaded or the default data if no
  data_path was provided

- treatments:

  Dataframe of the treatments in the data. `NULL` if `is_data_valid` is
  `FALSE`

- outcome:

  character. Whether the data is `binary` or `continuous`

## Examples

``` r
# load data from a file
minimal_data_path <- system.file("extdata", "continuous_minimal.csv", package = "metainsight")
loaded_data <- setup_load(data_path = minimal_data_path,
                          outcome = "continuous")

# load default data
loaded_data <- setup_load(outcome = "binary")
```
