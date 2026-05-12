# Common parameters

Common parameters

## Arguments

- treatments:

  dataframe. Treatments

- outcome:

  character. Outcome type for the dataset. Either `binary` or
  `continuous`.

- outcome_measure:

  character. Outcome measure of the dataset. Either `OR`, `RR` or `RD`
  when `outcome` is `binary` or `MD` or `SMD` when `outcome` is
  `continuous`

- connected_data:

  dataframe. Input data set created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or `setup_exclude`

- ranking_option:

  character. `good` if the treatment effect is desirable, else `bad`

- reference_treatment:

  character. The reference treatment of the dataset

- effects:

  character. Type of model to fit, either `random` or `fixed`

- configured_data:

  list. Input dataset created by
  [`setup_configure()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_configure.md)
  or
  [`setup_exclude()`](https://apps.crsu.org.uk/MetaInsight/docs/reference/setup_exclude.md)

- seed:

  numeric. Seed used to fit the models.

- xmin:

  numeric. Minimum x-axis value. Default `NULL` in which case it is
  calculated internally

- xmax:

  numeric. Maximum x-axis value. Default `NULL` in which case it is
  calculated internally

- n_adapt:

  numeric. Number of adaptation iterations. Defaults to `5000` and can
  normally be left unchanged

- n_iter:

  numeric. Number of simulation iterations. Defaults to `20000` and can
  normally be left unchanged

- logger:

  Stores all notification messages to be displayed in the Log Window.
  Insert the logger reactive list here for running in shiny, otherwise
  leave the default `NULL`

- async:

  Whether or not the function is being used asynchronously. Default
  `FALSE`
