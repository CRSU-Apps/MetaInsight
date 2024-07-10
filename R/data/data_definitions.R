
# Regular expression explanation:
# ^ = Start of string
# (?i) = Ignore case for matching
# (\\.([0-9]+))? = Optional group of full stop, followed by at least one digit
# $ = End of string
# (.+) = Group of at least one character

.study_definition <- data.frame(
  name = "Study",
  required = TRUE,
  type_check = "is.character",
  type_name = "character (text)",
  pattern = "^(?i)Study(\\.([0-9]+))?$",
  replacement ="Study\\1",
  number_group = NA
)

.t_definition <- data.frame(
  name = "T",
  required = TRUE,
  type_check = "is.character",
  type_name = "character (text)",
  pattern = "^(?i)T(\\.([0-9]+))?$",
  replacement ="T\\1",
  number_group = "\\2"
)

.n_definition <- data.frame(
  name = "N",
  required = TRUE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)N(\\.([0-9]+))?$",
  replacement ="N\\1",
  number_group = "\\2"
)

.mean_definition <- data.frame(
  name = "Mean",
  required = TRUE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)Mean(\\.([0-9]+))?$",
  replacement ="Mean\\1",
  number_group = "\\2"
)

.sd_definition <- data.frame(
  name = "SD",
  required = TRUE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)SD(\\.([0-9]+))?$",
  replacement ="SD\\1",
  number_group = "\\2"
)

.covariate_definition <- data.frame(
  name = "covar.*",
  required = FALSE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)covar\\.(.+)$",
  replacement ="covar.\\1",
  number_group = NA
)

.r_definition <- data.frame(
  name = "R",
  required = TRUE,
  type_check = "is.numeric",
  type_name = "numeric",
  pattern = "^(?i)R(\\.([0-9]+))?$",
  replacement ="R\\1",
  number_group = "\\2"
)

continuous_column_names <- data.frame() %>%
  rbind(.study_definition) %>%
  rbind(.t_definition) %>%
  rbind(.n_definition) %>%
  rbind(.mean_definition) %>%
  rbind(.sd_definition) %>%
  rbind(.covariate_definition)

binary_column_names <- data.frame() %>%
  rbind(.study_definition) %>%
  rbind(.t_definition) %>%
  rbind(.r_definition) %>%
  rbind(.n_definition) %>%
  rbind(.covariate_definition)
