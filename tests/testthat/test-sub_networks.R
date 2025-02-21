
test_that("CreateGraph() contains correct links", {
  data <- read.csv("data\\Test_directness.csv")
  graph <- CreateGraph(data)
  
  expected_connected_to <- list(
    "A" = c("B", "C", "D"),
    "B" = c("A", "C"),
    "C" = c("A", "B"),
    "D" = "A"
  )
  connected_to <- list(
    "A" = unique(names(graph[["A"]][[1]])),
    "B" = unique(names(graph[["B"]][[1]])),
    "C" = unique(names(graph[["C"]][[1]])),
    "D" = unique(names(graph[["D"]][[1]]))
  )
  
  expect_equal(expected_connected_to, connected_to)
})

test_that("CreateGraph() excludes links correctly", {
  data <- read.csv("data\\Test_directness.csv")
  graph <- CreateGraph(data, exclude = c("A", "B"))
  
  expected_connected_to <- list(
    "A" = c("C", "D"),
    "B" = "C",
    "C" = c("A", "B"),
    "D" = "A"
  )
  connected_to <- list(
    "A" = unique(names(graph[["A"]][[1]])),
    "B" = unique(names(graph[["B"]][[1]])),
    "C" = unique(names(graph[["C"]][[1]])),
    "D" = unique(names(graph[["D"]][[1]]))
  )
  
  expect_equal(expected_connected_to, connected_to)
})

test_that("IdentifySubNetworks() finds single subnetwork for fully connected network for continuous long format", {
  data <- CleanData(read.csv("data/Cont_long.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df)
  
  expect_equal(length(subnets), 1)
  expect_equal(!!names(subnets), c("subnet_1"))
  expect_equal(!!subnets$subnet_1$treatments, treatment_df$Number)
  expect_equal(!!subnets$subnet_1$studies, unique(data$Study))
})

test_that("IdentifySubNetworks() finds single subnetwork for fully connected network for continuous wide format", {
  data <- CleanData(read.csv("data/Cont_wide.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df)
  
  expect_equal(length(subnets), 1)
  expect_equal(!!names(subnets), c("subnet_1"))
  expect_equal(!!subnets$subnet_1$treatments, treatment_df$Number)
  expect_equal(!!subnets$subnet_1$studies, unique(data$Study))
})

test_that("IdentifySubNetworks() finds single subnetwork for fully connected network for binary long format", {
  data <- CleanData(read.csv("data/Binary_long.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df)
  
  expect_equal(length(subnets), 1)
  expect_equal(!!names(subnets), c("subnet_1"))
  expect_equal(!!subnets$subnet_1$treatments, treatment_df$Number)
  expect_equal(!!subnets$subnet_1$studies, unique(data$Study))
})

test_that("IdentifySubNetworks() finds single subnetwork for fully connected network for binary wide format", {
  data <- CleanData(read.csv("data/Binary_wide.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df)
  
  expect_equal(length(subnets), 1)
  expect_equal(!!names(subnets), c("subnet_1"))
  expect_equal(!!subnets$subnet_1$treatments, treatment_df$Number)
  expect_equal(!!subnets$subnet_1$studies, unique(data$Study))
})

test_that("IdentifySubNetworks() finds multiple subnetworks for disconnected network for continuous long format", {
  data <- CleanData(read.csv("data/continuous_long_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df)
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(1, 2, 3, 4, 8))
  expect_equal(!!subnets$subnet_1$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(5, 6, 7, 9))
  expect_equal(!!subnets$subnet_2$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(10, 11))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() finds multiple subnetworks for disconnected network for continuous wide format", {
  data <- CleanData(read.csv("data/continuous_wide_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df)
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(1, 2, 5, 6, 8))
  expect_equal(!!subnets$subnet_1$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(3, 7, 9, 11))
  expect_equal(!!subnets$subnet_2$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(4, 10))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() finds multiple subnetworks for disconnected network for binary long format", {
  data <- CleanData(read.csv("data/binary_long_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df)
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(1, 2, 3, 4, 8))
  expect_equal(!!subnets$subnet_1$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(5, 6, 7, 9))
  expect_equal(!!subnets$subnet_2$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(10, 11))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() finds multiple subnetworks for disconnected network for binary wide format", {
  data <- CleanData(read.csv("data/binary_wide_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df)
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(1, 2, 5, 6, 8))
  expect_equal(!!subnets$subnet_1$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(3, 7, 9, 11))
  expect_equal(!!subnets$subnet_2$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(4, 10))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() orders subnetworks with reference treatment for continuous long format", {
  data <- CleanData(read.csv("data/continuous_long_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df, "E")
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(5, 6, 7, 9))
  expect_equal(!!subnets$subnet_1$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(1, 2, 3, 4, 8))
  expect_equal(!!subnets$subnet_2$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(10, 11))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() orders subnetworks with reference treatment for continuous wide format", {
  data <- CleanData(read.csv("data/continuous_wide_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df, "E")
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(3, 7, 9, 11))
  expect_equal(!!subnets$subnet_1$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(1, 2, 5, 6, 8))
  expect_equal(!!subnets$subnet_2$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(4, 10))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() orders subnetworks with reference treatment for binary long format", {
  data <- CleanData(read.csv("data/binary_long_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df, "E")
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(5, 6, 7, 9))
  expect_equal(!!subnets$subnet_1$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(1, 2, 3, 4, 8))
  expect_equal(!!subnets$subnet_2$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(10, 11))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() orders subnetworks with reference treatment for binary wide format", {
  data <- CleanData(read.csv("data/binary_wide_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- IdentifySubNetworks(data, treatment_df, "E")
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(3, 7, 9, 11))
  expect_equal(!!subnets$subnet_1$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(1, 2, 5, 6, 8))
  expect_equal(!!subnets$subnet_2$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(4, 10))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() uses default ordering for invalid reference treatement for continuous long format", {
  data <- CleanData(read.csv("data/continuous_long_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- expect_warning(IdentifySubNetworks(data, treatment_df, "Omlette"))
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(1, 2, 3, 4, 8))
  expect_equal(!!subnets$subnet_1$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(5, 6, 7, 9))
  expect_equal(!!subnets$subnet_2$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(10, 11))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() uses default ordering for invalid reference treatement for continuous wide format", {
  data <- CleanData(read.csv("data/continuous_wide_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- expect_warning(IdentifySubNetworks(data, treatment_df, "Omlette"))
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(1, 2, 5, 6, 8))
  expect_equal(!!subnets$subnet_1$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(3, 7, 9, 11))
  expect_equal(!!subnets$subnet_2$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(4, 10))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() uses default ordering for invalid reference treatement for binary long format", {
  data <- CleanData(read.csv("data/binary_long_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- expect_warning(IdentifySubNetworks(data, treatment_df, "Omlette"))
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(1, 2, 3, 4, 8))
  expect_equal(!!subnets$subnet_1$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(5, 6, 7, 9))
  expect_equal(!!subnets$subnet_2$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(10, 11))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})

test_that("IdentifySubNetworks() uses default ordering for invalid reference treatement for binary wide format", {
  data <- CleanData(read.csv("data/binary_wide_disconnected.csv"))
  treatment_df <- CreateTreatmentIds(FindAllTreatments(data))
  data <- ReplaceTreatmentIds(data, treatment_df)
  
  subnets <- expect_warning(IdentifySubNetworks(data, treatment_df, "Omlette"))
  
  expect_equal(length(subnets), 3)
  expect_equal(!!names(subnets), c("subnet_1", "subnet_2", "subnet_3"))
  
  expect_equal(!!subnets$subnet_1$treatments, c(1, 2, 5, 6, 8))
  expect_equal(!!subnets$subnet_1$studies, c("Uno", "Deux", "Three", "Cinque", "Six"))
  
  expect_equal(!!subnets$subnet_2$treatments, c(3, 7, 9, 11))
  expect_equal(!!subnets$subnet_2$studies, c("Quatro", "Sept"))
  
  expect_equal(!!subnets$subnet_3$treatments, c(4, 10))
  expect_equal(!!subnets$subnet_3$studies, c("Ocho"))
})