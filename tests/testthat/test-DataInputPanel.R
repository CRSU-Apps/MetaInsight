
test_that("Continuous data registered as initially not uploaded", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Continuous' }), {
    expect_false(data_uploaded())
  })
})

test_that("Continuous data registered as uploaded on upload", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Continuous' }), {
    session$setInputs(data = data.frame(datapath = 'Cont_long.csv'))
    expect_true(data_uploaded())
  })
})

test_that("Treatments extracted from default continuous file", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Continuous' }), {
    treatment_df <- data.frame(Number = seq(6),
                               Label = c('the Great','the Younger','the Dung-named','the Little','the Butcher','the Slit-nosed'))
    
    expect_equal(treatment_list(), treatment_df)
  })
})

test_that("Treatments reordered from default continuous file when reference selected", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Continuous' }), {
    session$setInputs(reference_treatment = 'the Dung-named')
    treatment_df <- data.frame(Number = seq(6),
                               Label = c('the Dung-named','the Great','the Younger','the Little','the Butcher','the Slit-nosed'))
    
    expect_equal(treatment_list(), treatment_df)
  })
})

test_that("Data extracted from default continuous file", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Continuous' }), {
    expect_equal(colnames(data()), c('Study', 'T', 'N', 'Mean', 'SD'),
                 label = format_vector_to_string(colnames(data())))
    expect_equal(nrow(data()), 8,
                 label = nrow(data()))
    expect_equal(data()$Study, c('Constantine', 'Constantine', 'Constantine', 'Leo', 'Leo', 'Leo', 'Justinian', 'Justinian'),
                 label = format_vector_to_string(data()$Study))
    expect_equal(data()$T, c('the Great', 'the Younger', 'the Dung-named', 'the Little', 'the Great', 'the Butcher', 'the Great', 'the Slit-nosed'),
                 label = format_vector_to_string(data()$T))
    expect_equal(data()$N, c(30, 31, 32, 33, 34, 35, 36, 37),
                 label = format_vector_to_string(data()$N))
    expect_equal(data()$Mean, c(-1.0, -1.1, -1.2, -1.3, -1.4, -1.5, -1.6, -1.7),
                 label = format_vector_to_string(data()$Mean))
    expect_equal(data()$SD, c(11.1, 12.2, 13.3, 14.4, 15.5, 16.6, 17.7, 18.8),
                 label = format_vector_to_string(data()$SD))
  })
})

test_that("Continuous data passed back to module parent", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Continuous' }), {
    expect_equal(length(session$returned), 2)
    expect_equal(session$returned$data(), data())
    expect_equal(session$returned$treatment_list(), treatment_list())
  })
})

test_that("Binary data registered as initially not uploaded", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Binary' }), {
    expect_false(data_uploaded())
  })
})

test_that("Binary data registered as uploaded on upload", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Binary' }), {
    session$setInputs(data = data.frame(datapath = 'Binary_long.csv'))
    expect_true(data_uploaded())
  })
})

test_that("Treatments extracted from default binary file", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Binary' }), {
    treatment_df <- data.frame(Number = seq(6),
                               Label = c('the Great','the Younger','the Dung-named','the Little','the Butcher','the Slit-nosed'))
    
    expect_equal(treatment_list(), treatment_df)
  })
})

test_that("Treatments reordered from default binary file when reference selected", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Binary' }), {
    session$setInputs(reference_treatment = 'the Dung-named')
    treatment_df <- data.frame(Number = seq(6),
                               Label = c('the Dung-named','the Great','the Younger','the Little','the Butcher','the Slit-nosed'))
    
    expect_equal(treatment_list(), treatment_df)
  })
})

test_that("Data extracted from default binary file", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Binary' }), {
    expect_equal(colnames(data()), c('Study', 'T', 'R', 'N'),
                 label = format_vector_to_string(colnames(data())))
    expect_equal(nrow(data()), 8,
                 label = nrow(data()))
    expect_equal(data()$Study, c('Constantine', 'Constantine', 'Constantine', 'Leo', 'Leo', 'Leo', 'Justinian', 'Justinian'),
                 label = format_vector_to_string(data()$Study))
    expect_equal(data()$T, c('the Great', 'the Younger', 'the Dung-named', 'the Little', 'the Great', 'the Butcher', 'the Great', 'the Slit-nosed'),
                 label = format_vector_to_string(data()$T))
    expect_equal(data()$R, c(30, 31, 32, 33, 34, 35, 36, 37),
                 label = format_vector_to_string(data()$R))
    expect_equal(data()$N, c(100, 101, 102, 103, 104, 105, 106, 107),
                 label = format_vector_to_string(data()$N))
  })
})

test_that("Binary data passed back to module parent", {
  testServer(data_input_panel_server, args = list(metaoutcome = function() { 'Binary' }), {
    expect_equal(length(session$returned), 2)
    expect_equal(session$returned$data(), data())
    expect_equal(session$returned$treatment_list(), treatment_list())
  })
})