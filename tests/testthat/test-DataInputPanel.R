
test_that("Continuous data registered as initially not uploaded", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Continuous')
    
    expect_false(data_uploaded())
  })
})

test_that("Continuous data registered as uploaded on upload", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Continuous')
    
    session$setInputs(data = data.frame(datapath = 'Non_opioids_long.csv'))
    expect_true(data_uploaded())
  })
})

test_that("Treatments extracted from default continuous file", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Continuous')
    
    treatment_df <- data.frame(Number = seq(4),
                               Label = c('Glucocorticoids','Placebo','Ketamine','Gabapentinoids'))
    expect_equal(treatment_list(), treatment_df)
  })
})

test_that("Treatments reordered from default continuous file when reference selected", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Continuous')
    
    session$setInputs(reference_treatment = 'Placebo')
    treatment_df <- data.frame(Number = seq(4),
                               Label = c('Placebo','Glucocorticoids','Ketamine','Gabapentinoids'))

    expect_equal(treatment_list(), treatment_df)
  })
})

test_that("Data extracted from default continuous file", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Continuous')
    
    expect_equal(colnames(data()), c('Study', 'T', 'Mean', 'SD', 'N', 'covar.age'),
                 label = format_vector_to_string(colnames(data())))
    expect_equal(nrow(data()), 90,
                 label = nrow(data()))
    expect_equal(data()$Study, rep(1:45, each = 2),
                 label = format_vector_to_string(data()$Study))
    expect_equal(data()$T, c(rep(c("Glucocorticoids", "Placebo"), times = 4),
                             rep(c("Ketamine", "Placebo"), times = 18),
                             rep(c("Gabapentinoids", "Placebo"), times = 23)
                             ),
                 label = format_vector_to_string(data()$T))
    expect_equal(data()$N, c(62, 50,	92,	45,	78,	88,	46,	47,	43,	52,	84,	84,	33,	34,	34,	35,	73,	66,	18,	19,	8, 6, 31,	47,	25,	24,	15,	17,	33,	35,	49,	42,	118, 62, 22, 22, 8,	7, 55, 25, 39, 38, 31, 30, 21, 10, 65, 53, 20, 20, 27, 35, 23, 22, 2,	4, 60, 30, 50, 50, 20, 20, 30, 29, 17,	24,	50,	49,	83,	28,	100, 100, 22,	24,	52,	48,	62,	31,	18,	20,	16,	18,	46,	46,	27,	27,	70,	35,	75,	76),
                 label = format_vector_to_string(data()$N))
    expect_equal(data()$Mean, c(1.9, 2.2,	2, 2,	1.4, 1.4,	2.6, 2.9,	3, 5.4,	1, 1,	0, 0,	0, 0,	4, 4,	1.1, 0.3,	2, 0.5,	1.4, 1.3,	0, 0, 0.3, 1.5,	0.7, 1,	4.3, 4,	2, 2,	0, 1.5,	2.4, 3.4,	0.5, 1,	0.2, 0.1,	0.8, 0.8,	3.1, 3.4,	0, 0,	0.2, 0.6,	0.5, 0.6,	2.6, 2.2,	3, 2.5,	0.5,	1.2, 2.1,	2.2, 0.1,	0.2, 1,	2, 0.9,	1.1, 2,	3, 1.8,	2, 2.5,	3.4, 1.5,	1.9, 0,	0, 2,	3, 1.1,	1.5, 1,	0.5, 0,	0, 1.3,	1.5, 0.4,	0.03, 1.4,	1.2),
                 label = format_vector_to_string(data()$Mean))
    expect_equal(data()$SD, c(2.4,	2.5,	2.8,	3.7,	1.7,	1.6,	2.4,	2.5,	3.5,	3.6,	1.5,	1.5,	0.7,	0.2,	0.3,	0.1,	2.2,	3.3,	2.1,	0.7,	1.7,	3.7,	1.4,	1.5,	0.7,	1.9,	3,	4.4,	1.4,	1.7,	2.5,	2.1,	1.5,	1.5,	1.5,	2.2,	1.1,	3,	1.7,	1.8,	0.7,	0.7,	0.9,	1.1,	1.7,	1.6,	0.7,	0.7,	0.4,	0.9,	0.9,	0.1,	1.7,	1.4,	0.8,	1,	0.9,	1.5,	1.2,	0.9,	0.1,	0.1,	0.6,	1.5,	1.2,	1.5,	1.5,	1.9,	2,	2.5,	1.2,	1.4,	1.9,	2,	2.1,	0.7,	0.5,	0.8,	1.6,	2,	1.5,	1.5,	0.7,	0.7,	2.2,	2.8,	0.9,	0.2,	1.7,	1.7),
                 label = format_vector_to_string(data()$SD))
  })
})

test_that("Continuous data passed back to module parent", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Continuous')
    
    expect_equal(length(session$returned), 4)
    expect_equal(session$returned$data(), data())
    expect_equal(session$returned$is_default_data(), is_default_data())
    expect_equal(session$returned$treatment_list(), treatment_list())
    expect_equal(session$returned$metaoutcome(), session$input$metaoutcome)
  })
})

test_that("Continuous long data matches between .csv and .xlsx files", {
  testServer(data_input_panel_server, {
    session$setInputs(data = data.frame(datapath = 'Non_opioids_long.csv'), metaoutcome = 'Continuous')
    csv_data = data()
    
    session$setInputs(data = data.frame(datapath = 'Non_opioids_long.xlsx'))
    xlsx_data = data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Continuous wide data matches between .csv and .xlsx files", {
  testServer(data_input_panel_server, {
    session$setInputs(data = data.frame(datapath = 'Non_opioids_wide.csv'), metaoutcome = 'Continuous')
    csv_data = data()
    
    session$setInputs(data = data.frame(datapath = 'Non_opioids_wide.xlsx'))
    xlsx_data = data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Binary data registered as initially not uploaded", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Binary')
    
    expect_false(data_uploaded())
  })
})

test_that("Binary data registered as uploaded on upload", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Binary')
    
    session$setInputs(data = data.frame(datapath = 'Certolizumab_long.csv'))
    expect_true(data_uploaded())
  })
})

test_that("Treatments extracted from default binary file", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Binary')
    
    treatment_df <- data.frame(Number = seq(7),
                               Label = c('Placebo','Infliximab','Adalimumab','Tocilizumab','CZP','Rituximab', 'Etanercept'))

    expect_equal(treatment_list(), treatment_df)
  })
})

test_that("Treatments reordered from default binary file when reference selected", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Binary')
    
    session$setInputs(reference_treatment = 'CZP')
    treatment_df <- data.frame(Number = seq(7),
                               Label = c('CZP', 'Placebo','Infliximab','Adalimumab','Tocilizumab','Rituximab', 'Etanercept'))

    expect_equal(treatment_list(), treatment_df)
  })
})

test_that("Data extracted from default binary file", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Binary')
    
    expect_equal(colnames(data()), c('Study', 'T', 'N', 'R', 'covar.duration'),
                 label = format_vector_to_string(colnames(data())))
    expect_equal(nrow(data()), 24,
                 label = nrow(data()))
    expect_equal(data()$Study, rep(c('Abe2006', 'ARMADA', 'ATTEST', 'CHARISMA', 'DE019', 'Kim2007', 'OPTION', 'RAPID1', 'RAPID2', 'START', 'Strand2006', 'Weinblatt1999'), each = 2),
                 label = format_vector_to_string(data()$Study))
    expect_equal(data()$T, c('Placebo', 'Infliximab', 'Placebo', 'Adalimumab', 'Placebo', 'Infliximab', 'Placebo', 'Tocilizumab', 'Placebo', 'Adalimumab', 'Placebo', 'Adalimumab', 'Placebo', 'Tocilizumab', 'Placebo', 'CZP', 'Placebo', 'CZP', 'Placebo', 'Infliximab', 'Placebo', 'Rituximab', 'Placebo', 'Etanercept'
),
                 label = format_vector_to_string(data()$T))
    expect_equal(data()$R, c(0, 15, 5, 37, 22, 61, 14, 26, 19, 81, 9, 28, 22, 90, 15, 146, 4, 80, 33, 110, 5, 5, 1, 23),
                 label = format_vector_to_string(data()$R))
    expect_equal(data()$N, c(47, 49, 62, 67, 110, 165, 49, 50, 200, 207, 63, 65, 204, 205, 199, 393, 127, 246, 363, 360, 40, 40, 30, 59),
                 label = format_vector_to_string(data()$N))
  })
})

test_that("Binary data passed back to module parent", {
  testServer(data_input_panel_server, {
    session$setInputs(metaoutcome = 'Binary')
    
    expect_equal(length(session$returned), 4)
    expect_equal(session$returned$data(), data())
    expect_equal(session$returned$is_default_data(), is_default_data())
    expect_equal(session$returned$treatment_list(), treatment_list())
    expect_equal(session$returned$metaoutcome(), session$input$metaoutcome)
  })
})

test_that("Binary long data matches between .csv and .xlsx files", {
  testServer(data_input_panel_server, {
    session$setInputs(data = data.frame(datapath = 'Certolizumab_long.csv'), metaoutcome = 'Binary')
    csv_data = data()
    
    session$setInputs(data = data.frame(datapath = 'Certolizumab_long.xlsx'))
    xlsx_data = data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Binary wide data matches between .csv and .xlsx files", {
  testServer(data_input_panel_server, {
    session$setInputs(data = data.frame(datapath = 'Certolizumab_wide.csv'), metaoutcome = 'Binary')
    csv_data = data()
    
    session$setInputs(data = data.frame(datapath = 'Certolizumab_wide.xlsx'))
    xlsx_data = data()
    
    expect_equal(xlsx_data, csv_data)
  })
})