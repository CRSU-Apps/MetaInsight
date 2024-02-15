
test_that("Data wrangled from default continuous long file", {
  testServer(load_data_page_server, {
    session$setInputs("data_input_panel-metaoutcome" = 'Continuous')
    
    expect_equal(colnames(wrangled_data()), c("StudyID", colnames(data())),
                 label = format_vector_to_string(colnames(wrangled_data())))
    
    expect_equal(wrangled_data()$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
                 label = format_vector_to_string(wrangled_data()$StudyID))
    expect_equal(wrangled_data()$T, c(1, 2, 3, 4, 1, 5, 1, 6),
                 label = format_vector_to_string(wrangled_data()$T))
    
    expect_equal(nrow(wrangled_data()), nrow(data()),
                 label = nrow(wrangled_data()),
                 expected.label = nrow(data()))
    expect_equal(wrangled_data()$Study, data()$Study,
                 label = format_vector_to_string(wrangled_data()$Study),
                 expected.label = format_vector_to_string(data()$Study))
    expect_equal(wrangled_data()$N, data()$N,
                 label = format_vector_to_string(wrangled_data()$N),
                 expected.label = format_vector_to_string(data()$N))
    expect_equal(wrangled_data()$Mean, data()$Mean,
                 label = format_vector_to_string(wrangled_data()$Mean),
                 expected.label = format_vector_to_string(data()$Mean))
    expect_equal(wrangled_data()$SD, data()$SD,
                 label = format_vector_to_string(wrangled_data()$SD),
                 expected.label = format_vector_to_string(data()$SD))
  })
})

test_that("Continuous wide data wrangled with treatment IDs", {
  testServer(load_data_page_server, {
    session$setInputs("data_input_panel-metaoutcome" = 'Continuous', "data_input_panel-data" = list(datapath = 'Cont_wide.csv'))
    
    expect_equal(wrangled_data()$StudyID, c(1, 2, 3),
                 label = format_vector_to_string(wrangled_data()$StudyID))
    expect_equal(wrangled_data()$T.1, c(1, 2, 1),
                 label = format_vector_to_string(wrangled_data()$T.1))
    expect_equal(wrangled_data()$T.2, c(3, 1, 4),
                 label = format_vector_to_string(wrangled_data()$T.2))
    expect_equal(wrangled_data()$T.3, c(5, 6, NA),
                 label = format_vector_to_string(wrangled_data()$T.3))

    expect_equal(nrow(wrangled_data()), nrow(data()),
                 label = nrow(wrangled_data()),
                 expected.label = nrow(data()))
    expect_equal(wrangled_data()$Study, data()$Study,
                 label = format_vector_to_string(wrangled_data()$Study),
                 expected.label = format_vector_to_string(data()$Study))
    expect_equal(wrangled_data()$N.1, data()$N.1,
                 label = format_vector_to_string(wrangled_data()$N.1),
                 expected.label = format_vector_to_string(data()$N.1))
    expect_equal(wrangled_data()$Mean.1, data()$Mean.1,
                 label = format_vector_to_string(wrangled_data()$Mean.1),
                 expected.label = format_vector_to_string(data()$Mean.1))
    expect_equal(wrangled_data()$SD.1, data()$SD.1,
                 label = format_vector_to_string(wrangled_data()$SD.1),
                 expected.label = format_vector_to_string(data()$SD.1))
    expect_equal(wrangled_data()$N.2, data()$N.2,
                 label = format_vector_to_string(wrangled_data()$N.2),
                 expected.label = format_vector_to_string(data()$N.2))
    expect_equal(wrangled_data()$Mean.2, data()$Mean.2,
                 label = format_vector_to_string(wrangled_data()$Mean.2),
                 expected.label = format_vector_to_string(data()$Mean.2))
    expect_equal(wrangled_data()$SD.2, data()$SD.2,
                 label = format_vector_to_string(wrangled_data()$SD.2),
                 expected.label = format_vector_to_string(data()$SD.2))
    expect_equal(wrangled_data()$N.3, data()$N.3,
                 label = format_vector_to_string(wrangled_data()$N.3),
                 expected.label = format_vector_to_string(data()$N.3))
    expect_equal(wrangled_data()$Mean.3, data()$Mean.3,
                 label = format_vector_to_string(wrangled_data()$Mean.3),
                 expected.label = format_vector_to_string(data()$Mean.3))
    expect_equal(wrangled_data()$SD.3, data()$SD.3,
                 label = format_vector_to_string(wrangled_data()$SD.3),
                 expected.label = format_vector_to_string(data()$SD.3))
  })
})

test_that("Wrangled continuous long data passed back to module parent", {
  testServer(load_data_page_server, {
    session$setInputs("data_input_panel-metaoutcome" = 'Continuous')
    
    expect_equal(length(session$returned), 4)
    expect_equal(session$returned$data(), wrangled_data())
    expect_equal(session$returned$is_default_data(), is_default_data())
    expect_equal(session$returned$treatment_df(), treatment_list())
    expect_equal(session$returned$metaoutcome(), metaoutcome())
  })
})

test_that("Data wrangled from default binary long file", {
  testServer(load_data_page_server, {
    session$setInputs("data_input_panel-metaoutcome" = 'Binary')
    
    expect_equal(colnames(wrangled_data()), c("StudyID", colnames(data())),
                 label = format_vector_to_string(colnames(wrangled_data())))

    expect_equal(wrangled_data()$StudyID, c(1, 1, 1, 2, 2, 2, 3, 3),
                 label = format_vector_to_string(wrangled_data()$StudyID))
    expect_equal(wrangled_data()$T, c(1, 2, 3, 4, 1, 5, 1, 6),
                 label = format_vector_to_string(wrangled_data()$T))

    expect_equal(nrow(wrangled_data()), nrow(data()),
                 label = nrow(wrangled_data()),
                 expected.label = nrow(data()))
    expect_equal(wrangled_data()$Study, data()$Study,
                 label = format_vector_to_string(wrangled_data()$Study),
                 expected.label = format_vector_to_string(data()$Study))
    expect_equal(wrangled_data()$R, data()$R,
                 label = format_vector_to_string(wrangled_data()$R),
                 expected.label = format_vector_to_string(data()$R))
    expect_equal(wrangled_data()$N, data()$N,
                 label = format_vector_to_string(wrangled_data()$N),
                 expected.label = format_vector_to_string(data()$N))
  })
})

test_that("Binary wide data wrangled with treatment IDs", {
  testServer(load_data_page_server, {
    session$setInputs("data_input_panel-metaoutcome" = 'Binary', "data_input_panel-data" = list(datapath = 'Binary_wide.csv'))
    
    expect_equal(wrangled_data()$StudyID, c(1, 2, 3),
                 label = format_vector_to_string(wrangled_data()$StudyID))
    expect_equal(wrangled_data()$T.1, c(1, 2, 1),
                 label = format_vector_to_string(wrangled_data()$T.1))
    expect_equal(wrangled_data()$T.2, c(3, 1, 4),
                 label = format_vector_to_string(wrangled_data()$T.2))
    expect_equal(wrangled_data()$T.3, c(5, 6, NA),
                 label = format_vector_to_string(wrangled_data()$T.3))

    expect_equal(nrow(wrangled_data()), nrow(data()),
                 label = nrow(wrangled_data()),
                 expected.label = nrow(data()))
    expect_equal(wrangled_data()$Study, data()$Study,
                 label = format_vector_to_string(wrangled_data()$Study),
                 expected.label = format_vector_to_string(data()$Study))
    expect_equal(wrangled_data()$R.1, data()$R.1,
                 label = format_vector_to_string(wrangled_data()$R.1),
                 expected.label = format_vector_to_string(data()$R.1))
    expect_equal(wrangled_data()$N.1, data()$N.1,
                 label = format_vector_to_string(wrangled_data()$N.1),
                 expected.label = format_vector_to_string(data()$N.1))
    expect_equal(wrangled_data()$R.2, data()$R.2,
                 label = format_vector_to_string(wrangled_data()$R.2),
                 expected.label = format_vector_to_string(data()$R.2))
    expect_equal(wrangled_data()$N.2, data()$N.2,
                 label = format_vector_to_string(wrangled_data()$N.2),
                 expected.label = format_vector_to_string(data()$N.2))
    expect_equal(wrangled_data()$R.3, data()$R.3,
                 label = format_vector_to_string(wrangled_data()$R.3),
                 expected.label = format_vector_to_string(data()$R.3))
    expect_equal(wrangled_data()$N.3, data()$N.3,
                 label = format_vector_to_string(wrangled_data()$N.3),
                 expected.label = format_vector_to_string(data()$N.3))
  })
})

test_that("Wrangled binary long data passed back to module parent", {
  testServer(load_data_page_server, {
    session$setInputs("data_input_panel-metaoutcome" = 'Binary')
    
    expect_equal(length(session$returned), 4)
    expect_equal(session$returned$data(), wrangled_data())
    expect_equal(session$returned$is_default_data(), is_default_data())
    expect_equal(session$returned$treatment_df(), treatment_list())
    expect_equal(session$returned$metaoutcome(), metaoutcome())
  })
})
