
test_that("Data wrangled from default continuous file", {
  testServer(load_data_page_server, args = list(metaoutcome = function() { 'Continuous' }), {
    expect_equal(colnames(wrangled_data()), colnames(data()),
                 label = format_vector_to_string(colnames(wrangled_data())),
                 expected.label = format_vector_to_string(colnames(data())))
    
    expect_equal(wrangled_data()$T, c(1, 2, 3, 4, 1, 5, 1, 6),
                 label = format_vector_to_string(wrangled_data()$T))
    
    expect_equal(nrow(wrangled_data()), nrow(data()),
                 label = nrow(wrangled_data()),
                 expected.label = nrow(data()))
    expect_equal(wrangled_data()$StudyID, data()$StudyID,
                 label = format_vector_to_string(wrangled_data()$StudyID),
                 expected.label = format_vector_to_string(data()$StudyID))
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

test_that("Wrangled continuous long data passed back to module parent", {
  testServer(load_data_page_server, args = list(metaoutcome = function() { 'Continuous' }), {
    expect_equal(length(session$returned), 2)
    expect_equal(session$returned$data(), wrangled_data())
    expect_equal(session$returned$treatment_df(), treatment_list())
  })
})

test_that("Data wrangled from default binary file", {
  testServer(load_data_page_server, args = list(metaoutcome = function() { 'Binary' }), {
    expect_equal(colnames(wrangled_data()), colnames(data()),
                 label = format_vector_to_string(colnames(wrangled_data())),
                 expected.label = format_vector_to_string(colnames(data())))
    
    expect_equal(wrangled_data()$T, c(1, 2, 3, 4, 1, 5, 1, 6),
                 label = format_vector_to_string(wrangled_data()$T))
    
    expect_equal(nrow(wrangled_data()), nrow(data()),
                 label = nrow(wrangled_data()),
                 expected.label = nrow(data()))
    expect_equal(wrangled_data()$StudyID, data()$StudyID,
                 label = format_vector_to_string(wrangled_data()$StudyID),
                 expected.label = format_vector_to_string(data()$StudyID))
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


test_that("Wrangled binary data passed back to module parent", {
  testServer(load_data_page_server, args = list(metaoutcome = function() { 'Binary' }), {
    expect_equal(length(session$returned), 2)
    expect_equal(session$returned$data(), wrangled_data())
    expect_equal(session$returned$treatment_df(), treatment_list())
  })
})