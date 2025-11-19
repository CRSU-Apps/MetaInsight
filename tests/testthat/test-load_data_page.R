# needs refactoring into setup_load / setup_configure
#
# test_that("Data wrangled from default continuous long file", {
#   testServer(load_data_page_server, {
#     session$setInputs("data_input_panel-metaoutcome" = 'Continuous')
#
#     expect_equal(colnames(wrangled_data()), c("StudyID", colnames(data())[c(1, 2, 5, 3, 4, 6)]),
#                  label = format_vector_to_string(colnames(wrangled_data())))
#
#     expect_equal(wrangled_data()$StudyID, rep(1:45, each = 2),
#                  label = format_vector_to_string(wrangled_data()$StudyID))
#     expect_equal(wrangled_data()$T, c(rep(c(1, 2), times = 4), rep(c(1, 3), times = 18), rep(c(1, 4), times = 23)),
#                  label = format_vector_to_string(wrangled_data()$T))
#
#     expect_equal(nrow(wrangled_data()), nrow(data()),
#                  label = nrow(wrangled_data()),
#                  expected.label = nrow(data()))
#     expect_equal(wrangled_data()$Study, data()$Study,
#                  label = format_vector_to_string(wrangled_data()$Study),
#                  expected.label = format_vector_to_string(data()$Study))
#     expect_equal(wrangled_data()$N, data()$N,
#                  label = format_vector_to_string(wrangled_data()$N),
#                  expected.label = format_vector_to_string(data()$N))
#     expect_equal(wrangled_data()$Mean, data()$Mean,
#                  label = format_vector_to_string(wrangled_data()$Mean),
#                  expected.label = format_vector_to_string(data()$Mean))
#     expect_equal(wrangled_data()$SD, data()$SD,
#                  label = format_vector_to_string(wrangled_data()$SD),
#                  expected.label = format_vector_to_string(data()$SD))
#   })
# })
#
# test_that("Continuous wide data wrangled with treatment IDs", {
#   testServer(load_data_page_server, {
#     session$setInputs("data_input_panel-metaoutcome" = 'Continuous', "data_input_panel-data" = list(datapath = 'data/Non_opioids_wide.csv'))
#
#     expect_equal(wrangled_data()$StudyID, 1:45,
#                  label = format_vector_to_string(wrangled_data()$StudyID))
#     expect_equal(wrangled_data()$T.1, c(rep(1, times = 45)),
#                  label = format_vector_to_string(wrangled_data()$T.1))
#     expect_equal(wrangled_data()$T.2, c(rep(2, times = 4), rep(3, times = 18), rep(4, times = 23)),
#                  label = format_vector_to_string(wrangled_data()$T.2))
#
#     expect_equal(nrow(wrangled_data()), nrow(data()),
#                  label = nrow(wrangled_data()),
#                  expected.label = nrow(data()))
#     expect_equal(wrangled_data()$Study, data()$Study,
#                  label = format_vector_to_string(wrangled_data()$Study),
#                  expected.label = format_vector_to_string(data()$Study))
#     expect_equal(wrangled_data()$N.1, data()$N.1,
#                  label = format_vector_to_string(wrangled_data()$N.1),
#                  expected.label = format_vector_to_string(data()$N.1))
#     expect_equal(wrangled_data()$Mean.1, data()$Mean.1,
#                  label = format_vector_to_string(wrangled_data()$Mean.1),
#                  expected.label = format_vector_to_string(data()$Mean.1))
#     expect_equal(wrangled_data()$SD.1, data()$SD.1,
#                  label = format_vector_to_string(wrangled_data()$SD.1),
#                  expected.label = format_vector_to_string(data()$SD.1))
#     expect_equal(wrangled_data()$N.2, data()$N.2,
#                  label = format_vector_to_string(wrangled_data()$N.2),
#                  expected.label = format_vector_to_string(data()$N.2))
#     expect_equal(wrangled_data()$Mean.2, data()$Mean.2,
#                  label = format_vector_to_string(wrangled_data()$Mean.2),
#                  expected.label = format_vector_to_string(data()$Mean.2))
#     expect_equal(wrangled_data()$SD.2, data()$SD.2,
#                  label = format_vector_to_string(wrangled_data()$SD.2),
#                  expected.label = format_vector_to_string(data()$SD.2))
#   })
# })
#
# test_that("Wrangled continuous long data passed back to module parent", {
#   testServer(load_data_page_server, {
#     session$setInputs("data_input_panel-metaoutcome" = 'Continuous')
#
#     expect_equal(length(session$returned), 4)
#     expect_equal(session$returned$data(), wrangled_data())
#     expect_equal(session$returned$is_default_data(), is_default_data())
#     expect_equal(session$returned$treatment_df(), wrangled_treatment_list())
#     expect_equal(session$returned$metaoutcome(), metaoutcome())
#     expect_equal(session$returned$metaoutcome(), metaoutcome())
#   })
# })
#
# test_that("Data wrangled from default binary long file", {
#   testServer(load_data_page_server, {
#     session$setInputs("data_input_panel-metaoutcome" = 'Binary')
#
#     expect_equal(colnames(wrangled_data()), c("StudyID", colnames(data())[c(1, 2, 4, 3, 5)]),
#                  label = format_vector_to_string(colnames(wrangled_data())))
#
#     expect_equal(wrangled_data()$StudyID, rep(1:12, each = 2),
#                  label = format_vector_to_string(wrangled_data()$StudyID))
#     expect_equal(wrangled_data()$T, c(1, 2, 1, 3, 1, 2, 1, 4, 1, 3, 1, 3, 1, 4, 1, 5, 1, 5, 1, 2, 1, 6, 1, 7),
#                  label = format_vector_to_string(wrangled_data()$T))
#
#     expect_equal(nrow(wrangled_data()), nrow(data()),
#                  label = nrow(wrangled_data()),
#                  expected.label = nrow(data()))
#     expect_equal(wrangled_data()$Study, data()$Study,
#                  label = format_vector_to_string(wrangled_data()$Study),
#                  expected.label = format_vector_to_string(data()$Study))
#     expect_equal(wrangled_data()$R, data()$R,
#                  label = format_vector_to_string(wrangled_data()$R),
#                  expected.label = format_vector_to_string(data()$R))
#     expect_equal(wrangled_data()$N, data()$N,
#                  label = format_vector_to_string(wrangled_data()$N),
#                  expected.label = format_vector_to_string(data()$N))
#   })
# })
#
# test_that("Binary wide data wrangled with treatment IDs", {
#   testServer(load_data_page_server, {
#     session$setInputs("data_input_panel-metaoutcome" = 'Binary', "data_input_panel-data" = list(datapath = 'data/Certolizumab_wide.csv'))
#
#     expect_equal(wrangled_data()$StudyID, 1:12,
#                  label = format_vector_to_string(wrangled_data()$StudyID))
#     expect_equal(wrangled_data()$T.1, rep(1, times = 12),
#                  label = format_vector_to_string(wrangled_data()$T.1))
#     expect_equal(wrangled_data()$T.2, c(2, 3, 2, 4, 3, 3, 4, 5, 5, 2, 6, 7),
#                  label = format_vector_to_string(wrangled_data()$T.2))
#
#     expect_equal(nrow(wrangled_data()), nrow(data()),
#                  label = nrow(wrangled_data()),
#                  expected.label = nrow(data()))
#     expect_equal(wrangled_data()$Study, data()$Study,
#                  label = format_vector_to_string(wrangled_data()$Study),
#                  expected.label = format_vector_to_string(data()$Study))
#     expect_equal(wrangled_data()$R.1, data()$R.1,
#                  label = format_vector_to_string(wrangled_data()$R.1),
#                  expected.label = format_vector_to_string(data()$R.1))
#     expect_equal(wrangled_data()$N.1, data()$N.1,
#                  label = format_vector_to_string(wrangled_data()$N.1),
#                  expected.label = format_vector_to_string(data()$N.1))
#     expect_equal(wrangled_data()$R.2, data()$R.2,
#                  label = format_vector_to_string(wrangled_data()$R.2),
#                  expected.label = format_vector_to_string(data()$R.2))
#     expect_equal(wrangled_data()$N.2, data()$N.2,
#                  label = format_vector_to_string(wrangled_data()$N.2),
#                  expected.label = format_vector_to_string(data()$N.2))
#   })
# })
#
# test_that("Wrangled binary long data passed back to module parent", {
#   testServer(load_data_page_server, {
#     session$setInputs("data_input_panel-metaoutcome" = 'Binary')
#
#     expect_equal(length(session$returned), 4)
#     expect_equal(session$returned$data(), wrangled_data())
#     expect_equal(session$returned$is_default_data(), is_default_data())
#     expect_equal(session$returned$treatment_df(), wrangled_treatment_list())
#     expect_equal(session$returned$metaoutcome(), metaoutcome())
#     expect_equal(session$returned$metaoutcome(), metaoutcome())
#   })
# })
