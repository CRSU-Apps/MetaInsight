validate_plot <- function(file_path) {
  img <- magick::image_read(file_path)
  return(magick::image_info(img)$width > 0)  # Check if the image has a valid width
}

test_plot_downloads <- function(app, module, pair = TRUE) {
  if (pair){
    download_all_id <- paste0(module, "-download_all")
    download_sub_id <- paste0(module, "-download_sub")

    app$set_inputs("download_format" = "png")
    png_all <- app$get_download(download_all_id)
    png_sub <- app$get_download(download_sub_id)
    expect_gt(file.info(png_all)$size, 1000)
    expect_gt(file.info(png_sub)$size, 1000)
    expect_true(validate_plot(png_all))
    expect_true(validate_plot(png_sub))

    unlink(c(png_all, png_sub))

  } else {
    download_id <- paste0(module, "-download")

    app$set_inputs("download_format" = "png")
    png <- app$get_download(download_id)
    expect_gt(file.info(png)$size, 1000)
    expect_true(validate_plot(png))

    unlink(png)
  }
}

# similar to above, but for bayes modules which use submodules
test_bayes_plot_downloads <- function(app, module, plot_id, dual = TRUE) {
  download_all_id <- paste0(module, "-all-download", plot_id)

  app$set_inputs("download_format" = "png")
  png_all <- app$get_download(download_all_id)
  expect_gt(file.info(png_all)$size, 1000)
  expect_true(validate_plot(png_all))

  unlink(png_all)

}


