validate_plot <- function(file_path, format) {
  tryCatch({
    if (format == "pdf") {
      pdf_info <- pdftools::pdf_info(file_path)
      return(pdf_info$pages > 0)  # Check if the PDF has at least one page
    } else if (format == "svg") {
      svg_content <- xml2::read_xml(file_path)
      return(length(xml2::xml_ns(svg_content)) > 0)  # Check for SVG definitions
    } else if (format == "png") {
      img <- magick::image_read(file_path)
      return(magick::image_info(img)$width > 0)  # Check if the image has a valid width
    } else {
      stop("Unsupported format. Use 'pdf', 'svg', or 'png'.")
    }
  }, error = function(e) {
    return(FALSE)
  }
  )
}

test_plot_downloads <- function(app, module, pair = TRUE) {
  if (pair){
    download_all_id <- paste0(module, "-download_all")
    download_sub_id <- paste0(module, "-download_sub")

    app$set_inputs("download_format" = "pdf")
    pdf_all <- app$get_download(download_all_id)
    pdf_sub <- app$get_download(download_sub_id)
    expect_gt(file.info(pdf_all)$size, 1000)
    expect_gt(file.info(pdf_sub)$size, 1000)
    expect_true(validate_plot(pdf_all, "pdf"))
    expect_true(validate_plot(pdf_sub, "pdf"))

    app$set_inputs("download_format" = "png")
    png_all <- app$get_download(download_all_id)
    png_sub <- app$get_download(download_sub_id)
    expect_gt(file.info(png_all)$size, 1000)
    expect_gt(file.info(png_sub)$size, 1000)
    expect_true(validate_plot(png_all, "png"))
    expect_true(validate_plot(png_sub, "png"))

    app$set_inputs("download_format" = "svg")
    svg_all <- app$get_download(download_all_id)
    svg_sub <- app$get_download(download_sub_id)
    expect_gt(file.info(svg_all)$size, 1000)
    expect_gt(file.info(svg_sub)$size, 1000)
    expect_true(validate_plot(svg_all, "svg"))
    expect_true(validate_plot(svg_sub, "svg"))

    unlink(c(pdf_all, pdf_sub, png_all, png_sub, svg_all, svg_sub))
  } else {
    download_id <- paste0(module, "-download")

    app$set_inputs("download_format" = "pdf")
    pdf <- app$get_download(download_id)
    expect_gt(file.info(pdf)$size, 1000)
    expect_true(validate_plot(pdf, "pdf"))

    app$set_inputs("download_format" = "png")
    png <- app$get_download(download_id)
    expect_gt(file.info(png)$size, 1000)
    expect_true(validate_plot(png, "png"))

    app$set_inputs("download_format" = "svg")
    svg <- app$get_download(download_id)
    expect_gt(file.info(svg)$size, 1000)
    expect_true(validate_plot(svg, "svg"))

    unlink(c(pdf, png, svg))
  }
}

# similar to above, but for bayes modules which use submodules
test_bayes_plot_downloads <- function(app, module, plot_id) {
  download_all_id <- paste0(module, "-all-download", plot_id)
  download_sub_id <- paste0(module, "-sub-download", plot_id)

  app$set_inputs("download_format" = "pdf")
  pdf_all <- app$get_download(download_all_id)
  pdf_sub <- app$get_download(download_sub_id)
  expect_gt(file.info(pdf_all)$size, 1000)
  expect_gt(file.info(pdf_sub)$size, 1000)
  expect_true(validate_plot(pdf_all, "pdf"))
  expect_true(validate_plot(pdf_sub, "pdf"))

  app$set_inputs("download_format" = "png")
  png_all <- app$get_download(download_all_id)
  png_sub <- app$get_download(download_sub_id)
  expect_gt(file.info(png_all)$size, 1000)
  expect_gt(file.info(png_sub)$size, 1000)
  expect_true(validate_plot(png_all, "png"))
  expect_true(validate_plot(png_sub, "png"))

  app$set_inputs("download_format" = "svg")
  svg_all <- app$get_download(download_all_id)
  svg_sub <- app$get_download(download_sub_id)
  expect_gt(file.info(svg_all)$size, 1000)
  expect_gt(file.info(svg_sub)$size, 1000)
  expect_true(validate_plot(svg_all, "svg"))
  expect_true(validate_plot(svg_sub, "svg"))

  unlink(c(pdf_all, pdf_sub, png_all, png_sub, svg_all, svg_sub))
}
