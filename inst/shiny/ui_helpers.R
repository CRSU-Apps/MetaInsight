uiTop <- function(mod_INFO) {
  modID <- mod_INFO$modID
  modName <- mod_INFO$modName
  pkgName <- mod_INFO$pkgName

  ls <- list(span(div(paste("Module: ", modName), class = "mod"),
                  actionLink(paste0(modID, "Help"),
                             label = "", icon = icon("circle-question"),
                             class = "modHelpButton")
  ))

  ls <- c(ls,
          list(span(span("R packages:", class = "rpkg"),
                    span(paste(pkgName, collapse = ", "), class = "pkgDes")))
  )


  ls
}

uiBottom <- function(mod_INFO) {
  modAuts <- mod_INFO$modAuts
  pkgName <- mod_INFO$pkgName
  pkgAuts <- mod_INFO$pkgAuts
  pkgTitl <- mod_INFO$pkgTitl

  ls <- list(span(span('Module developers:', class = "rpkg"),
                  span(modAuts, class = "pkgDes")))

  for (i in seq_along(pkgName)) {
    ls <- c(ls, list(
      div(class = "package_spacer",
          span(
            span(paste0(pkgName[i], ":"), class = "rpkg"),
            span(span(pkgTitl[i], class = "pkgTitl"))
          ),
          div(paste('Package Developers:', pkgAuts[i]), class = "pkgDes"),
          span(
            a("CRAN", href = file.path("http://cran.r-project.org/web/packages",
                                       pkgName[i], "index.html"), target = "_blank"), " | ",
            a("documentation", href = file.path("https://cran.r-project.org/web/packages",
                                                pkgName[i], paste0(pkgName[i], ".pdf")), target = "_blank"), br()
          )
      )
    ))
  }
  ls
}

ui_top <- function(pkgName, modName, modAuts, modID) {
  uiTop(infoGenerator(pkgName, modName, modAuts, modID))
}
ui_bottom <- function(pkgName, modName, modAuts, modID) {
  uiBottom(infoGenerator(pkgName, modName, modAuts, modID))
}

infoGenerator <- function(pkgName, modName, modAuts, modID) {
  # Use installed package only (some packages are Suggested)
  pkgName <- pkgName[vapply(pkgName, requireNamespace, TRUE, quietly = TRUE)]

  pkgInfo <- sapply(pkgName, packageDescription, simplify = FALSE)
  pkgTitl <- sapply(pkgInfo, function(x) x$Title)
  # remove square brackets and spaces before commas
  pkgAuts <- sapply(pkgInfo, function(x) gsub("\\s+,", ",", gsub("\n|\\[.*?\\]", "", x$Author)))
  # remove parens and spaces before commas
  pkgAuts <- sapply(pkgAuts, function(x) gsub("\\s+,", ",", gsub("\\(.*?\\)", "", x)))
  list(modID = modID,
       modName = modName,
       modAuts = modAuts,
       pkgName = pkgName,
       pkgTitl = pkgTitl,
       pkgAuts = pkgAuts)
}

# Add radio buttons for all modules in a component
insert_modules_options <- function(component, exclude = NULL) {
  modules <- COMPONENT_MODULES[[component]]
  modules <- modules[!names(modules) %in% exclude]
  unlist(setNames(
    lapply(modules, `[[`, "id"),
    lapply(modules, `[[`, "short_name")
  ))
}

# Add the UI for a module
insert_modules_ui <- function(component, long_component, exclude = NULL) {
  modules <- COMPONENT_MODULES[[component]]
  modules <- modules[!names(modules) %in% exclude]
  tagList(
    conditionalPanel(
      glue("input.tabs == '{component}'"),
      div(glue("{long_component}"), class = "componentName"),
      shinyWidgets::radioGroupButtons(
        glue("{component}Sel"), "",
        choices = insert_modules_options(component, exclude = exclude),
        direction = "vertical",
        status = "outline-secondary",
        width = "100%"
      ),
      lapply(modules, function(module) {
        conditionalPanel(
          glue("input.{component}Sel == '{module$id}'"),
          card(id = glue("{component}_input_panel"),
            ui_top(
              modID = module$id,
              modName = module$long_name,
              modAuts = module$authors,
              pkgName = module$package
            ),
            do.call(module$ui_function, list(module$id)),
            class = "sidebar_card"
          ),
          card(
            tags$details(
              tags$summary("Module attribution"),
              card_body(
                ui_bottom(
                  modID = module$id,
                  modName = module$long_name,
                  modAuts = module$authors,
                  pkgName = module$package
                ),
                class = "package_info"
              ),
              class = "sidebar_card"
            )
          )
        )
      })
    )
  )
}

# Add the results section UI of all modules in a component
insert_modules_results <- function(component) {
  lapply(COMPONENT_MODULES[[component]], function(module) {
    if (is.null(module$result_function)){
      conditionalPanel(
        glue("input.{component}Sel == '{module$id}'"),
        tagList(tags$br(), tags$h3(glue("{module$short_name} does not produce results"))))
    } else {
      conditionalPanel(
        glue("input.{component}Sel == '{module$id}'"),
        do.call(module$result_function, list(module$id))
      )
    }
  })
}

# adjust layout width depending on screen size
flex_wrap <- function(content) {
  layout_columns(
    col_widths = breakpoints(sm = c(12),
                             md = c(-1, 10, -1),
                             lg = c(-2, 8, -2),
                             xl = c(-2, 8, -2),
                             xxl = c(-3, 6, -3)),
    content,
  )
}

####################### #
# LOADING MODAL #
####################### #

#' @title show_loading_modal
#' @description For internal use. Show a modal when something is loading
#' @param message The message to be displayed to the user
show_loading_modal <- function(message){
  shinybusy::show_modal_spinner(
    spin = "self-building-square",
    color = "#446e9b",
    text = message
  )
}
#' @title close_loading_modal
#' @description For internal use. Close the modal once loading is complete
#' @param session The session object passed to function given to shinyServer.

close_loading_modal <- function (session = getDefaultReactiveDomain())
{
  session$sendModal("remove", NULL)
}

####################### #
# ADD TOOLTIP #
####################### #
#' Add a tooltip to the label of an input
#'
#' @param label character. The text to display initially
#' @param message character. The text to display in the tooltip
add_tooltip <- function(label, message){
  span(label, tooltip(icon("circle-question"), message))
}

#' Create a pair of download buttons
#'
#' @param id The id of the module
#' @return tagList of downloadButtons
download_button_pair <- function(id){
  ns <- NS(id)
  div(class = "download_buttons",
      bslib::layout_columns(
        downloadButton(ns("download_all"), "All studies"),
        downloadButton(ns("download_sub"), "Selected studies excluded")
      )
  )
}

#' Put an svg in a container with buttons for fullscreen
#'
#' @param svg character. Output from `crop_svg()`
#' @param class character. Class of the container default `svg_container`
#' @param style character. Styles to add to the container
svg_container <- function(svg, class = "svg_container", style = ""){
  div(class = class, style = style,
      tags$button(
        class = "height-toggle-btn",
        onclick = "shinyjs.scrollingPlot(this)",
        # left-right arrow unicode
        "\u2194 Full width"
      ),
      tags$button(
        class = "fullscreen-btn",
        onclick = "shinyjs.fullscreenPlot(this.parentElement)",
        # diagonal arrow unicode
        "\u2922"
      ),
      svg)
}


####################### #
# RESET DATA #
####################### #

#' @title reset_data
#' @description For internal use. Clears the common structure of data and resets all plots etc.
#' @keywords internal
#' @param common The common data structure
reset_data <- function(common, session){
  modules <- names(common$meta)
  # clear data
  common$reset()
  # blank outputs
  for (module in modules){
    gargoyle::trigger(module)
    # reset triggers
    session$userData[[module]](0)
  }
}

#' @title run_all
#' @description For internal use. Runs all modules for a given component, excluding the model and nodesplit modules.
#' @keywords internal
#' @param COMPONENTS character. named vector of all components
#' @param COMPONENT_MODULES list. Containing details of all modules
#' @param component character. The component to run all the modules of.
#' @param logger common$logger
run_all <- function(COMPONENTS, COMPONENT_MODULES, component, logger){

  all_modules <- names(COMPONENT_MODULES[[component]])

  # exclude model and nodesplit modules
  if (component != "freq"){
    modules <- all_modules[-which(all_modules %in% c(glue::glue("{component}_model"), glue::glue("{component}_nodesplit")))]
  } else {
    modules <- all_modules
  }

  # workaround for regression modules where the run id differs
  # and append -run to module id to get run button ids
  modules <- paste0(ifelse(
    grepl("regression", modules),
    paste0(modules, "-", gsub("_regression", "", modules)),
    modules
  ), "-run")

  # click the run buttons
  shinyjs::runjs(
    paste(
      sprintf("$('#%s').click();", modules),
      collapse = "\n"
    )
  )

  # message for logger
  full_component <- names(COMPONENTS[COMPONENTS == component])

  if (component %in% c("bayes", "baseline", "covariate")){
    nodesplit_message <- " apart from the nodesplit module"
  } else {
    nodesplit_message <- ""
  }

  logger |> writeLog(type = "info",
                     glue::glue("Running all {full_component} modules{nodesplit_message}. This might
                                take several minutes and progress will appear in the logger."))

  invisible()
}


#' @title hide_and_show
#' @description For internal use. Hides content inside the <module_id>_div class
#' when a module has not been run and shows it once `trigger(module_id)` has
#' been called. The show option is `TRUE` by default but can be set to `FALSE`
#' to manually control when the content is displayed e.g.  if it should
#' be when an extendedtask completes instead of when the run button is
#' pressed.
#' @keywords internal
#' @param module_id character. The module identifier.
#' @param show logical. Whether to show the div when `trigger(module_id)` is
#' called
hide_and_show <- function(module_id, show = TRUE){
  observe({
    gargoyle::watch(module_id)
    if (gargoyle::watch(module_id) == 0){
      shinyjs::hide(selector = glue::glue(".{module_id}"))
    } else {
      if (show){
        shinyjs::show(selector = glue::glue(".{module_id}"))
      }
    }
  })
}
