close_connection <- function() {
  on_connection_closed()
  print("Connection closed")
}

on_connection_closed <- function() {
  observer <- getOption("connectionObserver")
  if (!is.null(observer))
    observer$connectionClosed(type = "rtweet", host = "rtweet")
}

list_objects <- function(includeType) {
  tables <- c("me", "my_timeline", "direct_messages")
  if (includeType) {
    data.frame(
      name = tables,
      type = rep_len("table", length(tables)),
      stringsAsFactors = FALSE
    )
  } else {
    tables
  }
}

#' @importFrom attempt without_warning

list_columns <- function(table) {
  if (table == "me"){
    tok <- get_token()
    res <- lookup_users(tok$credentials$screen_name)
    res <- res[, c("user_id", "name","screen_name","location",
            "description", "followers_count", "friends_count",
            "listed_count","statuses_count", "favourites_count",
            "account_created_at","verified", "account_lang")]
    if (nrow(res) != 0 ) {
      res <-   data.frame(
        name = names(res),
        type = as.character(res[1,]),
        stringsAsFactors = FALSE
      )
    } 
  } else if (table == "my_timeline"){
    nw <- attempt::without_warning(get_my_timeline)
    res <- nw(n = 10)
    if (nrow(res) != 0 ) {
      res <-   data.frame(
        name = res$screen_name,
        type = paste(res$created_at, "-" ,res$text),
        stringsAsFactors = FALSE
      )
    }
  } else if (table == "direct_messages"){
    nw <- attempt::without_warning(direct_messages)
    res <- nw(n = 10)
    if (nrow(res) != 0 ) {
      res <-   data.frame(
        name = res$sender_screen_name,
        type = res$text,
        stringsAsFactors = FALSE
      )
    }
  }
  
  res
}

#' @importFrom attempt without_warning

preview_object <- function(table, limit) {
  if (table == "me"){
    res <- iris
  } else if (table == "my_timeline"){
    nw <- attempt::without_warning(get_my_timeline)
    res <- nw()
  } else if (table == "direct_messages"){
    nw <- attempt::without_warning(direct_messages)
    res <- nw()
  }
  head(res, limit)
}



#' @keywords internal
#' @importFrom utils browseURL
#' @export

on_connection_opened <- function(token) {
  observer <- getOption("connectionObserver")
  if(!is.null(observer)){
    observer$connectionOpened(type = "rtweet",
                              host = "rtweet",
                              displayName = "Twitter Connection",
                              icon = system.file("icons","rtweet.png", package = "rtweet"),
                              connectCode = '# Launch con pane \nlibrary(rtweet)\nlaunch_rtweet_pane()',
                              disconnect = function() {
                                close_connection()
                              },
                              listObjectTypes = function () {
                                return(list(
                                  table = list(contains = "data")))
                              },
                              
                              listObjects = function(type = "table") {
                                list_objects(includeType = TRUE)
                              },
                              listColumns = function(table) {
                                list_columns(table)
                              },
                              previewObject = function(rowLimit, table) {
                                preview_object(table, rowLimit)
                              },
                              actions = list(
                                GitHub = list(
                                  icon = system.file("icons","github.png", package = "rtweet"),
                                  callback = function() {
                                    utils::browseURL("https://github.com/mkearney/rtweet")
                                  }
                                ),
                                Doc = list(
                                  icon = system.file("icons","documentation.png", package = "rtweet"),
                                  callback = function() {
                                    utils::browseURL("http://rtweet.info/")
                                  }
                                ),
                                SendTweet = list(
                                  icon = system.file("icons","twitter_PNG28.png", package = "rtweet"),
                                  callback = function() {
                                    tweet_widget()
                                  }
                                )
                              ),
                              connectionObject = token )
  }
}

# Shiny APP
rsApiUpdateDialog <- function(code) {
  if (exists(".rs.api.updateDialog")) {
    updateDialog <- get(".rs.api.updateDialog")
    updateDialog(code = code)
  }
}

#' @importFrom shiny tags div textInput

ui <- function(){
  tags$div(
    tags$head(
      tags$style(
        HTML(paste("
          .shiny-input-container {
            display: table-row;
            height: 24px;
          }
          .shiny-input-container > label {
            display: table-cell;
            width: 145px;
          }
          .shiny-input-container > input {
            display: table-cell;
            width: 300px;
          }
        ", sep = ""))
      )
    ),
    div(style = "table-row",
        textInput(
          "appname",
          "appname:", 
          value = "rtweet"
        ),
        textInput(
          "key",
          "key:"
        ),
        textInput(
          "secret",
          "secret:"
        )
    )
  )
  
}

#' @importFrom glue glue

build_code <- function(appname, key, secret){
  paste(
    "library(rtweet)\n",
    glue("twitter_token <- create_token( app = '{appname}',
         consumer_key = '{key}',
         consumer_secret = '{secret}')
         launch_rtweet_pane()")
  )
}

#' @importFrom shiny shinyApp

server <- function(input, output, session) {
  observe({
    rsApiUpdateDialog(build_code(input$appname, input$key, input$secret))
  })
}

#' @keywords internal
#' @importFrom shiny shinyApp
#' @export

run_app <- function(){
  shinyApp(ui, server)
}

#' Send a Tweet through the Widget
#' 
#' @param token your rtweet token
#' 
#' @importFrom miniUI miniPage gadgetTitleBar miniTabstripPanel miniTabPanel miniContentPanel
#' @importFrom shiny textAreaInput icon fileInput h6 actionButton observeEvent stopApp runGadget paneViewer
#' @importFrom glue glue
#' 
#' @export

tweet_widget <- function(token = get_token()) {
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Create a new tweet"),
    miniUI::miniContentPanel(
      shiny::textAreaInput(inputId = "status", label = "Status", value = "my first rtweet #rstats"),
      shiny::fileInput(inputId = "media", label = "Media"),
      shiny::h6("If you upload a file, this might take some time"),
      shiny::textAreaInput(inputId = "in_reply_to_status_id", label = "in reply to status",
                    value = NULL),
      shiny::actionButton("button", "Send")
    )
  )
  
  server <- function(input, output, session) {
    shiny::observeEvent(input$button, {
      inFile <- input$media
      if (is.null(inFile)) {
        media <- NULL
      } else {
        media <- normalizePath(inFile$datapath)
      }
      returnValue <- list(status = input$status,
                          media = media,
                          in_reply_to_status_id = input$in_reply_to_status_id)
      post_tweet(status = returnValue$status,
                 media = returnValue$media,
                 in_reply_to_status_id = returnValue$in_reply_to_status_id, 
                 token = token)
      utils::browseURL(glue::glue("https://twitter.com/{token$credentials$screen_name}"))
    })
    shiny::observeEvent(input$done, { shiny::stopApp() } )
  }
  shiny::runGadget(ui, server, viewer = shiny::paneViewer())
}

#' Launch the RStudio Connection Pane
#' 
#' @param token Your rtweet token
#'
#' @export
#' 

launch_rtweet_pane <- function(token = get_tokens()){
  on_connection_opened(token)
}
