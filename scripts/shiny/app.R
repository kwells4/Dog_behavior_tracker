library(tidyverse)
library(openxlsx)
library(googledrive)

fieldsMandatory <- c("name", "date")

# Path to file on google drive

drive_file <- "Riley/riley_reaction_history"


# Create workbook
riley_reactivity_wb <- openxlsx::createWorkbook()

merge_style <- openxlsx::createStyle(wrapText = TRUE)

# Fields I want
# Date
# Person
# Did Riley react on the walk? - 5 possible ones to fill out
  # Description of person
  # Score - drop down list - 1 -5
  # Distance
  # Notes
# Did Riley have positive interactions on her walk? - 5 possible ones to fill out
  # Description of person
  # Distance
  # Notes
# Did Riley have negative behaviors with you? - 5 possible ones to fill out
  # Type of behavior (drop down list, include other)
  # Notes
# Did Riley have positive behaviors with you? - 5 opssible ones to fill out
  # Type of behavior (drop down list, include other)
  # Notes
# Did you give Riley mental stimulation today - 5 possible ones to fill out
  # Drop down list
  # Time spent
  # Notes
# Did Riley sleep with you? - 2 possible ones to fill out
  # Time of day
  # Amount of time
  # Notes
# Did Riley spend time in her crate?
  # Amount of time
  # Notes
# Did Riley have any other strange behaviors or anxieties? - 5 possible
  # Notes



# Figure out how to check if the field is blank...?

# Mandatory?

save_dir <- file.path("files")

responsesDir <- save_dir

ifelse(!dir.exists(save_dir), dir.create(save_dir), "TRUE")

epochTime <- function() {
  as.integer(Sys.time())
}

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"


shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("Riley daily data"),
    
    
    # Make the form
    div(
      id = "form",
      
      h3("Your info"),
      # Name and date
      textInput("name", labelMandatory("Name"), ""),
      textInput("date", labelMandatory("Date (YYYY/MM/DD)"), ""),
      
      # Stranger reactivity -----------------------------------
      h3("Did Riley have negative interacitons with Strangers?"),
      
      # Eventually figure out how to do this in a loop
      # Reactivity 1
      h4("-------- Reactivity 1 --------"),
      textInput("reactivity_1", "Who did she react to?"),
      selectInput("reactivity_score_1", "What was her score?",
                  c("Pick a score",
                    "1 - Growl, no barking",
                    "2 - Small barks, easy to distract",
                    "3 - Barked, no lunging",
                    "4 - Lunged and barked aggressively",
                    "5 - Made contact with human")),
      textInput("reactivity_distance_1", "How far away was the stranger?"),
      textInput("reactivity_notes_1", "Describe the event"),

      # Reactivity 2
      h4("-------- Reactivity 2 --------"),
      textInput("reactivity_2", "Who did she react to?"),
      selectInput("reactivity_score_2", "What was her score?",
                  c("Pick a score",
                    "1 - Growl, no barking",
                    "2 - Small barks, easy to distract",
                    "3 - Barked, no lunging",
                    "4 - Lunged and barked aggressively",
                    "5 - Made contact with human")),
      textInput("reactivity_distance_2", "How far away was the stranger?"),
      textInput("reactivity_notes_2", "Describe the event"),

      # Reactivity 3
      h4("-------- Reactivity 3 --------"),
      textInput("reactivity_3", "Who did she react to?"),
      selectInput("reactivity_score_3", "What was her score?",
                  c("Pick a score",
                    "1 - Growl, no barking",
                    "2 - Small barks, easy to distract",
                    "3 - Barked, no lunging",
                    "4 - Lunged and barked aggressively",
                    "5 - Made contact with human")),
      textInput("reactivity_distance_3", "How far away was the stranger?"),
      textInput("reactivity_notes_3", "Describe the event"),

      # Reactivity 4
      h4("-------- Reactivity 4 --------"),
      textInput("reactivity_4", "Who did she react to?"),
      selectInput("reactivity_score_4", "What was her score?",
                  c("Pick a score",
                    "1 - Growl, no barking",
                    "2 - Small barks, easy to distract",
                    "3 - Barked, no lunging",
                    "4 - Lunged and barked aggressively",
                    "5 - Made contact with human")),
      textInput("reactivity_distance_4", "How far away was the stranger?"),
      textInput("reactivity_notes_4", "Describe the event"),

      # Reactivity 5
      h4("-------- Reactivity 5 --------"),
      textInput("reactivity_5", "Who did she react to?"),
      selectInput("reactivity_score_5", "What was her score?",
                  c("Pick a score",
                    "1 - Growl, no barking",
                    "2 - Small barks, easy to distract",
                    "3 - Barked, no lunging",
                    "4 - Lunged and barked aggressively",
                    "5 - Made contact with human")),
      textInput("reactivity_distance_5", "How far away was the stranger?"),
      textInput("reactivity_notes_5", "Describe the event"),

      actionButton("submit", "Submit", class = "btn-primary")
    ),
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
    )  
  ),
  server = function(input, output, session) {
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
  
    
    formData <- reactive({
      return_list <- list()
      name <- return_list$name <- input$name
      date <- return_list$date <- as.Date(input$date)
      
      # Add reactivity data
      reactivity <- lapply(1:5, function(x){
        reactivity_name <- paste0("reactivity_", x)
        reactivity_person <- input[[reactivity_name]]
        if(!is.null(reactivity_person) & reactivity_person != ""){
          reactivity_score <- input[[paste0("reactivity_score_", x)]]
          reactivity_distance <- input[[paste0("reactivity_distance_", x)]]
          reactivity_notes <- input[[paste0("reactivity_notes_", x)]]
          return_df <- data.frame("Name" = name, "Date" = date,
                                  "Type_of_person" = reactivity_person,
                                  "Score" = reactivity_score,
                                  "Notes" = reactivity_notes,
                                  "Days_since_last_event" = "NA")
        }
      })
      
      reactivity <- do.call(rbind, reactivity)
      
      return_list$reactivity <- reactivity
      
      return_list

    })
    

    
    save_data <- function(data, excel_wb, save_name){
      # When you add in downloading, add in a second Rbind
      existing_excel_file <- drive_download(drive_file,
                                            path = file.path(save_dir,
                                                             "riley_data.xlsx"),
                                            overwrite = TRUE)
      
      # Reactivity addition
      existing_reactivity <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Negative_reaction_history")
      

      
      existing_reactivity$Date <- openxlsx::convertToDate(existing_reactivity$Date)
      
      existing_tracker <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Reactivity_tracker")
      
      if(is.null(data$reactivity)){
        total_reactivity <- existing_reactivity
        total_tracker <- existing_tracker
      } else {
        total_reactivity <- rbind(existing_reactivity, data$reactivity)
        
        days_since_reactive <- c("NA", diff(total_reactivity$Date))
        total_reactivity$Days_since_last_event <- days_since_reactive
        
        reactivity_ratings <- c("1 - Growl, no barking",
                                "2 - Small barks, easy to distract",
                                "3 - Barked, no lunging",
                                "4 - Lunged and barked aggressively",
                                "5 - Made contact with human")
        
        total_tracker <- lapply(reactivity_ratings, function(x){
          subset_reactivity <- total_reactivity %>%
            filter(Score == x)
          
          if(nrow(subset_reactivity) > 0){
            last_reaction <- subset_reactivity[nrow(subset_reactivity),]$Date
            
            time_since_reaction <- as.numeric(as.Date(data$date) - 
                                                as.Date(last_reaction))
            

          } else {
            time_since_reaction <- "NA"
          }

          return_data <- data.frame("Reaction" = x,
                                    "Days_since_reaction" = time_since_reaction)
          
          return(data.frame("Reaction" = x,
                            "Days_since_reaction" = time_since_reaction))
          
        })

        total_tracker <- do.call(rbind, total_tracker)
      }
      
      # Save reaction history
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Negative_reaction_history")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Negative_reaction_history", 
                          x = total_reactivity)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Negative_reaction_history",
                         style = merge_style,
                         cols = 5,
                         rows = 2:nrow(total_reactivity))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Negative_reaction_history",
                             cols = c(1, 2, 3, 4, 5),
                             widths = c(12, 12, 16, 30, 60))
      
      # Save tracker
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Reactivity_tracker")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Reactivity_tracker", 
                          x = total_tracker)
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Reactivity_tracker",
                             cols = c(1, 2),
                             widths = c(30, 12))
      
      openxlsx::saveWorkbook(wb = excel_wb, file = save_name,
                             overwrite = TRUE)
      
      # Move file to drive
      drive_upload(save_name, drive_file, type = "spreadsheet",
                   overwrite = TRUE)
      
    }

    

    # Close the form once submitted
    observeEvent(input$submit, {
      save_data(data = formData(),
                excel_wb = riley_reactivity_wb,
                save_name = file.path(save_dir, "riley_reaction_history.xlsx"))
      #saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })   
    
  }
)