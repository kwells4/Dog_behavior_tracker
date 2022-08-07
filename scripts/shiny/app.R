library(tidyverse)
library(openxlsx)
library(googledrive)

fieldsMandatory <- c("name", "date")

# Path to file on google drive
drive_file <- "Riley/riley_reaction_history"

reactivity_ratings <- c("1 - Growl, no barking",
                        "2 - Small barks, easy to distract",
                        "3 - Barked, no lunging",
                        "4 - Lunged and barked aggressively",
                        "5 - Made contact with human")

reactivity_input <- c("Pick a score", reactivity_ratings)

negative_behaviors <- c("Barking - work",
                        "Barking - food",
                        "Barking - play",
                        "Barking - other",
                        "Jumping - work",
                        "Jumping - other",
                        "Pawing",
                        "Mouthing", "Antsy - work",
                        "Antsy - other",
                        "Chewing furnature",
                        "Crawling into front of car",
                        "Pulling on walk",
                        "Chasing Squirrles",
                        "Chasing Rabbits",
                        "other")

negative_behaiors_input <- c("Pick a behavior", negative_behaviors)

positive_behaviors <- c("Hold back mouthing",
                        "Little or no pulling on walk",
                        "No barking while hugging",
                        "Asking nicely",
                        "Patience in the morning",
                        "Staying in the back of the car",
                        "Leaving rabbits",
                        "Leaving squirrles",
                        "Not barking before walk",
                        "Not barking before food",
                        "Chill with different routein",
                        "other")

positive_behaiors_input <- c("Pick a behavior", positive_behaviors)


mental_stimulation <- c("Lick mat",
                        "Kong", 
                        "Food ball (any shape)",
                        "Snuffle mat",
                        "Kong wobble",
                        "Collagen chew",
                        "Bully stick",
                        "Topple",
                        "Walk",
                        "Play",
                        "other")

mental_stimulation_input <- c("Pick a behavior", mental_stimulation)

# Create workbook
riley_reactivity_wb <- openxlsx::createWorkbook()

merge_style <- openxlsx::createStyle(wrapText = TRUE)

# Fields I want
# Did Riley spend time in her crate?
  # Amount of time
  # Notes
# Did Riley have any other strange behaviors or anxieties? - 5 possible
  # Notes



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
      h3("Did Riley have negative interacitons with Strangers? --------------"),
      
      lapply(1:5, function(x){
        list(
          h4(paste0("-------- Reactivity ", x, " --------")),
          textInput(paste0("reactivity_", x), "Who did she react to?"),
          selectInput(paste0("reactivity_score_", x), "What was her score?",
                     reactivity_input),
          textInput(paste0("reactivity_distance_", x),
                   "How far away was the stranger?"),
          textInput(paste0("reactivity_notes_", x), "Describe the event"))
      }),
      
      # Positive stranger interactions ---------------------------------
      h3("Did Riley have positive interacitons with Strangers? --------------"),
      
      lapply(1:5, function(x){
        list(     
          h4(paste0("-------- Positive Reactivity ", x, " --------")),
          textInput(paste0("pos_reactivity_", x), "Who did she not react to?"),
          textInput(paste0("pos_reactivity_distance_", x),
                    "How far away was the stranger?"),
          textInput(paste0("pos_reactivity_notes_", x), "Describe the event"))
      }),
      
      # Negative behavior at home ---------------------------------
      h3("Did Riley have negative behaviors at home? ------------------------"),
      
      lapply(1:5, function(x){
        list(
          h4(paste0("-------- Negative behaviors ", x, " --------")),
          selectInput(paste0("neg_behavior_", x), "What was the behavior?",
                      negative_behaiors_input),
          textInput(paste0("neg_behavior_manual_",x),
                    "If other, what was the behavior?"),
          textInput(paste0("neg_behavior_notes_", x), "Describe the behavior")
        )
      }),
      
      # Positive behavior at home ---------------------------------
      h3("Did Riley have positive behaviors at home? ------------------------"),
      
      lapply(1:5, function(x){
        list(
          h4(paste0("-------- Positive behaviors ", x, " --------")),
          selectInput(paste0("pos_behavior_", x), "What was the behavior?",
                      positive_behaiors_input),
          textInput(paste0("pos_behavior_manual_",x),
                    "If other, what was the behavior?"),
          textInput(paste0("pos_behavior_notes_", x), "Describe the behavior")
        )
      }),
      
      # Mental simulation/exercise ---------------------------------
      h3("Did Riley have mental stimulation or exercise? --------------------"),
      
      lapply(1:5, function(x){
        list(
          h4(paste0("-------- Stimulation/exercise ", x, " --------")),
          selectInput(paste0("stimulation_", x), "What was the activity?",
                      mental_stimulation_input),
          textInput(paste0("stimulation_manual_",x),
                    "If other, what was the activity?"),
          textInput(paste0("stimulation_time_", x),
                    "How long did the activity last?"),
          textInput(paste0("stimulation_notes_", x), "Describe the activity")
        )
      }),
      
      # Sleep time ---------------------------------
      h3("Did Riley sleep with you? -----------------------------------------"),
      
      lapply(1:5, function(x){
        list(
          h4(paste0("-------- Sleep time ", x, " --------")),
          selectInput(paste0("sleep_time_", x), "When did she sleep?",
                      c("Morning",
                        "Afternoon")),
          textInput(paste0("sleep_duration_", x),
                    "How long did she sleep?"),
          textInput(paste0("sleep_notes_", x), "Anything to add?")
        )
      }),
      
      # Crate time ---------------------------------
      h3("Did Riley spend time in her crate? --------------------------------"),
      
      selectInput("crate_alone", "Were you home or out?",
                  c("Home", "Out")),
      textInput("crate_time", "How long was she in her crate?"),
      textInput("crate_notes", "How was she when you took her out?"),
     
      
      # Other strange behaviors ---------------------------------
      h3("Did Riley have any other strange behaviors or anxieties? ----------"),
      
      lapply(1:5, function(x){
        list(
          h4(paste0("-------- Anxieties/behaviors ", x, " --------")),
          textInput(paste0("anxieties_", x),
                      "What was the behavior/anxiety?"),
          textInput(paste0("anxieties_notes_", x),
                    "Describe the behavior or anxiety")
        )
      }),
      
      # Things that worked ---------------------------------
      h3("Did you do anything that worked well? -----------------------------"),
      h5("For example, that calmed Riley down at home or helped he on a walk."),
      
      textInput("worked_notes", "Describe what you did."),
      
      
      
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
      
      ## Add reactivity data -----------------------------------------
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
                                  "Distance" = reactivity_distance,
                                  "Notes" = reactivity_notes,
                                  "Days_since_last_event" = "NA")
        }
      })
      
      reactivity <- do.call(rbind, reactivity)
      
      ## Add positive reactivity data --------------------------------
      pos_reactivity <- lapply(1:5, function(x){
        pos_reactivity_name <- paste0("pos_reactivity_", x)
        pos_reactivity_person <- input[[pos_reactivity_name]]
        if(!is.null(pos_reactivity_person) & pos_reactivity_person != ""){
          pos_reactivity_distance <- input[[paste0("pos_reactivity_distance_", x)]]
          pos_reactivity_notes <- input[[paste0("pos_reactivity_notes_", x)]]
          return_df <- data.frame("Name" = name, "Date" = date,
                                  "Type_of_person" = pos_reactivity_person,
                                  "Distance" = pos_reactivity_distance,
                                  "Notes" = pos_reactivity_notes)
        }
      })
      
      pos_reactivity <- do.call(rbind, pos_reactivity)
      
      return_list$pos_reactivity <- pos_reactivity
      
      ## Add negative behavior data --------------------------------
      negative_behavior <- lapply(1:5, function(x){
        neg_behavior_name <- paste0("neg_behavior_", x)
        neg_behavior <- input[[neg_behavior_name]]
        if(neg_behavior != "Pick a behavior"){
          if(neg_behavior == "other"){
            neg_behavior <- input[[paste0("neg_behavior_manual_",x)]]
          }
          neg_behavior_notes <- input[[paste0("neg_behavior_notes_", x)]]
          return_df <- data.frame("Name" = name, "Date" = date,
                                  "Behavior" = neg_behavior,
                                  "Notes" = neg_behavior_notes)
        }
      })
      
      negative_behavior <- do.call(rbind, negative_behavior)
      
      return_list$negative_behavior <- negative_behavior
      
      ## Add positive behavior data --------------------------------
      positive_behavior <- lapply(1:5, function(x){
        pos_behavior_name <- paste0("pos_behavior_", x)
        pos_behavior <- input[[pos_behavior_name]]
        if(pos_behavior != "Pick a behavior"){
          if(pos_behavior == "other"){
            pos_behavior <- input[[paste0("pos_behavior_manual_",x)]]
          }
          pos_behavior_notes <- input[[paste0("pos_behavior_notes_", x)]]
          return_df <- data.frame("Name" = name, "Date" = date,
                                  "Behavior" = pos_behavior,
                                  "Notes" = pos_behavior_notes)
        }
      })
      
      positive_behavior <- do.call(rbind, positive_behavior)
      
      return_list$positive_behavior <- positive_behavior
      
      ## Add stimulation and exercise data --------------------------------
      stimulation <- lapply(1:5, function(x){
        stimulation_name <- paste0("stimulation_", x)
        stimulation <- input[[stimulation_name]]
        if(stimulation != "Pick a behavior"){
          if(stimulation == "other"){
            stimulation <- input[[paste0("stimulation_manual_",x)]]
          }
          stimulation_time <- input[[paste0("stimulation_time_", x)]]
          stimulation_notes <- input[[paste0("stimulation_notes_", x)]]
          return_df <- data.frame("Name" = name, "Date" = date,
                                  "Activity" = stimulation,
                                  "Complete_time" = stimulation_time,
                                  "Notes" = stimulation_notes)
        }
      })
      
      stimulation <- do.call(rbind, stimulation)
      
      return_list$stimulation <- stimulation
      
      ## Add sleep data --------------------------------
      sleep <- lapply(1:5, function(x){
        sleep_time <- paste0("sleep_time_", x)
        sleep <- input[[sleep_time]]
        if( input[[paste0("sleep_duration_", x)]] != ""){
          sleep_duration <- input[[paste0("sleep_duration_", x)]]
          sleep_notes <- input[[paste0("sleep_notes_", x)]]
          return_df <- data.frame("Name" = name, "Date" = date,
                                  "Sleep_time" = sleep,
                                  "Sleep_duration" = sleep_duration,
                                  "Notes" = sleep_notes)
        }
      
      })
      
      sleep <- do.call(rbind, sleep)
      
      return_list$sleep <- sleep
      
      ## Add Crate data --------------------------------
      crate_time <- input[["crate_time"]]
      if(crate_time != ""){
        crate_alone <- input[["crate_alone"]]
        crate_notes <- input[["crate_notes"]]
        crate <- data.frame("Name" = name, "Date" = date,
                            "Home_out" = crate_alone,
                            "Crate_time" = crate_time,
                            "Notes" = crate_notes)
      } else {
        crate <- NULL
      }
      
      return_list$crate <- crate
      
      ## Add anxieties data --------------------------------
      anxieties <- lapply(1:5, function(x){
        anxieties_input <- paste0("anxieties_", x)
        anxieties <- input[[anxieties_input]]
        if(anxieties != ""){
          anxieties_notes <- input[[paste0("anxieties_notes_", x)]]
          return_df <- data.frame("Name" = name, "Date" = date,
                                  "Anxiety_behavior" = anxieties,
                                  "Notes" = anxieties_notes)
        }
        
      })
      
      anxieties <- do.call(rbind, anxieties)
      
      return_list$anxieties <- anxieties
      
      ## Add things that woked data --------------------------------
      worked_notes <- input[["worked_notes"]]
      if(worked_notes != ""){
        worked <- data.frame("Name" = name, "Date" = date,
                            "Notes" = worked_notes)
      } else {
        worked <- NULL
      }
      
      return_list$worked <- worked
      
      return_list

    })
    

    
    save_data <- function(data, excel_wb, save_name){
      # When you add in downloading, add in a second Rbind
      existing_excel_file <- drive_download(drive_file,
                                            path = file.path(save_dir,
                                                             "riley_data.xlsx"),
                                            overwrite = TRUE)
      
      ## Reactivity addition --------------------------------------
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
        
      }
      
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
      
      ## Positive reactivity ----------------------------------------
      existing_pos_reactivity <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Positive_reaction_history")
      
      
      
      existing_pos_reactivity$Date <- 
        openxlsx::convertToDate(existing_pos_reactivity$Date)
      
  
      if(is.null(data$pos_reactivity)){
        total_pos_reactivity <- existing_pos_reactivity
      } else {
        total_pos_reactivity <- rbind(existing_pos_reactivity,
                                      data$pos_reactivity)
      }

      ## Negative behavior ----------------------------------------
      existing_neg_behavior <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Negative_home_behaviors")
      
      
      
      existing_neg_behavior$Date <- 
        openxlsx::convertToDate(existing_neg_behavior$Date)
      
      
      if(is.null(data$negative_behavior)){
        total_negative_behavior <- existing_neg_behavior
      } else {
        total_negative_behavior <- rbind(existing_neg_behavior,
                                      data$negative_behavior)
      }
      
      ## Positive behavior ----------------------------------------
      existing_pos_behavior <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Positive_home_behaviors")
      
      
      
      existing_pos_behavior$Date <- 
        openxlsx::convertToDate(existing_pos_behavior$Date)
      
      
      if(is.null(data$positive_behavior)){
        total_positive_behavior <- existing_pos_behavior
      } else {
        total_positive_behavior <- rbind(existing_pos_behavior,
                                         data$positive_behavior)
      }
      
      ## Stimulation/exercise ----------------------------------------
      existing_stimulation <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Stimulation_exercise")
      
      
      
      existing_stimulation$Date <- 
        openxlsx::convertToDate(existing_stimulation$Date)
      

      if(is.null(data$stimulation)){
        total_stimulation <- existing_stimulation
      } else {
        total_stimulation <- rbind(existing_stimulation,
                                         data$stimulation)
      }
      
      ## Sleep ----------------------------------------
      existing_sleep <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Sleep")
      
      
      
      existing_sleep$Date <- 
        openxlsx::convertToDate(existing_sleep$Date)
      
      
      if(is.null(data$sleep)){
        total_sleep <- existing_sleep
      } else {
        total_sleep <- rbind(existing_sleep,
                                   data$sleep)
      }
      
      ## Crate ----------------------------------------
      existing_crate <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Crate")
      
      
      
      existing_crate$Date <- 
        openxlsx::convertToDate(existing_crate$Date)
      
      
      if(is.null(data$crate)){
        total_crate <- existing_crate
      } else {
        total_crate <- rbind(existing_crate,
                             data$crate)
      }
      

      ## Anxieties ----------------------------------------
      existing_anxieities <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Anxieties")
      
      
      
      existing_anxieities$Date <- 
        openxlsx::convertToDate(existing_anxieities$Date)
      
      
      if(is.null(data$anxieties)){
        total_anxieites <- existing_anxieities
      } else {
        total_anxieites <- rbind(existing_anxieities,
                             data$anxieties)
      }
      
      
      ## Things that worked ----------------------------------------
      exisiting_worked <- 
        openxlsx::readWorkbook(xlsxFile = file.path(save_dir,
                                                    "riley_data.xlsx"),
                               sheet = "Things_worked")
      
      
      
      exisiting_worked$Date <- 
        openxlsx::convertToDate(exisiting_worked$Date)
      
      
      if(is.null(data$worked)){
        total_worked <- exisiting_worked
      } else {
        total_worked <- rbind(exisiting_worked,
                                 data$worked)
      }

      ## Add to wb --------------------------------------------------
      
      # Save reaction history
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Negative_reaction_history")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Negative_reaction_history", 
                          x = total_reactivity)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Negative_reaction_history",
                         style = merge_style,
                         cols = 6,
                         rows = 2:nrow(total_reactivity))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Negative_reaction_history",
                             cols = c(1, 2, 3, 4, 5, 6),
                             widths = c(12, 12, 16, 30, 16, 60))
      
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
      
      # Save positive reaction history
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Positive_reaction_history")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Positive_reaction_history", 
                          x = total_pos_reactivity)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Positive_reaction_history",
                         style = merge_style,
                         cols = 5,
                         rows = 2:nrow(total_pos_reactivity))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Positive_reaction_history",
                             cols = c(1, 2, 3, 4, 5),
                             widths = c(12, 12, 16, 16, 60))
      
      # Save negative behaviors
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Negative_home_behaviors")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Negative_home_behaviors", 
                          x = total_negative_behavior)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Negative_home_behaviors",
                         style = merge_style,
                         cols = 4,
                         rows = 2:nrow(total_negative_behavior))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Negative_home_behaviors",
                             cols = c(1, 2, 3, 4),
                             widths = c(12, 12, 20, 60))
      
      # Save positive behaviors
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Positive_home_behaviors")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Positive_home_behaviors", 
                          x = total_positive_behavior)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Positive_home_behaviors",
                         style = merge_style,
                         cols = 4,
                         rows = 2:nrow(total_positive_behavior))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Positive_home_behaviors",
                             cols = c(1, 2, 3, 4),
                             widths = c(12, 12, 25, 60))
      
      # Save Stimulation
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Stimulation_exercise")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Stimulation_exercise", 
                          x = total_stimulation)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Stimulation_exercise",
                         style = merge_style,
                         cols = 5,
                         rows = 2:nrow(total_stimulation))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Stimulation_exercise",
                             cols = c(1, 2, 3, 4, 5),
                             widths = c(12, 12, 25, 16, 60))
      
      # Save Sleep
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Sleep")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Sleep", 
                          x = total_sleep)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Sleep",
                         style = merge_style,
                         cols = 5,
                         rows = 2:nrow(total_sleep))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Sleep",
                             cols = c(1, 2, 3, 4, 5),
                             widths = c(12, 12, 16, 16, 60))
      
      # Save Crate
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Crate")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Crate", 
                          x = total_crate)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Crate",
                         style = merge_style,
                         cols = 5,
                         rows = 2:nrow(total_crate))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Crate",
                             cols = c(1, 2, 3, 4, 5),
                             widths = c(12, 12, 16, 16, 60))
      
      # Save Anxieties
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Anxieties")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Anxieties", 
                          x = total_anxieites)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Anxieties",
                         style = merge_style,
                         cols = 4,
                         rows = 2:nrow(total_anxieites))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Anxieties",
                             cols = c(1, 2, 3, 4),
                             widths = c(12, 12, 16, 60))
      
      # Save things that worked
      openxlsx::addWorksheet(wb = excel_wb,
                             sheet = "Things_worked")
      openxlsx::writeData(wb = excel_wb,
                          sheet = "Things_worked", 
                          x = total_worked)
      
      openxlsx::addStyle(wb = excel_wb,
                         sheet = "Things_worked",
                         style = merge_style,
                         cols = 3,
                         rows = 2:nrow(total_worked))
      
      openxlsx::setColWidths(wb = excel_wb,
                             sheet = "Things_worked",
                             cols = c(1, 2, 3),
                             widths = c(12, 12, 60))
      
      # Save workbook
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