# Dog behavior tracker

This is an `R shiny` script that assists in keeping track of dog behavior - from reactivity to dogs and humans to positive and negative behaviors around the house.

## Layout

Running the `R shiny` app opens up a form with fields for

1. Reactivity
2. Positive interactions/reactions
3. Negative behaviors at home
4. Positive behaviors at home
5. Sleep time
6. Mental stimulation and exercise time
7. Other strange behaviors or notes

Once any or all of the fields of the form are completed, hitting the submit button triggers the following events

1. The google sheet is downloaded from google drive
2. Information from the form is appended to each corresponding tab of the downloaded excel spreadsheet
3. This new information is written to the appropriate sheet of a new excel file
4. The file is reuploaded to google drive as a google sheet.

The output will also keep track of the number of days since each level of reactivity.

The final form on google will be a well organized document that is easy to use to track your dog's progress over time.

## Required packages

* `tidyverse`
* `openxlsx`
* `googledrive`