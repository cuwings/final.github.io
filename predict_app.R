

library(shiny)
library(tidyverse)

my_model = readRDS("fit_alc.rds")

shiny_df = read_csv("data/combine.csv") |>
  drop_na() |>
  mutate(poverty_income = case_match(poverty_level,
                                     "Above 185% of Poverty Guidelines" ~ "Greater than $13,856",
                                     "Below 130% of Poverty Guidelines" ~ "Below $9,737",
                                     "Between 130% and 185% of Poverty Guidelines" ~"Between $9,737 and $13,856"))

genders = shiny_df |> distinct(gender) |> pull()
races = shiny_df |> distinct(race) |> pull()
marital = shiny_df |> distinct(marital_status) |> pull()
education = shiny_df |> distinct(education_level_20) |> pull()
incomes = shiny_df |> distinct(poverty_income) |> pull()

alcohols = c("Never in the last year", "1 to 2 times in the last year", "3 to 6 times in the last year", "7 to 11 times in the last year", "Once a month", "2 to 3 times a month", "Once a week", "2 times a week", "3 to 4 times a week", "Nearly every day", "Every day")

shinyApp(
  ui = fluidPage(
    titlePanel("Regression Model Prediction"),
    
    sidebarLayout(
      sidebarPanel(
        # Input for each x variable
        numericInput("age_input", h3("Enter your age:"), value = 22),
        
        radioButtons(inputId = "gender_choice", 
                     label = h3("Select gender:"),
                     choices = genders, 
                     selected = "Female"),
        
        selectInput(inputId = "race_choice",
                    label = h3("Select race:"),
                    choices = races,
                    selected = "Non-Hispanic Asian"),
        
        radioButtons(inputId = "marital_choice",
                     label = h3("Select marital status:"),
                     choices = marital,
                     selected = "Never married"),
        
        selectInput(inputId = "education_choice",
                    label = h3("Select education level:"),
                    choices = education,
                    selected = "College graduate or above"),
        
        radioButtons(inputId = "income_choice", 
                     label = h3("Select average family income:"),
                     choices = incomes, 
                     selected = "Greater than $13,856"),
        
        selectInput(inputId = "alcohol_choice",
                    label = h3("Select how often did you drink any type of alcoholic beverage during the past 12 months:"),
                    choices = alcohols,
                    selected = "Never in the last year"),
        
        actionButton("predict_button", "Predict")
      ),
      mainPanel(
        img(src = "image/image1.jpg", width = "80%", height = "80%"),
        
        verbatimTextOutput("prediction_output2"),
        verbatimTextOutput("prediction_output")
      )
      )
  ),
  
  server = function(input, output) {
    # Function to make predictions
    make_prediction <- function(alcohol, age, gender, race, marital, education, poverty) {
      new_data <- data.frame(alcohol_use_cat = alcohol, age = age, gender = gender, race = race, marital_status = marital, education_level_20 = education, poverty_level = poverty)  # Create a data frame with user inputs
      prediction <- predict(my_model, newdata = new_data)  # Make prediction
      return(prediction)
    }
    
    # React to the button click event
    observeEvent(input$predict_button, {
      age_input <- input$age_input
      gender_input <- input$gender_choice
      race_input <- input$race_choice
      marital_input <- input$marital_choice
      education_input <- input$education_choice
      poverty_input <- case_match(input$income_choice, "Greater than $13,856" ~ "Above 185% of Poverty Guidelines", "Between $9,737 and $13,856" ~ "Between 130% and 185% of Poverty Guidelines","Below $9,737" ~ "Below 130% of Poverty Guidelines")
      
      alcohol_input = case_when(input$alcohol_choice %in% c("Never in the last year", "1 to 2 times in the last year", "3 to 6 times in the last year", "7 to 11 times in the last year") ~ "Light Drinker", input$alcohol_choice %in% c("Once a month", "2 to 3 times a month", "Once a week", "2 times a week") ~ "Moderate Drinker", input$alcohol_choice %in% c("3 to 4 times a week", "Nearly every day", "Every day") ~ "Heavy Drinker")
      
      
      
      # Call the function to make predictions
      prediction_result = make_prediction(alcohol_input, age_input, gender_input, race_input, marital_input, education_input, poverty_input)
      
      # Display the prediction result
      
      output$prediction_output2 = renderText({
        # Your prediction result text
        paste("Considering only on these variables, your tend to have a cholesterol level approximately at: ", exp(prediction_result), "mg/dL.")
      })
      
      output$prediction_output = renderText({
        # Your prediction result text
        paste("Your risk of high cholesterol is: ", case_when(prediction_result > log(200) ~ "Above Normal Level",
                                          prediction_result <= log(200) ~ "At Normal Level"))
      })
    })
  }
)




