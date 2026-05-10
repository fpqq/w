library(shiny)
library(tidyverse)

`%||%` <- function(x, y) if (is.null(x)) y else x

score_choice <- function(x, map) {
  unname(map[[x]] %||% 50)
}

app_title <- "Prius Plug-in Benefit Index (PPBI)"

ui <- fluidPage(
  titlePanel(app_title),
  
  tags$div(
    style = "margin-top:-10px; margin-bottom:20px; color:#555555; font-size:14px; font-style:italic;",
    HTML("&copy; Dr. Reza Norouzian, 2026<br/>Contact: F.Keyhaniha@gmail.com")
  ),
  
  tags$p(
    "This tool estimates how beneficial a Prius Plug-in Hybrid may be for a specific buyer, ",
    "using driving habits, charging access, household context, purchase economics, space needs, ",
    "electricity/incentives, and ownership preferences."
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      h4("1. Driving Pattern"),
      
      selectInput("daily_miles", "Typical daily miles",
                  c("Under 10 miles", "10–20 miles", "20–35 miles",
                    "35–50 miles", "50–80 miles", "Over 80 miles")),
      
      selectInput("driving_type", "Driving type",
                  c("Mostly city/suburban stop-and-go driving",
                    "Mostly mixed driving",
                    "Mostly highway driving",
                    "Mostly rural/open-road driving")),
      
      selectInput("long_trips", "Trips longer than 150 miles in one day",
                  c("Rarely", "A few times per year", "Monthly",
                    "Weekly", "Multiple times per week")),
      
      selectInput("annual_miles", "Annual mileage",
                  c("Under 5,000", "5,000–10,000", "10,000–15,000",
                    "15,000–20,000", "Over 20,000")),
      
      hr(),
      
      h4("2. Charging"),
      
      selectInput("home_charging", "Home charging access",
                  c("Yes, dedicated garage charging",
                    "Yes, outdoor outlet near parking",
                    "Yes, but somewhat inconvenient",
                    "Possibly, with installation/work",
                    "No reliable home charging access")),
      
      sliderInput("plug_likelihood", "Likelihood of plugging in regularly",
                  min = 1, max = 5, value = 4),
      
      checkboxGroupInput("charging_locations", "Where would charging occur?",
                         c("Home", "Workplace", "Public charging stations",
                           "Family/friends", "Unsure")),
      
      selectInput("plug_feeling", "Regularly plugging in would feel:",
                  c("Enjoyable/satisfying", "Neutral", "Slight inconvenience",
                    "Moderate inconvenience", "Major inconvenience")),
      
      selectInput("forget_tasks", "How often do you forget recurring charging/electronic tasks?",
                  c("Rarely", "Occasionally", "Sometimes", "Often", "Very often")),
      
      hr(),
      
      h4("3. Household Context"),
      
      selectInput("vehicle_role", "This Prius Plug-in would be:",
                  c("The household’s only vehicle",
                    "The primary household vehicle",
                    "A secondary household vehicle",
                    "A commuter/daily-use vehicle",
                    "A backup/occasional-use vehicle")),
      
      selectInput("num_vehicles", "Number of household vehicles",
                  c("1", "2", "3", "4 or more")),
      
      checkboxGroupInput("household_vehicles", "Household already owns:",
                         c("Full EV", "Hybrid vehicle", "Pickup truck",
                           "Large SUV/minivan", "None of the above")),
      
      hr(),
      
      h4("4. Space & Practicality"),
      
      selectInput("driver_height", "Your approximate height",
                  c("Under 5'6\"", "5'6\"–5'10\"", "5'10\"–6'1\"",
                    "6'1\"–6'4\"", "Over 6'4\"")),
      
      selectInput("tallest_passenger", "Tallest regular passenger",
                  c("Under 5'8\"", "5'8\"–6'0\"", "6'0\"–6'3\"", "Over 6'3\"")),
      
      selectInput("rear_adults", "How often would adults ride in rear seats?",
                  c("Rarely", "Occasionally", "Frequently", "Almost daily")),
      
      checkboxGroupInput("rear_passengers", "Rear passengers may include:",
                         c("Teenagers", "Tall adults", "Child car seats",
                           "Multiple children", "Pets", "None of the above")),
      
      sliderInput("cargo_importance", "Importance of cargo/storage flexibility",
                  min = 1, max = 5, value = 3),
      
      selectInput("cargo_needs", "Typical cargo needs",
                  c("Mostly groceries/small errands",
                    "Luggage for occasional travel",
                    "Frequent large shopping trips",
                    "Sports/outdoor equipment",
                    "Strollers/kids equipment",
                    "Work/business equipment")),
      
      selectInput("space_expectation", "Which statement best matches your expectations?",
                  c("I prefer large SUVs/trucks regardless of actual need",
                    "I want the smallest vehicle that comfortably fits my needs",
                    "I value efficient use of space",
                    "I strongly prioritize interior openness",
                    "I am unsure")),
      
      selectInput("prius_perception", "Before this assessment, how would you classify the Prius interior?",
                  c("Very small/cramped", "Small compact", "Moderate/average",
                    "Similar to midsize sedans", "Spacious", "Unsure")),
      
      selectInput("sat_in_prius", "Have you sat in or driven the latest-generation Prius?",
                  c("Yes, extensively", "Yes, briefly", "Only seen photos/videos", "No")),
      
      hr(),
      
      h4("5. Purchase Economics"),
      
      selectInput("discount", "Expected discount below TSRP before tax, title, and license",
                  c("Paying above TSRP",
                    "TSRP/no discount",
                    "1–3% below TSRP",
                    "3–5% below TSRP",
                    "5–8% below TSRP",
                    "8–11% below TSRP",
                    "More than 11% below TSRP")),
      
      selectInput("dealer_addons", "Dealer add-ons relative to TSRP",
                  c("No required dealer add-ons",
                    "Minor add-ons under $500",
                    "Moderate add-ons $500–$1,500",
                    "High add-ons over $1,500",
                    "Unsure")),
      
      selectInput("apr", "Expected financing APR",
                  c("Cash purchase", "Under 2%", "2–4%", "4–6%", "6–8%", "Above 8%")),
      
      selectInput("loan_term", "Expected loan term",
                  c("36 months", "48 months", "60 months", "72 months", "84+ months")),
      
      sliderInput("upfront_cost", "Importance of minimizing upfront cost",
                  min = 1, max = 5, value = 3),
      
      sliderInput("insurance_concern", "Concern about higher insurance/registration",
                  min = 1, max = 5, value = 3),
      
      hr(),
      
      h4("6. Electricity & Incentives"),
      
      selectInput("electricity_cost", "Approximate home electricity cost",
                  c("Under $0.10/kWh", "$0.10–0.15/kWh", "$0.15–0.20/kWh",
                    "$0.20–0.30/kWh", "Over $0.30/kWh", "Unsure")),
      
      checkboxGroupInput("incentives", "Available incentives/programs",
                         c("EV utility rebate", "Time-of-use/off-peak rate",
                           "Smart charging incentive", "Free workplace charging",
                           "State EV incentives", "Reduced EV registration",
                           "Apartment/community charging", "None of the above",
                           "Unsure")),
      
      sliderInput("use_incentives", "Likelihood of using incentives/programs",
                  min = 1, max = 5, value = 3),
      
      hr(),
      
      h4("7. Preferences"),
      
      sliderInput("quiet_smooth", "Importance of quiet/smooth driving",
                  min = 1, max = 5, value = 4),
      
      sliderInput("fuel_efficiency", "Importance of fuel efficiency",
                  min = 1, max = 5, value = 5),
      
      sliderInput("reliability", "Importance of long-term reliability",
                  min = 1, max = 5, value = 5),
      
      sliderInput("gas_station", "Importance of fewer gas station visits",
                  min = 1, max = 5, value = 4),
      
      sliderInput("ev_transition", "Interest in transitioning toward EV ownership",
                  min = 1, max = 5, value = 4),
      
      hr(),
      
      h4("8. Climate & Ownership"),
      
      selectInput("climate", "Climate",
                  c("Mostly mild year-round", "Moderate seasons",
                    "Very hot summers", "Very cold winters",
                    "Both very hot and very cold")),
      
      selectInput("terrain", "Terrain",
                  c("Mostly flat", "Mixed terrain", "Hilly/mountainous")),
      
      selectInput("ownership_length", "Expected ownership length",
                  c("Under 3 years", "3–5 years", "5–8 years",
                    "8–12 years", "Over 12 years")),
      
      selectInput("alternative", "Most likely alternative vehicle",
                  c("Regular Prius hybrid", "Camry hybrid", "RAV4 hybrid",
                    "Full EV", "Gasoline compact sedan", "Gasoline SUV",
                    "Other hybrid", "Unsure")),
      
      checkboxGroupInput("top_factors", "Select up to 3 most important factors",
                         c("Lowest total ownership cost", "Fuel efficiency",
                           "Reliability", "Quiet/smooth driving",
                           "Convenience", "Environmental impact",
                           "No charging hassle", "Resale value")),
      
      hr(),
      
      h4("9. Final Self-Assessment"),
      
      sliderInput("excitement", "Excitement about owning a plug-in hybrid",
                  min = 1, max = 5, value = 4),
      
      selectInput("maximize_ev", "Would you consistently maximize EV usage?",
                  c("Definitely yes", "Probably yes", "Unsure",
                    "Probably not", "Definitely not"))
    ),
    
    mainPanel(
      width = 8,
      
      h2("Prius Plug-in Benefit Index"),
      
      tags$div(
        style = "font-size: 52px; font-weight: bold;",
        textOutput("overall_score")
      ),
      
      tags$div(
        style = "font-size: 22px; margin-bottom: 20px;",
        textOutput("recommendation")
      ),
      
      hr(),
      
      h3("Subscores"),
      tableOutput("subscores"),
      
      hr(),
      
      h3("Interpretation"),
      verbatimTextOutput("interpretation"),
      
      hr(),
      
      h3("Prius Plug-in vs Regular Prius Insight"),
      verbatimTextOutput("comparison")
    )
  )
)

server <- function(input, output) {
  
  scores <- reactive({
    
    driving_score <- mean(c(
      score_choice(input$daily_miles, list(
        "Under 10 miles" = 85,
        "10–20 miles" = 100,
        "20–35 miles" = 100,
        "35–50 miles" = 90,
        "50–80 miles" = 65,
        "Over 80 miles" = 40
      )),
      score_choice(input$driving_type, list(
        "Mostly city/suburban stop-and-go driving" = 100,
        "Mostly mixed driving" = 85,
        "Mostly highway driving" = 55,
        "Mostly rural/open-road driving" = 50
      )),
      score_choice(input$long_trips, list(
        "Rarely" = 100,
        "A few times per year" = 90,
        "Monthly" = 75,
        "Weekly" = 55,
        "Multiple times per week" = 35
      )),
      score_choice(input$annual_miles, list(
        "Under 5,000" = 50,
        "5,000–10,000" = 75,
        "10,000–15,000" = 100,
        "15,000–20,000" = 90,
        "Over 20,000" = 75
      ))
    ))
    
    charging_score <- mean(c(
      score_choice(input$home_charging, list(
        "Yes, dedicated garage charging" = 100,
        "Yes, outdoor outlet near parking" = 90,
        "Yes, but somewhat inconvenient" = 70,
        "Possibly, with installation/work" = 50,
        "No reliable home charging access" = 15
      )),
      input$plug_likelihood * 20,
      score_choice(input$plug_feeling, list(
        "Enjoyable/satisfying" = 100,
        "Neutral" = 80,
        "Slight inconvenience" = 60,
        "Moderate inconvenience" = 35,
        "Major inconvenience" = 10
      )),
      score_choice(input$forget_tasks, list(
        "Rarely" = 100,
        "Occasionally" = 80,
        "Sometimes" = 60,
        "Often" = 35,
        "Very often" = 15
      ))
    ))
    
    household_score <- mean(c(
      score_choice(input$vehicle_role, list(
        "The household’s only vehicle" = 70,
        "The primary household vehicle" = 85,
        "A secondary household vehicle" = 95,
        "A commuter/daily-use vehicle" = 100,
        "A backup/occasional-use vehicle" = 55
      )),
      score_choice(input$num_vehicles, list(
        "1" = 70,
        "2" = 95,
        "3" = 90,
        "4 or more" = 85
      )),
      ifelse("Large SUV/minivan" %in% input$household_vehicles, 100, 75),
      ifelse("Hybrid vehicle" %in% input$household_vehicles, 80, 90)
    ))
    
    space_score <- mean(c(
      score_choice(input$driver_height, list(
        "Under 5'6\"" = 100,
        "5'6\"–5'10\"" = 100,
        "5'10\"–6'1\"" = 90,
        "6'1\"–6'4\"" = 70,
        "Over 6'4\"" = 45
      )),
      score_choice(input$tallest_passenger, list(
        "Under 5'8\"" = 100,
        "5'8\"–6'0\"" = 90,
        "6'0\"–6'3\"" = 70,
        "Over 6'3\"" = 45
      )),
      score_choice(input$rear_adults, list(
        "Rarely" = 100,
        "Occasionally" = 85,
        "Frequently" = 60,
        "Almost daily" = 45
      )),
      input$cargo_importance * 15,
      score_choice(input$cargo_needs, list(
        "Mostly groceries/small errands" = 100,
        "Luggage for occasional travel" = 90,
        "Frequent large shopping trips" = 75,
        "Sports/outdoor equipment" = 60,
        "Strollers/kids equipment" = 55,
        "Work/business equipment" = 50
      )),
      score_choice(input$space_expectation, list(
        "I prefer large SUVs/trucks regardless of actual need" = 35,
        "I want the smallest vehicle that comfortably fits my needs" = 100,
        "I value efficient use of space" = 100,
        "I strongly prioritize interior openness" = 55,
        "I am unsure" = 70
      ))
    ))
    
    economics_score <- mean(c(
      score_choice(input$discount, list(
        "Paying above TSRP" = 5,
        "TSRP/no discount" = 30,
        "1–3% below TSRP" = 45,
        "3–5% below TSRP" = 65,
        "5–8% below TSRP" = 85,
        "8–11% below TSRP" = 100,
        "More than 11% below TSRP" = 100
      )),
      score_choice(input$dealer_addons, list(
        "No required dealer add-ons" = 100,
        "Minor add-ons under $500" = 85,
        "Moderate add-ons $500–$1,500" = 55,
        "High add-ons over $1,500" = 20,
        "Unsure" = 60
      )),
      score_choice(input$apr, list(
        "Cash purchase" = 90,
        "Under 2%" = 100,
        "2–4%" = 85,
        "4–6%" = 60,
        "6–8%" = 40,
        "Above 8%" = 20
      )),
      score_choice(input$loan_term, list(
        "36 months" = 90,
        "48 months" = 95,
        "60 months" = 90,
        "72 months" = 75,
        "84+ months" = 45
      )),
      100 - input$upfront_cost * 10,
      100 - input$insurance_concern * 12
    ))
    
    electricity_score <- mean(c(
      score_choice(input$electricity_cost, list(
        "Under $0.10/kWh" = 100,
        "$0.10–0.15/kWh" = 90,
        "$0.15–0.20/kWh" = 75,
        "$0.20–0.30/kWh" = 50,
        "Over $0.30/kWh" = 25,
        "Unsure" = 60
      )),
      ifelse(any(input$incentives %in% c(
        "EV utility rebate", "Time-of-use/off-peak rate",
        "Smart charging incentive", "Free workplace charging",
        "State EV incentives"
      )), 90, 50),
      input$use_incentives * 20
    ))
    
    preference_score <- mean(c(
      input$quiet_smooth * 20,
      input$fuel_efficiency * 20,
      input$reliability * 20,
      input$gas_station * 20,
      input$ev_transition * 20,
      input$excitement * 20,
      score_choice(input$maximize_ev, list(
        "Definitely yes" = 100,
        "Probably yes" = 85,
        "Unsure" = 60,
        "Probably not" = 35,
        "Definitely not" = 10
      ))
    ))
    
    climate_ownership_score <- mean(c(
      score_choice(input$climate, list(
        "Mostly mild year-round" = 100,
        "Moderate seasons" = 90,
        "Very hot summers" = 80,
        "Very cold winters" = 60,
        "Both very hot and very cold" = 70
      )),
      score_choice(input$terrain, list(
        "Mostly flat" = 100,
        "Mixed terrain" = 85,
        "Hilly/mountainous" = 70
      )),
      score_choice(input$ownership_length, list(
        "Under 3 years" = 45,
        "3–5 years" = 65,
        "5–8 years" = 85,
        "8–12 years" = 100,
        "Over 12 years" = 100
      ))
    ))
    
    tibble(
      domain = c(
        "Driving Fit",
        "Charging Feasibility",
        "Household Compatibility",
        "Space & Practicality",
        "Purchase Economics",
        "Electricity & Incentives",
        "Preference Fit",
        "Climate & Ownership Horizon"
      ),
      score = c(
        driving_score,
        charging_score,
        household_score,
        space_score,
        economics_score,
        electricity_score,
        preference_score,
        climate_ownership_score
      ),
      weight = c(.15, .20, .10, .12, .18, .10, .10, .05)
    ) %>%
      mutate(weighted_score = score * weight)
  })
  
  overall <- reactive({
    round(sum(scores()$weighted_score))
  })
  
  output$overall_score <- renderText({
    paste0(overall(), "%")
  })
  
  output$recommendation <- renderText({
    x <- overall()
    s <- scores()
    
    weak_domains <- s %>%
      filter(score < 65) %>%
      pull(domain)
    
    caveat_text <- if (length(weak_domains) == 0) {
      "No major caveats were identified."
    } else {
      paste0("Main caveats: ", paste(weak_domains, collapse = ", "), ".")
    }
    
    case_when(
      x >= 85 ~ paste0(
        "Excellent fit: the Prius Plug-in is likely highly beneficial for this buyer. ",
        "The person likely has strong charging access, favorable driving patterns, good ownership fit, and acceptable purchase economics. ",
        caveat_text
      ),
      
      x >= 70 ~ paste0(
        "Strong fit: the Prius Plug-in is likely beneficial, but the final decision depends on the weaker areas. ",
        caveat_text,
        " These are the specific factors that could reduce the plug-in advantage compared with a regular Prius hybrid."
      ),
      
      x >= 55 ~ paste0(
        "Moderate fit: the Prius Plug-in may be beneficial, but the advantage is not automatic. ",
        caveat_text,
        " The buyer should compare carefully against a regular Prius hybrid, especially if charging is inconvenient, TSRP discount is weak, add-ons are high, or EV usage will be inconsistent."
      ),
      
      x >= 40 ~ paste0(
        "Limited plug-in advantage: the buyer may still like the Prius Plug-in, but the practical or financial benefit appears modest. ",
        caveat_text,
        " A regular Prius hybrid may be the safer value choice unless the buyer strongly values EV driving."
      ),
      
      TRUE ~ paste0(
        "Low benefit: the plug-in version is probably not the best match under these conditions. ",
        caveat_text,
        " A regular hybrid, regular Prius, Camry hybrid, or another vehicle may provide better overall value."
      )
    )
  })
  
  output$subscores <- renderTable({
    scores() %>%
      transmute(
        Domain = domain,
        `Subscore (%)` = round(score),
        `Weight (%)` = weight * 100
      )
  })
  
  output$interpretation <- renderText({
    s <- scores()
    
    weak <- s %>%
      filter(score < 60) %>%
      pull(domain)
    
    strong <- s %>%
      filter(score >= 85) %>%
      pull(domain)
    
    paste0(
      "Strongest areas: ",
      ifelse(length(strong) == 0, "None above 85%.", paste(strong, collapse = ", ")),
      "\n\nPotential limitations: ",
      ifelse(length(weak) == 0, "No major weak areas.", paste(weak, collapse = ", ")),
      "\n\nThis score estimates the incremental benefit of choosing a Prius Plug-in Hybrid, not whether the Prius itself is a good vehicle."
    )
  })
  
  output$comparison <- renderText({
    plug_score <- overall()
    
    regular_prius_flag <- case_when(
      input$home_charging == "No reliable home charging access" ~ TRUE,
      input$plug_likelihood <= 2 ~ TRUE,
      input$dealer_addons %in% c("Moderate add-ons $500–$1,500", "High add-ons over $1,500") ~ TRUE,
      input$electricity_cost %in% c("$0.20–0.30/kWh", "Over $0.30/kWh") &&
        input$discount %in% c("Paying above TSRP", "TSRP/no discount", "1–3% below TSRP") ~ TRUE,
      plug_score < 60 ~ TRUE,
      TRUE ~ FALSE
    )
    
    if (regular_prius_flag) {
      "A regular Prius hybrid may deserve serious consideration. Your responses suggest that the plug-in benefits may be reduced by charging access, charging behavior, electricity cost, purchase economics, required add-ons, or overall usage pattern."
    } else {
      "The Prius Plug-in appears to offer meaningful additional benefit over a regular Prius hybrid, especially if you regularly charge and receive a strong TSRP discount with minimal or no dealer add-ons."
    }
  })
}

shinyApp(ui, server)
