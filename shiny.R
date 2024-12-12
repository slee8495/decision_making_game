library(shiny)
library(shinythemes)

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Decision Quest: Save the Company!"),
  
  # Story-Based UI with Navigation Buttons
  fluidPage(
    uiOutput("story_ui")
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values for game state
  score <- reactiveVal(0)
  achievements <- reactiveVal(c())
  character <- reactiveValues(name = "", role = "")
  current_page <- reactiveVal("character_creation")
  
  # Dynamic UI Rendering
  output$story_ui <- renderUI({
    if (current_page() == "character_creation") {
      fluidPage(
        h3("Welcome to Decision Quest!"),
        p("This web game was created as a personal action plan for the UCSD MSBA FW2024 cohort, MGTA459-661441: Managerial Judg/Decis Making class."),
        p("The purpose of this game is to distill the most useful concepts from the course into an interactive format that helps me apply them in the future."),
        p("The game is based on critical course concepts, including understanding biases (e.g., overconfidence bias, survivorship bias), decision-making strategies, and actionable insights for real-world challenges."),
        p("As you progress through the game, each level represents a scenario inspired by the course material. You will be challenged to make decisions and apply the lessons from the course."),
        p("I hope this game will serve as a tool to remind me of the lessons learned in the course as I move forward in my career."),
        textInput("character_name", "Enter your character's name:", value = ""),
        selectInput("character_role", "Select your role:",
                    choices = c("CEO", "Manager", "Entrepreneur", "Analyst")),
        actionButton("start_story", "Start Your Journey")
      )
    } else if (current_page() == "story_intro") {
      fluidPage(
        h3("Your Mission Begins"),
        p(paste("Welcome,", character$name, "! As the", character$role, ", you’ve just been hired to turn around a struggling company.")),
        p("The company is on the brink of failure. Revenue has dropped 40% over the last year, customer retention is at an all-time low, and employee morale is in the gutter. The board of directors is placing immense pressure on you to turn things around quickly or face severe consequences."),
        p("Your first task is to stabilize the company and restore hope. Each level will challenge your strategic thinking, decision-making skills, and ability to analyze complex scenarios under pressure. Are you ready to lead this company to greatness?"),
        actionButton("to_level1", "Start Level 1")
      )
    } else if (current_page() == "level1") {
      fluidPage(
        h3("Level 1: Overconfidence Bias"),
        p("Challenge Background: The company has an opportunity to invest in a high-risk, high-reward startup. This startup has gained significant media attention, promising revolutionary technology in its field. While initial data looks promising, your financial analyst highlights several risks, including a lack of scalability and potential market saturation."),
        p("Class Concept: Overconfidence bias often leads to poor decision-making, as individuals tend to underestimate risks and overestimate their own predictive abilities. This level emphasizes the importance of balancing optimism with data-driven analysis."),
        p("Scenario: You have access to preliminary financial projections and market research. Your team looks to you for guidance on whether to proceed with the investment. Your options are:"),
        tableOutput("data_table"),
        radioButtons("level1_choice", "Your Decision:",
                     choices = list(
                       "Invest everything" = "risky",
                       "Invest cautiously" = "cautious",
                       "Avoid entirely" = "avoid"
                     )),
        actionButton("submit_level1", "Submit Decision"),
        textOutput("level1_feedback")
      )
    } else if (current_page() == "level2") {
      fluidPage(
        h3("Level 2: Survivorship Bias"),
        p("Challenge Background: Customer retention numbers show that existing clients are highly satisfied with your products. However, sales figures reveal a steady decline in new customer acquisition. The marketing team suggests improving features based on feedback from loyal customers, but your data analyst warns that ignoring lost customers may lead to blind spots in your strategy."),
        radioButtons("level2_choice", "Your Decision:",
                     choices = list(
                       "Focus on existing customers" = "current",
                       "Analyze lost customers" = "lost",
                       "Invest in new features" = "new"
                     )),
        actionButton("submit_level2", "Submit Decision"),
        textOutput("level2_feedback")
      )
    } else if (current_page() == "level3") {
      fluidPage(
        h3("Level 3: Framing Bias"),
        p("Challenge Background: The company faces budget constraints that require a reduction in headcount. The HR team proposes two different ways to communicate this to employees: one frames the decision positively (saving jobs), while the other emphasizes transparency (letting people go)."),
        radioButtons("level3_choice", "Your Decision:",
                     choices = list(
                       "Save 70% of jobs" = "positive",
                       "Let 30% of employees go" = "negative"
                     )),
        actionButton("submit_level3", "Submit Decision"),
        textOutput("level3_feedback")
      )
    } else if (current_page() == "level4") {
      fluidPage(
        h3("Level 4: Hiring Decisions"),
        p("Challenge Background: The company is developing a high-stakes project to regain market share. The success of this project hinges on hiring the right leader, but each candidate presents unique trade-offs."),
        radioButtons("level4_choice", "Your Decision:",
                     choices = list(
                       "Candidate A: High Experience, $50,000" = "A",
                       "Candidate B: Moderate Experience, $30,000" = "B",
                       "Candidate C: Low Experience, $15,000" = "C"
                     )),
        actionButton("submit_level4", "Submit Decision"),
        textOutput("level4_feedback")
      )
    } else if (current_page() == "congratulations") {
      fluidPage(
        h3("Congratulations!"),
        p("Your journey as a leader has concluded. Here’s how you transformed your company:"),
        uiOutput("final_story"),
        h3("A Note to Future Me:"),
        p(paste(character$name, "! When you are looking back at this course because you just got promoted to Sr. Director or COO, please remember:")),
        tags$ul(
          tags$li("The importance of balancing optimism with data-driven decision-making."),
          tags$li("Always consider the unseen factors, like lost customers or hidden biases."),
          tags$li("Communicate clearly and positively to influence others effectively.")
        ),
        p("May this game remind you of the lessons learned and inspire you to apply them to every challenge ahead.")
      )
    }
  })
  
  # Button Actions to Navigate Pages
  observeEvent(input$start_story, {
    character$name <- input$character_name
    character$role <- input$character_role
    current_page("story_intro")
  })
  
  observeEvent(input$to_level1, {
    current_page("level1")
  })
  
  observeEvent(input$submit_level1, {
    feedback <- ""
    if (input$level1_choice == "risky") {
      feedback <- "Overconfidence! You ignored the risks and lost your investment."
      score(score() - 20)
    } else if (input$level1_choice == "cautious") {
      feedback <- "Great decision! You balanced risk and reward effectively."
      score(score() + 20)
      achievements(c(achievements(), "Level 1 Mastered"))
    } else {
      feedback <- "Safe choice, but you missed an opportunity."
      score(score() + 10)
    }
    output$level1_feedback <- renderText(feedback)
    current_page("level2")
  })
  
  observeEvent(input$submit_level2, {
    feedback <- ""
    if (input$level2_choice == "lost") {
      feedback <- "Great choice! Understanding lost customers prevents survivorship bias."
      score(score() + 20)
      achievements(c(achievements(), "Level 2 Mastered"))
    } else if (input$level2_choice == "current") {
      feedback <- "Focusing on existing customers is safe but ignores crucial insights."
      score(score() + 10)
    } else {
      feedback <- "Investing in new features without understanding losses is risky."
      score(score() - 10)
    }
    output$level2_feedback <- renderText(feedback)
    current_page("level3")
  })
  
  observeEvent(input$submit_level3, {
    feedback <- ""
    if (input$level3_choice == "positive") {
      feedback <- "Good framing! Focusing on saving jobs boosts morale."
      score(score() + 20)
      achievements(c(achievements(), "Level 3 Mastered"))
    } else {
      feedback <- "Transparency is important but may hurt morale."
      score(score() + 10)
    }
    output$level3_feedback <- renderText(feedback)
    current_page("level4")
  })
  
  observeEvent(input$submit_level4, {
    feedback <- ""
    if (input$level4_choice == "B") {
      feedback <- "Excellent balance of cost and experience!"
      score(score() + 20)
      achievements(c(achievements(), "Level 4 Mastered"))
    } else if (input$level4_choice == "A") {
      feedback <- "Strong candidate, but very costly."
      score(score() + 10)
    } else {
      feedback <- "Inexpensive, but risky due to lack of experience."
      score(score() - 10)
    }
    output$level4_feedback <- renderText(feedback)
    current_page("congratulations")
  })
  
  # Example Data for Level 1
  observe({
    startup_data <- data.frame(
      Year = 1:5,
      Revenue = c(5000, 15000, 30000, 60000, 120000),
      Risk = c(0.8, 0.7, 0.6, 0.4, 0.3)
    )
    output$data_table <- renderTable(startup_data)
  })
  
  # Final Story Summary
  output$final_story <- renderUI({
    story <- paste(
      "Starting as", character$role, ",", character$name, ", you faced monumental challenges:",
      "navigating biases, analyzing customer data, and making tough HR decisions."
    )
    
    if (score() == 80 && length(achievements()) == 4) {
      story <- paste(
        story,
        "Your flawless decision-making transformed the company. Investors are thrilled, employees trust your leadership, and the company is now an industry leader!"
      )
    } else if (score() >= 50) {
      story <- paste(
        story,
        "Your strong decision-making and strategic vision stabilized the company. You mastered some challenges but missed minor opportunities. The outlook is promising."
      )
    } else if (score() >= 20) {
      story <- paste(
        story,
        "Despite your efforts, inconsistent decisions have left the company struggling. Employee morale is low, and financial stability remains precarious."
      )
    } else {
      story <- paste(
        story,
        "Unfortunately, your decisions caused significant setbacks. The company faces severe challenges, and recovery will require substantial effort."
      )
    }
    
    HTML(story)
  })
}

# Run the App
shinyApp(ui = ui, server = server)
