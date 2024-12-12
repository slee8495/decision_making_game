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
  character <- reactiveValues(name = NULL, role = NULL)
  current_page <- reactiveVal("character_creation")
  next_enabled <- reactiveVal(FALSE) # Track whether the "Next Level" button is enabled
  
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
        br(), br(), # Add space between buttons
        uiOutput("next_button"), # Display "Next Level" button dynamically
        textOutput("level1_feedback")
      )
    } else if (current_page() == "level2") {
      fluidPage(
        h3("Level 2: Survivorship Bias"),
        p("Challenge Background: Customer retention numbers show that existing clients are highly satisfied with your products. However, sales figures reveal a steady decline in new customer acquisition. The marketing team suggests improving features based on feedback from loyal customers, but your data analyst warns that ignoring lost customers may lead to blind spots in your strategy."),
        p("Class Concept: Survivorship bias occurs when we focus on successful cases and ignore failures, leading to misleading conclusions."),
        p("Scenario: You need to decide whether to focus on existing customers, analyze lost customers, or invest in new features."),
        radioButtons("level2_choice", "Your Decision:",
                     choices = list(
                       "Focus on existing customers" = "current",
                       "Analyze lost customers" = "lost",
                       "Invest in new features" = "new"
                     )),
        actionButton("submit_level2", "Submit Decision"),
        br(), br(),
        uiOutput("next_button"),
        textOutput("level2_feedback")
      )
    } else if (current_page() == "level3") {
      fluidPage(
        h3("Level 3: Framing Bias"),
        p("Challenge Background: The company faces budget constraints that require a reduction in headcount. The HR team proposes two different ways to communicate this to employees: one frames the decision positively (saving jobs), while the other emphasizes transparency (letting people go)."),
        p("Class Concept: Framing bias shows how people react differently depending on how information is presented."),
        p("Scenario: Choose how to frame the communication to your employees."),
        radioButtons("level3_choice", "Your Decision:",
                     choices = list(
                       "Save 70% of jobs" = "positive",
                       "Let 30% of employees go" = "negative"
                     )),
        actionButton("submit_level3", "Submit Decision"),
        br(), br(),
        uiOutput("next_button"),
        textOutput("level3_feedback")
      )
    } else if (current_page() == "level4") {
      fluidPage(
        h3("Level 4: Hiring Decisions"),
        p("Challenge Background: The company is developing a high-stakes project to regain market share. The success of this project hinges on hiring the right leader, but each candidate presents unique trade-offs."),
        p("Class Concept: Decision-making under uncertainty and evaluating trade-offs."),
        p("Scenario: Choose the best candidate for leading the project."),
        radioButtons("level4_choice", "Your Decision:",
                     choices = list(
                       "Candidate A: High Experience, $50,000" = "A",
                       "Candidate B: Moderate Experience, $30,000" = "B",
                       "Candidate C: Low Experience, $15,000" = "C"
                     )),
        actionButton("submit_level4", "Submit Decision"),
        br(), br(),
        uiOutput("next_button"),
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
        p("May this game remind you of the lessons learned and inspire you to apply them to every challenge ahead."),
        actionButton("play_again", "Play Again")
      )
    }
  })
  
  # Button Actions
  observeEvent(input$start_story, {
    character$name <- input$character_name
    character$role <- input$character_role
    if (character$name != "" && character$role != "") {
      current_page("story_intro")
    }
  })
  
  observeEvent(input$to_level1, {
    current_page("level1")
  })
  
  observeEvent(input$submit_level1, {
    feedback <- ""
    if (input$level1_choice == "risky") {
      feedback <- "Overconfidence! You ignored the risks and lost your investment."
      showModal(modalDialog(
        title = "Feedback: Invest Everything",
        "Overconfidence bias often leads to poor decisions. By ignoring the risks, you overestimated the chances of success.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() - 20)
    } else if (input$level1_choice == "cautious") {
      feedback <- "Great decision! You balanced risk and reward effectively."
      showModal(modalDialog(
        title = "Feedback: Invest Cautiously",
        "This approach balances optimism with a data-driven strategy, ensuring you consider potential risks and rewards.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() + 20)
    } else if (input$level1_choice == "avoid") {
      feedback <- "Safe choice, but you missed an opportunity."
      showModal(modalDialog(
        title = "Feedback: Avoid Investment",
        "While avoiding risks ensures stability, it also means you missed a potential growth opportunity.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() + 10)
    }
    output$level1_feedback <- renderText(feedback)
    next_enabled(TRUE)
  })
  
  observeEvent(input$submit_level2, {
    feedback <- ""
    if (input$level2_choice == "lost") {
      feedback <- "Great choice! Understanding lost customers prevents survivorship bias."
      showModal(modalDialog(
        title = "Feedback: Analyze Lost Customers",
        "By analyzing lost customers, you gain insights into why they left, allowing you to improve and attract new clients.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() + 20)
    } else if (input$level2_choice == "current") {
      feedback <- "Focusing on existing customers is safe but ignores crucial insights."
      showModal(modalDialog(
        title = "Feedback: Focus on Existing Customers",
        "While keeping current customers happy is important, ignoring why others leave may hurt long-term growth.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() + 10)
    } else {
      feedback <- "Investing in new features without understanding losses is risky."
      showModal(modalDialog(
        title = "Feedback: Invest in New Features",
        "Adding new features might not address the underlying issues causing customer loss.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() - 10)
    }
    output$level2_feedback <- renderText(feedback)
    next_enabled(TRUE)
  })
  
  observeEvent(input$submit_level3, {
    feedback <- ""
    if (input$level3_choice == "positive") {
      feedback <- "Good framing! Focusing on saving jobs boosts morale."
      showModal(modalDialog(
        title = "Feedback: Positive Framing",
        "Framing the situation positively can help maintain employee morale during tough times.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() + 20)
    } else if (input$level3_choice == "negative") {
      feedback <- "Transparency is important but may hurt morale."
      showModal(modalDialog(
        title = "Feedback: Transparent Framing",
        "Being transparent is ethical but framing negatively can demotivate remaining employees.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() + 10)
    }
    output$level3_feedback <- renderText(feedback)
    next_enabled(TRUE)
  })
  
  observeEvent(input$submit_level4, {
    feedback <- ""
    if (input$level4_choice == "B") {
      feedback <- "Excellent balance of cost and experience!"
      showModal(modalDialog(
        title = "Feedback: Candidate B",
        "Choosing Candidate B offers a good mix of experience and affordability, making it a strategic choice.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() + 20)
    } else if (input$level4_choice == "A") {
      feedback <- "Strong candidate, but very costly."
      showModal(modalDialog(
        title = "Feedback: Candidate A",
        "While highly experienced, the cost may strain your budget.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() + 10)
    } else if (input$level4_choice == "C") {
      feedback <- "Inexpensive, but risky due to lack of experience."
      showModal(modalDialog(
        title = "Feedback: Candidate C",
        "Low cost is appealing, but lack of experience may jeopardize the project's success.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      score(score() - 10)
    }
    output$level4_feedback <- renderText(feedback)
    next_enabled(TRUE)
  })
  
  # Handle Next Level Logic
  observe({
    if (next_enabled()) {
      output$next_button <- renderUI({
        actionButton("next_level", "Next Level")
      })
    } else {
      output$next_button <- renderUI(NULL)
    }
  })
  
  observeEvent(input$next_level, {
    next_enabled(FALSE)
    if (current_page() == "level1") {
      current_page("level2")
    } else if (current_page() == "level2") {
      current_page("level3")
    } else if (current_page() == "level3") {
      current_page("level4")
    } else if (current_page() == "level4") {
      current_page("congratulations")
    }
  })
  
  observeEvent(input$play_again, {
    score(0)
    achievements(c())
    character$name <- NULL
    character$role <- NULL
    current_page("character_creation")
  })
  
  # Example Data for Level 1
  output$data_table <- renderTable({
    data.frame(
      Year = 1:5,
      Revenue = c(5000, 15000, 30000, 60000, 120000),
      Risk = c(0.8, 0.7, 0.6, 0.4, 0.3)
    )
  })
  
  # Final Story Summary
  output$final_story <- renderUI({
    story <- paste(
      "Starting as", character$role, ",", character$name, ", you faced monumental challenges:",
      "navigating biases, analyzing customer data, and making tough HR decisions."
    )
    if (score() >= 70) {
      story <- paste(
        story,
        "Your flawless decision-making transformed the company. Investors are thrilled, employees trust your leadership, and the company is now an industry leader!"
      )
    } else if (score() >= 50) {
      story <- paste(
        story,
        "Your strong decision-making and strategic vision stabilized the company. You mastered some challenges but missed minor opportunities. The outlook is promising."
      )
    } else {
      story <- paste(
        story,
        "Your decisions left the company struggling. There's room for improvement, but the experience has provided valuable lessons for future endeavors."
      )
    }
    HTML(story)
  })
}

# Run the App
shinyApp(ui = ui, server = server)
