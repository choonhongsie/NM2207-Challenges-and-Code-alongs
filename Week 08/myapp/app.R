library(shiny)
library(ggplot2)

num<-c(1,2,3,4,5)
let<-c("A","B","C","D","E")
date<-c("2023-10-1","2023-10-2","2023-10-3","2015-10-4","2015-10-5")
df <- data.frame(num,let,date)
# Define UI ----

ui1 <- fluidPage(
  titlePanel(title=h4("Races", align="center")),
  sidebarPanel( 
    sliderInput("num", "Number:",min = 0, max = 5,step=1,value=c(1,2))),
  mainPanel(plotOutput("plot2")))

ui2 <- fluidPage(
  titlePanel("Choon Hong is using Shiny for the first time!"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30),
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title"),
      h6("Sixth level title")
    ),
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      p("p creates a paragraph of text."),
      p(
        "A new p() command starts a new paragraph. Supply a style attribute to change the format"
      ),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style."),
      p(
        "span does the same thing as div, but it works with",
        span("groups of words", style = "color:blue"),
        "that appear inside a paragraph."
      )
    )
  )
)
# Define server logic ----
server5 <- function(input, output) {
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "yellow",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("More Widgets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("rock", "pressure", "cars","choice 4")),
      
      # Input: Specify the number of observations to view ----
      numericInput("obs", "Number of observations to view:", 10),
      
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      sliderInput("num", "Number:",min = 0, max = 5,step=1,value=c(1,2)),
      actionButton("update", "Update View")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      img(
        src = "./Images/RStudio-Logo.png",
        height = 140,
        width = 400
      ),
      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: Header + table of distribution ----
      h4("Observations"),
      tableOutput("view"),
      
      plotOutput("plot2")
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  dat <- reactive({
    test <- df[df$num %in% seq(from=min(input$num),to=max(input$num),by=1),]
    print(test)
    test
  })
  output$plot2<-renderPlot({
    ggplot(dat(),aes(x=date,y=num))+geom_point(colour='red')},height = 400,width = 600)
  
  # Return the requested dataset ----
  # Note that we use eventReactive() here, which depends on
  # input$update (the action button), so that the output is only
  # updated when the user clicks the button
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  }, ignoreNULL = FALSE)
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    head(datasetInput(), n = isolate(input$obs))
  })
  dat <- reactive({
    test <- df[df$num %in% seq(from=min(input$num),to=max(input$num),by=1),]
    print(test)
    test
  })
}
server3 <- function(input,output){
  dat <- reactive({
    test <- df[df$num %in% seq(from=min(input$num),to=max(input$num),by=1),]
    print(test)
    test
    })
  output$plot2<-renderPlot({
    ggplot(dat(),aes(x=date,y=num))+geom_point(colour='red')},height = 400,width = 600)}
# Create Shiny app ----
shinyApp(ui, server)
