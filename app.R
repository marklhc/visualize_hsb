## app.R ##
library(shinydashboard)
library(tidyverse)
library(lme4)

# Read data
hsball <- haven::read_sav("data/hsball.sav") %>%
  mutate_at(vars(id, minority, female, sector, himinty), as.character)
sch_list <- unique(hsball$id)

ui <- dashboardPage(
  dashboardHeader(title = "Visualizing the HSB data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard", 
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(
                  title = "Controls",
                  selectInput("y", "Outcome variable:",
                              choices = "mathach", 
                              selected = "mathach"), 
                  selectInput("x", "X variable:",
                              choices = c(None = "none", 
                                          # select_vars(names(hsball), -id, 
                                          #             -mathach)), 
                                          "ses"), 
                              selected = "none"), 
                  selectInput("sch_id1", "School 1 id:", 
                              choices = c(None = "none", 
                                          sch_list), 
                              selected = "none"), 
                  selectInput("sch_id2", "School 2 id:", 
                              choices = c(None = "none", 
                                          sch_list), 
                              selected = "none"), 
                  selectInput("sch_id3", "School 3 id:", 
                              choices = c(None = "none", 
                                          sch_list), 
                              selected = "none"), 
                  width = 4
                ),
                
                box(plotOutput("dist_plot"), width = 8)
              ), 
              fluidRow(
                box(plotOutput("dist_plot1"), width = 4), 
                box(plotOutput("dist_plot2"), width = 4), 
                box(plotOutput("dist_plot3"), width = 4)
              )
      ), 
      
      # Second tab content
      tabItem(tabName = "widgets", 
              h2("Widgets tab content"))
    )
  )
)

server <- function(input, output) {
  m1 <- reactive({
    hsball <- hsball
    form <- as.formula(paste(input$y, "~", input$x, "+ (", input$x, "| id)"))
    lmer(form, data = hsball)
  })
  p1 <- reactive({
    if (input$x == "none") {
      plot_call <- substitute(qplot(x, data = hsball), 
                              list(x = as.name(input$y)))
      eval(plot_call)
      # line_p <- NULL
    } else {
      # plot_call <- substitute(qplot(x, y, data = hsball), 
      #                         list(x = as.name(input$x), 
      #                              y = as.name(input$y)))
      p1 <- ggplot(hsball, aes_string(x = input$x, y = input$y)) + 
        geom_point()
      # m1 <- substitute(lmer(y ~ x + (1 | id), data = hsball),
      #                  list(x = as.name(input$x),
      #                       y = as.name(input$y)))
      # m1 <- eval(m1)
      # line_p <- geom_abline(intercept = m1@beta[1], 
      #                       slope = m1@beta[2], col = "red")
      if (input$sch_id1 != "none") {
        eb_l1 <- ranef(m1())$id[input$sch_id1, ] %>% unlist()
        p1 <- p1 + geom_point(data = hsball %>% filter(id == input$sch_id1), 
                              col = "skyblue", size = 2.5) + 
          geom_abline(intercept = m1()@beta[1] + eb_l1[1], 
                      slope = m1()@beta[2] + eb_l1[2], 
                      col = "skyblue")
      }
      if (input$sch_id2 != "none") {
        eb_l2 <- ranef(m1())$id[input$sch_id2, ] %>% unlist()
        p1 <- p1 + geom_point(data = hsball %>% filter(id == input$sch_id2), 
                              col = "green", size = 2.5) + 
          geom_abline(intercept = m1()@beta[1] + eb_l2[1], 
                      slope = m1()@beta[2] + eb_l2[2], 
                      col = "green")
      }
      if (input$sch_id3 != "none") {
        eb_l3 <- ranef(m1())$id[input$sch_id3, ] %>% unlist()
        p1 <- p1 + geom_point(data = hsball %>% filter(id == input$sch_id3), 
                              col = "orange", size = 2.5) + 
          geom_abline(intercept = m1()@beta[1] + eb_l3[1], 
                      slope = m1()@beta[2] + eb_l3[2], 
                      col = "orange")
      }
      p1 + geom_abline(intercept = m1()@beta[1], 
                       slope = m1()@beta[2], col = "red")
        # geom_smooth(method = "lm", se = TRUE)
    }
  })
  output$dist_plot <- renderPlot({
    # eval(plot_call) + line_p
    p1()
  })
  
  p2 <- reactive({
    ggplot(hsball, aes_string(x = input$x, y = input$y)) + 
      geom_smooth(method = "lm", se = FALSE)
  })
  
  output$dist_plot1 <- renderPlot({
    req(input$sch_id1 != "none") # ensure availability of value before proceeding
    hsb1 <- hsball %>% filter(id == input$sch_id1)
    # if (input$x == "none") {
    #   plot_call <- substitute(qplot(x, data = hsb1), 
    #                           list(x = as.name(input$y)))
    # } else {
    #   plot_call <- substitute(qplot(x, y, data = hsb1), 
    #                           list(x = as.name(input$x), 
    #                                y = as.name(input$y)))
    # }
    # eval(plot_call)
    # p1() %+% hsb1
    eb_l1 <- ranef(m1())$id[input$sch_id1, ] %>% unlist()
    p2() %+% hsb1 + 
      geom_point(size = 2, col = "skyblue") + 
      geom_abline(intercept = m1()@beta[1] + eb_l1[1], 
                  slope = m1()@beta[2] + eb_l1[2], 
                  col = "skyblue")
  })
  
  output$dist_plot2 <- renderPlot({
    req(input$sch_id2 != "none") # ensure availability of value before proceeding
    hsb2 <- hsball %>% filter(id == input$sch_id2)
    eb_l2 <- ranef(m1())$id[input$sch_id2, ] %>% unlist()
    p2() %+% hsb2 + 
      geom_point(size = 2, col = "green") + 
      geom_abline(intercept = m1()@beta[1] + eb_l2[1], 
                  slope = m1()@beta[2] + eb_l2[2], 
                  col = "green")
  })
  
  output$dist_plot3 <- renderPlot({
    req(input$sch_id3 != "none") # ensure availability of value before proceeding
    hsb3 <- hsball %>% filter(id == input$sch_id3)
    eb_l3 <- ranef(m1())$id[input$sch_id3, ] %>% unlist()
    p2() %+% hsb3 + 
      geom_point(size = 2, col = "orange") + 
      geom_abline(intercept = m1()@beta[1] + eb_l3[1], 
                  slope = m1()@beta[2] + eb_l3[2], 
                  col = "orange")
  })
}

shinyApp(ui, server)