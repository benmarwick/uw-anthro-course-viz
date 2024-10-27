library(shiny)
library(ggplot2)
library(readr)

# functions we need in the server --------------------------------------

format_time_label <- function(x) {
  hour(x)
}

new_names <- c( "Course" = "Course #" )

get_the_quarter_fn <- function(input_df, 
                               quarter = "AU"){
  input_df %>% 
    filter(Quarter == quarter) %>% 
    select(Course,
           Title,
           Mon:Fri, 
           `Time Start`,
           `Time End`,
           Quarter, 
           Prefix) %>% 
    pivot_longer(cols = Mon:Fri,
                 names_to = "day") %>% 
    drop_na(value) %>% 
    mutate(day = factor(day, c("Mon", "Tue", "Wed", "Thu", "Fri"))) %>% 
    mutate(crse1 = str_sub(Course, 1,1),
           crse2 = str_sub(Course, 2,3),
           Course = factor(Course)) %>% 
    group_by(crse1, crse2) %>% 
    filter(crse1 == min(crse1))  
}

plot_the_schedule_fn <- function(quarter,
                              dodge_width = 0.5){
  ggplot(quarter, 
         aes(xmin   = `Time Start`, 
             xmax   = `Time End`, 
             y      = day,
             label = Title)) +
    annotate("rect",
             xmin =  parse_date_time("8:30:00 AM", '%I:%M:%S %p'),
             xmax =  parse_date_time("6:00:00 PM", '%I:%M:%S %p'),
             ymax = c(0.5, 2.5, 4.5),
             ymin = c(1.5, 3.5, 5.5),
             fill = "grey80",
             alpha = 0.4) +
    geom_linerange(aes(color = Course),
                   position = position_dodge(width = dodge_width), 
                   size = 3) + 
    geom_shadowtext(aes(label = Course,
                        x = `Time Start`,
                        group = Course),
                    position = position_dodge2(width = dodge_width),
                    size = 3,
                    bg.colour= "white",
                    colour = "black") +
    scale_y_discrete(breaks = c("Mon", "Tue", "Wed", "Thu", "Fri"),
                     labels = c("Mon", "Tue", "Wed", "Thu", "Fri"),
                     drop=FALSE) +
    scale_x_datetime(date_breaks = "1 hour", 
                     labels = format_time_label
    ) +
    labs(x = "Time of day",
         y = "") +
    theme_minimal() +
    theme(legend.position = "bottom")  +
    ggtitle(paste0("Class days and times for ", 
                   unique(quarter$Prefix), 
                   " in the ",
                   unique(quarter$Quarter),
                   " quarter"))
}

# UI -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("UW Anthro Dept Course Scheduling Visualization App"),
  sidebarLayout(
    sidebarPanel(
      helpText(
        tags$p(
          "First, go to one of our course planning sheets: ",
          tags$a("ANTH", 
                 href = "https://docs.google.com/spreadsheets/d/1LEg20-MM1noo5Kq6W4MYFe4IUObB5ZveDj8uGZ75fPE/edit?gid=925449122#gid=925449122",
                 target="_blank"),", ", 
          tags$a("ARCHY", 
                 href = "https://docs.google.com/spreadsheets/d/1qu6Dl1ua2dLnkeR-j3ek1OJWAEVATlIdSJzjpH8CoAQ/edit?gid=1500143646#gid=1500143646",
                 target="_blank"),", ",        
          tags$a("BIO A", 
                 href = "https://docs.google.com/spreadsheets/d/15UlwHFfsHngtT5VXnyLs6EinHqBtWuyOphy5ZaHLmXo/edit?gid=509696154#gid=509696154",
                 target="_blank"),".",
          tags$p(),       
          "Second, select all the cells for one sheet, and copy them to your clipboard (CTRL + c or CMD + c).",
          tags$p(),
          "Third, paste them into the box below.",
          tags$p(), 
          "Fourth, click the 'Submit' button and plots should appear to the right.",
          tags$p(), 
         "The source code for this app is freely available at",
          tags$a("https://github.com/benmarwick/uw-anthro-curriculum", 
                 href = "https://github.com/benmarwick/uw-anthro-curriculum",
                 target="_blank"),"."
        )),
      textAreaInput("dataInput", 
                    "Paste data from the course planning sheet here:"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("plot1"),
      plotOutput("plot2"),
      plotOutput("plot3")
    )
  )
)

# Server ----------------------------------------------------------------------

server <- function(input, output) {
  
  # get the data
  dataInput <- eventReactive(input$submit, {
    read_delim(I(input$dataInput)) %>% 
      # drop rows with no times
      filter(!is.na(`Time Start`)) %>% 
      # convert times to a format R knows  <dttm>
      mutate(`Time Start` = parse_date_time(`Time Start`, orders = "HMS"),
             `Time End` =   parse_date_time(`Time End`, orders = "HMS")) %>% 
      rename(any_of(new_names))
  })
  
  output$plot1 <- renderPlot({
    if (!is.null(dataInput())) {
      plot_the_schedule_fn(get_the_quarter_fn(input_df = dataInput(),
                                              quarter = "AU"))
    }
  })
  
  output$plot2 <- renderPlot({
    if (!is.null(dataInput())) {
      plot_the_schedule_fn(get_the_quarter_fn(input_df = dataInput(),
                                              quarter = "WI"))
    }
  })
  
  output$plot3 <- renderPlot({
    if (!is.null(dataInput())) {
      plot_the_schedule_fn(get_the_quarter_fn(input_df = dataInput(),
                                              quarter = "SP"))
    }
  })
  
}

shinyApp(ui = ui, server = server)


# shinylive steps from https://posit-dev.github.io/r-shinylive/

# generate site
# shinylive::export(".", "site")

# deploy as github pages
# usethis::use_github_action(url="https://github.com/posit-dev/r-shinylive/blob/actions-v1/examples/deploy-app.yaml")




