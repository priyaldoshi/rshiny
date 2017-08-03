ui <- dashboardPage(
  dashboardHeader(title = "LPP"),
  dashboardSidebar(
    sidebarMenu(
      # menuItemOutput("menuitem"),
      selectInput("L1","L1 Category",names(table(attributes$L1)),selected=T,multiple = F),
      selectInput("L4","L4 Categpry",names(table(attributes$L4)),selected=T,multiple = F)
    )
  ),
  dashboardBody(
    tabBox(width=1000,height=800,id = "conditionedPanels",
           tabPanel("Choose Data",icon=icon("database"), 
                    tableOutput('attributes2'),
                    downloadButton('downloadData', 'Download'),
                    tableOutput('lpp_sample1'),
                    value = 1),
           tabPanel("Linear Performance",icon=icon("cubes"),
                    plotOutput('plot1',hover = "plot_hover"),
                    verbatimTextOutput('equation'),
                    verbatimTextOutput('rsquared'),
                    tableOutput('anova'),
                    value = 2)
    )
  )
)
