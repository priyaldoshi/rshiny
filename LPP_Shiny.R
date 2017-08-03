library(shiny)
library(shinydashboard)
# install.packages("tidyverse")
library(tidyverse)
library(MASS)
library(car)
library(caret)
# install.packages("caret")

#Data Preprocessing
setwd("D:\\mahindra\\LPP\\Codes")

attributes <- read.csv("Category - Attributes.csv")
lpp_sample <- read.csv("LPP_Sample_Data1.csv")

#Replace empty with NA
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

## transform all blank column values with NA
attributes <- attributes %>% mutate_all(funs(empty_as_na)) 


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

server <- function(input, output, session) {
  # output$menuitem <- renderMenu({
  #   menuItem("Menu item", icon = icon("calendar"))
  # })
  
  observe({
    l1<-input$L1
    L4_choices<-as.character(attributes[attributes$L1 == input$L1,"L4"])
    updateSelectInput(session,"L4",choices=L4_choices,selected = l1)
  })

  
  output$attributes2 <- renderTable({
    drops <- c("L1","L2","L3","L4")
    attributes1 <- attributes[(attributes$L1 == input$L1) & (attributes$L4 == input$L4),!(names(attributes) %in% drops)]
    attributes2 <<- gather(attributes1)%>% filter(!is.na(value))
    return(attributes2)
    })
  
  output$lpp_sample1 <- renderTable({
    drops <- c("L1","L2","L3","L4")
    lpp_sample1 <- lpp_sample[(lpp_sample$L1 == input$L1) & (lpp_sample$L4 == input$L4),!(names(lpp_sample) %in% drops)]
    lpp_non_attribute <- data.frame("Part"=lpp_sample1$Part,"Vendor"=lpp_sample1$Vendor,"PO.price"=lpp_sample1$PO.price)
    lpp_attribute <- lpp_sample1[,-which(names(lpp_sample1) %in% names(lpp_non_attribute))]
    lpp_attribute1 <- lpp_attribute[,which(names(lpp_attribute) %in% names(table(attributes2$value)))] 
    lpp_sample1 <<- data.frame(lpp_non_attribute,lpp_attribute1)
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste0("stats", '.xlsx')},
    content = function(file) {
      write.csv(lpp_sample1,file)
    })
  
  output$equation <- renderText({
    drops <- c("Part","Vendor")
    lpp_sample1 <- lpp_sample1[,!(names(lpp_sample1) %in% drops)]
    attach(lpp_sample1)
    lpp2 <<- stepAIC(lm(PO.price ~.,data = lpp_sample1),direction="backward",test="F")
    detach(lpp_sample1)
    leveragePlots(lpp2)
    qqPlot(lpp2)
    
    par(mfrow = c(2, 2))
    plot(lpp2)
    #Making the regression Equation
    regEq <- function(lmObj, dig) {
      gsub(":", "*", 
           paste0(
             names(lmObj$model)[1]," = ",
             paste0(
               c(round(lmObj$coef[1], dig), round(sign(lmObj$coef[-1])*lmObj$coef[-1], dig)),
               c("", rep("*", length(lmObj$coef)-1)),
               paste0(c("", names(lmObj$coef)[-1]), c(ifelse(sign(lmObj$coef)[-1]==1," + "," - "), "")),
               collapse=""
             )
           )
      )
    }
    
    equation <- regEq(lpp2,2)
  })
  
  
  output$anova <- renderPrint({
      anova <- lpp2$anova})
  
  output$rsquared <- renderText({
      r.squared <- summary(lpp2)$r.squared})
 
  output$plot1 <- renderPlot({
      #plot actual vs predicted 
      plot1 <- plot(lpp2$fitted.values,lpp_sample1$PO.price,
           xlab="Should be Price",ylab="Actual Price")
      abline(a=0,b=1,col = "blue")
      # print(plot1)
      })
  
  output$dynamic <- renderUI({
    req(input$plot_hover) 
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    hover <- input$plot_hover 
    y <- nearPoints(lpp_sample1$PO.price,input$plot_hover)
    req(nrow(y) != 0)
    y
  })
}

rm(lpp_sample1)
shinyApp(ui, server)
runApp(host = "10.2.198.193",port = 80)