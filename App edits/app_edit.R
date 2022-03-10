#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
devtools::install_github("ramnathv/rCharts")3
library(shiny)
library(RODBC)
library(tidyverse)
library(ggplot2)
library(rCharts)
library(shinycssloaders)
library(DT)
library(writexl)
library(shinythemes)
library(haven)
#library(gganimate)

con <- odbcConnect("Summaryindicators")
databases <- sqlQuery(con,"SELECT name 
                      FROM master.sys.databases
                      where name not in ('master','tempdb','model','msdb','Marriage','GHS')")


Makore <- c(1992,1993,1994,1995,1996,1997, 1998, 1999,2000,2001, 2002, 2003,2004, 2005, 2006, 2007, 2008, 2009, 2010,2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,2020)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
  
titlePanel(img(src="logo.jpg",height=50,width=1000)),
#shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel(p("This application is used for internal data probing for data and analytics team and also used for producing reports for the Community Engangement team", 
                   a("MRC/Wits Rural Public Health and Health Transitions Research Unit (Agincourt). ", 
                     href = "https://www.agincourt.co.za/")),
      # Add a slider selector for years to filter
      selectInput("year", "Choose database to explore", choices = databases, multiple = FALSE, selected = "RedCross"), p("Data come from ", 
                                                                                          a("MRC/Wits Rural Public Health and Health Transitions Research Unit (Agincourt) Database. ", 
                                                                                            href = "https://www.agincourt.co.za/"))
    
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tables", selectInput("indicator", "Indicator"
                         , choices = c("Total population by Year","Population by Age Group","Total Households by Year", "Total Fertility Rate by Year", "Fertility by Age Group", "Adult mortality rates","Child mortality rates","Infant Mortality Rates","Life expectancy at Birth","Unemployment by Year","Unemployment by Agegroup")
                         , multiple = FALSE, selected = 1), DT::dataTableOutput("table1"),downloadButton('download',"Download the data")),
        tabPanel("Visualisations", selectInput("viz", "Indicator"
                                               ,choices = c("Population pyramid","Top 10 causes of death", "Life expectancy", "Adult mortality", "Child mortality", "Infant mortality")
                                               , multiple = FALSE, selected = 1),uiOutput("photo1")),
        tabPanel("Reports",
                 downloadButton("report1", "Generate English Main household factsheet"),
                 downloadButton("report2", "Generate English Education factsheet"),
                 downloadButton("report3", "Generate English Water factsheet"), 
                 downloadButton("report4", "Generate English Household factsheet"),
                 downloadButton("report5", "Generate Shangaan Education factsheet"),
                 downloadButton("report6", "Generate Shangaan Water factsheet"),
                 downloadButton("report7", "Generate Shangaan Household factsheet"),
                 downloadButton("report8", "Generate Cause of death factsheet")),
        tabPanel("Covid-19")

      )
      
    )
  )
)

# Define the server logic
server <- function(input, output) {
   pop1 <- reactive({
    
     pop$AgeGroup <- reorder(pop$AgeGroup,pop$Ordered)
     
      if(input$villno > 20 & input$villno < 35){
    
     pop %>%
       filter(VillNo== input$villno, Years > 2012) %>%
       #rename(Male = MalePop, Female = FemalePop) %>%
       gather(key = Gender, value = Population, -c(Years, VillNo, PoliticalVillage, Ordered,AgeGroup, MidYearPop)) %>%
       mutate(Population = replace(Population,Gender=="Male",-Population))  %>%
      # mutate(abs_pop = abs(Population)) %>%
       arrange(Ordered)
      } else {
        pop %>%
          filter(VillNo== input$villno) %>%
         # rename(Male = MalePop, Female = FemalePop) %>%
          gather(key = Gender, value = Population, -c(Years, VillNo, PoliticalVillage, Ordered,AgeGroup, MidYearPop)) %>%
          mutate(Population = replace(Population,Gender=="Male",-Population))  %>%
         # mutate(abs_pop = abs(Population)) %>%
          arrange(Ordered) 
      }
   })
   
   watersupply <- reactive({
     if(input$villno > 20 & input$villno < 35){
     sqlQuery(con,"select * from cfb..WaterSupply") %>%
         filter(VillNo== input$villno, Year > 2012) 
       }  else {
         sqlQuery(con,"select * from cfb..WaterSupply") %>%
           filter(VillNo== input$villno) 
       }
   })
     
   education <- reactive({
     if(input$villno > 20 & input$villno < 35){
        sqlQuery(con,"select * from cfb..CurrentEdu") %>%
         filter( VillNo == input$villno, Years > 2012) %>%
         select(Years, Age, PreSchool, Grade1to4, Grade5to7, Grade8to12, Incomplete_Tertiary, Complete_Tertiary, Total_Pop) %>%
         group_by(Years,Age) %>%
         mutate(None = (Total_Pop-PreSchool-Grade1to4-Grade5to7-Grade8to12-Incomplete_Tertiary-Complete_Tertiary)/Total_Pop*100, PreSchool = PreSchool/Total_Pop* 100, Grade1to4 = Grade1to4/Total_Pop* 100
                ,Grade5to7 = Grade5to7/Total_Pop* 100, Grade8to12 = Grade8to12/Total_Pop* 100
                ,Incomplete_Tertiary = Incomplete_Tertiary/Total_Pop* 100, Complete_Tertiary = Complete_Tertiary/Total_Pop* 100
                )
       
      
     }  else {
       sqlQuery(con,"select * from cfb..CurrentEdu") %>%
         filter(VillNo== input$villno)  %>%
         select(Years, Age, PreSchool, Grade1to4, Grade5to7, Grade8to12, Incomplete_Tertiary, Complete_Tertiary, Total_Pop) %>%
         group_by(Years,Age) %>%
         mutate(None = (Total_Pop-PreSchool-Grade1to4-Grade5to7-Grade8to12-Incomplete_Tertiary-Complete_Tertiary)/Total_Pop*100,PreSchool = PreSchool/Total_Pop* 100, Grade1to4 = Grade1to4/Total_Pop* 100
                ,Grade5to7 = Grade5to7/Total_Pop* 100, Grade8to12 = Grade8to12/Total_Pop* 100
                ,Incomplete_Tertiary = Incomplete_Tertiary/Total_Pop* 100, Complete_Tertiary = Complete_Tertiary/Total_Pop* 100)
       
     
      
    
     }
   })
   
      table_a <-reactive ({
        if (input$indicator == "Population by Age Group") {
     pop %>%
       filter(Years == input$year, VillNo == input$villno) %>%
       arrange(Ordered) %>%
       select(AgeGroup, MalePop,FemalePop) 
          
   }
   else if (input$indicator == "Fertility by Age Group")  {
     asfr %>%
       filter(Years == input$year, VillNo == input$villno) %>%
       select(AgeGroup, ASFR) %>%
       arrange(AgeGroup)
   }
   else if (input$indicator == "Total population by Year")  {
     tpop %>%
       filter(VillNo == input$villno) %>%
       arrange(Years) %>%
       select(Years, Male,Female, Population)
   }
        else if (input$indicator == "Total population by Year")  {
          tpop %>%
            filter(VillNo == input$villno) %>%
            arrange(Years) %>%
            select(Years, Male,Female, Population)
        }
        else if (input$indicator == "Life expectancy at Birth")  {
            mortality %>%
            dplyr::rename(Population = e0_mf, Male = e0_m, Female = e0_f) %>%
            select(year, Male, Female, Population)
        }
        else if (input$indicator == "Unemployment by Year")  {
          tunemp %>%
            filter(VillNo == input$villno) %>%
            arrange(Years) %>%
            select(Years, MaleUnemployed, FemaleUnemployed, MaleEmployed, FemaleEmployed, TotalEmployed, TotalUnemployed, LabourForce, Rate)
           
        }
        else if (input$indicator == "Unemployment by Agegroup")  {
          unemp %>%
            filter(Years == input$year, VillNo == input$villno) %>%
            select(AgeGroup, MaleUnemployed, FemaleUnemployed, MaleEmployed, FemaleEmployed, TotalEmployed, ToTalUnemployed, LabourForce, Rate) %>%
            arrange(AgeGroup)
        }
        # else if (input$indicator == "Life expectancy at birth")  {
        #   mortality %>%
        #     filter(Years == input$year, VillNo == input$villno) %>%
        #     select(AgeGroup, MaleUnemployed, FemaleUnemployed, MaleEmployed, FemaleEmployed, TotalEmployed, ToTalUnemployed, LabourForce, Rate) %>%
        #     arrange(AgeGroup)
        # }
   else {
     
   }
      })
      
       photo_c <- reactive ({
         if (input$viz == "Population pyramid") {
          "agincourt_sa_1.gif"
         }
         else if (input$viz == "Top 10 causes of death") {
           "cod_interactive.gif"
         }
       else {

       }
       })
      
      output$photo1 <- renderUI({
        tags$img(src = photo_c())
      })
      

    
 
  output$report1 <-  downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("Main factsheet",".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("C:/Users/User/Documents/CFB Reports/Report production", "mainfactsheet.Rmd")
      file.copy("mainfactsheet.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(year = input$year, village = input$villno)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$report2 <-  downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("Education factsheet",".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("C:/Users/User/Documents/CFB Reports/Report production", "educationenglish.Rmd")
      file.copy("educationenglish.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(year = input$year, village = input$villno)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
                        
                        
      )
    }
  )
  output$report3 <-  downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("Water factsheet",".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("C:/Users/User/Documents/CFB Reports/Report production", "waterenglish.Rmd")
      file.copy("waterenglish.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(year = input$year, village = input$villno)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$report4 <-  downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("household factsheet",".html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("C:/Users/User/Documents/CFB Reports/Report production", "householdenglish.Rmd")
      file.copy("householdenglish.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(year = input$year, village = input$villno)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$report5 <-  downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("Education factsheet",".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("C:/Users/User/Documents/CFB Reports/Report production","education_shangaan.Rmd")
      file.copy("education_shangaan.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(year = input$year, village = input$villno)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$report6 <-  downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("water shangaan factsheet",".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("C:/Users/User/Documents/CFB Reports/Report production", "watershangaan.Rmd")
      file.copy("watershangaan.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(year = input$year, village = input$villno)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$report7 <-  downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("household shangaan factsheet",".html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("C:/Users/User/Documents/CFB Reports/Report production", "household_shangaan.Rmd")
      file.copy("household_shangaan.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(year = input$year, village = input$villno)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$report8 <-  downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("Cause of death factsheet",".pdf"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("C:/Users/User/Documents/CFB Reports/Report production", "CauseofDeath.Rmd")
      file.copy("CauseofDeath.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(year = input$year)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  output$table1 <- DT::renderDataTable({
    datatable(table_a(), options = list("pagelength"=40))
  }
)
  
  output$download <- downloadHandler(
    filename = function(){"table.xlsx"}, 
    content = function(fname){
      write_xlsx(table_a(), fname)})
}




shinyApp(ui = ui, server = server)