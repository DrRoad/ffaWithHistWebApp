# P:\HRRD\FRF_Group\FEH Local\StatModel\develop
library(shiny)



# Define UI for random distribution application 
#shinyUI(pageWithSidebar(
shinyUI(fixedPage(
      
      headerPanel("Combining historical flood data with systematic records"),
      
        fixedRow(
        column(12,
               h5(includeMarkdown("introText.md"), style = "line-height: 1.4"),
               fixedRow(
                 column(5,
                        strong("Systematic data information"),
                                       # textInput("Station", h5("NRFA Gauging station number:"),value="39001"),
                                       fileInput("amaxFile", h5("Load a .AM file")),
                                       textInput("addAMAX", h5("or give a series of AMAX values:"), value="10, 12, 18, 13, 12, 17"),
                                       radioButtons("dist", h5("Distribution type:"), c("GLO" = "glo","GEV" = "gev"))),
                  column(7,strong("Historical peak flows information"),
                                       textInput("hdata", h5("Add historical peak flow information:"),value="NULL"),
                                       textInput("h", h5("give the length of period length:"),value="give h"),
                                       textInput("X0", h5("and the perception threshold:"),value="give X0"),
                                       radioButtons("binCens", "Indicate the type of historical data:", c("Peak values" = "FALSE","Information that the X0 threshold is exceeded" = "TRUE"))), 
                
                submitButton("Submit"), p()),

               tabsetPanel(
                      tabPanel("Floofreq Plot", plotOutput("ffaplot")),
                      # tabPanel("Variance ratio Plot",plotOutput("varplot")),
                      tabPanel("AMAX Plot", plotOutput("dataplot")),
                      tabPanel("Estimated pars", verbatimTextOutput("summary"))
                      ),
               
               h5(includeMarkdown("footnote.md"), style = "line-height: 1.4") 
        )
      )
               )
    
    )




  # 
  # 
  # titlePanel("Combining flood historical data with systematic records"),
  # # headerPanel("Effects of adding historical data"),
  # 
  # 
  # sidebarLayout(
  #   sidebarPanel("Station Information",
  #                textInput("Station", "Gauging station number:",value="39001"),
  #                textInput("addAMAX", "or give a series of AMAX values:",value="100,12, 18, 163, 12, 17"),
  #                radioButtons("dist", "Distribution type:", c("GLO" = "glo","GEV" = "gev")), 
  #                "Give information the historical peak flows",
  #                textInput("hdata", "Add historical peak flow information:",value="NULL"),
  #                textInput("h", "give the length of period length:",value="give h"),
  #                textInput("X0", "and the perception threshold:",value="give X0"),
  #                radioButtons("binCens", "Indicate the type of historical data:", c("Peak values" = "FALSE","Information that the X0 threshold is exceeded" = "TRUE"))),
  #   
  #   
  #   # c("Nitrate dioxide" = "NO2",
  #   #   "Pm 10" = "PM10"))),
  #   
  #   mainPanel(
  #     tabsetPanel(
  #       tabPanel("AMAX Plot", plotOutput("dataplot")), 
  #       tabPanel("Floofreq Plot", plotOutput("ffaplot")), 
  #       tabPanel("Variance ratio Plot", plotOutput("varplot")), 
  #       tabPanel("Estimated pars", verbatimTextOutput("summary")))
  #   )
  # )
