# Kan Luo    ST558 Project3   7/18/2020

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(knitr)
library(plyr)
library(DT)
library(readxl)
library(httr)

#Readin data from my Github url address
url1<-'https://github.com/luokan1227/Project3/raw/master/antibody.xlsx'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
antibody <- read_excel(tf)

dashboardPage(
    
    dashboardHeader(title = "Ig Classification"),
    
    #define sidebar items.
    dashboardSidebar(sidebarMenu(
            menuItem("Data Information", tabName = "information", icon=icon("refresh")),
            menuItem("Data Exploration", tabName = "exploration", icon=icon("bar-chart-o")),
            menuItem("PCA", tabName = "analysis", icon = icon("list-alt")),
            menuItem("Modeling", tabName = "modeling", icon = icon("dashboard")),
            menuItem("Data View / Download", tabName = "data", icon = icon("table"))
        )),
    dashboardBody(
        tabItems(
            #first tab content : information
            tabItem(tabName = "information",
                    fluidRow(
                        
                        #Add in latex functionality
                        withMathJax(),
                        #information about the data
                        column(6,
                               h1("Data background"),
                               box(background = "red", width = 12,
                                   h4("The antibody data used in this app came from a non-human primate study mimicked RV144 HIV clinic trial, which adopted ALVAC prime/ALVAC + AIDSVAX B/E boost and induced an estimated 31% of long term protection. More information about RV144 trial can be found through following link:"),
                                   h4(uiOutput("tab")),
                                   h4("In this study, we found ", strong("human and rhesus share the same glutamic acid - aspartic acid (ED) motif in response to HIV vaccine,"), "shown the conserve of antibody evolution between human and rhesus."),
                                   h4("For this antibody data set,", em("PTID"), "indicates the rhesus subject ID, ", em("Sample"), "represents the tissue where antibodies been isolated. ", em("Isotype")," means this antibody is an IgG, or IgA, ect.. ", em("HV, HD, HJ, LD, LJ"), "are the germline useage information of that antibody. Heavy and light chain mutation rate is calculated based on following equation:"),
                                   h4("$$MutationRate = \\frac{Mutation + Inseration + Deletion}{Total Base}$$"),
                                   h4("Lastly, the information about HIV envelope reactivity and clone lineage is listed by ", em("Reactivity"), "and", em("Clone"), "variables, where 1 means env-positive/clone belonged and 0 means env-negative/not clone belonged. "),
                                   h4("We will interest in if a antibody can bind HIV envelope, and if it belongs to a clone lineage. The first aspect implies how does vaccine work, tells if the vaccine can induce specific antibodies, and it's also the first step of generate protective neutralizing antibody. The second aspect shows if that antibody has successfully expanded or not. It also reflect the stimulation of vaccine works in germline center."))
                               ),
                        #information about the app
                        column(6,
                               h1("Introduce APP"),
                               box(background = "blue", width = 12,
                                   h4(strong("Data Exploration Tab"),": You can select PTID to see antibody number table, Ig isotype plot and HCDR3 vs H_mutation rate plot by animal subjects. You can also select", em("By sample tissue type"), "box and", em("By HIV envelope reactivity"), "box for further analysis. Plots can be downloaded by the button on the plot, the dataset used for creating table/plots can be downloaded by ", em("Download Data Set"), "button. "),
                                   h4(strong("PCA Tab"), ": You can choose which animal you want to look at by selecting PTID. Plot of the proportion of 4 variables that can explain the variability of the data set is shown by PTID. You can select 2 of 4 variables to see the biplot of principal components. Plots can be downloaded by the button under the plot"),
                                   h4(strong("Modeling Tab"), ": In this tab, you can select the value of 4 variables and use them to predict the antibody feature. One model can be used to predict the antibody is clone lineage related or not, another model can predict the antibody is an HIV-antibody or not."),
                                   h4(strong("Data View/Download"),": You can view and download whole antibody data set used in this app.")))
                    )),  #end of first tab content
            
            #Second tab content : exploration
            tabItem(tabName = "exploration",
                    fluidRow(
                        #side column
                        column(3,
                               #UserenderUI/uiOutput to dynamic change subtitle
                               h3("Select a PTID"),
                               selectizeInput("ptid", "PTID", selected = "22-11", choices = levels(as.factor(antibody$PTID))),
                               checkboxInput("sample", h4("By sample tissue type")),
                               #Use conditionalPanel to dynamic create checkbox.
                               conditionalPanel(
                                   condition = "input.sample >0", checkboxInput("env", h4("By HIV envelope reactivity"))
                                   ),  #close conditionalPanel
                               #Download button
                               downloadButton("downloadData", "Download Data Set")
                               ), #close side column
                        #plot column
                        column(9,
                               tabsetPanel(
                                   # 1st sub-tabpanel
                                   tabPanel(uiOutput("subt1"),
                                            fluidRow(
                                                column(12,
                                                       tableOutput("abnumber"))
                                            )),  #close the 1st sub-tabPanel
                                   # 2nd sub-tabpanel
                                   tabPanel(uiOutput("subt2"),
                                            fluidRow(
                                                column(12,
                                                       plotlyOutput("isotype"))
                                            )),  #close the 2nd sub-tabPanel
                                   # 3rd sub-tabpanel
                                   tabPanel(uiOutput("subt3"),
                                            fluidRow(
                                                column(12,
                                                       plotlyOutput("hcdr3mut"))
                                            ))  #close the 3rd sub-tabpanel
                               )) #close plot column
                    )),  #close second tab content
            
            
            #Third tab content : principal components analysis
            tabItem(tabName = "analysis",
                    fluidRow(
                        #side column
                        column(3,
                               #UserenderUI/uiOutput to dynamic change subtitle
                               h3("Select a PTID"),
                               selectizeInput("ptid2", "PTID", selected = "22-11", choices = levels(as.factor(antibody$PTID))),
                               #Download button
                               downloadButton("downloadData2", "Download Data Set"),
                               br(),
                               br(),
                               h3("Select 2 of the variables to specify biplot algorithm"),
                               checkboxInput("hv", h5("PC1: Heavy_V"), value = TRUE),
                               checkboxInput("hcdr3", h5("PC2: HCDR3"), value = TRUE),
                               checkboxInput("hmuated", h5("PC3: Heavy_Mutation")),
                               checkboxInput("lmutated", h5("PC4: Light_Mutation"))
                               
                        ), #close side column
                        #plot column
                        column(9,
                               tabsetPanel(
                                   # 1st sub-tabpanel
                                   tabPanel(uiOutput("subt4"),
                                            fluidRow(
                                                column(12,
                                                       plotOutput("pov"),
                                                       downloadButton("povdownload", "Download Plot"),
                                                       tableOutput("povtable"),
                                                       textOutput("povexp"))
                                            )),  #close the 1st sub-tabPanel
                                   tabPanel(uiOutput("subt5"),
                                            fluidRow(
                                                column(12,
                                                       plotOutput("bip"),
                                                       downloadButton("bipdownload", "Download Plot"),)
                                            )
                                   )
                               )) #close plot column
                    )),  #close second tab content
            
            
            #Fourth tab content : modeling
            tabItem(tabName = "modeling",
                    fluidRow(
                        h3("Select antibody heavy and light chain mutation rate, heavy chain germline number, and HCDR3 length to predict an antibody's character "),
                        h5("Click plot below to select Heavy chain mutation rate (x-axis) and Light chain mutation rate (y-axis)"),
                        #Output H/L mutation plot for click
                        plotOutput("mutclick", click = "plot_click"),
                        #Show value of clicked point
                        verbatimTextOutput("info"),
                        column(3,
                               #Select model
                               selectizeInput("choosemodel", "Choose Model", 
                                              selected = "Clone Model", choices = c("Clone Model", "Env Reactivity Model")),
                               #slider select HV
                               sliderInput("heavyv", "Select Heavy chain germline", 
                                           min = 1, max = 7, value = 4, step = 1),
                               #slider select HCDR3 length
                               sliderInput("modelhcdr3", "Select HCDR3 Length", 
                                           min = 1, max = 50, value = 25, step = 1)
                               
                               ),
                        column(9,
                               h4("Model coefficients: "),
                               br(),
                               tableOutput("coef"),
                               br(),
                               br(),
                               h4("The prediction result is: "),
                               br(),
                               #Output the predict result
                               textOutput("predictresult")
                               
                               )
                    )), #Close the 4th tab content
            
            #Fifth tab content: data
            tabItem(tabName = "data",
                    fluidRow(
                        column(2,
                               h3("All antibody data available here"),
                               #Download button
                               downloadButton("downloadData3", "Download Data Set")
                        ),
                        column(10,
                               box(title = "All antibody data set", width = NULL, status = "primary", div(style = 'overflow-x: scroll', DT::dataTableOutput('table3'))))
                    ))  #Close the 5th tab content
        )
    )
)
