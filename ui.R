
# Load packages ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinyjs, shinythemes, shinyalert, shinycssloaders)
pacman::p_load(dplyr, data.table, leaflet, DT, plyr)
pacman::p_load(MASS, rsconnect, readxl, tools, purrr, sp)

# Load default RData sets ------------------------------------------------------
load("RData/OrgRData/materialCode.RData")
load("RData/OrgRData/metCode.RData")
load("RData/OrgRData/ref.RData")
load("RData/OrgRData/spl.RData")
load("RData/OrgRData/age.RData") 
load("RData/OrgRData/value_ar.RData")
load("RData/OrgRData/value_cosmo.RData")
load("RData/OrgRData/value_kar.RData")
load("RData/OrgRData/value_luhf.RData")
load("RData/OrgRData/value_osl.RData")
load("RData/OrgRData/value_smnd.RData")
load("RData/OrgRData/value_upb.RData")

# Make user directories -----------------------
if(!file.exists("RData/UserFile")){
  dir.create("RData/UserFile")
  dir.create("RData/UserRData")
  dir.create("RData/Temp")
}

# Load user (custom) RData sets if exist --------------------------------------
if (file.exists("RData/UserRData/ref_custom.RData")) {
  load("RData/UserRData/ref_custom.RData")
  load("RData/UserRData/spl_custom.RData")
  load("RData/UserRData/age_custom.RData")
  # update RData DB for with custom database ----------------------------------
  if(nrow(ref_custom) == 0){ref <- ref}else if(nrow(ref_custom) != 0){ref <- rbind(ref, ref_custom)}
  if(nrow(spl_custom) == 0){
    spl <- spl
  }else if(nrow(spl_custom) != 0){
    spl <- rbind(spl, spl_custom)
  }
  if(nrow(age_custom) == 0){age <- age}else if(nrow(age_custom) != 0){age <- rbind(age, age_custom)}
}

spl <- join(spl, material_cd, by = "Material")

for(i in 1:nrow(spl)){
  if(grepl("Ig", spl[i,]$Material) == T){
    spl[i,]$material_num <- c(1)
  }else if(grepl("Sedi", spl[i,]$Material) == T){
    spl[i,]$material_num <- c(2)
  }else if(grepl("Meta", spl[i,]$Material) == T){
    spl[i,]$material_num <- c(3)
  }else if(grepl("Depo", spl[i,]$Material) == T){
    spl[i,]$material_num <- c(4)
  }else if(grepl("Clay", spl[i,]$Material) == T){
    spl[i,]$material_num <- c(5)
  }
}

age$Era <- gsub(" ", "", age$Era)
age$Period <- gsub(" ", "", age$Period)
age$Epoch <- gsub(" ", "", age$Epoch)

age$Era <- gsub("[[:space:]]", "", age$Era)
age$Period <- gsub("[[:space:]]", "", age$Period)
age$Epoch <- gsub("[[:space:]]", "", age$Epoch)

# Initialize age names for interactive user interface
chrono <- data.frame(age$Era, age$Period, age$Epoch)
chrono <- data.frame(unique(chrono))
names(chrono) <- c("Era", "Period", "Epoch")
chrono <- chrono[order(chrono$Era ),]

ui <- fluidPage(
  tags$head(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }")
  ),
  navbarPage(
    theme = shinytheme("flatly"), collapsible = TRUE, header = NULL, footer = NULL,
    title = "CHIP", 
    windowTitle = "CHIP: geoCHronology Interactive Personal database",
    ## Panel: About ---------
    tabPanel("About",
            includeHTML("RData/About.html")
      ),
    ## Panel: Search data ----------------------------------------------------------------
    navbarMenu("Search Data", 
      ## reference -----------------------------------------------------
      "Reference",
        tabPanel("Method",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "refmet", label = "Method",
                  choices = as.vector(met_cd$Method),
                  selected = "Ar-Ar"),
                downloadButton("DBrefmet", "Download")
              ),
              mainPanel(
                h3(HTML("<b>Reference Information</b>")),
                DTOutput("refmettbl"),
                h3(HTML("<b>Sample Information</b>")),
                DTOutput("subrefmettbl")
              )
            )
        ),
        tabPanel("Geological Age",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "refage", label = "Age",
                  choices = as.vector(unique(age$Era)),
                  selected = "Cenozoic"),
                downloadButton("DBrefage", "Download")
              ),
              mainPanel(
                h3(HTML("<b>Reference Information</b>")),
                DTOutput("refagetbl"),
                h3(HTML("<b>Sample Information</b>")),
                DTOutput("subrefagetbl")
              )
            )
        ),
      "----", 
      ## analysis value -----------------------------------------------------
      "Analysis Value",
        tabPanel("Method",
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  inputId = "sammet", label = "Method",
                  choices = as.vector(met_cd$Method),
                  selected = "Ar-Ar"),
                downloadButton("DBsplmet", "Download Sample"),
                br(), br(),
                downloadButton("DBsplmetvalue", "Download Value")
              ),
              mainPanel(
                h3(HTML("<b>Sample Information</b>")),
                DTOutput("splmettbl"),
                h3(HTML("<b>Analysis Value</b>")),
                DTOutput("subsplmettbl")
              )
            )
          ),
        tabPanel("Geological Age",
            tabPanel("Chronostratigraphy", hr(),
              sidebarLayout(
                sidebarPanel(
                  selectInput("splera", "Era", choices = unique(chrono$Era)),
                  selectInput("splperi", "Period", choices = NULL),
                  selectInput("splepo", "Epoch", choices = NULL),
                  downloadButton("DBsplage", "Download Sample"),
                  br(), br(),
                  downloadButton("DBsplagevalue", "Download Value")
                ),
                mainPanel(
                  h3(HTML("<b>Sample Information</b>")),
                  DTOutput("splagetbl"), br(), br(),
                  h3(HTML("<b>Analysis Value</b>")),
                  DTOutput("subsplagetbl")
                )
              )
            )
          ),
      "----",
      ## Map -----------------------------------------------------
      "Map",
        tabPanel("Map",
          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "mapmet",label = "Method: ",
                choices = c("ALL", as.vector(met_cd$Method)),
                selected = "ALL"),
              selectInput(
                inputId = "mapage",label = "Era: ",
                choices = c("ALL", as.vector(unique(age$Era))),
                selected = "ALL")),
            mainPanel(leafletOutput("mapping", height = 600))
          )
        )
    ), # end of Search Data
    ## Panel: Submit Data ----------------------------------------------------             
    navbarMenu("Upload",
      tabPanel("Guideline",
        sidebarLayout(
          sidebarPanel(
            tags$h4("Infomation Template"),
            downloadButton("DBinfo", "Information"),
            hr(),
            tags$h4("Value Template"),
            downloadButton("DBar", "Ar-Ar"),
            br(),br(),
            downloadButton("DBcosmo", "Cosmogenic1Be10"),
            br(),br(),
            downloadButton("DBkar", "K-Ar"),
            br(),br(),
            downloadButton("DBluhf", "Lu-Hf"),
            br(),br(),
            downloadButton("DBosl", "OSL"),
            br(),br(),
            downloadButton("DBsmnd", "Sm-Nd"),
            br(),br(),
            downloadButton("DBupb", "U-Pb")
          ),
          mainPanel("null") #htmlOutput("template"))
        )
      ),
      # Submission process ----------------------------------------------------
      tabPanel("Submit Data",
        navbarPage(title = "STEP", id = "tabs",
          # panel 1 -----------------------------------------------------------------
          tabPanel(title = "Step 1",
            sidebarLayout(
              sidebarPanel(
                h4(HTML("<b>Check Duplicates</b>"), style = "color:red"),
                useShinyjs(), br(),
                fileInput('dupliinfo', 'Choose Information xlsx file',
                          multiple = F, accept = c(".xlsx")),
                actionButton(inputId = "dupliNext", label = "Confirm and Next", width = "100%")
              ),
              mainPanel(
                h3(HTML("<b>Reference Data</b>")),
                DTOutput("dupliref"),
                hr(), br(), br(),
                h3(HTML("<b>Sample Data</b>")),
                DTOutput("duplispl"), 
                hr(), br(), br(),
                h3(HTML("<b>Age Data</b>")),
                DTOutput("dupliage")
              )
            )
          ),
          # Panel 2 --------------------------------------------------------------
          tabPanel(title = "Step 2",
            sidebarLayout(
              sidebarPanel(
                h4(HTML("<b>Upload Information File</b>"), style = "color:red"),
                br(),
                useShinyjs(),
                fileInput('fileinfo', 'Choose Information xlsx file',
                          multiple = F, accept = c(".xlsx")),
                actionButton(inputId = "info_temp", label = "Confirm and Next", width = "100%"),
                br(),br(),
                actionButton(inputId = "info_reset", label = "Cancel", width = "100%")
              ),
              mainPanel(
                h3(HTML("<b>Reference Data</b>")),
                DTOutput("refUpload"),
                hr(), br(), br(),
                h3(HTML("<b>Sample Data</b>")),
                DTOutput("splUpload"), 
                hr(),br(),br(),
                h3(HTML("<b>Age Data</b>")),
                DTOutput("ageUpload")
              )
            )
          ),
          # Panel 3 ------------------------------------------------------------------
          tabPanel(title = "Step 3",
            sidebarLayout(
              sidebarPanel(
                h4(HTML("<b>Upload Value Files</b>"), style = "color:red"),
                br(),
                useShinyjs(),
                fileInput("filevalue",'Choose Value xlsx file',
                          multiple = T, accept = c(".xlsx")),
                actionButton(inputId = "value_temp", label = "Confirm and Next", width = "100%"),
                br(),br(),
                actionButton(inputId = "value_reset", label = "Cancel", width = "100%")
              ),
              mainPanel(
                h3(HTML("<b>Value Data</b>")),
                br(),
                withSpinner(uiOutput("valueUpload")) # animation for loading
              )
            )
          ),
          # Panel 4 ------------------------------------------------------------------
          tabPanel(title = "Step 4",
            sidebarLayout(
              sidebarPanel(
                h4(HTML("<b>Submit Data</b>"), style = "color:red"),
                br(),
                useShinyjs(),
                actionButton(inputId = "submit_all", label = "Submit", width = "100%"),
                uiOutput("allSubmit")
              ),
              mainPanel(
                h3(HTML("<b>Reference Data</b>")),
                br(),
                DTOutput("AllrefUpload"),
                hr(), br(), br(),
                h3(HTML("<b>Sample Data</b>")),
                br(),
                DTOutput("AllsplUpload"),
                hr(), br(), br(),
                h3(HTML("<b>Age Data</b>")),
                br(),
                DTOutput("AllageUpload"),
                hr(), br(), br(),
                h3(HTML("<b>Value Data</b>")),
                br(),
                uiOutput("AllvalueUpload")
              )
            )
          )
        )
      )
    ),
    ## Panel: Board ----------------------------------------------------   
    navbarMenu("Board",
      tabPanel("FAQ"),
      tabPanel("Contact",
        mainPanel(
               h3("Name: Su-Min,Yang"),
               h3("Contact: +82-42-821-7567"),
               h3("E-Mail: sumineee@o.cnu.ac.kr")
        )
      )
    )
  )
)