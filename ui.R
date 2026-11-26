library(shinyFiles)
library(shiny)
library(shinydashboard)
library(DT)
library(shinybusy)
#ccolor
indic=1
skin_choice=c("blue","green","blue","yellow","red", "black", "purple")
status_choice=c("primary","success", "info", "warning", "danger")
skin=skin_choice[indic]
status= status_choice[indic]
#option
options(shiny.maxRequestSize = 1e6 * 1024^2)
root_dir<-"/home/admincyto/ShinyAppData/"

dashboardPage(
  
  skin = skin,
  dashboardHeader(title = 'AutoSpectral'),
  dashboardSidebar(
    sidebarMenu(
      id = "tab",
      menuItem(" Upload FCS", tabName = "upload", icon = icon("upload")),
      menuItem(" Parameters", tabName = "parameters", icon = icon("scissors")),
      menuItem(" Control preparation", tabName = "preparation", icon = icon("vials")),
      menuItem(" Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    
    fluidRow(
      uiOutput("progressBoxes")
    ),
    
    tabItems(
      
## ================= UPLOAD TAB =================

tabItem(
  tabName = "upload",
  fluidRow(
  box(
    width = 12,
    title = p(
      "Choose files to upload"
    ),
    status = status,
    solidHeader = TRUE,
    collapsible = TRUE,
    
    box(
      width = 12,
      HTML("<h4> <span style='background-color:#fdfd96;'> STEP 1 :</span> Select the Cytometer </h4>"),
      fluidRow(
        column(4, selectInput("machine", "Machine used for the acquisition",choices=c("Aurora"="aurora","S8"="s8"),selected="aurora"))

      )
    ),
    
    box(
      width = 12,
      HTML("<h4><span style='background-color:#fdfd96;'>STEP 2 :</span> Upload Files</h4>"),
      fluidRow(
        column(6,fileInput("control","Upload controls",multiple=TRUE)),
        column(6, fileInput("raw", "Upload raw data",multiple = TRUE))
      ),
      div(style = "text-align:center;", uiOutput("raw_list"))
    ),
    
    
  ))
),

## ================= PARAMETERS TAB =================================================================================

tabItem(
  tabName = "parameters",
  fluidRow(
  box(
    width = 12,

    title =p( "Modify the parameters for your analysis"),
    status = status,
    solidHeader = TRUE,
    collapsible = TRUE,
    
    box(
      width = 12,
      HTML("<h4><span style='background-color:#fdfd96;'>STEP 1 :</span> Autofluorescence parameters</h4>"),
      selectInput("negative_beads", "Choose no fluorescent beads (default: billes (Beads).fcs)", choices = NULL),
      selectInput("negative_cells", "Choose no fluorescent cells", choices = NULL)
    ),

    
    uiOutput("table_ui")

  ))
  
),

## ================= CONTROL PREPARATION TAB =================
tabItem(
  tabName = "preparation",
  fluidRow(
  box(
    width = 12,
    title = p(icon("vials"), " Control preparation"),
    status = status,
    solidHeader = TRUE,
    collapsible = TRUE,

    box(
      width = 4,
      height= 150,
      uiOutput("prep_summary_ui")
    ),

    box(
      width = 4,
      height= 150,
      actionButton("prepare", "Prepare controls"),
      textOutput("warning"),
      uiOutput("error")
    ),

    box(
      width = 4,
      height= 150,
      div(style = "margin-bottom:25px;", uiOutput("download_matrix_ui")),
      uiOutput("download_figures_ui")
    ),

    uiOutput("visual_ui"),

    # optional: closed until the user opens it
    box(
      width = 12,
      title = HTML("<h4><span style='background-color:#fdfd96;'>STEP 2 :</span> Manual control gate (optional)</h4>"),
      collapsible = TRUE,
      collapsed = TRUE,
      uiOutput("gate_ui")
    )

  ))

),

## ================= ANALYSIS TAB =================
tabItem(
  tabName = "analysis",
  fluidRow(
  box(
    width = 12,
    title = p(icon("chart-line"), " Analysis"),
    status = status,
    solidHeader = TRUE,
    collapsible = TRUE,

    box(
      width = 4,
      height= 180,
      selectInput("method",
                  p("Unmixing method", actionButton("HELP_METHOD", "", icon("question-circle"), class = "btn-xs")),
                  choices=c("AutoSpectral"="AutoSpectral","OLS"="OLS","WLS"="WLS","FastPoisson"="FastPoisson"),selected="AutoSpectral"),
      conditionalPanel(
        condition = "input.method == 'AutoSpectral'",
        selectInput("af_speed", "AutoSpectral speed", choices = c("fast", "medium", "slow"), selected = "medium")
      )
    ),

    box(
      width = 4,
      height= 180,
      actionButton("analysis", "Start"),
      textOutput("analysis_status"),
      uiOutput("unmix_error")
    ),

    box(
      width = 4,
      height= 180,
      uiOutput("download_unmixed_ui")
    ),

    uiOutput("gating_ui")

  ))
)
)
)
)
