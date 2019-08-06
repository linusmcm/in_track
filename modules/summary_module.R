# ---------------------------------------------------------- #
# UI function ######
# ---------------------------------------------------------- #
summary_moduleUI <- function(id) 
{
  ns <- NS(id)
  tagList(
    #verbatimTextOutput(ns("dfTest")),
    fluidRow(
      column(3, uiOutput(ns("testBox")))
    #, column(3, valueBoxOutput(ns("yearBox")))
    #, column(3, valueBoxOutput(ns("yearBox")))
    #, column(3, valueBoxOutput(ns("yearBox")))
      ), 
    fluidRow(
    )# end fluid
    #, verbatimTextOutput(ns("specialityCode"))
    # ---------------------------------------------------------- #
  ) #end tagList
} # end summary_moduleUI
# ---------------------------------------------------------- #
# SERVER function ----
# ---------------------------------------------------------- #
summary_module <- function(input, output, session) 
{
  # ---------------------------------------------------------- #
  ns <- session$ns
  # ---------------------------------------------------------- #
  # -----------------------------------------------------------------------#
  # PRINT TEST Function ----
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  #output$dfTest <- renderPrint({ print(str(BoxOutputValues())) })
  #output$testDFF <- renderUI({ verbatimTextOutput('dfTest') })
  # ------------------------------------------------------------------------#
  output$testBox <- renderUI(
    {
      bs4InfoBox(
        title = "Bookmarks",
        status = "info",
        value = 240,
        icon = "bookmark")
    })
  # ------------------------------------------------------------------------------ #
  # END SERVER MODULE ------------------------------------------------------#
  # ------------------------------------------------------------------------------ #
} 
