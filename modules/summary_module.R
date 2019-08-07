# ---------------------------------------------------------- #
# UI function ######
# ---------------------------------------------------------- #
summary_moduleUI <- function(id) 
{
  ns <- NS(id)
  tagList(
     #verbatimTextOutput(ns("dfTest")),
    fluidRow(
         column(3, uiOutput(ns("cobeBox")))
       , column(3, uiOutput(ns("caleBox")))
       , column(3, uiOutput(ns("coseBox")))
       , column(3, uiOutput(ns("cohmBox"))))# end fluid
    , fluidRow(
       column(3, bs4ValueBoxOutput(ns("cobeValueGreen"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("caleValueGreen"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("coseValueGreen"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("cohmValueGreen"), width = 12)))# end fluid
    , fluidRow(
       column(3, bs4ValueBoxOutput(ns("cobeValueYellow"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("caleValueYellow"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("coseValueYellow"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("cohmValueYellow"), width = 12)))# end fluid
    , fluidRow(
        column(3, bs4ValueBoxOutput(ns("cobeValueRed"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("caleValueRed"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("coseValueRed"), width = 12))
      , column(3, bs4ValueBoxOutput(ns("cohmValueRed"), width = 12)))# end fluid
    #, verbatimTextOutput(ns("specialityCode"))
    # ---------------------------------------------------------- #
    ) #end tagList
} # end summary_moduleUI
# ---------------------------------------------------------- #
# SERVER function ----
# ---------------------------------------------------------- #
summary_module <- function(input, output, session, nav_bar_df) 
{
  # ---------------------------------------------------------- #
  ns <- session$ns
  # ---------------------------------------------------------- #
  # -----------------------------------------------------------------------#
  nav_bar_DF <- reactive(nav_bar_df())
  
  # PRINT TEST Function ----
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  #output$dfTest <- renderPrint({ print(str(BoxOutputValues())) })
  #output$testDFF <- renderUI({ verbatimTextOutput('dfTest') })
  # ------------------------------------------------------------------------#
  output$cohmValueGreen <- renderbs4ValueBox({
                                bs4ValueBox(
                                  value = 150
                                , subtitle = paste0(nav_bar_DF()$college_short_name[4],  " - On Track")
                                , status = "success"
                                , icon = "trophy"
                                , elevation = 5
                                , href = "#")
                              })
  # ------------------------------------------------------------------------#c
  output$coseValueGreen <- renderbs4ValueBox({
                                bs4ValueBox(
                                  value = 150
                                , subtitle = paste0(nav_bar_DF()$college_short_name[3],  " - On Track")
                                , status = "success"
                                , icon = "trophy"
                                , elevation = 5
                                , href = "#")
                              })
  # ------------------------------------------------------------------------#
  output$caleValueGreen <- renderbs4ValueBox({
                                bs4ValueBox(
                                  value = 150
                                , subtitle = paste0(nav_bar_DF()$college_short_name[2],  " - On Track")
                                , status = "success"
                                , icon = "trophy"
                                , elevation = 5
                                , href = "#")
                              })
  # ------------------------------------------------------------------------#
  output$cobeValueGreen <- renderbs4ValueBox({
                              bs4ValueBox(
                                  value = 150
                                , subtitle = paste0(nav_bar_DF()$college_short_name[1],  " - On Track")
                                , status = "success"
                                , icon = "trophy"
                                , elevation = 5
                                , href = "#")
                            })
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  output$cohmValueYellow <- renderbs4ValueBox({
                                bs4ValueBox(
                                  value = 150
                                  , subtitle = paste0(nav_bar_DF()$college_short_name[4],  " - Issues")
                                  , status = "warning"
                                  , icon = "exclamation-triangle"
                                  , elevation = 5
                                  , href = "#")
  })
  # ------------------------------------------------------------------------#c
  output$coseValueYellow <- renderbs4ValueBox({
                                bs4ValueBox(
                                  value = 150
                                  , subtitle = paste0(nav_bar_DF()$college_short_name[3],  " - Issues")
                                  , status = "warning"
                                  , icon = "exclamation-triangle"
                                  , elevation = 5
                                  , href = "#")
  })
  # ------------------------------------------------------------------------#
  output$caleValueYellow <- renderbs4ValueBox({
                                bs4ValueBox(
                                  value = 150
                                  , subtitle = paste0(nav_bar_DF()$college_short_name[2],  " - Issues")
                                  , status = "warning"
                                  , icon = "exclamation-triangle"
                                  , elevation = 5
                                  , href = "#")
  })
  # ------------------------------------------------------------------------#
  output$cobeValueYellow <- renderbs4ValueBox({
                                  bs4ValueBox(
                                    value = 150
                                    , subtitle = paste0(nav_bar_DF()$college_short_name[1],  " - Issues")
                                    , status = "warning"
                                    , icon = "exclamation-triangle"
                                    , elevation = 5
                                    , href = "#")
  })
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  output$cohmValueRed <- renderbs4ValueBox({
                              bs4ValueBox(
                                value = 150
                                , subtitle = paste0(nav_bar_DF()$college_short_name[4],  " - At Risk")
                                , status = "danger"
                                , icon = "bomb"
                                , elevation = 5
                                , href = "#")
  })
  # ------------------------------------------------------------------------#c
  output$coseValueRed <- renderbs4ValueBox({
                              bs4ValueBox(
                                value = 150
                                , subtitle = paste0(nav_bar_DF()$college_short_name[3],  " - At Risk")
                                , status = "danger"
                                , icon = "bomb"
                                , elevation = 5
                                , href = "#")
  })
  # ------------------------------------------------------------------------#
  output$caleValueRed <- renderbs4ValueBox({
                              bs4ValueBox(
                                value = 150
                                , subtitle = paste0(nav_bar_DF()$college_short_name[2],  " - At Risk")
                                , status = "danger"
                                , icon = "bomb"
                                , elevation = 5
                                , href = "#")
  })
  # ------------------------------------------------------------------------#
  output$cobeValueRed <- renderbs4ValueBox({
    bs4ValueBox(
      value = 150
      , subtitle = paste0(nav_bar_DF()$college_short_name[1],  " - At Risk")
      , status = "danger"
      , icon = "bomb"
      , elevation = 5
      , href = "#")
  })
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  output$cobeBox <- renderUI(
    {
      bs4InfoBox(
        title = nav_bar_DF()$college_long_name[1]
       # , gradientColor = "grey"
        , status = "secondary"
        , value = nav_bar_DF()$college_short_name[1] 
        , icon = "bookmark"
        , width = 12
       , elevation = 5)
    })
  # ------------------------------------------------------------------------#
  output$caleBox <- renderUI(
    {
      bs4InfoBox(
        title = nav_bar_DF()$college_long_name[2]
        # , gradientColor = "grey"
        , status = "secondary"
        , value = nav_bar_DF()$college_short_name[2] 
        , icon = "bookmark"
        , width = 12
        , elevation = 5)
    })
  # ------------------------------------------------------------------------#
  output$coseBox <- renderUI(
    {
      bs4InfoBox(
        title = nav_bar_DF()$college_long_name[3]
        # , gradientColor = "grey"
        , status = "secondary"
        , value = nav_bar_DF()$college_short_name[3] 
        , icon = "bookmark"
        , width = 12
        , elevation = 5)
    })
  # ------------------------------------------------------------------------#
  output$cohmBox <- renderUI(
    {
      bs4InfoBox(
        title = nav_bar_DF()$college_long_name[4]
        # , gradientColor = "grey"
        , status = "secondary"
        , value = nav_bar_DF()$college_short_name[4] 
        , icon = "bookmark"
        , width = 12
        , elevation = 5)
    })
  # ------------------------------------------------------------------------------ #
  # END SERVER MODULE ------------------------------------------------------#
  # ------------------------------------------------------------------------------ #
} 
