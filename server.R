shinyServer(function(input, output) 
{
# ------------------------------------------------------------- #    
nav_bar_df <- reactive(load_nav_bar_menu())
# ------------------------------------------------------------- #
# NAV BAR Buttons ######
# ------------------------------------------------------------- #
output$firstButton <- renderUI(
{
    actionBttn(inputId = nav_bar_df()$college_short_name[1]
               , label = nav_bar_df()$college_long_name [1]
               , size = buttonSize
               , icon = NULL
               , style= buttonStyle
               #, block = T 
               )
})
# ------------------------------------------------------------- #
output$secondButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[2]
                   , label = nav_bar_df()$college_long_name [2]
                   , size = buttonSize
                   , icon = NULL
                   , style= buttonStyle
                   #, block = T 
                   )
})
# ------------------------------------------------------------- #
output$thirdButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[3]
                   , label = nav_bar_df()$college_long_name [3]
                   , size = buttonSize
                   , icon = NULL
                   , style= buttonStyle
                   #, block = T 
                   )
})
# ------------------------------------------------------------- #
output$fourthButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[4]
                   , label = nav_bar_df()$college_long_name [4]
                   , size = buttonSize
                   , icon = NULL
                   , style= buttonStyle
                   #, block = T 
                    )
})


# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# inital load page ######
output$body <- renderUI({ fluidPage("Summary", summary_moduleUI("summary_interface")) })
output$left_side_nav_buttons <- renderUI({   })
# ------------------------------------------------------------- #
# COSE ####
# ------------------------------------------------------------- #
observeEvent(input$COSE,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COSE")
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
    # output$body <- renderUI({ fluidPage("Main", main_module_UI("main_interface")) })
    output$left_side_nav_buttons <- renderUI(
        { 
        tagList(
                actionBttn(inputId = "add_strategy"
                   , label = "Add Strategy"
                   , size = buttonSize
                   , icon = NULL
                   , style= buttonStyle
                   , block = T))
        })
    output$body <- renderUI(
        {
            #___BODY - Speciality_Overview ####
            tabBox(
                id = paste0(nav_bar_item$college_short_name,"_tab")
                , width = 12 
                , title = nav_bar_item$college_long_name
                #, headerBorder = T
                # ------------------------------------------------------------------------ #git checkout 
                # ------------------------------------------------------------------------ #
                , bs4TabPanel(
                    tabName = paste0("Strategy - ", nav_bar_item$college_short_name)
                    , active = TRUE
                    , "DMS Item Codes"
                    , "Summary"
                       #  , summary_moduleUI("summary_interface")
                ), 
                # ------------------------------------------------------------------------ #
                bs4TabPanel(
                    tabName = paste0("Initiative - ", nav_bar_item$college_short_name)
                    , active = F
                    , "DMS Item Codes"
                    , "Demographics"
                       #  , demographics_moduleUI("demographics_interface")
                ), 
                bs4TabPanel(
                    tabName = paste0("Milestone - ", nav_bar_item$college_short_name)
                    , active = F
                    , "DMS Item Codes"
                        # , dms_item_codes_moduleUI("dms_item_codes_interface")
                ),
                # ------------------------------------------------------------------------ #
                bs4TabPanel(
                    tabName = paste0("Spare - ", nav_bar_item$college_short_name)
                    , active = F
                    , "DMS Item Codes"
                    , "Provider Group Profile"
                        # , pgp_moduleUI("pgp_interface")
                )
                # ------------------------------------------------------------------------ #
                # ------------------------------------------------------------------------ #
            ) # end tabBox
        }) # END renderUI
})
# ------------------------------------------------------------- #
# COBE ####
# ------------------------------------------------------------- #
observeEvent(input$COBE,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COBE")
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
})
# ------------------------------------------------------------- #
# CALE ####
# ------------------------------------------------------------- #
observeEvent(input$CALE,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "CALE")
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
})
# ------------------------------------------------------------- #
# COHM ####
# ------------------------------------------------------------- #
observeEvent(input$COHM,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COHM")
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
})
# ------------------------------------------------------------------------ #
observeEvent(input$add_stratergy,
             {
                 showModal(strategy_load())
             })
# ------------------------------------------------------------------------ #
# ------------------------------------------------------------------------ #
# MODULES ###################################
# ------------------------------------------------------------------------ #
# ------------------------------------------------------------------------ #
# summary_module ####
# ------------------------------------------------------------------------ #
callModule(summary_module
           , "summary_interface"
           , nav_bar_df = reactive(nav_bar_df()))
# ------------------------------------------------------------------------ #
# ------------------------------------------------------------------------ #
callModule(main_module
           , "main_interface"
           , nav_bar_df = reactive(nav_bar_df()))
# ------------------------------------------------------------------------ #
# END MODULES ###################################
# ------------------------------------------------------------------------ #
}) # END SERVER
# ------------------------------------------------------------------------ #
