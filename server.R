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
               , block = T )
})
# ------------------------------------------------------------- #
output$secondButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[2]
                   , label = nav_bar_df()$college_long_name [2]
                   , size = buttonSize
                   , icon = NULL
                   , style= buttonStyle
                   , block = T )
})
# ------------------------------------------------------------- #
output$thirdButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[3]
                   , label = nav_bar_df()$college_long_name [3]
                   , size = buttonSize
                   , icon = NULL
                   , style= buttonStyle
                   , block = T )
    })
# ------------------------------------------------------------- #
output$fourthButton <- renderUI(
    {
        actionBttn(inputId = nav_bar_df()$college_short_name[4]
                   , label = nav_bar_df()$college_long_name [4]
                   , size = buttonSize
                   , icon = NULL
                   , style= buttonStyle
                   , block = T )
    })
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# inital load page ######
output$body <- renderUI({ fluidPage("Summary", summary_moduleUI("summary_interface")) })
# ------------------------------------------------------------- #
# COSE ####
# ------------------------------------------------------------- #
observeEvent(input$COSE,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COSE")
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
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
# ------------------------------------------------------------------------ #
# MODULES ###################################
# ------------------------------------------------------------------------ #
# ------------------------------------------------------------------------ #
# summary_module ####
# ------------------------------------------------------------------------ #
callModule(summary_module, "summary_interface")
# ------------------------------------------------------------------------ #
# END MODULES ###################################
# ------------------------------------------------------------------------ #
}) # END SERVER
# ------------------------------------------------------------------------ #
