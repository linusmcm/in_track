shinyServer(function(input, output, session) 
{
# ------------------------------------------------------------- #
# INITIAL Loader ######
# ------------------------------------------------------------- #    
# REACTIVE VALUES ###################################
# ------------------------------------------------------------- #    
college_vector <- reactiveVal(NULL)
c_id <- reactiveVal(NULL) #college id to pass back to databases
description_name <- reactiveVal(NULL)
strategy_name <- reactiveVal(NULL)
# ------------------------------------------------------------- #    
nav_bar_df <- reactive(load_nav_bar_menu())
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# COSE ####
# ------------------------------------------------------------- #
observeEvent(input$COSE,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COSE")
    college_vector(nav_bar_item)
    c_id(nav_bar_item$college_id)
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
})
# ------------------------------------------------------------- #
# COBE ####
# ------------------------------------------------------------- #
observeEvent(input$COBE,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COBE")
    college_vector(nav_bar_item)
    c_id(nav_bar_item$college_id)
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
})
# ------------------------------------------------------------- #
# CALE ####
# ------------------------------------------------------------- #
observeEvent(input$CALE,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "CALE")
    college_vector(nav_bar_item)
    c_id(nav_bar_item$college_id)
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
})
# ------------------------------------------------------------- #
# COHM ####
# ------------------------------------------------------------- #
observeEvent(input$COHM,
{
    nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COHM")
    college_vector(nav_bar_item)
    c_id(nav_bar_item$college_id)
    output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = substring(nav_bar_item$college_long_name, 12)) })
})
# ------------------------------------------------------------- #
# MODULES ###################################
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# summary_module ####
# ------------------------------------------------------------- #
callModule(summary_module
           , "summary_interface"
           , nav_bar_df = reactive(nav_bar_df()))
# ------------------------------------------------------------- #
# main_module ####
# ------------------------------------------------------------- #
callModule(main_module
           , "main_interface"
           , nav_bar_df = reactive(nav_bar_df()))
# ------------------------------------------------------------- #
# END MODULES ###################################
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# NAV BAR Buttons ######
# ------------------------------------------------------------- #
output$firstButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[1]
                   , label = nav_bar_df()$college_long_name[1]
                   , size = BUTTON_SIZE
                   , icon = NULL
                   , style= BUTTON_STYLE)
})
# ------------------------------------------------------------- #
output$secondButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[2]
                   , label = nav_bar_df()$college_long_name[2]
                   , size = BUTTON_SIZE
                   , icon = NULL
                   , style= BUTTON_STYLE)
})
# ------------------------------------------------------------- #
output$thirdButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[3]
                   , label = nav_bar_df()$college_long_name[3]
                   , size = BUTTON_SIZE
                   , icon = NULL
                   , style= BUTTON_STYLE)
})
# ------------------------------------------------------------- #
output$fourthButton <- renderUI(
{
        actionBttn(inputId = nav_bar_df()$college_short_name[4]
                   , label = nav_bar_df()$college_long_name[4]
                   , size = BUTTON_SIZE
                   , icon = NULL
                   , style= BUTTON_STYLE)
})
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# OBSERVE EVENTS ###################################
# ------------------------------------------------------------- #
observeEvent(input$add_strategy,
{
    showModal(strategy_load())
})
# ------------------------------------------------------------- #
observeEvent(input$strategy_ok,
{
    strategy_name(req(input$strategy_name))
    description_name(req(input$description_name))
    rValue <- write_strategy(c_id(), req(strategy_name()), req(description_name()))
    if_else(rValue == T, removeModal(), NULL)
})
# ------------------------------------------------------------- #
observeEvent(input$add_initiative,
             {
                 showModal(initiative_load())
             })
# ------------------------------------------------------------- #
# observeEvent(input$strategy_ok,
#              {
#                  strategy_name(req(input$strategy_name))
#                  description_name(req(input$description_name))
#                  rValue <- write_strategy(college_vector()$college_short_name, req(strategy_name()), req(description_name()))
#                  if_else(rValue == T, removeModal(), NULL)
#              })
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# UI ELEMENTS ###################################
# ------------------------------------------------------------- #
output$collegeTitle <- renderText(college_vector()$college_long_name)
# ------------------------------------------------------------- #
output$new_strategy_title <- renderUI(
{
    textInput(inputId = "strategy_name"
                , label = "New Strategy Name"
                , placeholder = "Please add a Strategy Name"
                , width = TEXT_AREA_WIDTH)
})
# ------------------------------------------------------------- #
output$new_strategy_description <- renderUI(
{
        textAreaInput(inputId = "description_name"
                  , label = "Strategy Description"
                  , placeholder = "Please add a Strategy description"
                  , width = TEXT_AREA_WIDTH
                  , height = TEXT_AREA_WIDTH)
})
# ------------------------------------------------------------- #
output$new_initiative_title <- renderUI(
    {
        textInput(inputId = "initiative_name"
                  , label = "New Initiative Name"
                  , placeholder = "Please add a Initiative Name"
                  , width = TEXT_AREA_WIDTH)
    })
# ------------------------------------------------------------- #
output$new_initiative_description <- renderUI(
    {
        textAreaInput(inputId = "initiative_description"
                      , label = "Initiative Description"
                      , placeholder = "Please add a Initiative description"
                      , width = TEXT_AREA_WIDTH
                      , height = TEXT_AREA_WIDTH)
    })
# ------------------------------------------------------------- #
output$body <- renderUI(
    { 
        if(is_empty(college_vector()))
        {
            tagList(fluidPage(summary_moduleUI("summary_interface"))) 
        }
        else
        {
            tagList(
                tabBox(
                    id = paste0(college_vector()$college_short_name,"_tab")
                    , width = 12
                    , title = college_vector()$college_long_name
                    #, headerBorder = T
                    # --------------------------------------------- #
                    # --------------------------------------------- #
                    , bs4TabPanel(
                        tabName = paste0("Strategy - ", college_vector()$college_short_name)
                        , active = TRUE
                        #, "DMS Item Codes"
                        #, "Summary"
                        #  , summary_moduleUI("summary_interface")
                    ),
                    # --------------------------------------------- #
                    bs4TabPanel(
                        tabName = paste0("Initiative - ", college_vector()$college_short_name)
                        , active = F
                        #, "DMS Item Codes"
                        #, "Demographics"
                        #  , demographics_moduleUI("demographics_interface")
                    ),
                    bs4TabPanel(
                        tabName = paste0("Milestone - ", college_vector()$college_short_name)
                        , active = F
                        #, "DMS Item Codes"
                        # , dms_item_codes_moduleUI("dms_item_codes_interface")
                    ),
                    # --------------------------------------------- #
                    bs4TabPanel(
                        tabName = paste0("Spare - ", college_vector()$college_short_name)
                        , active = F
                        #, "DMS Item Codes"
                        #, "Provider Group Profile"
                        # , pgp_moduleUI("pgp_interface")
                    )
                    # --------------------------------------------- #
                    # --------------------------------------------- #
                )# end tabBox
            ) #end taglist 
        } # end if
    }) # END renderUI
# ------------------------------------------------------------- #
output$left_side_nav_buttons <- renderUI(
    { 
        if(is_empty(college_vector()))
        {
            tagList( renderUI({   }) ) 
        }
        else
        {
        tagList(
              actionBttn(inputId = "add_strategy"
                       , label = "Add Strategy"
                       , size = BUTTON_SIZE
                       , icon = NULL
                       , style= BUTTON_STYLE
                       , block = T)
            , HTML("<br>")
            , actionBttn(inputId = "add_initiative"
                        , label = "Add Initiative"
                        , size = BUTTON_SIZE
                        , icon = NULL
                        , style= BUTTON_STYLE
                        , block = T)
            , HTML("<br>")
            , actionBttn(inputId = "add_mileStone"
                         , label = "Add Milestone"
                         , size = BUTTON_SIZE
                         , icon = NULL
                         , style= BUTTON_STYLE
                         , block = T)
            ) #end taglist
        }
    })
# ------------------------------------------------------------- #
output$strategy_picker <- renderUI(
{
    
    strategy_list <- read_strategies(college_vector()$college_short_name)
    selectInput(inputId="strategy_choice",label="Please Select A strategy:", choices= strategy_list, selected="Please Select a Strategy")
})

output$start_date_picker <- renderUI(
{
        dateInput("date", label = h3("Start Date "), value = "2014-01-01")
})


# ------------------------------------------------------------- #
}) # END SERVER
# ------------------------------------------------------------- #
