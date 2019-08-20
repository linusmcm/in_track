shinyServer(function(input, output, session) 
{
    # ------------------------------------------------------------- #
    # INITIAL Loader ######
    # ------------------------------------------------------------- #    
    # REACTIVE VALUES ###################################
    # ------------------------------------------------------------- #    
    college_vector <- reactiveVal(NULL)
    c_id <- reactiveVal(NULL) #college id to pass back to databases
    s_id <- reactiveVal(NULL) #strategy id to pass back to databases
    i_id <- reactiveVal(NULL) #initiative id to pass back to databases
    description_name <- reactiveVal(NULL)
    title_name <- reactiveVal(NULL)
    strategy_df <- reactiveVal(NULL)
    initiative_df <- reactiveVal(NULL)
    actionType <- reactiveVal(NULL)
    milestone_end_date <- reactiveVal(NULL)
    milestone_start_date <- reactiveVal(NULL)
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
                     output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = nav_bar_item$college_short_name) })
                 })
    # ------------------------------------------------------------- #
    # COBE ####
    # ------------------------------------------------------------- #
    observeEvent(input$COBE,
                 {
                     nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COBE")
                     college_vector(nav_bar_item)
                     c_id(nav_bar_item$college_id)
                     output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = nav_bar_item$college_short_name) })
                 })
    # ------------------------------------------------------------- #
    # CALE ####
    # ------------------------------------------------------------- #
    observeEvent(input$CALE,
                 {
                     nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "CALE")
                     college_vector(nav_bar_item)
                     c_id(nav_bar_item$college_id)
                     output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = nav_bar_item$college_short_name) })
                 })
    # ------------------------------------------------------------- #
    # COHM ####
    # ------------------------------------------------------------- #
    observeEvent(input$COHM,
                 {
                     nav_bar_item <- nav_bar_df() %>% filter(college_short_name == "COHM")
                     college_vector(nav_bar_item)
                     c_id(nav_bar_item$college_id)
                     output$sideTilte <- renderUI({ bs4SidebarUserPanel(img = "utas-logo-int.png", text = nav_bar_item$college_short_name) })
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
               , nav_bar_df = reactive(nav_bar_df())
               , college_id = reactive(c_id())
               #, strategy_id = reactive(s_id())
               # , initiative_id = reactive(i_id())
    )
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
                     actionType("Add Strategy")
                 })
    # ------------------------------------------------------------- #
    observeEvent(input$strategy_ok,
                 {
                     title_name(req(input$strategy_name))
                     description_name(req(input$description_name))
                     rValue <- write_strategy(c_id(), req(title_name()), req(description_name()))
                     if_else(rValue == T, removeModal(), NULL)
                 })
    # ------------------------------------------------------------- #
    observeEvent(input$add_initiative,
                 {
                     showModal(initiative_load())
                     actionType("Add Initiative")
                 })
    # ------------------------------------------------------------- #
    observeEvent(input$initiative_ok,
                 {
                     title_name(req(input$initiative_name))
                     description_name(req(input$initiative_description))
                     rValue <- write_initiative(title_name(), description_name(),  s_id(), c_id())
                     if_else(rValue == T, removeModal(), NULL)
                 })
    # ------------------------------------------------------------- #
    #observe(s_id(req(input$strategy_choice)))
    observeEvent(input$strategy_choice,
                 {
                     s_id(unlist(strategy_df() %>% 
                                     filter(strategy_name == req(input$strategy_choice)) %>% 
                                     distinct(strategy_id), use.names = F))
                 })
    
    # ------------------------------------------------------------- #
    observeEvent(input$add_mileStone,
                 {
                     showModal(milestone_load())
                     actionType("Add MileStone")
                 })
    # ------------------------------------------------------------- #
    observeEvent(input$milestone_ok,
                 {
                     milestone_start_date(req(input$start_date))
                     milestone_end_date(req(input$end_date))
                     i_id(unlist(initiative_df() %>% filter(initiative_name == req(input$initiative_choice)) %>% select(initiative_id), use.names = F))
                     title_name(req(input$milestone_name))
                     description_name(req(input$milestone_description))
                     rValue <- write_milestone(title_name(), description_name(), i_id(),milestone_start_date(), milestone_end_date())
                     if_else(rValue == T, removeModal(), NULL)
                 })
    # ------------------------------------------------------------- #
    observe(initiative_df(read_initiative(req(c_id()), req(s_id()))))
    # ------------------------------------------------------------- #
    # UI ELEMENTS ###################################
    # ------------------------------------------------------------- #
    output$collegeTitle <- renderText(paste0(actionType(), " - ", college_vector()$college_short_name))
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
    # ------------------------------------------------------------- #
    output$new_milestone_title <- renderUI(
        {
            textInput(inputId = "milestone_name"
                      , label = "New Milestone Name"
                      , placeholder = "Please add a Initiative Name"
                      , width = TEXT_AREA_WIDTH)
        })
    # ------------------------------------------------------------- #
    output$new_milestone_description <- renderUI(
        {
            textAreaInput(inputId = "milestone_description"
                          , label = "Milestone Description"
                          , placeholder = "Please add a Milestone description"
                          , width = TEXT_AREA_WIDTH
                          , height = MILESTONE_TEXT_AREA_HEIGHT)
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
                tagList(renderUI({   }) ) 
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
            
            strategy_df(read_strategies(c_id()))
            strategy_list <- unlist(strategy_df() %>% select(strategy_name),  use.names = F)
            selectInput(inputId="strategy_choice"
                        ,label="Please Select A strategy:"
                        , choices= strategy_list
                        , selected="Please Select a Strategy"
                        , width = "100%"
            )
        })
    # ------------------------------------------------------------- #
    output$initiative_picker <- renderUI(
        {
            
            initiative_df(read_initiative(c_id(), s_id()))
            initiative_list <- unlist(initiative_df() %>% select(initiative_name),  use.names = F)
            selectInput(inputId="initiative_choice"
                        ,label="Please Select An Initiative:"
                        , choices= initiative_list
                        , selected="Please Select an Initiative"
                        , width = "100%"
            )
        })
    # ------------------------------------------------------------- #
    output$start_date_picker <- renderUI(
        {
            dateInput("start_date", label = "Milestone Start Date", value = Sys.time())
        })
    # ------------------------------------------------------------- #
    output$end_date_picker <- renderUI(
        {
            dateInput("end_date", label = "Milestone End Date", value = Sys.time())
        })
    # ------------------------------------------------------------- #
    
    # ------------------------------------------------------------- #
}) # END SERVER
# ------------------------------------------------------------- #
