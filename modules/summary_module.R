# ---------------------------------------------------------- #
# UI function ######
# ---------------------------------------------------------- #
summary_moduleUI <- function(id) 
{
  ns <- NS(id)
  tagList(
     #verbatimTextOutput(ns("dfTest")),
    bs4Box(
      width = 12
      , height = "auto"
      , title = "College Strategy Summary"
    , fluidRow(
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
      , column(3, bs4ValueBoxOutput(ns("cohmValueRed"), width = 12))))# end fluid
    , fluidRow(
        bs4Box(
          width = 12
          , height = "auto"
          , title = "College Strategy Timelines"
          , timevisOutput(ns("college_time_lines"), width = "100%", height = "auto")))
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
  nav_bar_DF <- reactive(req(nav_bar_df()))
  main_strategy_df <- reactive(
    {
    return(
      dbGetQuery(db, "SELECT * FROM strategy") %>% 
          filter(active_strategy =="y") %>% 
          select(-date_created, -active_strategy))
    })
  
  combined_df <- reactive(req(main_strategy_df()) %>% left_join(nav_bar_DF()))
  s_list <-  reactive(unlist(combined_df() %>% distinct(strategy_id), use.names = F))
  
  main_init_df <- reactive(main_modul_read_initiative(s_list()))
  int_list <- reactive(unlist(main_init_df() %>% distinct(initiative_id), use.names = F))
  main_mile_df <- reactive(main_modul_read_milestone(int_list()))

  short_df <- reactive(combined_df() %>% left_join(main_init_df(), by= "strategy_id"))
  strategy_df <- reactive(main_mile_df() %>% left_join(short_df(), by="initiative_id"))
  
  df <- reactive(
    {
    return(
      strategy_df() %>%
        group_by(college_id, college_short_name, strategy_id, strategy_name ) %>%
        summarise(start_date = min(start_date), end_date = max(end_date), target_days = difftime(end_date, Sys.time(),units="days")) %>%
        mutate(style = case_when(
          target_days > 14 ~ "color: #fff; background-color: #28a745; border-color: #191818;"
          , target_days >= 1 & target_days < 13 ~ "color: #191818; background-color: #ffc107; border-color: #191818;"
          , target_days <= 0 ~ "color: #fff; background-color: #e42312; border-color: #191818;")) %>% 
        select(college_id, strategy_id, college_short_name, strategy_name, style, start_date, end_date))
    })  
  
  # ------------------------------------------------------------------------#
  group_time_line <- reactive(
    {
      return(
        data.frame(
          id = 1:nrow(df())
          , start = unlist(df()$start_date, use.names = F)
          , end = unlist(df()$end_date, use.names = F)
          , content = unlist(df()$strategy_name, use.names = F)
          , group = unlist(df()$college_id, use.names = F)
          , subgroup = unlist(df()$strategy_id, use.names = F)
          , style = unlist(df()$style, use.names = F)))
    })
  # ------------------------------------------------------------------------#
  groups <- reactive(data.frame(id = unlist(unique(df()$college_id)), content = unique(df()$college_short_name)))
  output$college_time_lines <- renderTimevis(
  {
      timevis(data = group_time_line()
              , groups = groups()
              , fit = T
              , options = list(stack = T))
  })
  # ------------------------------------------------------------------------#

  # ------------------------------------------------------------------------ #
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
      , subtitle = paste0(nav_bar_DF()$college_short_name[4],  " - At Risk")
      , status = "warning"
      , icon = "bomb"
      , elevation = 5
      , href = "#")
  })
  # ------------------------------------------------------------------------#c
  output$coseValueYellow <- renderbs4ValueBox({
    bs4ValueBox(
      value = 150
      , subtitle = paste0(nav_bar_DF()$college_short_name[3],  " - At Risk")
      , status = "warning"
      , icon = "bomb"
      , elevation = 5
      , href = "#")
  })
  # ------------------------------------------------------------------------#
  output$caleValueYellow <- renderbs4ValueBox({
    bs4ValueBox(
      value = 150
      , subtitle = paste0(nav_bar_DF()$college_short_name[2],  " - At Risk")
      , status = "warning"
      , icon = "bomb"
      , elevation = 5
      , href = "#")
  })
  # ------------------------------------------------------------------------#
  output$cobeValueYellow <- renderbs4ValueBox({
    bs4ValueBox(
      value = 150
      , subtitle = paste0(nav_bar_DF()$college_short_name[1],  " - At Risk")
      , status = "warning"
      , elevation = 5
      , href = "#")
  })
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
# 
# 
nav_bar_DF <- dbGetQuery(db, "SELECT * FROM colleges")

main_strategy_df <- dbGetQuery(db, "SELECT * FROM strategy") %>%
                  filter(active_strategy =="y") %>%
                  select(-date_created, -active_strategy)

combined_df <- main_strategy_df %>% left_join(nav_bar_DF)
s_list <-  unlist(combined_df %>% distinct(strategy_id), use.names = F)

main_init_df <- main_modul_read_initiative(s_list)
int_list <- unlist(main_init_df %>% distinct(initiative_id), use.names = F)
main_mile_df <- main_modul_read_milestone(int_list)


short_df <- combined_df %>% left_join(main_init_df, by= "strategy_id")
strategy_df <- main_mile_df %>% left_join(short_df, by="initiative_id")

df <- strategy_df %>%
  group_by(college_id, college_short_name, strategy_id, strategy_name ) %>%
  summarise(start_date = min(start_date), end_date = max(end_date), target_days = difftime(end_date, Sys.time(),units="days")) %>%
  mutate(style = case_when(
              target_days > 14 ~ "color: success;"
              , target_days >= 1 & target_days < 13 ~ "color: warning;"
              , target_days <= 0 ~ "color: danger;"
              )) %>% 
  select(college_id, strategy_id, college_short_name, strategy_name, target_days, style, start_date, end_date)



# ------------------------------------------------------------------------#

timedata <- data.frame(
      id = 1:nrow(df)
     , start = unlist(df$start_date, use.names = F)
     , end = unlist(df$end_date, use.names = F)
     , content = unlist(df$strategy_name, use.names = F)
     , group = unlist(df$college_id, use.names = F)
     , subgroup = unlist(df$strategy_id, use.names = F)
     )

counter <- length(unique(df$college_short_name))
groups <- data.frame(id = unlist(unique(df$college_id)), content = unique(df$college_short_name))
timevis::timevis(data = timedata, groups = groups, options = list(stack = T))

# 
# # ------------------------------------------------------------------------#
# main_modul_read_initiative <- function(id_list)
# {
#   return(dbGetQuery(db, "SELECT * FROM initiatives") %>%
#            filter(strategy_id %in% id_list) %>%
#            filter(initiative_active == "y") %>%
#            select(-date_created, -college_id, -initiative_active))
# }
# # ------------------------------------------------------------------------#
# main_modul_read_milestone <- function(id_list)
# {
#   return(dbGetQuery(db, "SELECT * FROM milestones") %>%
#            filter(initiative_id %in% id_list) %>%
#            filter(milestone_active == "y") %>%
#            mutate(initiative_id = as.integer(initiative_id)) %>%
#            select(-date_created, -milestone_active))
# }
# # ------------------------------------------------------------------------#

