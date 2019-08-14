# ---------------------------------------------------------- #
# UI function ######
# ---------------------------------------------------------- #
main_module_UI <- function(id) 
{
  ns <- NS(id)
  tagList(
    # ---------------------------------------------------------- #
    # timevisOutput(ns("time_line"))
    ) #end tagList
} # end summary_moduleUI
# ---------------------------------------------------------- #
# SERVER function ----
# ---------------------------------------------------------- #
main_module <- function(input, output, session
                        , nav_bar_df
                        , college_id) 
{
  # ---------------------------------------------------------- #
  ns <- session$ns
  # ---------------------------------------------------------- #
  # -----------------------------------------------------------------------#
  nav_bar_DF <- reactive(nav_bar_df())
  c_id <- reactive(college_id())
  # PRINT TEST Function 
  # ------------------------------------------------------------------------#
  # ------------------------------------------------------------------------#
  #output$dfTest <- renderPrint({ print(str(BoxOutputValues())) })
  #output$testDFF <- renderUI({ verbatimTextOutput('dfTest') })
  
  # ------------------------------------------------------------------------#
  # output$time_line <- renderTimevis(
  #                       timevis(
  #                         data,
  #                         options = list(editable = TRUE, multiselect = TRUE, align = "center")
  #                       )
  #                     )
  #read_strategies(c_id())
  
} 
# END SERVER FUNCTION #####
# ------------------------------------------------------------------------#
# ------------------------------------------------------------------------#
# MODULE FUNCTIONS ######
# ------------------------------------------------------------------------#
# ------------------------------------------------------------------------#
c_id <- 11
main_strategy_df <- read_strategies(c_id) %>% select(-date_created)
s_list <-  unlist(main_strategy_df %>% distinct(strategy_id), use.names = F)
main_init_df <- main_modul_read_initiative(s_list)
int_list <- unlist(main_init_df %>% distinct(initiative_id), use.names = F)
main_mile_df <- main_modul_read_milestone(int_list)
# ------------------------------------------------------------------------#
short_df <- main_strategy_df %>% left_join(main_init_df, by= "strategy_id")
strategy_df <- main_mile_df %>% left_join(short_df, by="initiative_id")
# ------------------------------------------------------------------------#
df <- strategy_df %>%
  group_by(milestone_name,milestone_description) %>%
  summarise(start_date = min(start_date), end_date = max(end_date)) %>%
  select(start = start_date, end = end_date, content = milestone_name)
# ------------------------------------------------------------------------#
timevis(df)
# ------------------------------------------------------------------------#
# ------------------------------------------------------------------------#
# ------------------------------------------------------------------------#





