source("global.R")
for (nm in list.files("modules", pattern = "[.][Rr]$", recursive = TRUE)) 
{
    source(file.path("modules", nm))
}
# ---------------------------------------------------------------------- #
shinyUI(
    ui = bs4DashPage(
        old_school = FALSE,
        
        sidebar_collapsed = FALSE,
        controlbar_collapsed = FALSE,
        title = "UTAS - Initiative Tracking",
        # ---------------------------------------------------------------------- #
        navbar = bs4DashNavbar(
            skin = "light",
            status = "danger",
            border = T,
            sidebarIcon = "bars",
            controlbarIcon = "th",
            fixed = F,
            elevation = 5,
            opacity = 1,
            leftUi = tagList(uiOutput("firstButton"), uiOutput("secondButton"), uiOutput("thirdButton"), uiOutput("fourthButton"))),
        # ---------------------------------------------------------------------- #
        sidebar = bs4DashSidebar(
            skin = "light",
            status = "danger",
            title = "UTAS - Initiative Tracker",
            brandColor = "danger",
            url = "http://127.0.0.1:4975/",
            #src = "www/utas-logo-int.png",
            elevation = 5,
            opacity = 1,
            uiOutput("sideTilte"),
            bs4SidebarMenu(
                id = "leftBar",
                uiOutput("left_side_nav_buttons"))
    
        ),
        # ---------------------------------------------------------------------- #
        controlbar = bs4DashControlbar(disable =T),
        # ---------------------------------------------------------------------- #
        body = bs4DashBody(
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "base.css"))
            , uiOutput("body")
        )
        # ---------------------------------------------------------------------- #
        # ---------------------------------------------------------------------- #
)# end fluidPage
)# end shinyUI
