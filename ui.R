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
            fixed = T,
            elevation = 5,
            opacity = 1,
            leftUi = tagList(uiOutput("firstButton"), uiOutput("secondButton"), uiOutput("thirdButton"), uiOutput("fourthButton"))),
        # ---------------------------------------------------------------------- #
        sidebar = bs4DashSidebar(
            skin = "light",
            status = "danger",
            title = "UTAS - Initiative Tracker",
            brandColor = "danger",
            url = "https://www.utas.edu.au",
            #src = "www/utas-logo-int.png",
            elevation = 5,
            opacity = 1,
            uiOutput("sideTilte"),
            bs4SidebarMenu(
                id = "testLeftBar",
               # bs4SidebarHeader("Header 1"),
                bs4SidebarMenuItem(
                    "Item 1",
                    tabName = "item1",
                    icon = "sliders"
                ),
                bs4SidebarMenuItem(
                    "Item 2",
                    tabName = "item2",
                    icon = "id-card"
                )
            )
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
