# ------------------------------------------------------------- #
# -------------------------- MOCK UP -------------------------- #
# -------------------- dropdown menus 2.0 --------------------- #

library(shiny)
library(shinydashboard)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Dropdowns 2.0", dropdownMenuOutput("menu")),
  
  dashboardSidebar(
    helpText("Change this slider and see how that affects the taskItems in the dropdownMenu"),
    
    sliderInput("bonus", "Bonus", 0, 50, 0),
    
    helpText("Our goal is to make the clicking of one of the three taskItems on the",
             "dropdownMenu be equivalent to selecting one of the three options below, respectively."),
    
    selectInput("todo", "Mock TODO", c(Choose = "", "code", "layout", "docs"), selectize = TRUE)
  ),
  
  dashboardBody()
)

server <- function(input, output, session) {
  
  # set up the tasks outside of any render functions, so they're accessible by
  # other functions/reactives/observers
  tasks <-  reactive({
    b <- input$bonus
    list(
      code = list(id = "code", value = 15 + b, color = "aqua", text = "Refactor code",
                  description = paste(rep("Refactor code", 30), collapse = "!! ")),
      
      layout = list(id = "layout", value = 40 + b, color = "green", text = "Design new layout",
                    description = paste(rep("Design new layout", 30), collapse = "!! ")),
      
      docs = list(id = "docs", value = 37 + b, color = "red", text = "Write documentation",
                  description = paste(rep("Write documentation", 30), collapse = "!! "))
    )
  })
  
  # actually render the dropdownMenu
  output$menu <- renderMenu({
    
    # If there was no extra data in tasks() aside from the value, color and text,
    # we could do just this: `items <- lapply(tasks(), function(el) { do.call(taskItem, el) })``
    # But having this more verbose statement allows us to have arbitrary keypairs in tasks()
    # (useful for the observeEvent below)
    items <- lapply(tasks(), function(el) {
      taskItem(value = el$value, color = el$color, text = el$text)
    })
    
    ### THE GOAL
    # dropdownMenu(id = "todo",
    #   type = "tasks", badgeStatus = "danger",
    #   .list = items
    # )
    
    ### TODAY (+ the mock input$todo in the UI)
    dropdownMenu(
      type = "tasks", badgeStatus = "danger",
      .list = items
    )
  })
  
  observeEvent(req(input$todo), {
    # match input$todo to the right task from the reactive tasks() (by id)
    # (I'm sure there's a simpler way of doing this)
    task <- lapply(tasks(), function(el) {
      if (el$id == input$todo) el
      else NULL
    }) %>% shiny:::dropNulls() %>% `[[`(1)
    
    # show a custom-made modal for the task (input$todo) that was just clicked
    showModal(modalDialog(title = task$text,
                          task$description,
                          easyClose = TRUE, footer = NULL
    ))
  })
}

shinyApp(ui, server)



## -------------------------- AGENDA --------------------------- #
# !! -- MUST DO
# 0. everything (`messageItem()`s, `notificationItems()`s and
#   `taskItem()`s) needs to have an id (or names or values,
#   something unique)
#
# 1. dropdownMenu takes in optional id that gets automatically
#    converted into a Shiny input with that name, whose value is
#    either NULL if no item (message/notification/task) is selected,
#    or the id/name/value of the last clicked item (which is then
#    selected and highlighted - see bullet point 2). To go back to
#    NULL, the user should unclick the currently selected item
#
# !! -- SHOULD DO
# 2. selected item (message/notification/task) should be high-
#    -lighted (like it is on hover)
#
# !! -- NICE TO HAVE
# 3. should have way of letting menu stay expanded (I think just
#    clicking on the dropdown icon should do it -- for example, in
#    the app above, the menu disappears when it needs to get re-
#    rendered, even it is just the slider on the taskItem moving a
#    bit as the result in user input elsewhere)
#
# !! -- DOCS
# 4. document and examplefy all of the above
#
# 5. write up a Google Analytics-specific piece -- there seems to
#    be a lot of confusion surrounding that... (for example, see
#    #128 and #151, which besides confusion, also hints possibility
#    of a bug to do with Shiny inputs in sidebar for GA)
#