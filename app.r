#
# A basic framework for using a GUI with a perimetry test.
#
# The gui has 7 possible states
#   g_state$stopped  - the test has not begun
#   g_state$wait_for_init - Waiting for opiInitialise to finish
#   g_state$start_pause   - opiInitialise done, waiting for user to click Start
#   g_state$running       - in the main test loop
#   g_state$pause         - in the main test loop but waiting for user to click Continue
#   g_state$finalising    - The test has finished but there is some tidying up to do
#   g_state$showing_error - the test threw an error and I am waiting for User to click Ok
#
#
#                          +-----Cancel---->---------+---Cancel--->--------+----Cancel------- -->---+
#                          ^                         ^                     ^                        |
#                          |                         |                     |                        V
#             +-> g_state$wait_for_init -> g_state$start_pause -> g_state$running -> g_state$pause  |
#            /               \                          /                /|  ^               |      |
#           /                 V                        V                / |  |               V      |
# g_state$stopped <------- g_state$showing_error ----------<-----------+  |  +--<-Continue---+      |
#      |                                                                  V                         |
#      +------<------test all finished-----------------<-------- g_state$finalising <----Cancel-----+

# Andrew Turpin
# Wed 31 Aug 2022 14:32:24 AEST
#
require(shiny)
require(shinyjs)
require(shinyWidgets)
require(OPI)


g_state <- list(
    stopped       = 1, # the test has not begun
    wait_for_init = 2, # Waiting for opiInitialise to finish
    start_pause   = 3, # opiInitialise done, waiting for user to click Start
    running       = 4, # in the main test loop
    pause         = 5, # in the main test loop but waiting for user to click Continue
    finalising    = 6, # The test has finished but there is some tidying up to do
    showing_error = 7  # the test threw an error and I am waiting for User to click Ok
)

g_speeds <- list(Fast = 1, Normal = 1.5, Slow = 2.0)

# plotting area
VF_SIZE <- 500 # pixels

#
# The UI is a simple sidebarLayout. Add whatever you like!
#
ui <- fluidPage(
    useShinyjs(),

    uiOutput("css_style"),

    titlePanel("Basic test framework"),

    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(4, sliderTextInput("speed", "Test Speed:", choices = g_speeds))
            ),

            radioButtons('test_choice', "Test Choice", width = "100%", choices = c("Center", "Periph"), inline = TRUE),

            fluidRow(
                column(8, actionButton('run', 'Initialise machine')),
                column(4, actionButton('kill', 'Cancel Test', class="cancel-button"))
            ),

            htmlOutput("status"),
        width = 6),
        mainPanel(
            plotOutput("plotArea"),
        width = 6
        )
    )## sidebarLayout
)## fluidPage


server <- function(input, output, session) {
    rvs <- reactiveValues()

        # The values that will be plotted in the GUI and probably altered by the test
    rvs$vf <- data.frame(
        X = 0,
        Y = 0,
        Value = NA,
        NP = 0,
        Done = FALSE
    )
    rvs$start_time <- 0

    rvs$run_state <- g_state$stopped    # Start at the first state

    #observe({
    #    print("--------------DEBUG----------------")
    #    print(shiny_receiver$rep())
    #    print(paste("State: ", rvs$run_state))
    #    invalidateLater(500)
    #}) 

    #
    # The run button is overloaded with Initialise and Pause, 
    # hence the if statements on run_state.
    #
    observeEvent(input$run,{
        if (rvs$run_state == g_state$stopped) {  # start the test

                # Reset local rvs$vf for a new test
            rvs$vf$Done <- FALSE
            rvs$vf$NP <- 0
            rvs$vf$Value <- NA
            rvs$vf$TT <- 30

                # Set up variables that the test needs
            rvs$for_the_test <- list(
                machine = "SimHenson",
                size = 0.43,
                fpr = 0.01,
                fnr = 0.01
            )

            rvs$run_state <- g_state$wait_for_init
        } else if (rvs$run_state == g_state$start_pause) { # wait for init to finish
            rvs$run_state <- g_state$running
            rvs$start_time <- Sys.time()  # for elapsed test time
        } else if (rvs$run_state == g_state$running) { # pause
            rvs$run_state <- g_state$pause
        } else if (rvs$run_state == g_state$pause) { # continue
            rvs$run_state <- g_state$running
        }
    })

    #
    # When the RUN state changes, send it off to test and alter GUI as appropriate
    #
    observeEvent(rvs$run_state, {
        if (rvs$run_state == g_state$stopped)       updateActionButton(session, "run", "Initialise machine")
        if (rvs$run_state == g_state$wait_for_init) updateActionButton(session, "run", "Waiting...")
        if (rvs$run_state == g_state$start_pause)   updateActionButton(session, "run", "Start test")
        if (rvs$run_state == g_state$running)       updateActionButton(session, "run", "Pause Test")
        if (rvs$run_state == g_state$pause)         updateActionButton(session, "run", "Continue Test")
        if (rvs$run_state == g_state$finalising)    updateActionButton(session, "run", "Waiting...")

        if (rvs$run_state == g_state$stopped)       rvs$status <- "Ready to initialise"
        if (rvs$run_state == g_state$wait_for_init) rvs$status <- "Initialising machine, please wait"
        if (rvs$run_state == g_state$start_pause)   rvs$status <- "Ready to start test"
        if (rvs$run_state == g_state$running)       rvs$status <- "Running"
        if (rvs$run_state == g_state$pause)         rvs$status <- "Paused"
        if (rvs$run_state == g_state$finalising)    rvs$status <- "Finalising results"

        if (rvs$run_state == g_state$stopped)       enable("test_choice")
        if (rvs$run_state == g_state$wait_for_init) enable("test_choice")
        if (rvs$run_state == g_state$start_pause)   enable("test_choice")
        if (rvs$run_state == g_state$running)       disable("test_choice")
        if (rvs$run_state == g_state$pause)         disable("test_choice")
        if (rvs$run_state == g_state$finalising)    disable("test_choice")
    })

    #
    # Status text area.
    #
    output$status <- renderText({paste('<div style="margin-top: 20px;"> Status: ', rvs$status, "</div>") })

    #
    # This is where the progress is plotted
    #
    output$plotArea <- renderPlot({
        print("hello")
        plot(rvs$vf$X, rvs$vf$Y,
            main = "",
            type = "n", xlab = "", ylab = "",
            ylim = c(-70,70),
            xlim = c(-70,70),
            asp = 1, las = 1)
        symbols(rep(0, 6), rep(0, 6), circle = seq(10, 65, 10), inch = FALSE, add = TRUE, fg = grey(0.9))
        z <- is.na(rvs$vf$Value)
        if (any(!z)) text(rvs$vf$X[!z], rvs$vf$Y[!z], rvs$vf$Value[!z], col = ifelse(rvs$vf$Done[!z], "red", "black"))
        if (any(z)) points(rvs$vf$X[z], rvs$vf$Y[z], pch = 19)

        text(-70, 70, sprintf("Presentations: %0.0f", sum(rvs$vf$NP)), pos = 4)
        if (rvs$start_time > 0) {
            t <- Sys.time() - rvs$start_time
            m <- floor(t / 60)
            s <- t - m * 60
            text(-70, 65, sprintf("Elapsed Time: %0.0f:%02.0f", m,s), pos = 4)
        }

        text(par('usr')[2], 70, paste("Locations Remaining: ", sum(!rvs$vf$Done)), pos = 2)
     }, width = VF_SIZE, height = VF_SIZE)

    #
    # Changing css
    #
    output$css_style <- renderUI({
        if (rvs$run_state == g_state$stopped) {
            tags$head(tags$style(HTML(
                ".cancel-button { opacity: 0.6; cursor: not-allowed; }"
            )))
        } else {
            tags$head(tags$style(HTML(
                ".cancel-button { opacity: 1.0; cursor: allowed; } "
            )))
        }
    })

    #
    # Test Pattern Choices - update rvs$vf appropriately
    # Change to whatever you want.
    #
    observeEvent(input$test_choice, {
        if (input$test_choice == "Center") {
            xs <- seq(-30, 30, 10)
            rvs$vf = data.frame(
                X = xs,
                Y = rep(0, length(xs)),
                Value = rep(NA, length(xs)),
                NP = rep(0, length(xs)),
                Done = rep(FALSE, length(xs))
            )
        } else {
            xs <- c(seq(-60, -40, 10), seq(40, 60, 10))
            rvs$vf = data.frame(
                X = xs,
                Y = rep(0, length(xs)),
                Value = rep(NA, length(xs)),
                NP = rep(0, length(xs)),
                Done = rep(FALSE, length(xs))
            )
        }
    })

 #
    # Cancel button - be careful here because test can still run for a while  XXX true?
    # during the cancel process. Hence we move to g_state$finalising and not
    # g_state$stopped.
    #
    observeEvent(input$kill,{
        kill_state <<- isolate(rvs$run_state)  # record where we are 

        if (kill_state != g_state$stopped && kill_state != g_state$finalising) {
            rvs$run_state <- g_state$pause
            showModal(modalDialog(
                "Test Paused. Are you sure you want to cancel?" ,
                footer = tagList(actionButton("confirmCancel", "Yes - Cancel"), 
                                 actionButton("notConfirmCanel", "No - Continue Test"))
            ))
        }
    })
    observeEvent(input$confirmCancel,  {rvs$run_state <- g_state$finalising; removeModal() })
    observeEvent(input$notConfirmCanel,{rvs$run_state <- kill_state    ; removeModal() })

    #
    # Step the test along
    #
    observe({
        start_pres_time <- Sys.time()

        if (rvs$run_state == g_state$wait_for_init) {
            source("test_initialise.r", local = TRUE)
        }

        if (rvs$run_state == g_state$running) {
            source("test_running.r", local = TRUE)
        }

        if (rvs$run_state == g_state$finalising) {
            source("test_finalising.r", local = TRUE)
        }

        w <- max(10, rnorm(1, input$speed * 1000, 0.1) - (Sys.time() - start_pres_time) * 1000)
        print(paste(Sys.time(), start_pres_time, w))

        invalidateLater(w)
    })

    show_error <- function(err_txt) {
        showModal(modalDialog(
            paste("Test Error.", err_txt),
            footer = tagList(actionButton("okError", "Ok"))
        ))
        rvs$run_state <- g_state$showing_error
    }
    observeEvent(input$okError, { rvs$run_state <- g_state$stopped ; removeModal() })
}

shinyApp(ui = ui, server = server)
