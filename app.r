#
# A basic framework for communication between GUI and OPI tests
#
# Assumes an OPI test will step through the 4 stages in the first row.
# The GUI maintains has the three extra states 
#   g_state$stopped  - the test has not begun
#   g_state$finalising - the test has finished but there is some tidying up to do
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
#      
#
# Andrew Turpin
# Sat 13 Nov 2021 10:01:08 AEDT
#
require(txtq)
require(shiny)
require(shinyjs)
require(shinyWidgets)
require(promises)
require(future)
plan(multisession)
onStop(function() plan(sequential))

source('test_state.r', local = TRUE)
kill_state <- NULL

# plotting area
VF_SIZE <- 500 # pixels

source("comm_queue.r")

shiny_sender <- CommQueue$new("GUI Sender", FALSE) # Messages from GUI to test
shiny_receiver <- CommQueue$new("GUI Receiver", FALSE)   # Messages from test to GUI


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

        # The values that will be plotted in the GUI and probably 
        # altered by messages coming in on the shiny_receiver queue
    rvs$vf <- data.frame(              # Note that the test checks for some of these columns
        X = 0, Y = 0,
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
            shiny_receiver$reset()
            shiny_sender$reset()

                # Send any parameters that might change during the test
            shiny_sender$push(title=g_msg$speed, input$speed)

                # Reset local rvs$vf for a new test
            rvs$vf$Done <- FALSE
            rvs$vf$NP <- 0
            rvs$vf$Value <- NA
            rvs$number_expected <- Inf  # the number of thresholds to wait for from test

                # Set up variables that the test is expecting to exist in its 'globals' list
            for_the_test <- list(
                static_last_time_called = as.numeric(Sys.time()),
                vf = cbind(isolate(rvs$vf), TT = 35),
                machine = "SimHenson",
                size = 0.43,
                fpr = 0.01,
                fnr = 0.01,
                ShinySender = shiny_sender, 
                ShinyReceiver = shiny_receiver 
            )
            source('static.r', local=TRUE)  # defines test_static <- future(... )

            rvs$run_state <- g_state$wait_for_init
            then(test_static,
                onFulfilled = function(x) rvs$run_state <- g_state$finalising,
                onRejected = function(err) rvs$run_state <- g_state$showing_error
            )
            return(NULL)  # is this needed?
        } else if (rvs$run_state == g_state$start_pause) { # wait for init to finish
            rvs$run_state <- g_state$running
            rvs$start_time <- Sys.time()  # for elapsed time. test start time set in test future.
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
        shiny_sender$push(title=g_msg$state, rvs$run_state)

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
    # When run_state is not g_state$finalising or g_state$stopped
    # check for errors coming from the test on shiny_receiver
    #
    observe({
        if (rvs$run_state != g_state$stopped && rvs$run_state != g_state$finalising) {
            err <- shiny_receiver$get(g_msg$test_error, block=FALSE)
            if (!is.na(err)) {
                showModal(modalDialog(
                    paste("Test Error.", err),
                    footer = tagList(actionButton("okError", "Ok"))
                ))
                rvs$status <- err    
            } else {
                invalidateLater(500)  # keep looking
            }
        }
    })
    observeEvent(input$okError, { rvs$run_state <- g_state$stopped ; removeModal() })

    #
    # When run_state is g_state$wait_for_init check shiny_receiver queue 
    # for g_msg$initialise_state, "1" message to show opiInitialise is done
    #
    observe({
        if (rvs$run_state == g_state$wait_for_init) {
            temp <- shiny_receiver$get(g_msg$initialise_state, block=FALSE)
            if (!is.na(temp) && temp == "1") {
                rvs$run_state <- g_state$start_pause
            } else {
                invalidateLater(200) # keep looking
            }
        }
    })

    #
    # Utility function to find g_msg$presentation and g_msg$loc_finished
    # in the shiny_receiver queue and update rvs$vf appropriately.
    #
    check_for_presentations <- function() {
        pres <- shiny_receiver$get(g_msg$presentation, block = FALSE)
        done <- shiny_receiver$get(g_msg$loc_finished, block = FALSE)
        
        if (!is.na(pres) > 0) {
            s <- strsplit(pres, " ")[[1]]
            ii <- which(rvs$vf$X == as.numeric(s[1]) & rvs$vf$Y == as.numeric(s[2]))
            if (!rvs$vf$Done[ii]) {
                rvs$vf$Value[ii] <- as.numeric(s[3])
                rvs$vf$NP[ii] <- rvs$vf$NP[ii] + 1
            }
        }
        
        if (!is.na(done) > 0) {
            s <- as.numeric(strsplit(done, " ")[[1]])
            ii <- which(rvs$vf$X == s[1] & rvs$vf$Y == s[2])
            rvs$vf$NP[ii]    <- s[3]
            rvs$vf$Value[ii] <- s[4]
            rvs$vf$Done[ii] <- TRUE
        }
    }# check_for_presentations()

    #
    # When run_state is g_state$running or g_state$pause check shiny_receiver queue 
    # for any g_msg$presentation or g_msg$loc_finished messages to update rvs$vf
    #
    observe({
        if (rvs$run_state == g_state$running || rvs$run_state == g_state$pause) {
            check_for_presentations()
            invalidateLater(200)   # keep looking
        }
    })

    #
    # If we are in g_state$finalising then the test is finished, but there might
    # be messages on shiny_receiver still to process
    # Note rvs$number_expected gets set from g_msg$test_finished.
    #
    observe({
        if (rvs$run_state == g_state$finalising) {
            if (sum(rvs$vf$Done) < rvs$number_expected) {
                check_for_presentations()
                
                temp <- shiny_receiver$get(g_msg$test_finished, block = FALSE)
                if (!is.na(temp)) {
                    rvs$number_expected <- as.numeric(temp)
                }
                invalidateLater(100) # keep looking
            } else {
                rvs$run_state <- g_state$stopped
            }
        }
    })

    #
    # Cancel button - be careful here because test can still run for a while
    # during the cancel process. Hence we move to g_state$finalising and not
    # g_state$stopped. Test will always send back a g_msg$test_finished if not in error.
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
    # Status text area.
    #
    output$status <- renderText({paste('<div style="margin-top: 20px;"> Status: ', rvs$status, "</div>") })

    #
    # This is where the progress is plotted
    #
    output$plotArea <- renderPlot({
        plot(rvs$vf$X, rvs$vf$Y, 
            main="",
            type="n", xlab="", ylab="",
            ylim=c(-70,70),
            xlim=c(-70,70),
            asp=1, las=1)
        symbols(rep(0, 6), rep(0, 6), circle=seq(10, 65, 10), inch=FALSE, add=TRUE, fg=grey(0.9))
        z <- is.na(rvs$vf$Value)
        if (any(!z)) text(rvs$vf$X[!z], rvs$vf$Y[!z], rvs$vf$Value[!z], col=ifelse(rvs$vf$Done[!z], "red", "black"))
        if (any(z)) points(rvs$vf$X[z], rvs$vf$Y[z], pch=19)

        text(-70, 70, sprintf("Presentations: %0.0f", sum(rvs$vf$NP)), pos=4)
        if (rvs$start_time > 0) {
            t <- Sys.time() - rvs$start_time
            m <- floor(t / 60)
            s <- t - m * 60
            text(-70, 65, sprintf("Elapsed Time: %0.0f:%02.0f", m,s), pos=4)
        }

        text(par('usr')[2], 70, paste("Locations Remaining: ", sum(!rvs$vf$Done)), pos=2)
     }, width=VF_SIZE, height=VF_SIZE)

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
    observeEvent(input$test_choice,{ 
        if(input$test_choice == "Center") {
            xs <- seq(-30,30,10)
            rvs$vf=data.frame(
                X=xs,
                Y=rep(0, length(xs)), 
                Value=rep(NA, length(xs)), 
                NP=rep(0, length(xs)),
                Done=rep(FALSE, length(xs))
            )
        } else {
            xs <- c(seq(-60,-40,10), seq(40,60,10))
            rvs$vf=data.frame(
                X=xs,
                Y=rep(0, length(xs)), 
                Value=rep(NA, length(xs)), 
                NP=rep(0, length(xs)),
                Done=rep(FALSE, length(xs))
            )
        }
    })

    #
    # ISI slider
    #
    observeEvent(input$speed,{ shiny_sender$push(g_msg$speed, input$speed) })
    # stop server
    onSessionEnded(function() shiny_sender$push(g_msg$state, g_state$stopped))
}

shinyApp(ui=ui, server = server)
