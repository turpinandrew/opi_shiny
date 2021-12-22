#
# A basic framework for communication between GUI and OPI tests
#
# Assumes an OPI test will step through the 4 stages in the first row.
# The GUI maintains has the three extra states 
#   RUN_STOPPED  - the test has not begun
#   RUN_FINALISING - the test has finished but there is some tidying up to do
#   RUN_SHOWING_ERROR - the test threw an error and I am waiting for User to click Ok
#
#
#                      +-----Cancel---->----+---Cancel--->-+----Cancel---->---+
#                      ^                    ^              ^                  |
#                      |                    |              |                  V
#             +-> RUN_WAIT_FOR_INIT -> RUN_START_PAUSE -> RUN_RUNNING -> RUN_PAUSE 
#            /               |         /                  /|  ^            |  |
#           /                V        V                  / |  |            V  |
# RUN_STOPPED<-------- RUN_SHOWING_ERROR<---------------+  |  +-<-Continue-+  |
#      |                                                   V                  |
#      +------<------test all finished-----<-------- RUN_FINALISING <--Cancel-+
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

source('test_state.r')

# plotting area
VF_SIZE <- 500 # pixels

ShinySender <- txtq(tempfile())    # Messages from GUI to test
ShinyReceiver <- txtq(tempfile())  # Messages from test to GUI

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
                column(4, sliderTextInput("speed", "Test Speed:", choices=SPEEDS))
            ),

            radioButtons('test_choice', "Test Choice", width = "100%", choices = c("Center", "Periph"), inline=TRUE),

            fluidRow(
                column(8, actionButton('run', 'Initialise machine')),
                column(4, actionButton('kill', 'Cancel Test', class="cancel-button"))
            ),

            htmlOutput("status")
        ),
        mainPanel(
            plotOutput("plotArea")
        )
    )## sidebarLayout
)## fluidPage


server <- function(input, output, session) {
    rvs <- reactiveValues()

        # The values that will be plotted in the GUI and probably 
        # altered by messages coming in on the ShinyReceiver queue
    rvs$vf=data.frame(              # Note that the test checks for some of these columns
        X=0, Y=0,
        Value=NA,
        NP=0,
        Done=FALSE
    )
    rvs$start_time <- 0

    rvs$run_state <- RUN_STOPPED    # Start at the first state

    #observe({
    #    print("--------------DEBUG----------------")
    #    print(ShinyReceiver$list())
    #    print(paste("State: ", rvs$run_state))
    #    invalidateLater(500)
    #}) 

    #
    # The run button is overloaded with Initialise and Pause, 
    # hence the if statements on run_state.
    #
    observeEvent(input$run,{
        if (rvs$run_state == RUN_STOPPED) {  # start the test
            ShinyReceiver$reset()
            ShinySender$reset()

                # Send any parameters that might change during the test
            ShinySender$push(title=MSG_SPEED, input$speed)

                # Reset local rvs$vf for a new test
            rvs$vf$Done <- FALSE
            rvs$vf$NP <- 0
            rvs$vf$Value <- NA
            rvs$number_expected <- Inf  # the number of thresholds to wait for from test

                # Set up variables that the test is expecting to exist in its 'globals' list
            vf <- isolate(rvs$vf)
            #vf <- cbind(vf, TT=30)
            vf$TT<-30 # IMF question to AT: why not like this?
            machine <- "SimHenson"
            size <- 0.43
            fpr <- 0.01
            fnr <- 0.01
            source('static.r', local=TRUE)  # defines test_static <- future(... )

            rvs$run_state <- RUN_WAIT_FOR_INIT
            then(test_static,
                onFulfilled = function(x) rvs$run_state <- RUN_FINALISING,
                onRejected = function(err) rvs$run_state <- RUN_SHOWING_ERROR
            )
            return(NULL)  # is this needed?
        } else if (rvs$run_state == RUN_START_PAUSE) { # wait for init to finish
            rvs$run_state <- RUN_RUNNING
            rvs$start_time <- Sys.time()  # for elapsed time. test start time set in test future.
        } else if (rvs$run_state == RUN_RUNNING) { # pause
            rvs$run_state <- RUN_PAUSE
        } else if (rvs$run_state == RUN_PAUSE) { # continue
            rvs$run_state <- RUN_RUNNING
        }
    })

    #
    # When the RUN state changes, send it off to test and alter GUI as appropriate
    #
    observeEvent(rvs$run_state, {
        ShinySender$push(title=MSG_STATE, rvs$run_state)

        if (rvs$run_state == RUN_STOPPED)       updateActionButton(session, "run", "Initialise machine")
        if (rvs$run_state == RUN_WAIT_FOR_INIT) updateActionButton(session, "run", "Waiting...")
        if (rvs$run_state == RUN_START_PAUSE)   updateActionButton(session, "run", "Start test")
        if (rvs$run_state == RUN_RUNNING)       updateActionButton(session, "run", "Pause Test")
        if (rvs$run_state == RUN_PAUSE)         updateActionButton(session, "run", "Continue Test")
        if (rvs$run_state == RUN_FINALISING)    updateActionButton(session, "run", "Waiting...")

        if (rvs$run_state == RUN_STOPPED)       rvs$status <- "Ready to initialise"
        if (rvs$run_state == RUN_WAIT_FOR_INIT) rvs$status <- "Initialising machine, please wait"
        if (rvs$run_state == RUN_START_PAUSE)   rvs$status <- "Ready to start test"
        if (rvs$run_state == RUN_RUNNING)       rvs$status <- "Running"
        if (rvs$run_state == RUN_PAUSE)         rvs$status <- "Paused"
        if (rvs$run_state == RUN_FINALISING)    rvs$status <- "Finalising results"

        if (rvs$run_state == RUN_STOPPED)       enable("test_choice")
        if (rvs$run_state == RUN_WAIT_FOR_INIT) enable("test_choice")
        if (rvs$run_state == RUN_START_PAUSE)   enable("test_choice")
        if (rvs$run_state == RUN_RUNNING)       disable("test_choice")
        if (rvs$run_state == RUN_PAUSE)         disable("test_choice")
        if (rvs$run_state == RUN_FINALISING)    disable("test_choice")
    })

    #
    # When run_state is not RUN_FINALISING or RUN_STOPPED
    # check for errors coming from the test on ShinyReceiver
    #
    observe({
        if (rvs$run_state != RUN_STOPPED && rvs$run_state != RUN_FINALISING) {
            err <- ""
            get_from_txtq(list(c("err", MSG_TEST_ERROR)), ShinyReceiver, block=FALSE)
            if (nchar(err) > 0) {
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
    observeEvent(input$okError, { rvs$run_state <- RUN_STOPPED ; removeModal() })

    #
    # When run_state is RUN_WAIT_FOR_INIT check ShinyReceiver queue 
    # for MSG_INITIALISE_STATE, "1" message to show opiInitialise is done
    #
    observe({
        if (rvs$run_state == RUN_WAIT_FOR_INIT) {
            temp <- ""
            get_from_txtq(list(c('temp', MSG_INITIALISE_STATE)), ShinyReceiver, block=FALSE)
            if (nchar(temp) > 0 && temp == "1") {
                rvs$run_state <- RUN_START_PAUSE
            } else {
                invalidateLater(200) # keep looking
            }
        }
    })

    #
    # Utility function to find MSG_PRESENTATION and MSG_LOC_FINISHED
    # in the ShinyReceiver queue and update rvs$vf appropriately.
    #
    check_for_presentations <- function() {
        pres <- done <- ""
        get_from_txtq(list(
            c("pres", MSG_PRESENTATION),
            c("done", MSG_LOC_FINISHED)
        ), ShinyReceiver, block=FALSE)
        
        if (nchar(pres) > 0) {
            s <- strsplit(pres, " ")[[1]]
            ii <- which(rvs$vf$X == as.numeric(s[1]) & rvs$vf$Y == as.numeric(s[2]))
            if (!rvs$vf$Done[ii]) {
                rvs$vf$Value[ii] <- as.numeric(s[3])
                rvs$vf$NP[ii] <- rvs$vf$NP[ii] + 1
            }
        }
        
        if (nchar(done) > 0) {
            s <- as.numeric(strsplit(done, " ")[[1]])
            ii <- which(rvs$vf$X == s[1] & rvs$vf$Y == s[2])
            rvs$vf$NP[ii]    <- s[3]
            rvs$vf$Value[ii] <- s[4]
            rvs$vf$Done[ii] <- TRUE
        }
    }# check_for_presentations()

    #
    # When run_state is RUN_RUNNING or RUN_PAUSE check ShinyReceiver queue 
    # for any MSG_PRESENTATION or MSG_LOC_FINISHED messages to update rvs$vf
    #
    observe({
        if (rvs$run_state == RUN_RUNNING || rvs$run_state == RUN_PAUSE) {
            check_for_presentations()
            invalidateLater(200)   # keep looking
        }
    })

    #
    # If we are in RUN_FINALISING then the test is finished, but there might
    # be messages on ShinyReceiver still to process
    # Note rvs$number_expected gets set from MSG_TEST_FINISHED.
    #
    observe({
        if (rvs$run_state == RUN_FINALISING) {
            if (sum(rvs$vf$Done) < rvs$number_expected) {
                check_for_presentations()
                
                temp <- ""
                get_from_txtq(list(c("temp", MSG_TEST_FINISHED)), ShinyReceiver, block=FALSE)
                if (nchar(temp) > 0) {
                    rvs$number_expected <- as.numeric(temp)
                }
                invalidateLater(100) # keep looking
            } else {
                rvs$run_state <- RUN_STOPPED
            }
        }
    })

    #
    # Cancel button - be careful here because test can still run for a while
    # during the cancel process. Hence we move to RUN_FINALISING and not
    # RUN_STOPPED. Test will always send back a MSG_TEST_FINISHED if not in error.
    #
    observeEvent(input$kill,{
        kill_state <<- isolate(rvs$run_state)  # record where we are 

        if (kill_state != RUN_STOPPED && kill_state != RUN_FINALISING) {
            rvs$run_state <- RUN_PAUSE
            showModal(modalDialog(
                "Test Paused. Are you sure you want to cancel?" ,
                footer = tagList(actionButton("confirmCancel", "Yes - Cancel"), 
                                 actionButton("notConfirmCanel", "No - Continue Test"))
            ))
        }
    })
    observeEvent(input$confirmCancel,  {rvs$run_state <- RUN_FINALISING; removeModal() })
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
        if (rvs$run_state == RUN_STOPPED) {
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
    observeEvent(input$speed,{ ShinySender$push(MSG_SPEED, input$speed) })
}

shinyApp(ui=ui, server = server)
