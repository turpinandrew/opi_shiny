#
# A fake test just to demonstrate the GUI.
#
# You need to decide on two different classes of variables you need from GUI.
#    (A) Ones that will not change during the test - list in globals parameter of future(.)
#    (B) Ones that might change during the test - read them from ShinySender
#
# Andrew Turpin
# Fri 17 Dec 2021 16:37:15 AEDT
#
require(future)

    # Get a new random seed from the current time
    # otherwise the future uses the same random seed each time.
if (!exists("static_last_time_called"))
    static_last_time_called <- as.numeric(Sys.time())
while (Sys.time() == static_last_time_called)
    Sys.sleep(0.01)
static_last_time_called <- as.numeric(Sys.time())

    #
    # Beginning of the test logic - note globals at the end.
    #
    # If there is an error condition raised in the test
    #   (a) send a g_msg$test_error on ShinyReceiver and 
    #   (b) use stop() to throw the problem back to the GUI
    #
test_static <- future({
source('test_state.r')
set.seed(as.numeric(rev(as.character(static_last_time_called))))

    # Check for any particular variables needed not listed in globals.
for (cc in c("X", "Y", "NP", "Done")) {
    if (! cc %in% colnames(vf)) {
        ShinyReceiver$push(g_msg$test_error, sprintf("Missing $s in vf",v))
        stop(sprintf("Missing $s in vf",v))
    }
}
if (substr(machine,1,3) == "Sim" && ! "TT" %in% colnames(vf)) {
    ShinyReceiver$push(g_msg$test_error, "Missing TT in vf")
    stop("Missing TT in vf")
}

    # Get any information needed from the GUI queue not passed as 
    # globals (ie can change during the test)
speed <- g_speeds[[1]]
s <- ShinySender$get(g_msg$speed, block=FALSE)
if (!is.na(s))
    speed <- s

    # Setup OPI in the usual way
if (!chooseOPI(machine)) {
    ShinyReceiver$push(g_msg$test_error, "Bad machine in chooseOPI")
    stop('Invalide machine for OPI')
}

    # opiInitialise in the usual way, signalling start and end with 
    # g_msg$initialise_state messages on ShinyReceiver so GUI knows 
    # what is happening.
ShinyReceiver$push(g_msg$initialise_state, g_state$stopped)
res <- opiInitialise()
if (!is.null(res)) {
    ShinyReceiver$push(g_msg$test_error, "opiInitialise failed")
    stop('opiInitialise failed')
}
ShinyReceiver$push(g_msg$initialise_state, g_state$wait_for_init)

    # Test needs to wait for GUI to give the green light as a message on ShinySender.
    # Note that we could get a variety of states from GUI
    # due to pausing, running or cancelling.
running_state <- g_state$wait_for_init
while (running_state == g_state$wait_for_init) {
    temp <- ShinySender$get(g_msg$state, block = FALSE)
    if (!is.na(temp))
        running_state <- temp
    Sys.sleep(0.5)  # don't forget to sleep so GUI gets some CPU time
}

    # usual OPI stuff
makeStimHelper <- function(x, y) {  # returns a function of (db,n)
    ff <- function(db, n) db+n
    body(ff) <- substitute({
        s <- list(x=x, y=y, level=dbTocd(db), size=size,
                duration=200, responseWindow=1500, checkFixationOK=NULL)
        class(s) <- "opiStaticStimulus"
        return(s)
    }, list(x=x,y=y))
    return(ff)
}

    #
    # Begin main test loop
    #
    # Note that in addition to the test logic (in this case length(unfinished) > 0) 
    # we need to check that the GUI is not in g_state$stopped or g_state$finalising states.
    # If it is, it means that the test has been cancelled from the GUI.
    # We also need to not present anything while the GUI is in a g_state$pause state.
    # As such, we need to check the ShinySender queue for g_msg$state quite often.
    #
unfinished <- as.list(1:nrow(vf))
start_time <- Sys.time()
start_pres_time <- startTime <- Sys.time()
while (running_state != g_state$stopped && running_state != g_state$finalising && length(unfinished) > 0) {
    if (speed == g_speeds[[1]]) Sys.sleep(0.1)
    if (speed == g_speeds[[2]]) Sys.sleep(1.0)

    if (running_state == g_state$running) {  # Could be paused, remember...
        loc_i <- sample.int(length(unfinished), 1)
        loc <- unfinished[[loc_i]]

        xy <- c(vf$X[loc], vf$Y[loc])
        val <- round(runif(1, 0, 40))
        r <- opiPresent(stim=makeStimHelper(xy[1], xy[2])(val,1), tt=vf$TT[loc], fpr=0.1, fnr=0.03)

            # Tell the GUI you presented (not essential if the GUI doesn't need to know)
        ShinyReceiver$push(g_msg$presentation, paste(xy[1], xy[2], val, r$seen))

        vf$NP[loc] <- vf$NP[loc] + 1

        if (vf$NP[loc] == 4) {
            unfinished <- unfinished[-loc_i]
            vf$Done[loc] <- TRUE
            vf$Value[loc] <- 666
                # Tell the GUI you finished this location. 
                # This is essential as the number of g_msg$loc_finished messages
                # needs to match the number sent back in g_msg$test_finished.
            ShinyReceiver$push(g_msg$loc_finished, paste(xy[1], xy[2], vf$NP[loc], vf$Value[loc]))
        }
    } else {
        Sys.sleep(0.5)  # Wait for pause to be over...
    }

        # Check the ShinyReceiver queue for paramters we might need and the GUI running state.
    s <- ShinySender$get(g_msg$state, block = FALSE) # Could be cancelled or paused
    if (!is.na(s))
        running_state <- s
    s <- ShinySender$get(g_msg$speed, block = FALSE)  # Other parameters that might have changed
    if (!is.na(s))
        speed <- s
}

    # Always send this at the end of the test, even if the message is "0"
    # The GUI will not finish the test until it has got at least this many
    # g_msg$loc_finished messages on ShinyReceiver.
ShinyReceiver$push(g_msg$test_finished, as.character(sum(vf$Done))) 


}, 
seed=TRUE,  # doesn't seem to work, hence my static_last_time_called thingie
globals= for_the_test,
packages = "OPI",
)
