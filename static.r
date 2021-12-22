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
    #   (a) send a MSG_TEST_ERROR on ShinyReceiver and 
    #   (b) use stop() to throw the problem back to the GUI
    #
test_static <- future({
require(OPI)
source('test_state.r')
set.seed(as.numeric(rev(as.character(static_last_time_called))))

    # Check for any particular variables needed not listed in globals.
for (cc in c("X", "Y", "NP", "Done")) {
    if (! cc %in% colnames(vf)) {
        ShinyReceiver$push(MSG_TEST_ERROR, sprintf("Missing $s in vf",v))
        stop(sprintf("Missing $s in vf",v))
    }
}
if (substr(machine,1,3) == "Sim" && ! "TT" %in% colnames(vf)) {
    ShinyReceiver$push(MSG_TEST_ERROR, "Missing TT in vf")
    stop("Missing TT in vf")
}

    # Get any information needed from the GUI queue not passed as 
    # globals (ie can change during the test)
speed <- SPEEDS[[1]]
get_from_txtq(list(c("speed", MSG_SPEED)), ShinySender, block=FALSE)

    # Setup OPI in the usual way
if (!chooseOPI(machine)) {
    ShinyReceiver$push(MSG_TEST_ERROR, "Bad machine in chooseOPI")
    stop('Invalide machine for OPI')
}

    # opiInitialise in the usual way, signalling start and end with 
    # MSG_INITIALISE_STATE messages on ShinyReceiver so GUI knows 
    # what is happening.
ShinyReceiver$push(MSG_INITIALISE_STATE, RUN_STOPPED)
res <- opiInitialise()
if (!is.null(res)) {
    ShinyReceiver$push(MSG_TEST_ERROR, "opiInitialise failed")
    stop('opiInitialise failed')
}
ShinyReceiver$push(MSG_INITIALISE_STATE, RUN_WAIT_FOR_INIT)

    # Test needs to wait for GUI to give the green light as a message on ShinySender.
    # Note that we could get a variety of states from GUI
    # due to pausing, running or cancelling.
running_state <- RUN_WAIT_FOR_INIT
while (running_state == RUN_WAIT_FOR_INIT) {
    get_from_txtq(list(c("running_state", MSG_STATE)), ShinySender, block=FALSE)
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
    # we need to check that the GUI is not in RUN_STOPPED or RUN_FINALISING states.
    # If it is, it means that the test has been cancelled from the GUI.
    # We also need to not present anything while the GUI is in a RUN_PAUSE state.
    # As such, we need to check the ShinySender queue for MSG_STATE quite often.
    #
unfinished <- as.list(1:nrow(vf))
start_time <- Sys.time()
start_pres_time <- startTime <- Sys.time()
while (running_state != RUN_STOPPED && running_state != RUN_FINALISING && length(unfinished) > 0) {
    if (speed == SPEEDS[[1]]) Sys.sleep(0.1)
    if (speed == SPEEDS[[2]]) Sys.sleep(1.0)

    if (running_state == RUN_RUNNING) {  # Could be paused, remember...
        loc_i <- sample.int(length(unfinished), 1)
        loc <- unfinished[[loc_i]]

        xy <- c(vf$X[loc], vf$Y[loc])
        val <- round(runif(1, 0, 40))
        r <- opiPresent(stim=makeStimHelper(xy[1], xy[2])(val,1), tt=vf$TT[loc], fpr=0.1, fnr=0.03)

            # Tell the GUI you presented (not essential if the GUI doesn't need to know)
        ShinyReceiver$push(MSG_PRESENTATION, paste(xy[1], xy[2], val, r$seen))

        vf$NP[loc] <- vf$NP[loc] + 1

        if (vf$NP[loc] == 4) {
            unfinished <- unfinished[-loc_i]
            vf$Done[loc] <- TRUE
            vf$Value[loc] <- 666
                # Tell the GUI you finished this location. 
                # This is essential as the number of MSG_LOC_FINISHED messages
                # needs to match the number sent back in MSG_TEST_FINISHED.
            ShinyReceiver$push(MSG_LOC_FINISHED, paste(xy[1], xy[2], vf$NP[loc], vf$Value[loc]))
        }
    } else {
        Sys.sleep(0.5)  # Wait for pause to be over...
    }

        # Check the ShinyReceiver queue for paramters we might need and the GUI running state.
    get_from_txtq(list(
        c("running_state", MSG_STATE),   # Could be canceled or paused
        c("speed", MSG_SPEED)            # Other parameters that might have changed
    ), ShinySender, block=FALSE)
}

    # Always send this at the end of the test, even if the message is "0"
    # The GUI will not finish the test until it has got at least this many
    # MSG_LOC_FINISHED messages on ShinyReceiver.
ShinyReceiver$push(MSG_TEST_FINISHED, as.character(sum(vf$Done))) 


}, 
seed=TRUE,  # doesn't seem to work, hence my static_last_time_called thingie
globals=list(
    machine=machine, 
    size=size, 
    fpr=fpr, 
    fnr=fnr, 
    vf=vf, 
    ShinySender=ShinySender, 
    ShinyReceiver=ShinyReceiver, 
    static_last_time_called=static_last_time_called)
)
