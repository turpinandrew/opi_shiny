#
# A fake test just to demonstrate the GUI.
#
#
# Andrew Turpin
# Wed 31 Aug 2022 14:59:21 AEST
#

makeStimHelper <- function(x, y) {  # returns a function of (db,n)
    ff <- function(db, n) db+n
    body(ff) <- substitute({
        s <- list(x=x, y=y, level=dbTocd(db), size = rvs$for_the_test$size,
                duration=200, responseWindow=1500, checkFixationOK=NULL)
        class(s) <- "opiStaticStimulus"
        return(s)
    }, list(x=x,y=y))
    return(ff)
}


    #
    # The main test loop
    #
    # Note that in addition to the test logic (in this case length(unfinished) > 0) 
    # we need to check that the GUI is not in g_state$stopped or g_state$finalising states.
    # If it is, it means that the test has been cancelled from the GUI.
    # We also need to not present anything while the GUI is in a g_state$pause state.
    #


if (rvs$run_state == g_state$running && length(rvs$unfinished) > 0) {
    vf <- isolate(rvs$vf)

    loc_i <- sample.int(length(rvs$unfinished), 1)
    loc <- rvs$unfinished[[loc_i]]

    xy <- c(vf$X[loc], vf$Y[loc])
    val <- round(runif(1, 0, 40))
    vf$Value[loc] <- val

    r <- opiPresent(stim = makeStimHelper(xy[1], xy[2])(val, 1), tt = vf$TT[loc], fpr=0.1, fnr=0.03)
    Sys.sleep(0.0) # just to allow GUI to update a bit

    vf$NP[loc] <- vf$NP[loc] + 1

    if (vf$NP[loc] == 4) {
        rvs$unfinished <- rvs$unfinished[-loc_i]
        vf$Done[loc] <- TRUE
        vf$Value[loc] <- 666
    }

    rvs$vf <- vf
}

if (length(rvs$unfinished) == 0)
    rvs$run_state <- g_state$finalising
