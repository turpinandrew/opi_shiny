#
# A fake test_initialise just to demonstrate the GUI.
#
# Do whatever you need to do to initialise the test, and then set
#   rvs$run_state <- g_states$start_pause
#
# Access locations in rvs$vf and other data in rvs$for_the_test
#
# If there is an error, call the show_error function.
#
# Andrew Turpin
# Wed 31 Aug 2022 14:51:29 AEST
#

if (substr(rvs$for_the_test$machine, 1, 3) == "Sim" && ! "TT" %in% colnames(rvs$vf)) {
    show_error(paste("TT column is required in rvs$vf for", rvs$for_the_test$machine))
}

    # Setup OPI in the usual way
if (!chooseOPI(rvs$for_the_test$machine)) {
    show_error(paste("Invalide machine for OPI", rvs$for_the_test$machine))
}

    # opiInitialise in the usual way, signalling start and end with 
    # g_msg$initialise_state messages on ShinyReceiver so GUI knows 
    # what is happening.
res <- opiInitialise()
if (!is.null(res)) {
    show_error("opiInitialise failed")
}

rvs$unfinished <- as.list(seq_len(nrow(rvs$vf)))    # set up the list of unfinished locations

rvs$run_state <- g_state$start_pause