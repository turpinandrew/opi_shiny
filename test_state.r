#
# Constants that are used to cintrol GUI (RUN_*) and communication MSG_*.
# Also has utility function get_from_txtq for reading txtq.
#
# Andrew Turpin
# Fri 17 Dec 2021 16:35:36 AEDT
#

    # Running states for GUI
    # These are the assumed possible states of an OPI test
g_state <- list(
    stopped = "0",
    wait_for_init = "1",
    start_pause = "2",
    running = "3",
    pause   = "4",
    finalising   = "5",
    showing_error   = "6"
)

    # Message titles for communication
    # Should allow for anything that can change in the GUI that affects the test (eg pause, cancel, speed)
    # and any information the GUI needs from the test during its execution (eg presentation, error).
g_msg <- list(
    test_error = "E",         # msg is description
    loc_finished = "F",       # msg is "X Y NP Value"
    initialise_state = "I",   # msg is 0 for not finished, 1 for done
    speed = "P",              # msg is one of SPEEDS
    presentation = "Q",       # msg is "X Y Value"
    state = "S",              # msg is one of RUN_*
    test_finished = "T"       # msg is number of locations that were completed
)

    # Test specific constants
g_speeds <- c("Normal", "Slow")
g_sizes <- c("III", "V")
