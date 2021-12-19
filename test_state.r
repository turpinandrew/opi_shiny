#
# Constants that are used to cintrol GUI (RUN_*) and communication MSG_*.
# Also has utility function get_from_txtq for reading txtq.
#
# Andrew Turpin
# Fri 17 Dec 2021 16:35:36 AEDT
#

    # Running states for GUI
    # These are the assumed possible states of an OPI test
RUN_STOPPED <- "0"
RUN_WAIT_FOR_INIT <- "1"
RUN_START_PAUSE <- "2"
RUN_RUNNING <- "3"
RUN_PAUSE   <- "4"
RUN_FINALISING   <- "5"
RUN_SHOWING_ERROR   <- "6"

    # Message titles for communication
    # Should allow for anything that can change in the GUI that affects the test (eg pause, cancel, speed)
    # and any information the GUI needs from the test during its execution (eg presentation, error).
MSG_TEST_ERROR <- "E"         # msg is description
MSG_LOC_FINISHED <- "F"       # msg is "X Y NP Value"
MSG_INITIALISE_STATE <- "I"   # msg is 0 for not finished, 1 for done
MSG_SPEED <- "P"              # msg is one of SPEEDS
MSG_PRESENTATION <- "Q"       # msg is "X Y Value"
MSG_STATE <- "S"              # msg is one of RUN_*
MSG_TEST_FINISHED <- "T"      # msg is number of locations that were completed

    # Test specific constants
SPEEDS <- c("Normal", "Slow")
SIZES <- c("III", "V")


###########################
# Assign variables in the parent.frame to messages in q from the parent.frame 
# with matching titles.
# Will loop forever until all of the variables in toget are assigned if block is TRUE.
#
# @param toget is a list of string pairs (variable name, message title)
# @param q     A txtq
# @param block If TRUE, loop until all toget satisfied, else abandon if one not found
###########################
get_from_txtq <- function(toget, q, block) {
    repeat {
#print(paste('gettin', toget))
        titles <- unlist(lapply(toget, "[", 2))
        if (!q$empty()) {
            d <- q$pop(q$count()) # get whole queue
            
            row_nums <- match(titles, d$title)
            
                # assign variables
            for (i_tit in which(!is.na(row_nums)))
                assign(toget[[i_tit]][[1]], d$message[[row_nums[[i_tit]]]], env=parent.frame())
            
            done <- row_nums[!is.na(row_nums)]
            
                # put back unused d
            for (rr in setdiff(1:nrow(d), done))
                q$push(d$title[[rr]], d$message[[rr]])  
            
                # throw out successes
            toget <- toget[-done]
        }
        
        if (!block || length(toget) == 0)
            break

        Sys.sleep(0.1)   # let's wait for next ones
    }
}
