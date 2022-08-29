#
# A wrapper for txtq that has a local cache
# to avoid multiple calls to the underlying file use (slow)
#
# Andrew Turpin
# Wed  3 Aug 2022 07:01:38 AEST

packs <- c("txtq", "R6")
for (p in packs) {
    if (!require(p, character.only = TRUE)) {
        install.packages(p, repos = "https://cloud.r-project.org")
        library(p, character.only = TRUE)
    }
}

CommQueue <- R6Class("CommQueue", # nolint
    public = list(
    initialize = function(name, debug = FALSE) {
        private$name <- name
        private$debug <- debug
        private$queue <- txtq(tempfile())
        private$cache <- NULL
    },

    # First look in the cache.
    # If it is there, great, return it and delete from cache.
    # If it is not there
    #   If the queue is blocking, then just read queue into cache and try again
    #   If the queue is !blocking, read queue into cache and try one more time.
    #
    # @param title Message title to get
    # @param blocking If TRUE, loop until title found, else abandon if one not found
    # @returns Message related to title found or NA if no title and not blocking
    get = function(title, blocking = FALSE) {
        result <- NA
        state <- "1st Look"
        while (TRUE) {
            if (private$debug) {
                cat(private$name, ": gettin ", title, " block= ", blocking, "\n")
                flush.console()
            }

            if (!is.null(private$cache) && nrow(private$cache) > 0) {
                ii <- which(private$cache$title == title)
                if (length(ii) > 0) {
                    ii <- head(ii, 1)
                    result <- private$cache$message[[ii]]
                    if (nrow(private$cache) == 0) {
                        private$cache <- NULL
                    } else {
                        private$cache <- private$cache[-ii, ]
                    }
                    return(result)
                }
            }

            if (blocking || state == "1st Look")
                private$cache <- rbind(private$cache, private$queue$pop(private$queue$count()))

            if (blocking) {
                Sys.sleep(0.3)   # let's wait for more
            } else {
                if (state == "2nd Look")
                    return(NA)
                state <- "2nd Look"
            }
        }
    },

        # do not push to cache as others might be reading this queue
    push = function(title, msg) {
        private$queue$push(title, msg)
        if (private$debug) {
            cat(private$name, ": push ", title, " ", msg, "\n")
            flush.console()
        }
    },

    pop = function() {
        if (private$debug) {
            cat(private$name, ": pop \n")
            flush.console()
        }
        if (nrow(private$cache) > 0) {
            result <- private$cache$message[1]
            private$cache <- private$cache[-1, ]
            return(result)
        } else {
            return(private$queue$pop())
        }
    },

    count = function() private$queue$count() + ifelse(is.null(private$cache), 0, nrow(private$cache)),

    empty = function() private$queue$empty() && is.null(private$cache),

    reset = function() {
        if (private$debug) {
            cat(private$name, ": RESET\n")
            flush.console()
        }
        private$queue$reset()
        private$cache <- NULL
    },

    rep = function() {
        s <- "\nCached"
        s <- c(s, capture.output(print(private$cache)))
        s <- c(s, "\nQueued")
        s <- c(s, capture.output(print(private$queue$list())))
    },

    set_name = function(name) private$name <- name
    ), # end public

    private = list(
        name = NA,
        debug = NA,
        queue = NA,
        cache = NA
    )
)


if (exists("TESTING_ON") && TESTING_ON) {
    library(tinytest)
    q <- CommQueue$new("Test Queue", FALSE)

    sapply(1:5, function(i) q$push("A", i))

    for (msg in 1:3)
        if (!(ee <- expect_equal(q$get("A", TRUE), as.character(msg))))
            print(ee)

    sapply(20:25, function(i) q$push("A", i))

    if (!(ee <- expect_true(!q$empty()))) print(ee)

    for (msg in c(4, 5, 20, 21))
        if (!(ee <- expect_equal(q$get("A", TRUE), as.character(msg))))
            print(ee)

    if (!(ee <- expect_equal(q$count(), 4))) print(ee)

    sapply(31:35, function(i) q$push("B", i))

    if (!(ee <- expect_equal(q$count(), 9))) print(ee)

    if (!(ee <- expect_equal(q$get("A", TRUE), "22"))) print(ee)
    if (!(ee <- expect_equal(q$get("A", TRUE), "23"))) print(ee)
    if (!(ee <- expect_equal(q$get("B", TRUE), "31"))) print(ee)
    if (!(ee <- expect_equal(q$get("A", TRUE), "24"))) print(ee)
    if (!(ee <- expect_equal(q$get("B", TRUE), "32"))) print(ee)
    if (!(ee <- expect_equal(q$get("A", TRUE), "25"))) print(ee)
    if (!(ee <- expect_equal(q$get("B", TRUE), "33"))) print(ee)

    s <- q$rep()

    if (!(ee <- expect_equal(q$get("B", TRUE), "34"))) print(ee)

    if (!(ee <- expect_true(!q$empty()))) print(ee)

    q$reset()
    if (!(ee <- expect_true(q$empty()))) print(ee)
    if (!(ee <- expect_equal(q$count(), 0))) print(ee)

    cat("--------------- After tests ------------- B 34 and 35\n")
    print(s)
    cat("\n")
    # expect B 34 and 35
}