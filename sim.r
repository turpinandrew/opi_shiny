#
# Run simulations outside the GUI.
#
# Andrew Turpin
# Fri 17 Dec 2021 20:14:42 AEDT
#
require(OPI)
require(txtq)
require(promises)
require(future)
plan(multiprocess)
source('test_state.r')

zip2 <- function(...) unlist(mapply(list, ..., SIMPLIFY=FALSE), recursive=FALSE)

STATIC_LOCS_ALL <- data.frame(
  X=c(10,20,30,35,40,50,60,  7,14,21,25,28,35,   0, 0, 0, 0, 0,  -7,-14,-21,-25,-28,-35,  -10,-20,-30,-35,-40,-50,-60,  -7,-14,-21,-25,-28,-35,-42,   0,  0,  0,  0,  0,  0,  0,  7, 14, 21, 25, 28, 35, 42),
  Y=c( 0, 0, 0, 0, 0, 0, 0,  7,14,21,25,28,35,  10,20,30,35,40,   7, 14, 21, 25, 28, 35,    0,  0,  0,  0,  0,  0,  0,  -7,-14,-21,-25,-28,-35,-42, -10,-20,-30,-35,-40,-50,-60, -7,-14,-21,-25,-28,-35,-42),
Ecc=c(10,20,30,35,40,50,60, 10,20,30,35,40,50,  10,20,30,35,40,  10, 20, 30, 35, 40, 50,   10, 20, 30, 35, 40, 50, 60,  10, 20, 30, 35, 40, 50, 60,  10, 20, 30, 35, 40, 50, 60, 10, 20, 30, 35, 40, 50, 60)
)

ShinySender <- txtq(qf1 <- tempfile())
ShinyReceiver <- txtq(qf2 <- tempfile())

##############
# setup params
##############
pp <- NULL
fcount <- 0
for (i_x in 1:1) {
    pp <- c(pp, list(list(    
        reps=2,
        tt=rep(30, length(STATIC_LOCS_ALL$X)),
        fpr=0.1,
        fnr=0.03,
        machine="SimHenson", 
        locs=STATIC_LOCS_ALL,
        title="testing params"
    )))
    fcount <- fcount + 1
}

##############
# now go
##############
for (params in pp) {
    print(paste(Sys.time(), params$title))

    res <- NULL
    for (i_rep in 1:params$reps) {

        ShinySender$push(title=MSG_SPEED, "Sim")
        ShinySender$push(title=MSG_STATE, RUN_WAIT_FOR_INIT)

        size <- 0.43
        machine <- params$machine
        fpr <- params$fpr
        fnr <- params$fnr
        vf=data.frame(
            X=params$locs$X, 
            Y=params$locs$Y, 
            TT=params$tt, 
            Value=rep(0, length(params$locs$X)), 
            NP=rep(0, length(params$locs$X)),
            Done=rep(FALSE, length(params$locs$X))
        )

        source('static.r')

        then(test_static,
            onFulfilled = function(x) print("Done"), 
            onRejected = function(err) print(paste("Failed", err))
        )

        init_done <- "0"
        while (init_done != "1") {
            get_from_txtq(list(c("init_done", MSG_INITIALISE_STATE)), ShinyReceiver, block=FALSE)
            Sys.sleep(0.5)
        }
        ShinySender$push(title=MSG_STATE, RUN_START_PAUSE)
        # no pause needed here
        ShinySender$push(title=MSG_STATE, RUN_RUNNING)

        if (is.null(res))
            res <- data.frame(X=vf$X, Y=vf$Y, TT=vf$TT)

        while (sum(!vf$Done) > 0) {
            done <- ""
            get_from_txtq(list(c("done", MSG_LOC_FINISHED)), ShinyReceiver, block=FALSE)
            if (nchar(done) > 0) {
                s <- as.numeric(strsplit(done, " ")[[1]])
                ii <- which(vf$X == s[1] & vf$Y == s[2])
                vf$NP[ii] <- s[3]
                vf$Value[ii] <- s[4]
                vf$Done[ii] <- TRUE
            }
            Sys.sleep(0.01)
        }

        res <- cbind(res, vf$Value)
        res <- cbind(res, vf$NP)
    }
    colnames(res) <- c("X", "Y", "TT", unlist(zip2(paste0("MT.", 1:params$reps), paste0("NP.", 1:params$reps))))

    #save(res, params, TestEnv, file=paste0("output/sim.",params$fn, ".Rdata"))
}

print(Sys.time())

unlink(qf1)
unlink(qf2)
