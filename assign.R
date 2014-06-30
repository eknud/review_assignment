# Code to assign reviewers to applications.
# Gregor Passolt, June 2014

# Goal is to read in a CSV with 3 columns:
#  - application id
#  - community college (CC)
#  - senior college (SC)
# And to then assign 2 reviewing institutions to each application.
# Applications should not be reviewed by either the CC or SC.
# An even distribution of review work is desired.
# There are fewer SC reviewers, so we should avoid doubling them
# up on a single application.

# Overview:
# Assign reviewers randomly, then replace over-reviewers with under-reviewers
# until equilibrium is met.

#------------------------
# Control parameters:
#------------------------
filepath <- "choose"
tolerance <- 2 
max.tries <- 5e6
seed <- NULL

# filepath -- Set to "choose" to select the CSV file to read in.
# (Note that RStudio sometimes crashes with this.)
# Otherwise set it to the absolute filepath of the CSV,
# e.g. "C:/My Documents/applications.csv"
# Note that forward slashes should be used in the filepath even on Windows.

# tolerance -- set this to the maximum allowable difference in number of 
# reviews between the mos-worked and least-worked institutions.
# Must be > 0.

# seed -- set to a positive integer to set the random seed so that the
# same input will produce the same output. Probably you should just leave it
# as NULL.

# max.tries -- the maximum number of iterations of the loop. To prevent 
# an infinite loop.

#------------------------

if (!require(stringr)) install.packages("stringr")
require(stringr)

if (!is.null(seed)) set.seed(seed)

raw <- read.csv(
    file = ifelse(filepath == "choose", file.choose(), filepath),
    stringsAsFactors = FALSE)

# Some input checking
if (ncol(raw) > 3) {
    warning("More than 3 column detected, dropping extras.")
    raw <- raw[, 1:3]
}
original.names <- names(raw)
names(raw) <- c("id", "cc", "sc")
raw$cc <- str_trim(raw$cc)
raw$sc <- str_trim(raw$sc)

cc.all <- c("BMCC", "Bronx", "Guttman", "Hostos",
            "KBCC", "LaGuardia", "QBCC")
sc.all <- c("Brooklyn", "City", "Hunter", "Lehman", "Queens")
sc.counts <- table(raw$sc)
cc.counts <- table(raw$cc)

if (any(! raw$cc %in% cc.all)) {
    problem.rows <- which(! raw$cc %in% cc.all)
    err <- paste0("Some of the community colleges are not recognized.\n",
                 "Expected inputs:\n",
                 paste(cc.all, collapse = ", "),
                 "\nProblems in row(s) ",
                 paste(problem.rows, collapse = ", "))
    stop(err)
}

if (any(! raw$sc %in% sc.all)) {
    problem.rows <- which(! raw$sc %in% sc.all)
    err <- paste0("Some of the senior colleges are not recognized.\n",
                  "Expected inputs:\n",
                  paste(sc.all, collapse = ", "),
                  "\nProblems in row(s) ",
                  paste(problem.rows, collapse = ", "))
    stop(err)
}

# The first reviewer can be either CC or SC, the second reviewer will always
# be SC.

# For first reviewer, we'll intially assign only SC reviewers
# The second reviewer will be initialized as CC only
# In both cases, the assignments are weighted by the applications.
# That is, if there's a whole lot of applications to Hunter,
# then there are few applications to other schools,
# so we'll make them more likely to get a Hunter reviewer (initially).
reviewer.assign.1 <- function(sc, n) {
    sample(setdiff(sc.all, sc), size = n, replace = T,
           prob = sc.counts[! names(sc.counts) %in% sc])
}

reviewer.assign.2 <- function(cc, n) {
    sample(setdiff(cc.all, cc), size = n, replace = T,
           prob = cc.counts[! names(cc.counts) %in% cc])
}

# Initialize reviewer columns
raw$r1 <- NA
raw$r2 <- NA

for (sc.i in sc.all) {
    raw$r1[raw$sc == sc.i] <- reviewer.assign.1(sc.i, sc.counts[sc.i])
}

for (cc.i in cc.all) {
    raw$r2[raw$cc == cc.i] <- reviewer.assign.2(cc.i, cc.counts[cc.i])
}

#----------------------

# Now that the initial assignments are done, we need a test statistic
good_yet <- function(dat) {
    counts <- table(c(dat$r1, dat$r2))
    counts <- counts[order(counts)]
    diff <- tail(counts, 1) - head(counts, 1)
    return(list(counts = counts, diff = diff))
}
# The diff is the max reviewer count minus the min reviewer count

status <- good_yet(raw)
counter <- 0
while (status$diff > tolerance) {
    if (counter > max.tries) stop("Failed. Try again. If you see this a lot, email Gregor")
    to.change <- names(status$diff)
    # We'll change a random entry with the most common reviewer
    to.change.i <- sample(which(c(raw$r1, raw$r2) == to.change), size = 1)
    to.change.row <- to.change.i - ifelse(to.change.i > nrow(raw), nrow(raw), 0)
    to.change.col <- ifelse(to.change.i > nrow(raw), 5, 4)
    
    # To an under-represented reviewer
    change.options <- status$counts[1:6]
    # Can't change to something already in the row:
    change.options <- change.options[! names(change.options) %in% raw[to.change.row, 2:5]]
    # Can't change to a SC in the second column:
    if (to.change.col == 5) change.options <- setdiff(names(change.options), sc.all) 
    if (length(a) == 0) { # If we got an unlucky draw, let's try again
        counter <- counter + 1
        next
    }
    change.to <- sample(names(change.options), size = 1, prob = change.options)
    raw[to.change.row, to.change.col] <- change.to
    
    # Then recalculate the status
    status <- good_yet(raw)
    
    # Counter, just to reassure us it's still going
    # in case it takes a long time.
    counter <- counter + 1
    if (counter %% 100 == 0) message(paste("Starting iteration", counter))
}


## Final Check
if (with(raw, any(cc == r1 | cc == r2 | sc == r1 | sc == r2 | r1 == r2))) {
    stop("Illegal assignments made :(  try again.")
}

names(raw) <- c(original.names, "Reviewer 1", "Reviewer 2")
write.csv(x = raw,
          file = "assigned_reviewers.csv",
          row.names = FALSE)
