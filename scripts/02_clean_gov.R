#
# Clean the data
# 

# Set Directory
setwd(githubdir)

# Governor PID
ngagov <- read.csv("nga/data/governor.csv", header = T)

# Splitting start term, end term data into respective columns
# Get the max. number of terms gov. ruled in the data
max(sapply(strsplit(ngagov$time.in.office, '\n\n'), length))

# Create max number (4) new cols then
term.names <- paste0(rep(c("start.term", "end.term"), 4), gl(4, 2))
ngagov[, term.names] <- NA

# take out parentheses, and \n, and substitute missing values with NA 
a <- lapply(strsplit(ngagov$time.in.office, '\n\n'), 
	        function(x) {
              temp <- gsub("\\(|)|\\n", "", x)
              gsub("-$", "-NA", temp)
            }
           )
b <- lapply(a, strsplit, "-")
r <- lapply(b, unlist)

for(i in 1:nrow(ngagov)) {
  ngagov[i, 25:(24 + length(r[[i]]))] <- r[[i]]
}

# Convert all string years to numeric
ngagov[, 25:32] <- lapply(ngagov[, 25:32], as.numeric)

# If there are two start terms with corresponding end terms as NA, investigate
boolv <- complete.cases(cbind(ngagov$start.term1, ngagov$start.term2)) & is.na(ngagov$end.term1) & is.na(ngagov$end.term2)
temp  <- ngagov[boolv, ]
# temp$start.term2 - temp$start.term1

# So it appears we need to move start.term2 to start.term1, and nuke start.term2
ngagov[boolv, ]$start.term1 <- ngagov[boolv, ]$start.term2
ngagov[boolv, ]$start.term2 <- NA

# weirdness 2: some issue with start.term1 that doesn't end and start term2 that ends 
boolt <- complete.cases(cbind(ngagov$start.term1, ngagov$start.term2)) & is.na(ngagov$end.term1) & !is.na(ngagov$end.term2)
temp  <- ngagov[boolt, ]
# temp$start.term2 - temp$start.term1
# 0 year diff. - use start.term2 and end.term2 obviously
# for one year diff. (start.term2 is later than start.term1 by 1 year), start.term2 is right 
# checked for two year diff. also - start.term2 and end.term2 are the right ones
# temp$start.term1[temp$start.term1 - temp$start.term2 ==-1]
# Fix the start term and the end term
ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(0, -1, -2),]$start.term1 <-  ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(0, -1, -2),]$start.term2 
ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(0, -1, -2),]$end.term1   <-  ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(0, -1, -2),]$end.term2 
ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(0, -1, -2), c("start.term2", "end.term2")] <-  NA 

# Three years reflects second half term, 4 years reflects typically 2 terms, 7 - 3 terms
ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(2, 3, 4, 6, 7) & ngagov$start.term1 < ngagov$end.term2,]$end.term1 <-  ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(2, 3, 4, 6, 7) & ngagov$start.term1 < ngagov$end.term2,]$end.term2 
ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(2, 3, 4, 6, 7) & ngagov$start.term1 < ngagov$end.term2,]$end.term2 <-  ngagov[boolt & (ngagov$start.term1 - ngagov$start.term2) %in% c(2, 3, 4, 6, 7) & ngagov$start.term1 < ngagov$end.term2,]$start.term1 -1

# if start.term1 > start.term3 and end.term1 is NA and end.term3 is not, and is > start.term1
boolt2 <- complete.cases(cbind(ngagov$start.term1, ngagov$start.term2)) & is.na(ngagov$end.term1) & !is.na(ngagov$end.term3)
ngagov[boolt2 & ngagov$end.term3 > ngagov$start.term1,]$end.term1 <- ngagov[boolt2 & ngagov$end.term3 > ngagov$start.term1,]$end.term3 

# if start.term1 is within one year of start.term2, and if end.term1==end.term2, then 
# start.term1 <- start.term2, and nuke start and end term2
boolt3 <- complete.cases(cbind(ngagov$start.term1, ngagov$start.term2, ngagov$end.term1, ngagov$end.term2)) & (ngagov$start.term2 - ngagov$start.term1) == 1 & ngagov$end.term1 == ngagov$end.term2
ngagov[boolt3, ]$start.term1 <- ngagov[boolt3, ]$start.term2 
ngagov[boolt3, ]$end.term2 <-  ngagov[boolt3, ]$start.term2  <- NA

# if start.term2-start.term1==1 is within one year of start.term2, and if end.term2 - end.term1==1, then 
# start.term1 <- start.term2, and nuke start and end term2
boolt4 <- complete.cases(cbind(ngagov$start.term1, ngagov$start.term2, ngagov$end.term1, ngagov$end.term2)) & (ngagov$start.term2 - ngagov$start.term1) == 1 & (ngagov$end.term2 - as.numeric(ngagov$end.term1)) == 1
ngagov[boolt4, ]$start.term1 <- ngagov[boolt4, ]$start.term2 
ngagov[boolt4, ]$end.term1 <- ngagov[boolt4, ]$end.term2 
ngagov[boolt4, c("end.term2", "start.term2")]  <- NA

# Retail Fixes
# ----------------

# Bill Clinton
ngagov[ngagov$name == "Gov. William Jefferson Clinton", "end.term1"] <- 1992

# Frank O'Bannon (died 2003)
ngagov[ngagov$name == "Gov. Frank  O'Bannon", "end.term1"] <- 2003

# Sewall (1941-1945)
ngagov[ngagov$name == "Gov. Sumner  Sewall", "start.term1"] <- 1941
ngagov[ngagov$name == "Gov. Sumner  Sewall", "end.term1"]   <- 1945

# James McGreevey
ngagov[ngagov$name == "Gov. James E. McGreevey","end.term1"]   <- 2004

# Richard Codey (2004 - 2006), 3 days in 2002, nuke that
ngagov[ngagov$name == "Gov. Richard J. Codey", "start.term1"] <- 2004
ngagov[ngagov$name == "Gov. Richard J. Codey", "end.term1"]   <- 2006
ngagov[ngagov$name == "Gov. Richard J. Codey", c("start.term2", "end.term2")] <- NA

# Mike Johanns
ngagov[ngagov$name == "Gov. Mike  Johanns", "end.term1"]   <- 2005

# Gov. Dirk  Kempthorne"
ngagov[ngagov$name == "Gov. Dirk  Kempthorne", "end.term1"]   <- 2006

# Gov. Michael Okerlund Leavitt
ngagov[ngagov$name == "Gov. Michael Okerlund Leavitt", "end.term1"]   <- 2003

# Order start.terms
# start.term2, start.term3, and start.term 4 are in ascending order
# unique(ngagov$start.term2 - ngagov$start.term3)
# ngagov$start.term3[!is.na(ngagov$start.term4) & ngagov$start.term3 - ngagov$start.term4 < 0]
# not so for start.term1 and 2
# ngagov$start.term2[!is.na(ngagov$start.term2) & ngagov$start.term1 - ngagov$start.term2 > 0]

# Change to date object
ngagov$startoffice  <- as.Date(ngagov$startoffice, "%b %d, %Y")
ngagov$endoffice    <- as.Date(ngagov$endoffice, "%b %d, %Y")
ngagov$startmonth   <- months(ngagov$startoffice)
ngagov$endmonth     <- months(ngagov$endoffice)
ngagov$startyear    <- as.POSIXlt(ngagov$startoffice)$year + 1900
ngagov$endyear      <- as.POSIXlt(ngagov$endoffice)$year + 1900

# Months As numbers
ngagov$startmonth.n <- match(ngagov$startmonth, month.name) 
ngagov$endmonth.n   <- match(ngagov$endmonth, month.name) 

# Get the state two digit code
stnum <- read.csv("nga/data/state_num.csv")
ngagov$statecode <- stnum$code[match(ngagov$state, stnum$state)]
