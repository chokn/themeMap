# From http://rpubs.com/technocrat/  

# Load libraries
x <- c("dplyr", "ggmap", "ggplot2", "RColorBrewer", "rgdal", "classInt", "RCurl")
lapply(x, library, character.only = TRUE)

# function to remove OCONUS
remove.farflung = function(.df) {
  subset(.df,
         .df$id != "AS" &
           .df$id != "MP" &
           .df$id != "GU" &
           .df$id != "PR" &
           .df$id != "VI" &
           .df$id != "AK" &
           .df$id != "HI"
  )
}

save(remove.farflung, file = "helpers/remove.farflung")

# provides options to display mapped data in discrete categories

#need to recheck this function
intervals = function(.df, ...){
  argList = match.call(expand.dots=FALSE)$...
  for(i in 1:length(argList)){
    colName <- argList[[i]]
    series_colName = eval(substitute(coLName), envir = .df, enclos=parent.frame())
    min <- min(series_colName)
    max <- max(series_colName)
    diff <- max - min
    std <- sd(series_colName)
    equal.interval <- seq(min, max, by = diff/6)
    quantile.interval <- quantile(series_colName, probs = seq(0, 1, by = 1/6))
    std.interval <- c(seq(min, max, by = std), max)
    natural.interval <- classIntervals(series_colName, n = 6, style = 'jenks')$brks
    .df$equal <- cut(series_colName, breaks = equal.interval, include.lowest= TRUE)
    names(.df)[names(.df)=="equal"] <- paste(colName,".","equal", sep='')
    .df$quantile <- cut(series_colName, breaks = quantile.interval, include.lowest = TRUE)
    names(.df)[names(.df)=="quantile"] <- paste(colName,".", "quantile", sep='')
    .df$std <- cut(series_colName, breaks = std.interval, include.lowest = TRUE)
    names(.df)[names(.df)=="std"] <- paste(colName,".","std", sep = '')
    .df$natural <- cut(series_colName, breaks = natural.interval, include.lowest = TRUE)
    names(.df)[names(.df)=="natural"] <- paste(colName, ".","natural", sep = '')
  }
  return(.df)
}

save(intervals, file = "helpers/intervals")

#categorizes continuous data into ranges
bucket <- function(x,y) trunc(x/y)*y
save(bucket, file = "helpers/bucket")

# definitions to save typing
no_ylab = ylab("")
no_xlab = xlab("")

plain_theme = theme(axis.text = element_blank()) + theme(panel.background = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank())

poly = coord_map("polyconic")

# setInternet2(use = TRUE) # use Ineternet Explorer to access internet (because of firewall)
pop_source = "https://www.census.gov/popest/data/national/totals/2014/files/NST_EST2014_ALLDATA.csv"
# needed to add option b/c of certificate error or something
population = read.csv(textConnection(getURL(pop_source,.opts = list(ssl.verifypeer = FALSE))))[-(1:5),]

save(population, file = "Helpers/population")

dsn <- "data/cb_2014_us_state_5m"
layer <- "cb_2014_us_state_5m"
cb5 <- readOGR(dsn, layer)
save(cb5, file = "data/cb5")

us <- fortify(cb5, region = "STUSPS")
us48 <- remove.farflung(us)

pop <- data.frame(population$NAME,population$POPESTIMATE2014)
names(pop) <- c("state", "pop")

converter <- read.csv("csv/state_fips_postal.csv", header = FALSE)
converter <- rename(converter, state = V1, fips = V2, id = V3)

#merge pop and converter by state name
pop <- merge(pop, converter)

#discard uneeded columns
pop$state = NULL
pop$fips = NULL

# trim outlying states
pop48 <- remove.farflung(pop)

#add fields for alternative breaks of population categories
pop48 = intervals(pop48, pop)
