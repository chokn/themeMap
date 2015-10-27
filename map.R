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
    series_colName = eval(substitute(colName), envir = .df, enclos=parent.frame())
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

map48 <- merge(us48, pop48)

#plotting the map
b <- ggplot(data = map48, aes(x=long, y=lat, group = group))

d <- b + geom_polygon(aes(x=long, y=lat, group = group), color = "dark grey", size = 0.3)

# Change default fill to white. add map projection
w <- d + geom_polygon(aes(x=long, y = lat, group = group), color = "dark grey", size = 0.3, fill = "white")
w

p = w + poly
p

# title and stuff
my_title <- "Population map of 48 contiguous states andd the District of Columbia,\ndefault settings"
sources <- "Source: U.S. Census Bureau 2014 Population Estimates http://1.usa.gov/1LPM7hc\n and Cartographic Boundary file (States, cb_2014_us_state_5m) http://1.usa.gov/1IZgFLV\n Prepared by: @technocrat 2015-07-15"

# Add population data to map
p <- b + geom_polygon(color = "dark grey", size = 0.3, aes(fill = pop)) + ggtitle(my_title) + annotate("text", x = -100, y = 22, label = sources, size = 3, family = "Times", color = "black")
p

#remove background and axis legends, add projection
my_title <- "Population Map of 48 Contiguous States and District of Columbia,\n no background or axis legends, polyconic projection"
p +  plain_theme + no_ylab + no_xlab + poly + ggtitle(my_title) + annotate("text", x = -100, y = 22, label = sources,  size = 3, family = "Times", color = "black")

# Disrete scale
buckets = bucket(pop48$pop, 5000000)
popb = data.frame(pop48$id, buckets)
names(popb) = c("id", "buckets")
map48b <- merge(us48, popb)

my_title <- "Population Map of 48 Contiguous States and District of Columbia,\n manual discrete scale, default colors"
m <- b + geom_polygon(color = "dark grey", size = 0.3, aes(fill = factor(map48b$buckets)))

m + plain_theme + no_xlab + no_ylab + poly + ggtitle(my_title) + annotate("text", x = -100, y = 22, label = sources, size = 3, family = "Times", color = "black")

#different color scheme (graded neutral)

my_title = "Population Map of 48 Contiguous States and District of Columbia,\n manual discrete scale, graded neutral colors"
m + plain_theme + no_xlab + no_ylab + poly + ggtitle(my_title) + annotate("Text", x = -100, y = 22, label = sources, size = 3, family = "Times", color = "black") + scale_fill_brewer(type = "seq", palette = "YlOrBr")

# better legend
my_title = "Population Map of 48 Contiguous States and District of Columbia,\nwith improved legend"
m + plain_theme + no_xlab + no_ylab + poly + ggtitle(my_title) + annotate("text", x = -100, y = 22, label = sources, size = 3, family = "Times", color = "black") + scale_fill_brewer(type = "seq", palette = "YlOrBr", name = "State Population", labels=c("Under 5 million", "5-10 million", "10-15 million", "15-20 million", "20-25 million", "more than 35 million"))

# with equal interval scale
my_title = "Population Map of 48 Contiguous States and District of Columbia,\nwith equal interval scale"
e <- b + geom_polygon(color = "dark grey", size = 0.3, aes(fill = pop.equal)) + ggtitle(my_title) + annotate("text", x = -100, y = 22, label = sources, size = 3, family = "Times", color = "black") + plain_theme + no_xlab + no_ylab + poly + scale_fill_brewer(type = "seq", palette = "YlOrBr", name = "State Population", labels = c("Under 6.95 million", "6.95 - 13.3 million", "19.7 - 26.1 million", "26.1 - 32.4 million", "34.2-38.8 million"))
e                                                                                                             

# quantile interval
my_title = "Population Map of 48 Contiguous States and District of Columbia,\nwith quantile interval scale"
q <- b + geom_polygon(aes(x = long, y = lat, group = group, fill = pop.quantile), color = "dark grey", size = 0.3) 
q <- q + plain_theme + no_ylab + no_xlab + poly + scale_fill_brewer(palette = "YlOrBr", name = "State Population", labels=c("Under 1.33 million","1.33-2.94 million","2.94-4.65 million","4.65-6.55  million","6.55-9.94 million", "9.94-38.8 million")) + ggtitle(my_title)
q

#standard deviation scale
my_title = "Population Map of 48 Contiguous States and District of Columbia,\nwith standard deviation scale"
s = b + geom_polygon(aes(x=long, y=lat, group=group, fill=pop.std), color = "dark grey", size = 0.3)
s = s + plain_theme + no_ylab + no_xlab + poly + scale_fill_brewer(palette = "YlOrBr", name = "State Population", labels=c("Under 7.77 million","7.77-15 million","15-22.2 million","22.2-29.3 million","36.5-38.8 million")) + ggtitle(my_title)
s

#natural scale
my_title = "Population Map of 48 Contiguous States and District of Columbia,\nwith natural scale"
n = b + geom_polygon(aes(x=long, y=lat, group=group, fill=pop.natural), color = "dark grey", size = 0.3)
n = n + plain_theme + no_ylab + no_xlab + poly + scale_fill_brewer(palette = "YlOrBr", name = "State Population", labels=c("Under 3.6 million","3.6-7.06 million","7.06-12.9 million","12.9-19.9 million","19.9-27 million","27-38.8 million")) + ggtitle(my_title)
n
