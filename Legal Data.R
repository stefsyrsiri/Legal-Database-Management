legal <- read.csv("https://query.data.world/s/5tpkrphqrkh5chf6okk4d2zi2tuqyo", header = TRUE, stringsAsFactors = FALSE)
str(legal)
attach(legal)


#--------1----------------------------------------------------------------------
Incident.Date <- as.POSIXlt(Incident.Date, tz = "GMT","%m/%d/%Y %I:%M:%S %p")
head(Incident.Date,10); class(Incident.Date)


#--------2----------------------------------------------------------------------
range(Incident.Date)

days <- as.numeric(max(Incident.Date) - min(Incident.Date))
months <- round(days/30)
hours <- days*24
cat("The total time period of recorded incidents is", 
    months, "months or", days, "days or", hours, "hours.")


#--------3----------------------------------------------------------------------
median(Incident.Date)


#--------4----------------------------------------------------------------------
Incident.Date[round(length(Incident.Date)/2)]


#--------5----------------------------------------------------------------------
which.max(table(factor(Incident.Date)))
table(factor(Incident.Date))["2014-08-28"]


#--------6----------------------------------------------------------------------
ampm <- factor(ifelse(Incident.Date$hour < 12, "AM","PM"))
ampm

levels(ampm)[2] <- "PM"
levels(ampm)
which.max(table(ampm))
which.max(table(Department, ampm))
rownames(table(Department, ampm))[3]
addmargins(table(Department, ampm))
round(addmargins(prop.table(table(Department, ampm)))*100,1)


#--------7----------------------------------------------------------------------
Department <- factor(Department)
levels(Department)[1] <- "WPD"
table <- sort((table(Department, useNA = "ifany")))
names(dimnames(table)) <- "Department"

length(table)
addmargins(table)

par(mar = c(7,15,10,5), bg="linen")
barplot(table,
    #orientation
        horiz = TRUE,
        las = 1,
        xlim = c(0, max(table(Department))),
        ylim = range(pretty(c(0, Department))),
    #text
        main = "Recorded incidents\nper Department",
        sub = "City of Austin, 2011 - 2015",
    #size
        cex.main = 1,
    #colors
        col.axis = "gray15",
        col.main = "gray15",
        col.sub = "gray15",
        col = "gray",
        border = "gray"
        )

grid(nx = NULL, ny = NA, lwd = 2, lty = 2, col = "gray60")


#--------8----------------------------------------------------------------------
Amount <- as.numeric(substring(Amount, 2, nchar(Amount)))
head(Amount); class(Amount)
summary(Amount)

#NA treatment
Amount[is.na(Amount)] <- median(Amount, na.rm = TRUE)

#outlier treatment
Q3 <- unname(quantile(Amount)[4])
outliers <- which(Amount > Q3 + 1.5 * IQR(Amount))
for(i in 1:length(Amount[outliers])){
  Amount[outliers][i] <- median(Amount)
}

which.max(tapply(Amount, Category, sum))
tapply(Amount, Category, sum)["00 Auto"]


#--------9----------------------------------------------------------------------
which.max(tapply(Amount, Category, mean))


#--------10---------------------------------------------------------------------
head(Claim.Name,35)
list1 <- regmatches(Claim.Name, regexpr("(?<=,) +(?!Inc)[a-zA-Z]{2,30}", Claim.Name, perl = TRUE))
list1

list1 <- regmatches(list1, regexpr("[a-zA-Z]{2,30}", list1, perl = TRUE))
list1

list2 <- regmatches(Claim.Name, regexpr("(?<=and )[a-zA-Z]{2,30}", Claim.Name, perl = TRUE))
list2

firstnames <- factor(c(list1, list2))

nlevels(firstnames)
which.max(table(firstnames))
table(firstnames)["Jennifer"]
Claim.Name[grep("Jennifer", Claim.Name)]

detach(legal)
