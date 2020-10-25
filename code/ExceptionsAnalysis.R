library(RODBC)
library(stringr)
library(plyr)
library(RColorBrewer)
library(ggplot2)

#####
# Functions
#####
CategoriseException <- function(x) {
    out <- vector(length = length(x))
    for(v in seq_along(x)) {
        exc <- x[v]
        if(grepl("cannot assign", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "ASSIGN"
        } else if(grepl("kicked out a load with", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "FULL/LOAD"
        } else if(grepl("pressed load", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "FULL/LOAD"
        } else if(grepl("pressed delay", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "DELAY"
        } else if(grepl("enter option before delay", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "DELAY"
        } else if(grepl("pressed delay", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "DELAY"
        } else if(grepl("is delay for", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "DELAY"
        } else if(grepl("enter option before down", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "DOWN"
        } else if(grepl("pressed FULL with", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "FULL/LOAD"
        } else if(grepl("pressed Assign to a Delayed Shovel", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "ASSIGN"
        } else if(grepl(" pressed Assign.", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "ASSIGN"
        } else if(grepl(" pressed Ready ", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "READY"
            # } else if(grepl(" Standby.", exc, ignore.case = TRUE, perl = TRUE)) {
            #     out[v] <- "STANDBY"
        } else if(grepl(" is Standby for ", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "STANDBY"
        } else if(grepl(" Standby.", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "STANDBY"
        } else if(grepl(" pressed Arrive.", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "ARRIVE"
        } else if(grepl(" pressed Full", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "FULL/LOAD"
        } else if(grepl(" is Down for ", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "DOWN"
        } else if(grepl("is auto Delay for ", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "DELAY"
        } else if(grepl("is Delayed for", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "DELAY"
        } else if(grepl("has been assigned to", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "MANASN"
        } else if(grepl("Can't talk to", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "NOTALK"
        } else if(grepl("minutes late", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "LATE"
        } else if(grepl("pressed UNKNOWN", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "LOGIN"
        } else if(grepl("WRONG location", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "WRONGLOC"
        } else if(grepl("MISROUTE detected", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "WRONGLOC"
        } else if(grepl("has an EMERGENCY", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "EMERGENCY"
        } else if(grepl("pressed a button before operator OPER ID", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "LOGIN"
        } else if(grepl("A passed Full Inspection has not been found", exc, ignore.case = TRUE, perl = TRUE)) {
            out[v] <- "PRESTART"
        } else {
            out[v] <- "Other"
        }
    }
    return(out)
}

GetVehicle <- function(x) {
    out <- vector(length = length(x))
    for(v in seq_along(x)) {
        out[v] <- str_match(x[v], "(T\\d{3}|AN\\d{4}|LT\\d{3}|ST\\d{3}|STM\\d{3}|WC\\d{3}|EX\\d{3}|DZ\\d{2}|GD\\d{3}|SH\\d{3}|LD\\d{3}|WL\\d{3})")
    }
    return(out)
}

CategoriseResponseTime <- function(x) {
    out <- vector(length = length(x))
    for(v in seq_along(x)) {
        if(x[v] <= 10 ) {
            out[v] <- "ACTIONED_10s"
        } else if(x[v] <= 20 ) {
            out[v] <- "ACTIONED_20s"
        } else if(x[v] <= 30 ) {
            out[v] <- "ACTIONED_30s"
        } else if(x[v] <= 40 ) {
            out[v] <- "ACTIONED_40s"
        } else if(x[v] <= 50 ) {
            out[v] <- "ACTIONED_50s"
        } else if(x[v] <= 60 ) {
            out[v] <- "ACTIONED_60s"
        } else {
            out[v] <- "ACTIONED_>61s"
        }
    }
    out <- factor(out, levels = c("ACTIONED_10s", "ACTIONED_20s", "ACTIONED_30s", "ACTIONED_40s", "ACTIONED_50s", "ACTIONED_60s", "ACTIONED_>61s"))
    return(out)
}

getDelay <- function(x) {
    out <- vector(length = length(x))
    for(v in seq_along(x)) {
        out[v] <- substr(x[v], regexpr(" for ", x[v]) + 5, nchar(x[v]) - 1)
    }
    return(out)
}

shiftTimes <- read.csv("./data/ShiftTimes.csv", stringsAsFactors = FALSE, header = FALSE)
colnames(shiftTimes) <- c("Year", "Month", "Day", "ShiftName", "Crew", "StartTime", "EndTime")
shiftTimes$Shift <- paste0(shiftTimes$Year, formatC(shiftTimes$Month, width = 2, flag = "0"), formatC(shiftTimes$Day, width = 2, flag = "0"), tolower(substr(shiftTimes$ShiftName, 1, 1)))
shiftTimes <- shiftTimes[order(shiftTimes$StartTime),]

theExceptions <- read.csv("./data/Exceptions_181026_181126.csv", stringsAsFactors = FALSE, header = TRUE)
# colnames(theExceptions) <- c("Timestamp", "MessageBody")
theExceptions$MessageBody <- trimws(theExceptions$MessageBody)
theExceptions <- theExceptions[order(theExceptions$Timestamp),]
theExceptions <- theExceptions[grepl("^###|^!!!|^\\*\\*\\*|^\\d{2}:\\d{2}:\\d{2}$|---Ignored|---Accepted", theExceptions$MessageBody),]

theTimestamp    <- ''
theMessage      <- ''
theType         <- 'Informational'
theResponse     <- ''
theDispatcher   <- ''
theResponseTime <- ''
E <- data.frame()
for(s in 1:nrow(shiftTimes)) {
    message(paste("Processing shift", shiftTimes[s,]$Shift))
    
    thisShiftExceptions <- theExceptions[theExceptions$Timestamp >= shiftTimes[s,]$StartTime & theExceptions$Timestamp < shiftTimes[s,]$EndTime,]
    
    if(nrow(thisShiftExceptions) == 0) { # Something wrong, no exceptions found, continue
        message(paste("No exceptions found in", shiftTimes[s,]$Shift))
        next
    }
    
    SE <- data.frame()
    for(e in 1:nrow(thisShiftExceptions)) {
        # if(grepl("^\\d{2}:\\d{2}:\\d{2}$", thisShiftExceptions[e,]$MessageBody)) { # Starting a new exception
        #   if(grepl("---Ignored|---Accepted", thisShiftExceptions[e+2,]$MessageBody)) { # look two rows ahead to see if this an interactive exception
        #     SE <- rbind(SE, data.frame(Exception=thisShiftExceptions[e+1,]$MessageBody, Timestamp=thisShiftExceptions[e,]$Timestamp, TimePosted=thisShiftExceptions[e,]$MessageBody, Type="Interactive", Response=thisShiftExceptions[e+2,]$MessageBody))
        #   } else {
        #     SE <- rbind(SE, data.frame(Exception=thisShiftExceptions[e+1,]$MessageBody, Timestamp=thisShiftExceptions[e,]$Timestamp, TimePosted=thisShiftExceptions[e,]$MessageBody, Type="Informational", Response=""))
        #   }
        # }
        message(e)
        if(grepl("---Ignored|---Accepted", theExceptions[e,]$MessageBody)) {
            theType         <- 'Interactive'
            theResponse     <- theExceptions[e,]$MessageBody
            theResponseTime <- theExceptions[e,]$Timestamp
        } else if(grepl("\\+\\+\\+", theExceptions[e,]$MessageBody)) {
            theDispatcher <- theExceptions[e,]$MessageBody
        } else if(grepl("^\\d{2}:\\d{2}:\\d{2}$", theExceptions[e,]$MessageBody)) {
            # Close off previous exception,
            SE <- rbind(SE, data.frame(Timestamp=theTimestamp, Exception=theMessage, Type=theType, Response=theResponse, ResponseTime=theResponseTime, Dispatcher=theDispatcher))
            
            # Then reset for new exception
            theTimestamp    <- ''
            theMessage      <- ''
            theType         <- 'Informational'
            theResponse     <- ''
            theDispatcher   <- ''
            theResponseTime <- ''
            
            # Start new exception
            theTimestamp <- theExceptions[e,]$Timestamp
        } else {
            theMessage <- theExceptions[e,]$MessageBody
        }
    }
    
    SE$Shift <- shiftTimes[s,]$Shift
    SE$ExceptionCategory <- CategoriseException(SE$Exception)
    SE$Vehicle <- GetVehicle(SE$Exception)
    
    E <- rbind(E, SE)
}

E <- E[!grepl("^\\d{2}:\\d{2}:\\d{2}$", E$Exception),]
write.csv(E, file = "./data/processedExceptions.csv", row.names = FALSE)

E$Timestamp <- strptime(substr(E$Timestamp, 12, 19), format = "%H:%M:%S")
E$ResponseTime <- strptime(substr(E$ResponseTime, 12, 19), format = "%H:%M:%S")
E$ResponseDelay <- as.integer(E$ResponseTime - E$Timestamp)
E <- merge(E, shiftTimes[,c("Shift", "Crew")])

interactiveExc <- E[E$Type == "Interactive",]
interactiveExc$Response <- str_match(interactiveExc$Response, "---(\\w*).")[,2]
interactiveExc$Timestamp <- as.POSIXct(interactiveExc$Timestamp)
interactiveExc$ResponseTime <- as.POSIXct(interactiveExc$ResponseTime)
interactiveExc$ResponseCategory <- CategoriseResponseTime(interactiveExc$ResponseDelay)

# Aggregate the data by Crew / Category
DispOperExc <- ddply(interactiveExc, c("Crew", "ExceptionCategory"),
                     summarise,
                     Shifts      = length(unique(Shift)),
                     ExcAcc      = sum(Response == "Accepted"),
                     ExcRej      = sum(Response == "Ignored"),
                     ExcTot      = ExcAcc + ExcRej,
                     MinResp     = min(ResponseDelay),
                     MaxResp     = max(ResponseDelay),
                     MeanResp    = mean(ResponseDelay),
                     ExcPerShift = ExcTot / Shifts,
                     RejRate     = ExcRej / ExcTot * 100.0
)
write.csv(DispOperExc,  file = "./output/ExceptionsByDispatchExceptionCategory.csv", row.names = FALSE)

# Aggregate the data by Dispatcher / Response Category
DispRespExc <- ddply(interactiveExc, c("Crew", "ResponseCategory"),
                     summarise,
                     Shifts      = length(unique(Shift)),
                     ExcAcc      = sum(Response == "Accepted"),
                     ExcRej      = sum(Response == "Ignored"),
                     ExcTot      = ExcAcc + ExcRej,
                     MinResp     = min(ResponseDelay),
                     MaxResp     = max(ResponseDelay),
                     MeanResp    = mean(ResponseDelay),
                     ExcPerShift = ExcTot / Shifts,
                     RejRate     = ExcRej / ExcTot * 100.0
)
DispTotalIntExc <- ddply(interactiveExc, c("Crew"), summarise, totalExc = length(Exception))
DispRespExc <- merge(DispRespExc, DispTotalIntExc, by = "Crew")
DispRespExc$Rate <- (DispRespExc$ExcTot/DispRespExc$totalExc) * 100.0
write.csv(DispRespExc,  file = "./output/ExceptionsByCrewResponseCategory.csv", row.names = FALSE)

# Aggregate the data by Category
ExcCat <- ddply(interactiveExc, c("ExceptionCategory"),
                summarise,
                Shifts      = length(unique(Shift)),
                ExcAcc      = sum(Response == "Accepted"),
                ExcRej      = sum(Response == "Ignored"),
                ExcTot      = ExcAcc + ExcRej,
                MinResp     = min(ResponseDelay),
                MaxResp     = max(ResponseDelay),
                MeanResp    = mean(ResponseDelay),
                ExcPerShift = ExcTot / Shifts,
                RejRate     = ExcRej / ExcTot * 100.0
)
write.csv(ExcCat,  file = "./output/ExceptionsByExceptionCategory.csv", row.names = FALSE)

# Aggregate the data by Response Category
RespExc <- ddply(interactiveExc, c("ResponseCategory"),
                 summarise,
                 Shifts      = length(unique(Shift)),
                 ExcAcc      = sum(Response == "Accepted"),
                 ExcRej      = sum(Response == "Ignored"),
                 ExcTot      = ExcAcc + ExcRej,
                 MinResp     = min(ResponseDelay),
                 MaxResp     = max(ResponseDelay),
                 MeanResp    = mean(ResponseDelay),
                 ExcPerShift = ExcTot / Shifts,
                 RejRate     = ExcRej / ExcTot * 100.0
)
write.csv(RespExc,  file = "./output/ExceptionsByResponseCategory.csv", row.names = FALSE)

# There can be a large number of Dispatchers in a month, often more than the generic
# colour scales provide. This gives us up to 20, which should be safe for most sites
myColours = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 12))

# Plot Response time by Exception Category
png("./output/AvgRespTimeByDispExcCat.png", 1680, 744)
p <- ggplot(DispOperExc, aes(x=Crew, y=MeanResp, fill=Crew)) +
    ylim(0, max(DispOperExc$MeanResp) * 1.2) +
    geom_bar(stat="identity", color='black') +
    geom_hline(data=ExcCat, aes(yintercept=MeanResp), color='red') +
    scale_fill_manual(values = myColours) +
    facet_wrap(~ExceptionCategory, ncol=4) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
    theme(legend.position="none") +
    xlab("Dispatch Controller") +
    ylab("Avg. Response Time (sec)") +
    ggtitle("Avg. Exception Response Time by Dispatch Controller, Exception Category") +
    geom_text(aes(label=round(MeanResp, digits=1), vjust=-0.5))
print(p)
dev.off()

# Plot Response time for Response Category > 61 s
png("./output/AvgRespTimeByDispGT60.png", 1680, 744)
p <- ggplot(DispRespExc[DispRespExc$ResponseCategory == "ACTIONED_>61s",], aes(x=Crew, y=MeanResp, fill=Crew)) +
    geom_bar(stat="identity", color='black') +
    scale_fill_manual(values =  myColours) +
    geom_hline(data=RespExc[RespExc$ResponseCategory == "ACTIONED_>61s",], aes(yintercept=MeanResp), color='red') +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1)) +
    theme(legend.position="none") +
    xlab("Dispatch Controller") +
    ylab("Avg. Response Time (sec)") +
    ggtitle("Average Exception Response Time (>60s) by Dispatch Controller") +
    geom_text(aes(label=round(MeanResp, digits=1), vjust=-0.5))
print(p)
dev.off()

# Plot Reject Rates by response category
png("./output/ExcRejRateByRespCatDisp.png", 1680, 744)
p <- ggplot(DispRespExc, aes(x=ResponseCategory, y=RejRate, fill=ResponseCategory)) +
    ylim(0,105) +
    geom_bar(stat="identity", color='black') +
    facet_wrap(~Crew, ncol=4) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1)) +
    theme(legend.position="none") +
    xlab("Response Time Category") +
    ylab("% of Exceptions rejected") +
    ggtitle("Exception Rejection Rates by Response Time Category, Dispatch Controller") +
    geom_text(aes(label=round(RejRate, digits=1), vjust=-0.5))
print(p)
dev.off()

# Plot RespCat Rate by Dispatcher
png("./output/ExcRespCatDispatcher.png", 1680, 744)
p <- ggplot(DispRespExc, aes(x=ResponseCategory, y=Rate, fill=ResponseCategory)) +
    ylim(0,105) +
    geom_bar(stat="identity", color='black') +
    facet_wrap(~Crew, ncol=4) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1)) +
    theme(legend.position="none") +
    xlab("Response Time Category") +
    ylab("% of Exceptions handled") +
    ggtitle("% of Exceptions by Response Time Category, Dispatch Controller") +
    geom_text(aes(label=round(Rate, digits=1), vjust=-0.5))
print(p)
dev.off()

# Plot Reject Rates by exception category
png("./output/ExcRejRateByExcCatDisp.png", 1680, 744)
p <- ggplot(DispOperExc, aes(x=ExceptionCategory, y=RejRate, fill=ExceptionCategory)) +
    ylim(0,105) +
    geom_bar(stat="identity", color='black') +
    facet_wrap(~Crew, ncol=4) +
    theme(axis.text.x = element_text(angle=60, hjust=1, vjust=1)) +
    theme(legend.position="none") +
    xlab("Exception Category") +
    ylab("% of Exceptions rejected") +
    ggtitle("Exception Rejection Rates by Exception Category, Dispatch Controller") +
    geom_text(aes(label=round(RejRate, digits=1), vjust=-0.5))
print(p)
dev.off()

# Now by dispatcher
png("./output/ExcRejRateByDispExcCat.png", 1680, 744)
p <- ggplot(DispOperExc, aes(x=Crew, y=RejRate, fill=Crew)) +
    ylim(0,105) +
    geom_bar(stat="identity", color='black') +
    facet_wrap(~ExceptionCategory, ncol=4) +
    scale_fill_manual(values = myColours) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1)) +
    theme(legend.position="none") +
    xlab("Dispatch Controller") +
    ylab("% of Exceptions rejected") +
    ggtitle("Exception Rejection Rate by Dispatch Controller, Exception Category") +
    geom_text(aes(label=round(RejRate, digits=1), vjust=-0.5))
print(p)
dev.off()

DispatcherInterVsNonInter <- ddply(interactiveExc, c("Crew"), summarise,
                                   Shifts=length(unique(Shift)),
                                   Interactive=sum(Response != ""),
                                   NonInteractive=sum(Response == ""),
                                   Total=length(Shift))

DispatcherInteractiveBasics <- ddply(interactiveExc, c("Crew"), summarise,
                                     Shifts=length(unique(Shift)),
                                     Accepted=sum(Response=="Accepted"),
                                     Rejected=sum(Response=="Ignored"),
                                     RejectRate = (Rejected/(Accepted+Rejected)),
                                     Total=length(Shift),
                                     ShiftRate = (Total/Shifts))

DispatcherInteractiveAccRej <- ddply(interactiveExc, c("Crew", "Response"), summarise,
                                     Shifts=length(unique(Shift)),
                                     Total=length(Shift),
                                     ShiftRate = (Total/Shifts))

DispatcherInteractiveResponseCategory <- ddply(interactiveExc, c("Crew", "Response", "ExceptionCategory"), summarise,
                                               Shifts=length(unique(Shift)),
                                               Total=length(Shift),
                                               ShiftRate = (Total/Shifts))

DispatcherInteractiveCategory <- ddply(interactiveExc, c("Crew", "ExceptionCategory"), summarise,
                                       Shifts=length(unique(Shift)),
                                       Accepted=sum(Response=="Accepted"),
                                       Rejected=sum(Response=="Ignored"),
                                       RejectRate = (Rejected/(Accepted+Rejected)),
                                       Total=length(Shift))

# Dispatcher Plots
DispatcherInteractiveBasics$Crew<-reorder(DispatcherInteractiveBasics$Crew,DispatcherInteractiveBasics$ShiftRate)
png("./output/AvgIntByDisp.png", 1680, 744)
p<-ggplot(DispatcherInteractiveBasics,aes(x=Crew, y=ShiftRate)) +
    geom_bar(stat="identity") +
    xlab("Dispatcher") +
    ylab("Avg. # Interactive Exceptions per shift") +
    ggtitle("Avg. # of Interactive Exceptions per shift by Dispatcher") +
    geom_text(aes(label=round(ShiftRate, digits=1), vjust=-0.5)) +
    theme(axis.text.x = element_text(angle=75, hjust=1, vjust=1))
print(p)
dev.off()

png("./output/AvgIntByShiftDispResp.png", 1680, 744)
p<-ggplot(DispatcherInteractiveAccRej, aes(x=Crew, y=ShiftRate, fill=Response)) +
    geom_bar(stat="identity", position = position_dodge(), colour = "black") +
    xlab("Dispatcher") +
    ylab("Avg. # exceptions per shift") +
    ggtitle("Avg. # of Interactive Exceptions per shift by Dispatcher Response") +
    geom_text(aes(label=round(ShiftRate, digits=1), vjust=-0.5), position = position_dodge(width=1)) +
    theme(axis.text.x = element_text(angle=75, hjust=1, vjust=1))
print(p)
dev.off()

png("./output/AvgIntByShiftDispRespCat.png", 1680, 744)
p<-ggplot(DispatcherInteractiveResponseCategory[DispatcherInteractiveResponseCategory$ExceptionCategory!="NOTALK",], aes(x=Crew, y=ShiftRate, fill=Response)) +
    geom_bar(stat="identity", position = position_dodge(), colour = "black") +
    xlab("Dispatcher") +
    ylab("Avg. # exceptions per shift") +
    ylim(0,max(DispatcherInteractiveResponseCategory$ShiftRate) + 8) +
    ggtitle("Avg. # of Interactive Exceptions per shift by Dispatcher Response / Category") +
    geom_text(aes(label=round(ShiftRate, digits=1), vjust=-0.5), position = position_dodge(width=1)) +
    theme(axis.text.x = element_text(angle=75, hjust=1, vjust=1)) +
    facet_wrap(~ExceptionCategory, ncol=2)
print(p)
dev.off()

png("./output/AvgIntRejRateByDisp.png", 1680, 744)
p<-ggplot(DispatcherInteractiveCategory[DispatcherInteractiveCategory$ExceptionCategory!="NOTALK",], aes(x=Crew, y=RejectRate, fill=Crew)) +
    geom_bar(stat="identity", position = position_dodge(), colour = "black") +
    xlab("Dispatcher") +
    ylab("Reject Rate") +
    ylim(0,1.1) +
    ggtitle("Interactive Exceptions Reject Rate by Dispatcher / Category") +
    geom_text(aes(label=sprintf("%1.1f%%", 100*RejectRate), vjust=-0.1), position = position_dodge(width=1)) +
    theme(axis.text.x = element_text(angle=75, hjust=1, vjust=1)) +
    facet_wrap(~ExceptionCategory, ncol = 2)
print(p)
dev.off()

# DELAYS, usually the largest source of interactive exceptions
DELAYS <- E[E$ExceptionCategory == "DELAY",]
DELAYS$Timestamp <- as.POSIXct(DELAYS$Timestamp)
DELAYS$ResponseTime <- as.POSIXct(DELAYS$ResponseTime)
delaysIntVsNonInt <- ddply(DELAYS, c("Type"), summarise, NUM = length(Shift))
interactiveDelays <- DELAYS[DELAYS$Type == "Interactive",]
interactivedelayssummary <- ddply(interactiveExc[interactiveExc$Type == "Interactive",], c("Response"), summarise, NUM = length(Shift))
interactiveDelays$reason <- getDelay(interactiveDelays$Exception)
DelayReasonRates <- arrange(ddply(interactiveDelays, c("reason"), summarise, Ignored = sum(Response == "---Ignored."), Accepted = sum(Response == "---Accepted."), AcceptRate = Accepted / (Accepted + Ignored) * 100.0), desc(AcceptRate))
write.csv(DelayReasonRates, file = "./output/DelayReasonRates.csv", row.names = FALSE)

# Output some of the DFs for the PAs to analyse
write.csv(E,                  file = "./output/ShiftExceptions.csv",          row.names = FALSE)
write.csv(interactiveExc,     file = "./output/InteractiveExceptions.csv",    row.names = FALSE)
