testPlot <- function(){
        require(SixSigma)
        ss.study.ca(ss.data.ca$Volume, rnorm(42, 753, 3),
                    LSL = 740, USL = 760, T = 750, alpha = 0.05,
                    f.sub = "Winery Project")
}



DemoHist4 <- function(xTag = "Moisture", cTag = "APCOn"){
        
        require(ggplot2)
        require(plyr)
        
        data <- getData2()
        data <- data[data$Spec == "WMP",]
        data$xTag = data[,xTag]
        data$cTag = factor(data[,cTag])
        
        means <- ddply(data, cTag, summarise, xTag.mean=mean(xTag))
        means$cTag = factor(means[,cTag])
        sds <- ddply(data, cTag, summarise, xTag.sd=sd(xTag))
        sds$cTag = factor(means[,cTag])
        sds$plus = sds$xTag.sd + means$xTag.mean
        sds$minus =  means$xTag.mean - sds$xTag.sd
        
        p <- 
                ggplot(data, aes(x=xTag, fill=cTag)) + 
                geom_histogram(binwidth=.30, alpha=.5, position="identity") + 
                geom_vline(data=means, aes(xintercept=xTag.mean,  colour=cTag),
                           linetype="dashed", size=1) +
                geom_vline(data=sds, aes(xintercept=plus,  colour=cTag),
                           linetype="solid", size=0.5) +
                geom_vline(data=sds, aes(xintercept=minus,  colour=cTag),
                           linetype="solid", size=0.5) +    
                xlab(xTag) +
                scale_fill_discrete(name = cTag) 
        
        p
}

DemoQcc4 <- function(yTag = "Moisture",xTag = "Date", cTag = "APCOn"){
        require(qcc)
        data <- getData2()
        data <- data[data$Spec == "WMP",]
        data <- data[data$APCOn == 1,]
        data$Category = factor(data[,cTag])
        data$Date <- as.Date(as.character(data$DateTime), "%Y-%m-%d")
        data$xTag = data[,xTag]
        data$xTag = as.factor(data[,xTag])
        data$yTag = data[,yTag]
        data$sample = 1:length(data[,1])
        
        
        grps <- qcc.groups(data$yTag,data$Date)
        p <- qcc(grps[,1:25],type="xbar",data.name = yTag,add.stats = FALSE)
        p <- qcc(grps[,25:45],type="xbar",data.name = yTag,add.stats = FALSE)
        
        print(p)
        
}

DemoScat4 <- function(yTag = "Moisture",xTag = "Date", cTag = "APCOn"){
        data <- getData2()
        data$xTag = data[,xTag]
        data$yTag = data[,yTag]
        data$Category = factor(data[,cTag])
        library(ggplot2)
        
        q <- ggplot(data, aes(x=xTag, y=yTag,color=Category)) +
                geom_point(shape=1) +    # Use hollow circles
                geom_smooth() +
                labs(x = xTag,y = yTag,title = paste(xTag,yTag,sep=" "))
        print(q)
}

rChart <- function(){
        library(rCharts)
        names(iris) = gsub("\\.", "", names(iris))
        rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
        
}

randomDairyData2 <- function(date,batch,samples,APC){
        batchSize = samples / batch
        specs <- c("SMP LH","SMP MH","WMP")
        
        if (APC == TRUE){
                ThrouputSD = 0.3
                ThrouputMean = 6.3
                RateMean = 100
                RateSD = 5
                ThermalEfficiencyMean = 0.71
                ThermalEfficiencySD = 0.48
                MoistureSD = 0.388
                MoistureMean = 3.9
                MoistureMeanWMP = 3.25
                SolidsSD = 4.8
                SolidsMean = 53.38
                SolidsMeanWMP = 52.83
                ProteinSMP = 1.2
                ProteinWMP = 2.0
                ProteinMeanSMPLH = 33.0
                ProteinMeanSMPMH = 33.0
                ProteinMeanWMP = 25
                FatSMP = 0.7
                FatWMP = 1.3
                FatMeanSMP = 0.62
                FatMeanWMP = 26
        }
        else{
                ThrouputSD = 0.4
                ThrouputMean = 6.0
                RateMean = 100
                RateSD = 8
                ThermalEfficiencyMean = 0.68
                ThermalEfficiencySD = 0.42
                MoistureSD = 0.588
                MoistureMean = 3.8
                MoistureMeanWMP = 3.1
                SolidsMean = 51.38
                SolidsMeanWMP = 50.23
                SolidsSD = 8.2
                ProteinSMP = 2.2
                ProteinWMP = 2.4
                ProteinMeanSMPLH = 34.5
                ProteinMeanSMPMH = 34.0
                ProteinMeanWMP = 26 
                FatSMP = 1.2
                FatWMP = 2.3
                FatMeanSMP = 0.8
                FatMeanWMP = 27
        }
        
        
        
        df <- data.frame(DateTime = as.POSIXct(date)- 60*30*1:samples,
                         OnProduct = round(rnorm(n = samples,mean = 0.6,sd = 0.1), digits = 0),
                         APCOn = APC,
                         Throughput = rnorm(samples,mean=ThrouputMean,sd=ThrouputSD),
                         Rate =  rnorm(samples,mean=RateMean,sd=RateSD),
                         ThermalEfficiency  = rnorm(samples,mean=ThermalEfficiencyMean,sd=ThermalEfficiencySD),
                         Spec = rep(x = (c(rep(specs[1],samples/batchSize),rep(specs[2],samples/batchSize),rep(specs[3],samples/batchSize))),batchSize/3),
                         Moisture  = NA,
                         Solids = NA,
                         Protein = NA,
                         Fat = NA
        )
        df['Day of Week'] = strftime(df$DateTime,'%A')
        df$Week = format(df$DateTime+3, "%U")
        df$Year = format(df$DateTime, "%Y")   
        df$Month =  format(df$DateTime, "%m")  
        df$Day =  format(df$DateTime, "%d")  
        df$Day[1]
        
        df$Spec[df['Day of Week'] == "Monday" | df['Day of Week'] == "Tuesday"] = "SMP LH"
        df$Spec[df['Day of Week'] == "Wednesday" | df['Day of Week'] == "Thursday"  | df['Day of Week'] == "Friday"] = "SMP MH"
        df$Spec[df['Day of Week'] == "Saturday" | df['Day of Week'] == "Sunday"] = "WMP"
        
        df$Moisture[df$Spec=="SMP LH"] <- rnorm(length(df$Moisture[df$Spec=="SMP LH"]),mean = MoistureMean,sd = MoistureSD )
        df$Moisture[df$Spec=="SMP MH"] <- rnorm(length(df$Moisture[df$Spec=="SMP MH"]),mean = MoistureMean,sd = MoistureSD)
        df$Moisture[df$Spec=="WMP"] <- rnorm(length(df$Moisture[df$Spec=="WMP"]),mean = MoistureMeanWMP,sd = MoistureSD )
        
        df$Solids[df$Spec=="SMP LH"] <- rnorm(length(df$Solids[df$Spec=="SMP LH"]),mean = SolidsMean,sd = SolidsSD)
        df$Solids[df$Spec=="SMP MH"] <- rnorm(length(df$Solids[df$Spec=="SMP MH"]),mean = SolidsMean,sd = SolidsSD)
        df$Solids[df$Spec=="WMP"] <- rnorm(length(df$Solids[df$Spec=="WMP"]),mean = SolidsMeanWMP,sd = SolidsSD)
        
        df$Protein[df$Spec=="SMP LH"] <- rnorm(length(df$Protein[df$Spec=="SMP LH"]),mean = ProteinMeanSMPLH,sd = ProteinSMP)
        df$Protein[df$Spec=="SMP MH"] <- rnorm(length(df$Protein[df$Spec=="SMP MH"]),mean = ProteinMeanSMPMH,sd = ProteinSMP)
        df$Protein[df$Spec=="WMP"] <- rnorm(length(df$Protein[df$Spec=="WMP"]),mean = ProteinMeanWMP,sd = ProteinWMP)
        
        df$Fat[df$Spec=="SMP LH"] <- rnorm(length(df$Fat[df$Spec=="SMP LH"]),mean = 0.9,sd = FatSMP)
        df$Fat[df$Spec=="SMP MH"] <- rnorm(length(df$Fat[df$Spec=="SMP MH"]),mean = 0.8,sd = FatSMP)
        df$Fat[df$Spec=="WMP"] <- rnorm(length(df$Fat[df$Spec=="WMP"]),mean = 26.3,sd = FatWMP)
        
        
        df
}

getData2 <- function(){
        
        batch = 100
        samples <- 600
        date = Sys.Date()
        APC = 0
        APCOff <- randomDairyData2(date = date,batch = batch, samples = samples,APC)
        samples <- 1200
        date2 = min(APCOff$DateTime)
        APC = 1
        APCOn <-randomDairyData2(date = date,batch = batch, samples = samples,APC)
        data <- rbind(APCOff,APCOn)
        
        data
        
}



sixSigmaDemos <- function(){
        
        require(SixSigma)
        
        data <- getData2()
        
        vector <- data$Moisture
        
        
        SixSigma::ss.ca.cp(x = vector,LSL = 2.6,USL = 3.6,f.na.rm = T)
        
        SixSigma::ss.ca.cpk(x = vector,LSL = 2.6,USL = 3.6,f.na.rm = T)
        
        SixSigma::ss.ca.z(x = vector,LSL = 2.6,USL=3.6)
        
        library(qcc)
        X1 = matrix(c(72, 56, 55, 44, 97, 83, 47, 88, 57, 26, 46, 
                      49, 71, 71, 67, 55, 49, 72, 61, 35, 84, 87, 73, 80, 26, 89, 66, 
                      50, 47, 39, 27, 62, 63, 58, 69, 63, 51, 80, 74, 38, 79, 33, 22, 
                      54, 48, 91, 53, 84, 41, 52, 63, 78, 82, 69, 70, 72, 55, 61, 62, 
                      41, 49, 42, 60, 74, 58, 62, 58, 69, 46, 48, 34, 87, 55, 70, 94, 
                      49, 76, 59, 57, 46), ncol = 4)
        X2 = matrix(c(23, 14, 13, 9, 36, 30, 12, 31, 14, 7, 10, 
                      11, 22, 21, 18, 15, 13, 22, 19, 10, 30, 31, 22, 28, 10, 35, 18, 
                      11, 10, 11, 8, 20, 16, 19, 19, 16, 14, 28, 20, 11, 28, 8, 6, 
                      15, 14, 36, 14, 30, 8, 35, 19, 27, 31, 17, 18, 20, 16, 18, 16, 
                      13, 10, 9, 16, 25, 15, 18, 16, 19, 10, 30, 9, 31, 15, 20, 35, 
                      12, 26, 17, 14, 16), ncol = 4)
        X = list(X1 = X1, X2 = X2)
        
        q = mqcc(vector, type = "T2")
        summary(q)
        ellipseChart()
        ellipseChart(q, show.id = TRUE)
        
        effect <- "Flight Time"
        causes.gr <- c("Operator", "Environment", "Tools", "Design",
                       "Raw.Material", "Measure.Tool")
        causes <- vector(mode = "list", length = length(causes.gr))
        causes[1] <- list(c("operator #1", "operator #2", "operator #3"))
        causes[2] <- list(c("height", "cleaning"))
        causes[3] <- list(c("scissors", "tape"))
        causes[4] <- list(c("rotor.length", "rotor.width2", "paperclip"))
        causes[5] <- list(c("thickness", "marks"))
        causes[6] <- list(c("calibrate", "model"))
        ss.ceDiag(effect, causes.gr, causes, sub = "Paper Helicopter Project")
        
        ss.study.ca(xST = data$Moisture,LSL = 2.6,USL = 3.6,Target = 3.1)
        
        ss.cc("mr",data[1:25,],CTQ = "Moisture")
        
        ss.ci(x = Moisture,data=data,alpha = 0.05,sub="Moisture Test",xname = "Moisture",digits = 2)
        
        
        ss.lfa(lfa.data = data,lfa.ctq = "Moisture",lfa.Delta = 0.5,lfa.Y0 = 3.1,lfa.L0 = 0.0003,lfa.output = "both")
        
        
        boxplot(formula = Moisture ~ Spec,data = data)
        ss.rr(var = Moisture,part = Spec,data = data)
        d <- ss.data.rr
        example(ss.ci)
        
        example(ss.study.ca)
        example(ss.rr)
        example(ss.lf)
        example(ss.lfa)
        example(ss.ceDiag)
        example(ss.pMap)
        example(ss.ca.yield)
        example(ss.ca.z)
        example(ss.ca.cp)
        example(ss.ca.cpk)
        example(ss.cc)
        
        library(qcc)
        with(ss.data.pb3,
             plot(qcc,stockouts,orders,type="p"))
}


sscp <- function(yTag = "Moisture"){
        require(SixSigma)
        data <- getData2()
        data<-data[data$Spec=="WMP",]
        
        ss.ca.cp(x = data$Moisture,LSL = 0,USL = 4.5,alpha = 0.05)
        
}
sscpk <- function(yTag = "Moisture"){
        require(SixSigma)
        data <- getData2()
        data<-data[data$Spec=="WMP",]
        
        ss.ca.cpk(x = data$Moisture,LSL = 0,USL = 4.5,alpha = 0.05)
        
}

ssca <- function (yTag = "Moisture"){
        require(SixSigma)
        data <- getData2()
        data<-data[data$Spec=="WMP",]
        
        plot <- ss.study.ca(xST = data$Moisture[1:25],USL = 4.5,LSL = 0,Target = 3.7,alpha = 0.05,f.main = "Moisture Capability Study",f.sub = "Moisture")
        print(plot)
}

ssci <- function (yTag = "Moisture"){
        
        data <- getData2()
        data$test = data[,yTag]
        require(SixSigma)
        ss.ci(x = test, data = data, alpha = 0.05, sub = paste(yTag,"Test"),digits = 2)
        
}

sslfa <- function(yTag = "Moisture"){
        data <- getData2()
        data$test = data[,yTag]
        require(SixSigma)
        p <-ss.lfa(lfa.data = data,lfa.ctq = yTag,lfa.Delta = 0.5,lfa.Y0 = 3.1,lfa.L0 = 0.0003,lfa.output = "both")
        p
}

box.plot <- function(yTag = "Moisture",cTag = "Spec"){
        data <- getData2()
        data$y = data[,yTag]
        data$c = data[,cTag]
        
        boxplot(formula = Moisture ~ Spec,data = data)
        
}

ssrr <- function (){
        
        require(SixSigma)
        ss.rr(time1, prototype, operator, data=ss.data.rr, sub="Six Sigma Demo")
}

rChart <- function(){
        library(rCharts)
        names(iris) = gsub("\\.", "", names(iris))
        rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
        
}


runPlot <- function(){
        require(SixSigma)
        data <- getData2()
        data <- data[data$Spec=="WMP",]
        
        m = mean(data$Moisture[1:20])
        sd = sd(data$Moisture[1:20])
        data$Moisture[1] = m
        data$Colour="black"
        # Set new column values to appropriate colours
        data$Colour[data$Moisture>=m+sd]="red"
        data$Colour[data$Moisture<=m-sd]="red"
        
        # Plot all points at once, using newly generated colours
        
        
        
        plot(data$Moisture[1:20],
             type = "b",
             pch = 16,     
             axes = FALSE,
             col=data$Colour,
             main = "Run Chart for Moisture",
             sub = "Moisture",
             xlab = "Hour",
             ylab = "Moisture")
        
        axis(1,at = 1:24, cex.axis = 0.7)
        axis(2)
        box()
        grid()
        
        abline(h=m,lwd=1)
        abline(h=sd+m,lwd=0.4)
        abline(h=m-sd,lwd=0.4)
        
        
        
        
}

strip.plot <- function(){
        require(SixSigma)
        
        stripchart(pc.volume ~ pc.batch,
                   data = ss.data.pc,
                   pch = "-",
                   cex = 3,
                   xlab = "Hour",
                   ylab = "Throughput",
                   ylim = c(12,20),
                   vertical = T,
                   main = "Throughput Tier Chart")
        
        grid()
        
        for (i in 1:6){
                lines(x = rep(i,2),
                      lwd=3,
                      col = "#666666",
                      y = c(max(ss.data.pc$pc.volume[ss.data.pc$pc.batch==i]),
                            min(ss.data.pc$pc.volume[ss.data.pc$pc.batch==i])))
        }
        abline(h=16, lwd=2)
        
        
}