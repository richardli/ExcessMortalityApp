#' Function to create summary tables
#' 
#' @param time_case string indicating monthly or weekly input
#' @param T number of time periods
#' @param years a sequence of years to show
#' @param morData the long format data frame
#' 
#' @return a list of summary tables
#' 
#' @export
#' 
#' @examples
#' data(SampleInput1)
#' SampleInput1$sex <- SampleInput1$age <- "All"
#' SampleInput1$timeCol = SampleInput1$month
#' SampleInput1$popCol = SampleInput1$population
#' tab <- summary_table(SampleInput1, time_case = "Monthly", T = 12, years = 2015:2021)
#' data(SampleInput3)
#' SampleInput3$timeCol = SampleInput3$week
#' SampleInput3$popCol = SampleInput3$population
#' tab <- summary_table(SampleInput3, time_case = "Weekly", T = 53, years = 2015:2021)
#' 

summary_table <- function(time_case, T, years, morData){

	   cleanTab <- NULL 
	   tab <- data.frame(matrix(NA, nrow = T, ncol = length(years)))
       colnames(tab) <- years  
       if(time_case == "Monthly"){
         rownames(tab) <- month.name        
       }else{
         rownames(tab) <- 1:T
       }
       tab.rate <- tab

       # Overall
       for(i in 1:T){
         for(j in 1:ncol(tab)){
            tab[i, j] <- sum(morData$deaths[which(morData$timeCol == i & morData$year == years[j])])
            tab.rate[i, j] <-  tab[i, j] / sum(morData$popCol[which(morData$timeCol == i & morData$year == years[j])]) * 1e5
         }
       }
       tab1 <- cbind(rownames(tab), tab)
       colnames(tab1)[1] <- ifelse(time_case == "Monthly", "Month", "Week")
       tab1.rate <- cbind(rownames(tab.rate), tab.rate)
       colnames(tab1.rate)[1] <- ifelse(time_case == "Monthly", "Month", "Week")
       cleanTab[['Death Counts']][["All"]][["All"]] <- tab1
       cleanTab[['Death Rate (Number of Deaths Per 100,000 Population)']][["All"]][["All"]] <- tab1.rate


       # by Sex
       for(s in unique(morData$sexCol)){
        tab.sex <- tab.sex.rate <- tab * 0
         for(i in 1:T){
           for(j in 1:ncol(tab)){
              tab.sex[i, j] <- sum(morData$deaths[which(morData$timeCol == i & morData$year == years[j] & morData$sexCol == s)])
              tab.sex.rate[i, j] <- tab.sex[i, j] / sum(morData$popCol[which(morData$timeCol == i & morData$year == years[j] & morData$sexCol == s)]) * 1e5
           }
         }
         tab.sex <- cbind(rownames(tab.sex), tab.sex)
         colnames(tab.sex)[1] <- ifelse(time_case == "Monthly", "Month", "Week")
         tab.sex.rate <- cbind(rownames(tab.sex.rate), tab.sex.rate)
         colnames(tab.sex.rate)[1] <- ifelse(time_case == "Monthly", "Month", "Week")
         cleanTab[['Death Counts']][[s]][["All"]] <- tab.sex
         cleanTab[['Death Rate (Number of Deaths Per 100,000 Population)']][[s]][["All"]] <- tab.sex.rate
       }

       # by Age
       for(a in unique(morData$ageCol)){
        tab.age <- tab.age.rate <- tab * 0
         for(i in 1:T){
           for(j in 1:ncol(tab)){
              tab.age[i, j] <- sum(morData$deaths[which(morData$timeCol == i & morData$year == years[j] & morData$ageCol == a)])
              tab.age.rate[i, j] <- tab.age[i, j] / sum(morData$popCol[which(morData$timeCol == i & morData$year == years[j] & morData$ageCol == a)]) * 1e5
           }
         }
         tab.age <- cbind(rownames(tab.age), tab.age)
         colnames(tab.age)[1] <- ifelse(time_case == "Monthly", "Month", "Week")
         tab.age.rate <- cbind(rownames(tab.age.rate), tab.age.rate)
         colnames(tab.age.rate)[1] <- ifelse(time_case == "Monthly", "Month", "Week")
         cleanTab[['Death Counts']][["All"]][[a]] <- tab.age
         cleanTab[['Death Rate (Number of Deaths Per 100,000 Population)']][["All"]][[a]] <- tab.age.rate
       }

        # by Sex and Age
        for(s in unique(morData$sexCol)){
          for(a in unique(morData$ageCol)){
          tab.sexage <- tab.sexage.rate <- tab * 0
           for(i in 1:T){
             for(j in 1:ncol(tab)){
                tab.sexage[i, j] <- sum(morData$deaths[which(morData$timeCol == i & morData$year == years[j] & morData$sexCol == s & morData$ageCol == a)])
                tab.sexage.rate[i, j] <- tab.sexage[i, j] / sum(morData$popCol[which(morData$timeCol == i & morData$year == years[j] & morData$sexCol == s & morData$ageCol == a)]) * 1e5
             }
           }
           tab.sexage <- cbind(rownames(tab.sexage), tab.sexage)
           colnames(tab.sexage)[1] <- ifelse(time_case == "Monthly", "Month", "Week")
           tab.sexage.rate <- cbind(rownames(tab.sexage.rate), tab.sexage.rate)
           colnames(tab.sexage.rate)[1] <- ifelse(time_case == "Monthly", "Month", "Week")
           cleanTab[['Death Counts']][[s]][[a]] <- tab.sexage
           cleanTab[['Death Rate (Number of Deaths Per 100,000 Population)']][[s]][[a]] <- tab.sexage.rate
          }
        }

        return(cleanTab)
}