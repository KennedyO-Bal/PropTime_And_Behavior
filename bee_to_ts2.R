library(dplyr) 

behavior_type <- data.frame(
  behavior = c("Back", "U turn", "Withdraw", "C Posture", "Chases", "Lunges", 
               "Push", "Passes", "Follow", "Hd-Bk", "Mandible Lock", "Charge", 
               "Hd-Hd", "Antennation", "Back to Back", "Side by side", "MHA"
  ),
  category=c("Avoidance", "Avoidance", "Avoidance", "Aggressive", "Aggressive", 
             "Aggressive", "Aggressive", "Tolerance", "Tolerance", "Tolerance", 
             "Aggressive", "Aggressive", "Tolerance", "Tolerance", "Tolerance", 
             "Tolerance", "Aggressive")
)
  
bee_to_ts <- function(x,
                      behavior=c("Back", "U turn", "Withdraw", "C Posture", "Chases", "Lunges", 
                                 "Push", "Passes", "Follow", "Hd-Bk", "Mandible Lock", "Charge", 
                                 "Hd-Hd", "Antennation", "Back to Back", "Side by side", "MHA")) {
  time <- 0:1800
  
  dd <- as.data.frame(matrix(rep(FALSE, length(time)*length(behavior)), nrow=length(time)))
  colnames(dd) <- behavior
  
  tmp <- cbind(data.frame(time=time), dd)
  
  y <- x[!duplicated(x) & x$time <= 1800,]
  y$time <- round(y$time)
  
  reslist <- lapply(split(y, y$subject), function(z) {
    tmp2 <- tmp
    
    for (i in 1:nrow(z)) {
      tt <- (z$time[i]+1):max(time)
      
      tmp2[[z$behavior[i]]][tt] <- z$status[i]=="START"
      
    }
    
    tmp2$subject <- z$subject[1]
    
    tmp2
  })
  
  final <- do.call("rbind", reslist)
  
  rownames(final) <- NULL
  
  final2 <- final %>%
    tidyr::gather(key, value, -time, -subject) %>%
    rename(
      behavior=key,
      status=value
    )
  
  merge(final2, behavior_type)
}

ts_minute <- . %>% 
  mutate(
    minute=floor(time/60)
  ) %>%
  group_by(minute, behavior, subject, category) %>%
  summarize(
    mean=mean(status)
  )

