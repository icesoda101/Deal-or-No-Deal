f <- function() {
  ##a02q5b
  s <- c(.01, 1,5, 10, 25, 50, 75, 100 ,200, 300, 400, 500 ,750, 1000, 5000, 10000, 25000, 50000, 75000, 100000, 200000, 300000, 400000, 500000 ,750000, 1000000)
  contestant_case = sample(s,1,replace=FALSE)
  remain1 = setdiff(s,contestant_case)
  ##1st round
  round1 = sample(remain1,6,replace=FALSE)
  remain2 = setdiff(remain1, round1)
  Banker_known1 = setdiff(s, round1)
  sd1 = sqrt(var(Banker_known1)*(length(Banker_known1)-1)/length(Banker_known1))
  offer1 = mean(Banker_known1) - 0.1*sd1
  
  if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)) {
    final_value = offer1
  }else {
    ##2nd round
    round2 = sample(remain2,5,replace=FALSE)
    remain3 = setdiff(remain2, round2)
    Banker_known2 = setdiff(Banker_known1, round2)
    sd2 = sqrt(var(Banker_known2)*(length(Banker_known2)-1)/length(Banker_known2))
    offer2 = mean(Banker_known2) - 0.1*sd2
    if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)) {
      final_value = offer2
    }else {
      round3 = sample(remain3,4,replace=FALSE)
      remain4 = setdiff(remain3, round3)
      Banker_known3 = setdiff(Banker_known2, round3)
      sd3 = sqrt(var(Banker_known3)*(length(Banker_known3)-1)/length(Banker_known3))
      offer3 = mean(Banker_known3) - 0.1*sd3
      if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)) {
        final_value = offer3
      }else {
        round4 = sample(remain4,3,replace=FALSE)
        remain5 = setdiff(remain4, round4)
        Banker_known4 = setdiff(Banker_known3, round4)
        sd4 = sqrt(var(Banker_known4)*(length(Banker_known4)-1)/length(Banker_known4))
        offer4 = mean(Banker_known4) - 0.1*sd4
        if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)){
          final_value = offer4
        }else {
          round5 = sample(remain5,2,replace=FALSE)
          remain6 = setdiff(remain5, round5)
          Banker_known5 = setdiff(Banker_known4, round5)
          sd5 = sqrt(var(Banker_known5)*(length(Banker_known5)-1)/length(Banker_known5))
          offer5 = mean(Banker_known5) - 0.1*sd5
          if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)){
            final_value = offer5
          }else {
            round6 = sample(remain6,1,replace=FALSE)
            remain7 = setdiff(remain6, round6)
            Banker_known6 = setdiff(Banker_known5, round6)
            sd6 = sqrt(var(Banker_known6)*(length(Banker_known6)-1)/length(Banker_known6))
            offer6 = mean(Banker_known6) - 0.1*sd6
            if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)){
              final_value = offer6
            }else {
              round7 = sample(remain7,1,replace=FALSE)
              remain8 = setdiff(remain7, round7)
              Banker_known7 = setdiff(Banker_known6, round7)
              sd7 = sqrt(var(Banker_known7)*(length(Banker_known7)-1)/length(Banker_known7))
              offer7 = mean(Banker_known7) - 0.1*sd7
              if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)){
                final_value = offer7
              }else {
                round8 = sample(remain8,1,replace=FALSE)
                remain9 = setdiff(remain8, round8)
                Banker_known8 = setdiff(Banker_known7, round8)
                sd8 = sqrt(var(Banker_known8)*(length(Banker_known8)-1)/length(Banker_known8))
                offer8 = mean(Banker_known8) - 0.1*sd8
                if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)){
                  final_value = offer8
                }else {
                  round9 = sample(remain9,1,replace=FALSE)
                  remain10 = setdiff(remain9, round9)
                  Banker_known9 = setdiff(Banker_known8, round9)
                  sd9 = sqrt(var(Banker_known9)*(length(Banker_known9)-1)/length(Banker_known9))
                  offer9 = mean(Banker_known9) - 0.1*sd9
                  if (sample(c(TRUE,FALSE,FALSE,FALSE), 1, replace=TRUE)){
                    final_value = offer9
                  }else {
                    final_value = contestant_case
                  }
                }
              }
            }
          }
        }
      }
    } 
  }
  return(final_value)
}

data = replicate(10000, f())
mean(data)
                            

