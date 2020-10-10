Find_expression <- function(k){
  b <-c(1,2,3)
  matrix_1 <- permutations(3,8,b,repeats.allowed = TRUE)
  for(e in 1:6561){
    res<-0
    s<-""
    t<-""
    for(i in 1:8){
      paste('t','as.character(i)',sep='+')
                         j<-matrix_1[e,i]
                         if(j==1){
                           if(s==""){
                             res<-res+as.numeric(t)
                             s<-paste('s','+')
                           }else{
                             s<-paste('s','t','+')
                             if(flag==1){
                               res<-res+as.numeric(t)
                             }else{
                               res<-res-as.numeric(t)
                             }
                           }
                         flag<-1
                         t<-""
                    }else if(j==2){
                           if(s==""){
                             res<-as.numeric(t)
                             s<-paste('t','-')
                           }else{
                             s<-paste('s','t','-')
                             if(flag==1){
                               res<-res+as.numeric(t)
                             }else{
                               res<-res-as.numeric(t)
                             }
                           }
                           flag<-0
                           t<-""
                         }
                         if(i==8){
                           if(j==3){
                             t<-paste('t','9')
                             s<-paste('s','t')
                             if(flag==1){
                               res<-res+as.numeric(t)
                             }else{
                               res<-res-as.numeric(t)
                             }
                           }else{
                             t<-"9"
                             s<-paste('s','t')
                             if(flag==1){
                               res<-res+as.numeric(t)
                             }else{
                               res<-res-as.numeric(t)
                             }
                           }
                         }
    }
    if(res==k){
      s<-paste('s','as.character(k)',sep='=')
      return(s)
    }
    }
}
Find_expression(50)