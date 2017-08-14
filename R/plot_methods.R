

#' @import ggplot2
#' @export
plot.esttable<- function(dat, yvar){

  if(yvar=="error"){

    if(class(dat)[3]=="global"){
      p<-  ggplot(data=as.data.frame(dat), aes(x=estimator, y=error, fill=paste(method,"(",vartype,")",sep = ""))) +
        geom_bar(colour="black", stat="identity", position=position_dodge()) +
        labs(x="Estimator", y="Estimation Error [%]") +
        scale_fill_manual("Estimation Method", values=c("deepskyblue1", "deepskyblue3", "chartreuse4"))
    }

    if(class(dat)[3]=="smallarea"){
      p<- ggplot(data=as.data.frame(dat), aes(x=estimator, y=error, fill=paste(method,"(",vartype,")",sep = ""))) +
        geom_bar(colour="black", stat="identity", position=position_dodge()) +
        labs(x="Estimator", y="Estimation Error [%]") +
        facet_wrap( ~ area, ncol=5, scales = "free_y") +
        scale_fill_manual("Estimation Method", values=c("chartreuse4", "deepskyblue1", "royalblue1", "red3"))
    }

  }


  if(yvar=="estimate"){

    if(class(dat)[3]=="global"){
      p<- ggplot(data=as.data.frame(dat), aes(x=estimator, y=estimate, fill=paste(method,"(",vartype,")",sep = ""))) +
        geom_bar(colour="black", stat="identity", position=position_dodge()) +
        geom_errorbar(aes(ymax=ci_upper, ymin=ci_lower), position=position_dodge(width=0.9), width=0.25, lwd=0.75) +
        labs(x="Estimator", y="Timber Volume [m3/ha]") +
        scale_fill_manual("Estimation Method", values=c("deepskyblue1", "deepskyblue3", "chartreuse4"))
    }


    if(class(dat)[3]=="smallarea"){
      p<- ggplot(data=as.data.frame(dat), aes(x=estimator, y=estimate, fill=paste(method,"(",vartype,")",sep = ""))) +
        geom_bar(colour="black", stat="identity", position=position_dodge()) +
        geom_errorbar(aes(ymax=ci_upper, ymin=ci_lower), position=position_dodge(width=0.9), width=0.25, lwd=0.75) +
        labs(x="Estimator", y="Timber Volume [m3/ha]") +
        facet_wrap( ~ area, ncol=5, scales = "free_y") +
        scale_fill_manual("Estimation Method", values=c("chartreuse4", "deepskyblue1", "royalblue1", "red3"))
    }
  }

return(p)

} # end of plotting function
