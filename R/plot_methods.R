#' Plotting Estimation Results
#'
#' Function plots the estimation results of an object created by the \code{\link{estTable}} function.
#' Provides the possibility to visualize and compare the point estimates and their estimation errors
#' differentiated by the applied estimation method and estimator.
#'
#' @param x object of class \code{"list" "esttable"} created by the \code{\link{estTable}} function.
#'
#' @param yvar if set to \code{"error"} (default), the estimation error is plotted on the y-axis. If set to \code{"estimate"},
#'        point estimates with their confidene intervals are plotted.
#' @param ... ignored.
#'
#'
#' @example examples/example_plot_estTable.R
#'
#' @import ggplot2
#' @export


plot.esttable<- function(x, yvar="error", ...){


    getclass<- class(x)

    dat<- as.data.frame(x)
    levels(dat$estimator)<-  c("onephase", "psynth extended", "psynth", "psmall")
    dat$methest<-  factor(paste(dat$estimator,"(",dat$vartype,")",sep = ""))
    levels(dat$methest)<-  c("onephase(variance)", "psmall(ext_variance)", "psmall(g_variance)",
                             "psynth extended(ext_variance)", "psynth extended(g_variance)",
                             "psynth(g_variance)")

    colors.tab<- data.frame( methest= c("onephase(variance)", "psmall(ext_variance)", "psmall(g_variance)",
                                        "psynth extended(ext_variance)", "psynth extended(g_variance)",
                                        "psynth(g_variance)"),
                             colors=  c("chartreuse4", "deepskyblue1", "deepskyblue3",
                                        "royalblue1", "royalblue3", "red3"))


    colors.temp<- as.character(colors.tab$colors[colors.tab$methest %in% unique(dat$methest)])

    # ----------------------------------------------------------------------------- #
    if(yvar=="error"){


      # ************************* #
      if(getclass[3]=="global"){

        p<-  ggplot(data = dat, aes_q(x=quote(method), y=quote(error), fill=quote(methest))) +
          geom_bar(colour="black", stat="identity", position=position_dodge()) +
          labs(x="Estimator", y="Estimation Error [%]") +
          scale_fill_manual("Estimation Method", values=colors.temp)
      }


      # ************************* #
      if(getclass[3]=="smallarea"){

        p<-  ggplot(data=dat, aes_q(x=quote(method), y=quote(error), fill=quote(methest))) +
          geom_bar(colour="black", stat="identity", position=position_dodge()) +
          labs(x="Estimator", y="Estimation Error [%]") +
          facet_wrap( ~ area, ncol=5, scales = "free_y") +
          scale_fill_manual("Estimation Method", values=colors.temp)
      }

    }



    # ----------------------------------------------------------------------------- #
    if(yvar=="estimate"){

      # ************************* #
      if(getclass[3]=="global"){

        p<- ggplot(data=dat, aes_q(x=quote(method), y=quote(estimate), fill=quote(methest))) +
          geom_bar(colour="black", stat="identity", position=position_dodge()) +
          geom_errorbar(aes(ymax=ci_upper, ymin=ci_lower), position=position_dodge(width=0.9), width=0.25, lwd=0.75) +
          xlab("Estimator") +
          scale_fill_manual("Estimation Method", values=colors.temp)
      }


      # ************************* #
      if(getclass[3]=="smallarea"){

        p<- ggplot(data=dat, aes_q(x=quote(method), y=quote(estimate), fill=quote(methest))) +
          geom_bar(colour="black", stat="identity", position=position_dodge()) +
          geom_errorbar(aes(ymax=ci_upper, ymin=ci_lower), position=position_dodge(width=0.9), width=0.25, lwd=0.75) +
          xlab("Estimator") +
          facet_wrap( ~ area, ncol=5, scales = "free_y") +
          scale_fill_manual("Estimation Method", values=colors.temp)
      }

    }


    return(p)

  } # end of plotting function

