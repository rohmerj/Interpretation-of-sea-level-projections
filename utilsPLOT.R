### Option for ggplot2
tt <- theme(
		plot.title = element_text(size=14, face="bold"),
		axis.title.x = element_text(size=16, face="bold"),
		axis.title.y = element_text(size=16, face="bold"),
		axis.text.y= element_text(size=14),
		axis.text.x= element_text(size=14),
		axis.ticks = element_line(size = 1),
		legend.title=element_blank(),
		legend.position="none",
                legend.text=element_text(size=8),
		legend.key.size = unit(0.5, "cm")
		)

############################################################################
#### PLOT FUNCTION ADAPTED FROM https://github.com/ModelOriented
############################################################################
COL <- DALEX::colors_breakdown_drwhy()
COL[2] <- "grey"
plot.break_down <- function(x, ...,
                            baseline = NA,
                            max_features = 10,
                            min_max = NA,
                            vcolors = COL,
                            digits = 2, rounding_function = round,
                            add_contributions = TRUE, shift_contributions = 0.05,
                            plot_distributions = FALSE,
                            vnames = NULL,
                            title = "Break Down profile",
                            subtitle = "",
                            max_vars = NULL) {
  position <- cumulative <- prev <- pretty_text <- right_side <- contribution <- NULL
  # fix for https://github.com/ModelOriented/iBreakDown/issues/77
  colnames(x) <- gsub(colnames(x), pattern = "cummulative", replacement = "cumulative")

  # aliases
  if (!is.null(max_vars)) {
    max_features <- max_vars
  }

  if (plot_distributions) {
    vorder <- c(as.character(x$variable)[order(x$position)], "all data")
    df <- attr(x, "yhats_distribution")
    if (is.null(df))
      stop("You need to use keep_distributions=TRUE in the break_down() ")
    pl <- plot_break_down_distributions(df, vorder)
  } else {
    # how many features shall we plot
    #x <- select_only_k_features(x, max_features)
    # enrich dataframe with additional features
    tmp <- prepare_data_for_break_down_plot(x, baseline, rounding_function, digits)
    broken_baseline <- tmp$broken_baseline
    x <- tmp$x

    # fix for https://github.com/ModelOriented/iBreakDown/issues/85
    # check if correction is needed
    if (any(x[x$variable == "prediction", "right_side"] < broken_baseline$contribution)) {
      # put there max val
      x[x$variable == "prediction", "right_side"] <- pmax(x[x$variable == "prediction", "right_side"], broken_baseline$contribution)
    }
    if (any(x[x$variable == "intercept", "right_side"] < broken_baseline$contribution)) {
      # put there max val
      x[x$variable == "intercept", "right_side"] <- pmax(x[x$variable == "intercept", "right_side"], broken_baseline$contribution)
    }


    # base plot
    pl <- ggplot(x, aes(x = position + 0.5,
                        y = pmax(cumulative, prev),
                        xmin = position + 0.15, xmax = position + 0.85,
                        ymin = cumulative, ymax = prev,
                        fill = sign,
                        label = pretty_text))
    # add rectangles and hline
    pl <- pl +
      geom_errorbarh(data = x[x$variable_name != "", ],
                     aes(xmax = position - 0.85,
                         xmin = position + 0.85,
                         y = cumulative), height = 0,
                     color = "#371ea3") +
      geom_rect(alpha = 0.9) +
      geom_hline(data = broken_baseline, aes(yintercept = contribution), lty = 3, alpha = 0.5, color = "#371ea3") 
      #+facet_wrap(~label, scales = "free_y", ncol = 1)

    # add addnotations
    if (add_contributions) {
      drange <- diff(range(x$cumulative))
      pl <- pl + geom_text(aes(y = right_side),
                           vjust = 0.5,
                           nudge_y = drange*shift_contributions,
                           hjust = 0,
                           color = "#371ea3")
    }

    # set limits for contributions
    if (any(is.na(min_max))) {
      x_limits <- scale_y_continuous(expand = c(0.05,0.15), name = "")
    } else {
      x_limits <- scale_y_continuous(expand = c(0.05,0.15), name = "", limits = min_max)
    }

    if (is.null(vnames)) vnames <- x$variable
    pl <- pl + x_limits +
      scale_x_continuous(labels = vnames, breaks = x$position + 0.5, name = "") +
      scale_fill_manual(values = vcolors)
  }

  # add theme
   pl + coord_flip() +# DALEX::theme_drwhy_vertical() +
     theme(legend.position = "none")
     #+labs(title = title, subtitle = subtitle)
}

prepare_data_for_break_down_plot <- function(x, baseline, rounding_function, digits) {
  x$sign[x$variable_name == ""] <- "X"
  x$sign[x$variable == "intercept"] <- "X"
  x$prev <- x$cumulative - x$contribution
  broken_baseline <- x[x$variable_name == "intercept",]
  x$text <- x$prev
  if (is.na(baseline)) {
    for (lab in broken_baseline$label) {
      x[x$label == lab & x$variable == "prediction", "prev"] <-
        broken_baseline[broken_baseline$label == lab, "contribution"]
      x[x$label == lab & x$variable == "intercept", "prev"] <-
        broken_baseline[broken_baseline$label == lab, "contribution"]
    }
  } else {
    broken_baseline$contribution <- baseline
    x[x$variable == "prediction", "prev"] <- baseline
    x[x$variable == "intercept", "prev"] <- baseline
  }

  x$trans_contribution <- x$cumulative - x$text
  x$right_side <- pmax(x$cumulative,  x$cumulative - x$contribution)

  pretty_trans_contribution <- as.character(rounding_function(x$trans_contribution, digits))
  x$pretty_text <-
    paste0(ifelse((substr(pretty_trans_contribution, 1, 1) == "-") |
                    (x$variable == "prediction") |
                    (x$variable == "intercept"), "", "+"), pretty_trans_contribution)

  list(x = x, broken_baseline = broken_baseline)
}

conversion<-function(S0,ypred0,xinput0){
			
	contribution <- c(as.matrix(S0),ypred0)
	#noms <- c("Mean","num","iceFlow","init","res","SMB","kappa")

	noms <- c("Mean","Numerics","IceFlow","Initialisation","MinResolution",
			"MaxResolution","initialSMB","initialYear","BedTopo","kappa")	

	variable_name <- c("intercept",noms[-1],"")
	variable_value <- c(
		"1",
		levels(xinput0[,1])[unlist(xinput0)[1]],
		levels(xinput0[,2])[unlist(xinput0)[2]],
		levels(xinput0[,3])[unlist(xinput0)[3]],
	      unlist(xinput0)[4],
		unlist(xinput0)[5],
		levels(xinput0[,6])[unlist(xinput0)[6]],
	      unlist(xinput0)[7],
		levels(xinput0[,8])[unlist(xinput0)[8]],
		unlist(xinput0)[9],
		"")
	variable <- c("intercept",paste0(noms[-1]," = ",variable_value[c(-1,-11)]),"prediction")

	cumulative <- c(cumsum(as.matrix(S0)),ypred0)

	sign <- c(ifelse(S0>0,"1","-1"),"X")
	sign <- factor(as.character(sign),levels = c("-1", "0", "1", "X"))

	position <- (length(noms)+1):1

	label<-rep("rf",length(noms)+1)

	x_exemple<-data.frame(
  		variable = variable,
		contribution = c(contribution),
		variable_name = variable_name,
		variable_value = variable_value,
		cumulative = c(cumulative),
		sign = sign,
		position = position,
		label = label
	)

	class(x_exemple) <- c("break_down", "data.frame")

return(x_exemple)

}