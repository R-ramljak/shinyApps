library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(plotly)
library(ggmosaic)

helpPopup <- function(title, content,
                      placement=c('right', 'top', 'left', 'bottom'),
                      trigger=c('click', 'hover', 'focus', 'manual')) {
  tagList(
    singleton(
      tags$head(
        tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#", class = "btn btn-sm", `data-toggle` = "popover",
      `data-container`="body",
      title = title, `data-content` = content, `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],
      
      icon("question")
    )
  )
}


# Define UI for PPV application
ui <- fluidPage(
  fluidPage(theme = shinytheme("spacelab"),
                  
                  title = "Positive Predictive Value (PPV)",
                  shinyjs::useShinyjs(),
                  
                  h2(HTML("When does a significant <i>p</i>-value indicate a true effect?")),
                  h3(HTML("Understanding the Positive Predictive Value (PPV) of a <i>p</i>-value")),
                  
                  br(),
                  
                  # Sidebar to select inputs
                  fluidRow(
                    # ---------------------------------------------------------------------
                    # Parameter column
                    column(4,
                           p("Across all investigated hypotheses: What % of them is actually true?", style = "font-style: italic; font-size: 0.85em; color:grey"),    
                           sliderInput("percTrue", label = "% of a priori true hypotheses:", min = 0, max = 100, value = 30, step = 1),
                           
                           p(HTML("What is your Type I error (&alpha;; typically 5%)?"), style = "font-style: italic; font-size: 0.85em; color:grey"),
                           sliderInput("alpha", HTML("&alpha; level"), min = 0.005, max = 0.1, value = 0.05, step = 0.005),
                           
                           ## NG 17-04-24: add sample size (n), effect size (d), and power_select
                           p("Do you want to specify power directly or indirectly by specifying sample size per group and effect size? (Assuming a two-group t-test)", style = "font-style: italic; font-size: 0.85em; color:grey"),
                           radioButtons("power_select", label=NULL,
                                        choiceValues=list("power", "nd"),
                                        choiceNames=list(HTML('<span style = "font-size: 0.85em">specify power (1-&beta;) directly</span>'), list(HTML('<span style = "font-size: 0.85em">specify power indirectly through sample size (n) and effect size (d)</span>')))),
                           ## p(HTML("What is the power (&beta;) of the study?"), style = "font-style: italic; font-size: 0.85em; color:grey"),
                           sliderInput("power", label = HTML("Power (1-&beta;)"), min = 0.01, max = 0.99, value = 0.35, step = 0.01),
                           ## p("What is your sample size (n) per group?", style = "font-style: italic; font-size: 0.85em; color:grey"),
                           conditionalPanel("input.power_select == 'nd'",
                                            sliderInput("n", label = "Sample size per group (n)", min = 1, max = 100, value = 16, step = 1),
                                            ## p("What is the standardized effect size (d)?", style = "font-style: italic; font-size: 0.85em; color:grey"),
                                            sliderInput("d", label = "Effect size (d)", min = 0.1, max = 2, value = 1, step = .05)
                           ), 
                           
                           p(HTML("% of studies that report a significant result, although it's not"), helpPopup("What is 'p-hacking' in this context?", "The percentage refers to the proportion of all non-significant studies (both true negatives and false negatives), that are presented as significant. For details, see Ioannidis (2005):\n'Let u be the proportion of probed
analyses that would not have been 'research findings', but nevertheless end up presented and reported as
such, because of bias. [...] Bias can entail manipulation in the analysis or reporting of findings.'", placement='right', trigger='hover'), style = "font-style: italic; font-size: 0.85em; color:grey; line-height:30%"),
                           sliderInput("bias", label = "% of p-hacked studies", min = 0, max = 100, value = 0, step = 1),
                           selectInput("preset","Presets by Ioannidis (2005)", c(
                             "---"="---",
                             "1: Adequately powered RCT with little bias and 1:1 pre-study odds"="p1",
                             "2: Confirmatory meta-analysis of good-quality RCTs"="p2",
                             "3: Meta-analysis of small inconclusive studies"="p3",
                             "4: Underpowered, but well-performed phase I/II RCT"="p4",
                             "5: Underpowered, poorly performed phase I/II RCT"="p5",
                             "6: Adequately powered exploratory epidemiological study"="p6",
                             "7: Underpowered exploratory epidemiological study"="p7",
                             "8: Discovery-oriented exploratory research with massive testing"="p8",
                             "9: As in previous example, but with more limited bias (more standardized)"="p9"
                           ), selected = "p1")
                    ),	
                    
                    
                    # ---------------------------------------------------------------------
                    # Output column
                    
                    column(8,
                           # tableOutput("table"),
                           htmlOutput("res")
                    )			
                  ),
                  HTML("<b>This <a href='http://www.nicebread.de/whats-the-probability-that-a-significant-p-value-indicates-a-true-effect/'>blog post</a> gives an introduction to the app.</b><br>"),
                  HTML("This app is based on Ioannidis, J. P. A. (2005). Why most published research findings are false. PLoS Medicine, 2(8), e124. <a href='http://doi.org/10.1371/journal.pmed.0020124'>http://doi.org/10.1371/journal.pmed.0020124</a><br>
	Thanks to Nat Goodman for adding the option to specify power via effect size and sample size.")
))
    
# input <- list(percTrue=30, alpha=.05, power=.35, bias=0)
presetselection <- new.env()
presetselection$sel <- "---"
	
# Define server logic for PPV application
server <- function(input, output, session) {
	
	# compute PPV etc only once for all outputs--> reactive values
	computePPV <- reactive({ 
		
		# if user changes a parameter: reset preset selector to "---"
		isolate({updateSelectInput(session, inputId = "preset", selected=presetselection$sel)})
		presetselection$sel <- "---"
		
		c <- 500		# number of studies in plots
		nrow <- 25
		ncol <- c/nrow
		df <- expand.grid(x = c(1:nrow), y = c(1:ncol))

		# compute prestudy odds of true relationships true/false
		R <- input$percTrue/(100-input$percTrue)
		#if (R==0) R <- 0.001
		if (is.infinite(R)) R <- 100000
		
		# Alpha level
		alpha <- input$alpha
		# power
                # NG 17-04-24: let power be set indirectly by setting n and d
                # NG 17-04-24: adjust power or d sliders based on other values
                power <- input$power
                n <- input$n
                d <- input$d
                if (input$power_select == "power") {
                  d <- power.t.test(n=n,delta=NULL,power=power,sig.level=alpha)$delta
                  isolate({updateSelectInput(session, inputId = "d", selected=d)})
                } else {
                  power <- power.t.test(n=n,delta=d,power=NULL,sig.level=alpha)$power
                  isolate({updateSelectInput(session, inputId = "power", selected=power)})
                 }
		# beta Fehler
		beta <- 1 - power
		# bias
		u <- input$bias/100		# Ioannidis calls it "u"
		
		# Hits 
		hit <- (c*power*R + u*c*beta*R)/(R + 1)
		falsealarm <- (c*alpha + u*c*(1 - alpha))/(R + 1)
		miss <- (1-u)*c*beta*R / (R + 1)
		truerejection <- (1-u)*c*(1-alpha)/(R + 1)

		# marginals
		h1 <- round(c * R / (R + 1))
		h0 <- round(c / (R + 1))
		significant <- round(c*(R + alpha - beta*R + u - u*alpha + u*beta*R)/(R + 1))
		not.significant <- round(c*(1-u)*(1 - alpha + beta*R)/(R + 1))
		
		# sanity check:
		if (hit+falsealarm+miss+truerejection != c) print(paste0("Error: 1000 != ", hit+falsealarm+miss+truerejection))
		if (h1 + h0 != c) print(paste0("Error: 1000 != ", h1 + h0))
		if (significant + not.significant != c) print(paste0("Error: 1000 != ", significant + not.significant))

		# positive predictive value
		ppv <- (power * R + u*beta*R) / (R + alpha - beta*R + u - u*alpha + u*beta*R)
		fdr <- 1-ppv

		# fill in the ground truths types
		df$ground <- c(rep(FALSE, times=h0), rep(TRUE, times=h1))

		# fill in the false alarms: h0 is true and test is significant
		df$test <- FALSE
		df$test[df$ground==TRUE][1:hit] <- TRUE
		df$test[df$ground==FALSE][1:falsealarm] <- TRUE
		
		# check
		#table(test=df$test, ground=df$ground)		

		# combine types
		df$type[!df$ground & !df$test] <- "true negative"
		df$type[!df$ground & df$test] <- "false positive"
		df$type[df$ground & df$test] <- "true positive"
		df$type[df$ground & !df$test] <- "false negative"
		df$type <- factor(df$type, levels=c("false negative", "false positive", "true negative", "true positive"))
		return(list(df=df, ppv=ppv, fdr=fdr, hit=hit, falsealarm=falsealarm, miss=miss, truerejection=truerejection, c=c))
	})
	
	observeEvent(input$power_select, {
		if (input$power_select == "nd") {
			shinyjs::disable("power")
		} else {
			shinyjs::enable("power")
		}
	})
	
	df.raw <- reactive({
	  df.raw <- computePPV()$df %>%
	    select(ground, test)
	  # values <- unlist(computePPV())
	})


	# output$table <- renderTable({
	#   computePPV()$df %>%
	#     select(ground, test) %>%
	#     count(ground, test)
	# 
	# })

	# output$plot <- renderPlotly({
	#   plot1 <- ggplot(df.raw()) +
	#     geom_mosaic(aes(x = product(ground), fill = test)) +
	#     scale_x_continuous(position = "top") +
	#     labs(x = "Null Hypothesis", y = "Alternative Hypothesis")
	#   ggplotly(plot1)
	# 
	# })
	

	output$res <- renderUI({
	  PPV <- computePPV()
	  df.raw <- computePPV()$df %>%
	    select(ground, test)
	  
	  # color translation tables
	  colTrans <- c("darkblue", "red", "orange", "green3")
	  names(colTrans) <- c("false negative", "false positive", "true negative", "true positive")
	  
	  colTrans2 <- c("white", "red", "white", "green3")
	  names(colTrans2) <- c("false negative", "false positive", "true negative", "true positive")
	  
	  return(list(
	    HTML(paste0(
	      "true positives: ", round(PPV$hit/PPV$c*100, 1), "%; false negatives: ", round(PPV$miss/PPV$c*100, 1), 
	      "%; true negatives: ", round(PPV$truerejection/PPV$c*100, 1), "%; false positives: ", round(PPV$falsealarm/PPV$c*100, 1), "%<br><br>",
	      "<b>Positive predictive value (PPV)</b>: ", round(PPV$ppv*100, 1), "% of claimed findings are true</b><br>
				 <b>False discovery rate (FDR)</b>: ", round(PPV$fdr*100, 1), "% of claimed findings are false</b>")),
	    h4("If we consider all findings, it looks like this:"),
	    renderPlotly({
	        plot1 <- df.raw %>%
	          mutate(ground = case_when(ground == "TRUE" ~ "H0 is false",
	                                    ground == "FALSE" ~ "H0 is true")) %>%
	          mutate(test = case_when(test == "TRUE" ~ "H0 rejected",
	                                  test == "FALSE" ~ "H0 not rejected")) %>%
	          ggplot() +
	          geom_mosaic(aes(x = product(ground), fill = test), show.legend = F) +
	          labs(x = "", y = "") +
	          theme_classic()
	        ggplotly(plot1, height = 550)
	    })
	  ))
	  
	})
	
	
	# ---------------------------------------------------------------------
	# Load demo data
	observeEvent(input$preset, {
		switch(input$preset,
			"p1" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 0.50*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.80)
				updateSliderInput(session, inputId = "bias", value = 0.10*100)				
				},
			"p2" = {
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 2/3*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.95)
				updateSliderInput(session, inputId = "bias", value = 0.30*100)
				},
			"p3" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 0.25*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.80)
				updateSliderInput(session, inputId = "bias", value = 0.40*100)
				},
			"p4" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/6*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.20*100)
				},
			"p5" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/6*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.80*100)
				},
			"p6" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/11*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.80)
				updateSliderInput(session, inputId = "bias", value = 0.30*100)
				},
			"p7" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/11*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.30*100)
				},
			"p8" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/1001*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.80*100)
				},
			"p9" = {				
				updateSliderInput(session, inputId = "power_select", value = "power")
				updateSliderInput(session, inputId = "percTrue", value = 1/1001*100)
				updateSliderInput(session, inputId = "alpha", value = 0.05)
				updateSliderInput(session, inputId = "power", value = 0.20)
				updateSliderInput(session, inputId = "bias", value = 0.20*100)
				}					
		)
		presetselection$sel <- input$preset
	})
}

shinyApp(ui, server)
