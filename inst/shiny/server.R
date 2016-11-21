
library(shiny);
library(weco);


shinyServer(function(input, output, session) {

    source("weco_ui.R", local=TRUE);

    userLog                <- reactiveValues();
    userLog$selected.rules <- list();
    userLog$results        <- NULL;

    ##------------------------------------
    ##---------main page------------------
    ##------------------------------------
    output$mainpage <- renderUI({
        tab.main();
    })


    ##--------------------------------------
    ##---------data upload------------------
    ##--------------------------------------

    ##--display data uploaded-----
    output$uiData <- DT::renderDataTable({
                             if (input$displaydata) {
                                 get.data();
                             }
                         },
                         rownames=NULL,
                         selection="none",
                         options=list(pageLength=100))


    output$uiSelectVar <- renderUI({
        if (is.null(get.data()))
            return(NULL);

        num.cols <- which(sapply(get.data(), is.numeric));

        if (0 == length(num.cols)) {
            wellPanel(msg.box("No numeric columns in the uploaded data.", "error"));
        } else {
            wellPanel(h4("Select variable"),
                      msg.box('Please select a column from the uploaded data to be monitored.'),
                      fluidRow(
                          column(3,
                                 selectInput(inputId='inVar', label = "Numeric variables",
                                             choices = names(get.data())[num.cols],
                                             width   = "80%")
                                 ),
                          column(8, uiOutput("uiMuSd"))
                      ))
        }
    })

    ##mean and sd
    output$uiMuSd <- renderUI({
        cur.x <- get.x();

        if (is.null(cur.x))
            return(NULL);

        fluidRow(
            column(4, numericInput(inputId="inMux",   label="Mean", value = mean(cur.x))),
            column(4, numericInput(inputId="inSdx",   label="SD", value = sd(cur.x))),
            column(4, numericInput(inputId="inRunin", label="Run-In", value = 0, step=1))
        )
    })

    ##------------------------------------
    ##---------Rules----------------------
    ##------------------------------------

    ## add rules
    observeEvent(input$btnAdd, {

        all.pars <- get.rules.pars();

        rst <- list();
        for (i in get.const()$all.rules) {
            if (FALSE == input[[paste("inChkRule", i, sep="")]])
                next;

            ##reset
            updateCheckboxInput(session,
                                inputId = paste("inChkRule", i, sep=""),
                                value = FALSE);

            pars    <- all.pars[[i]]$pars;
            cur.rst <- list(rule = i);
            for (j in 1:length(pars)) {
                cur.nm <- names(pars)[j];
                cur.v  <- input[[paste("inR",i,cur.nm, sep="")]];
                if (!is.numeric(cur.v))
                    cur.v <- pars[j];
                cur.rst[[cur.nm]] <- cur.v;
            }

            if (length(cur.rst) > 0) {
                rst[[length(rst) + 1]] <- cur.rst;
            }
        }

        if (0 == length(rst))
            return(NULL);

        userLog$selected.rules[[length(userLog$selected.rules)+1]] <- rst;
    });


    ## show the selected rules
    output$uiRules <- renderUI({

        choices <- get.choices();

        if (is.null(choices))
            return(NULL);

        div(lapply(1:length(choices),
                   function(x) {
            checkboxInput(inputId = paste("inSelRule", x, sep=""),
                          label = HTML(paste("<i>", choices[x], "</i></p>")));
        }))
    })

    ## remove rules
    observeEvent(input$btnDel, {
        if (0 == length(userLog$selected.rules))
            return(NULL);

        del.sel <- NULL;
        for (i in 1:length(userLog$selected.rules)) {
            if (input[[paste("inSelRule", i, sep="")]])
                del.sel <- c(del.sel, i);
        }

        if (!is.null(del.sel))
            userLog$selected.rules <- userLog$selected.rules[-del.sel];
    });


    ##------------------------------------
    ##---------signal page----------------
    ##------------------------------------

    ##reset results
    observe({
        get.data();
        get.x();
        input$inMux;
        input$inSdx;
        input$inRunin;
        userLog$selected.rules;

        isolate({
            userLog$results <- NULL;
        })
    })

    ## remove rules
    observeEvent(input$btnCheck, {

        if (is.null(get.x()))
            return(NULL);

        rules <- userLog$selected.rules;
        if (0 == length(rules))
            return(NULL);

        ##Create a Progress object
        progress <- shiny::Progress$new(session, min=0, max=1);
        progress$set(message = "Checking in progress...", value=0);
        ##Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close());

        rst <- NULL;
        for (i in 1:length(rules)) {
            rst[[i]] <- weco.combine(get.x(),
                                     sdx = input$inSdx,
                                     mux = input$inMux,
                                     rules[[i]]
                                     )
            progress$set(value=i/length(rules),
                         detail=paste("check selected rule ", i, sep=""));
        }
        userLog$results <- rst;
    });

    ##panel
    output$uiTabSig <- renderUI({
        if (is.null(get.data())) {
            msg.box("Please upload data first.", "warning");
        } else if (0 == length(userLog$selected.rules)) {
            msg.box("Please select WECO rules first.", "warning");
        } else {
            div(div(actionButton("btnCheck", "Check Signals"),
                    style = "margin-bottom:20px"),
                uiOutput("uiRst"))
        }
    })

    ##results
    output$uiRst <- renderUI({
        if (is.null(userLog$results))
            return(NULL);

        rules   <- userLog$selected.rules;
        results <- userLog$results;

        tab.rst <- list(id   = "vistabs",
                        type = "pills");

        for (i in 1:length(rules)) {
            cur.rst <- tabPanel(paste("Check ", i, sep=""),

                                h4("Rules"),
                                htmlOutput(paste("rtxt", i, sep="")),

                                h4("Trace Plot"),
                                HTML("Note: drag to zoom in and double-click to reset."),
                                plotOutput(paste("rplot", i, sep=""),
                                           dblclick = paste("p_dblclick", i, sep = ""),
                                           brush = brushOpts(
                                               id = paste("p_brush",i,sep = ""),
                                               resetOnNew = TRUE)),

                                h4("Histogram of Average Running Length"),
                                plotOutput(paste("rdenrl", i, sep="")),
                                style="padding-top:30px");
            tab.rst[[length(tab.rst)+1]] <- cur.rst;
        }

        wellPanel(do.call(tabsetPanel, tab.rst));
    })

    ##present results
    observe({
        results <- userLog$results;
        if (0 == length(results))
            return(NULL);

        isolate({
            for (i in 1:length(results)) {
                local({
                    myi <- i;
                    uname <- paste("rtxt", myi, sep="");
                    output[[uname]] <- renderText({
                        HTML(get.choices()[myi]);
                    })

                    uname <- paste("rplot", myi, sep="");
                    output[[uname]] <- renderPlot({
                        cur.rst <- results[[myi]];
                        plot(cur.rst, start=cur.rst$start, end=cur.rst$end, runin=input$inRunin);
                    }, bg="transparent")

                    uname <- paste("rdenrl", myi, sep="");
                    output[[uname]] <- renderPlot({
                        cur.rl <- weco.rl(results[[myi]]);
                        if (is.null(cur.rl))
                            return(NULL);

                        hist(cur.rl, breaks = 100, freq = F,
                             xlab="Running Length",
                             ylab="Probability",
                             main=sprintf("ARL=%5.3f", mean(cur.rl)));
                    }, bg="transparent")

                    observeEvent(input[[paste("p_dblclick", myi, sep="")]], {
                        userLog$results[[myi]]$start <- NULL;
                        userLog$results[[myi]]$end   <- NULL;
                    })

                    observeEvent(input[[paste("p_brush", myi, sep="")]], {
                        brush <- input[[paste("p_brush", myi, sep="")]];
                        userLog$results[[myi]]$start <- ceiling(brush$xmin);
                        userLog$results[[myi]]$end   <- floor(brush$xmax);
                    })

                })
            }
        })
    })


    ##double click
    observeEvent(input$p_dblclick, {
        userLog$range <- NULL;
    })

    ##single click
    observeEvent(input$p_click, {
        if (is.null(userLog$range)) {
            range <- c(1,length(userLog$sim.rst$samples));
        } else {
            range <- userLog$range;
        }

        xx <- range[1]:range[2];
        df <- data.frame(x=xx, y=userLog$sim.rst$samples[xx]);
        np <- nearPoints(df, input$p_click, xvar="x", yvar="y");

        if (nrow(np) > 0)
            userLog$selected <- np[1,'x'];
    })

    ##brush
    observe({
        brush <- input$p_brush;
        if (!is.null(brush)) {
            userLog$range <- c(ceiling(brush$xmin),
                               floor(brush$xmax));
        }
    })

    ##color all the points for plotting
    col.points <- reactive({

        samples  <- userLog$sim.rst$samples;
        weco     <- userLog$weco;
        dis.t    <- userLog$sim.rst$dis;
        dis.len  <- userLog$sim.rst$len;
        cols     <- get.point.col(samples, dis.t, dis.len, weco$weco, weco$runin);
        cols
    })

    ##plot the points
    output$outPlotGrowth <- renderPlot({

        sim.rst  <- userLog$sim.rst;
        weco     <- userLog$weco;
        range    <- userLog$range;
        selected <- userLog$selected;

        if (is.null(range)) {
            range <- c(1,length(sim.rst$samples));
        }

        plot.points(sim.rst$samples, sim.rst$dis, sim.rst$len,
                    weco$weco, runin=weco$runin,
                    cols=col.points(),
                    start=range[1], end=range[2], selected=selected);
    }, bg="transparent");

    ##weco violations
    output$uiDataWeco <- DT::renderDataTable({
        dt <- userLog$weco$weco;
        if (is.null(dt))
            return(NULL);

        selected <- userLog$selected;
        runin    <- userLog$weco$runin;
        if (!is.null(selected)) {
            inx <- which(dt[,1] == selected)
            dt <- dt[inx,,drop=FALSE];
        }

        colnames(dt) <- c("Index", paste("Rule ", 1:(ncol(dt)-1), sep=""));
        data.frame(dt);
    }, server = FALSE, rownames=NULL, selection="single",
                                             options=list(pageLength=50, searching=FALSE));

    ##------------------------------------
    ##---------download-------------------
    ##------------------------------------
    output$uiDtnData <- renderUI ({
        if (is.null(get.data())) {
            msg.box("Please upload data first.", "warning");
        } else if (0 == length(userLog$selected.rules)) {
            msg.box("Please select WECO rules first.", "warning");
        } else if (is.null(userLog$results)) {
            msg.box("Please check signals first.", "warning");
        } else {
            div(wellPanel(h4("Download Results"),
                          downloadButton("btnDload", "Download")),
                wellPanel(h4("Results"),
                          tabsetPanel(tabPanel("Selected Rules",
                                               htmlOutput("txtRules")),
                                      tabPanel("Signals",
                                               DT::dataTableOutput("tblResults"))
                                      )))
        }
    })

    output$tblResults <- DT::renderDataTable({
                                 get.weco.all();
                             },
                             rownames=NULL,
                             options=list(pageLength=100, scrollX = TRUE))

    output$txtRules <- renderText({

        choices <- get.choices();

        rst <- "<div style='padding-top=20px'> <h6> The results used the following rules:</h6> <ul> ";
        for (i in 1:length(choices)) {
            rst <- paste(rst, "<li>");
            rst <- paste(rst, choices[i]);
            rst <- paste(rst, "</li>");
        }
        rst <- paste(rst, "</ul></div>");
        HTML(rst);
    })

    output$btnDload <- downloadHandler(
        filename=function() {
        paste('signals_',
              format(Sys.time(), "%m%d%Y%H%M%S"),
              '.txt',sep="")
    },

    content=function(file) {
        tfile <- tempfile();
        write.table(get.weco.all(), tfile, row.names=FALSE);
        bytes <- readBin(tfile, "raw", file.info(tfile)$size);
        writeBin(bytes, file);
    })

})
