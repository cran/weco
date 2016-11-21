##-------------------------------------------------------------
##           CONSTANTS
##-------------------------------------------------------------
get.const <- reactive({
    list(all.rules=1:8);
})

##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

##show different type of messages
msg.box <- function(contents, type="info") {
    switch(type,
           info    = cls <- "cinfo",
           warning = cls <- "cwarning",
           success = cls <- "csuccess",
           error   = cls <- "cerror");
    rst <- '<div class="';
    rst <- paste(rst, cls, '">');
    rst <- paste(rst, contents);
    rst <- paste(rst, "</div>");
    HTML(rst);
}

##tabset for data uploading
tab.upload <- function(){
    tabPanel("Upload Data",
             fluidPage(
                 wellPanel(h4("Upload data"),
                           msg.box('Please upload data file.'),
                           fluidRow(
                               column(3, h6("Choose File"),
                                      fileInput(inputId = 'userdata', label = '',
                                                accept=c('text/csv','text/comma-separated-values,text/plain'))),
                               column(2, h6("Separator"),
                                      radioButtons('sep', '',
                                                   c(Comma=',',Semicolon=';',Tab='\t',Space=' '),
                                                   '\t')),
                               column(2, h6("Quote"),
                                      radioButtons('quote', '',
                                                   c(None='','Double Quote'='"','Single Quote'="'"),
                                                   selected = '')),
                               column(2, h6("NA string"),
                                      radioButtons('nastrings', '', c('.'='.','NA'="NA"), selected = 'NA')),
                               column(2, h6("Other"),
                                      checkboxInput(inputId='header', label='Header', value=TRUE),
                                      checkboxInput(inputId="displaydata", label = "Show Data", value = TRUE)
                                      )
                           )),
                 uiOutput("uiSelectVar"),
                 conditionalPanel(condition="input.displaydata == true",
                                  wellPanel(h4("Review Data"),
                                            DT::dataTableOutput("uiData")))
                 )
             )
}

##tabset for select rules
tab.rules <- function(){
    tabPanel("Select Rules",
             wellPanel(
                 fluidRow(
                     column(6,
                            h4("Select Individual or Combination of Rules"),
                            actionButton("btnAdd", "Add Rule"),
                            lapply(get.const()$all.rules, function(x) {
                                cur.weco <- weco.info(x);
                                pars     <- cur.weco$pars;
                                div(class="content",
                                    checkboxInput(inputId = paste("inChkRule", x, sep=""),
                                                  label = HTML(paste("<b>",cur.weco$description, "</b>",
                                                                     sep=""))),
                                    div(class="intro",
                                        fluidRow(
                                            lapply(1:length(pars),
                                                   function(y) {
                                                column(3,
                                                       numericInput(inputId = paste("inR",x,names(pars)[y],sep=""),
                                                                    label = names(pars)[y],
                                                                    value = pars[y],
                                                                    min   = 0
                                                                    )
                                                       )
                                            })
                                        ))
                                    )
                            })
                            ),
                     column(6,
                            h4("Selected Rules"),
                            actionButton("btnDel", "Remove Rule"),
                            uiOutput("uiRules")
                            )
                     )
             ));
}


tab.main <- function() {
    tabsetPanel(type = "pills",
                id="mainpanel",
                selected="Upload Data",
                tabPanel("About", includeHTML("www/weco_about.html")),
                tab.upload(),
                tab.rules(),
                tabPanel("Signals", uiOutput("uiTabSig")),
                tabPanel("Download",
                         uiOutput("uiDtnData"))
                );
}


##-------------------------------------------------------------
##           SERVER FUNCTIONS
##-------------------------------------------------------------

##read data from user input
get.data <- reactive({
    inFile <- input$userdata;
    if (is.null(inFile)){
        rst <- NULL;
    } else{
        rst <- read.csv(inFile$datapath,
                        header=input$header,
                        sep=input$sep,
                        quote=input$quote,
                        na.strings=input$nastrings);
        ##rst$pid    <- 1:nrow(rst);
        names(rst) <- toupper(names(rst));
    }
    rst
})

## get the series data
get.x <- reactive({
    cur.data <- get.data();
    if (is.null(cur.data))
        return(NULL);

    if (is.null(input$inVar))
        return(NULL);

    cur.x <- cur.data[,input$inVar];
    rst   <- cur.x[which(!is.na(cur.x))];

    attr(rst, "name") <- input$inVar;

    if (0 == length(rst))
        return(NULL);

    rst
})

## get all rules with its defaults and pars
get.rules.pars <- reactive({
    rst <- NULL;
    for (i in get.const()$all.rules) {
        rst[[i]] <- weco.info(i);
    }
    rst
})


## get selected rules in text
get.choices <- reactive({
    if (0 == length(userLog$selected.rules))
        return(NULL);

    sel.rules <- userLog$selected.rules;
    choices   <- NULL;
    for (i in 1:length(sel.rules)) {
        cur.ch <- paste("<b> Check ", i, "</b><p>", sep="");
        for (j in 1:length(sel.rules[[i]])) {
            cur.r   <- sel.rules[[i]][[j]];
            cur.des <- do.call(weco.info, cur.r)$description;
            if (j > 1) {
                cur.ch <- paste(cur.ch, "<b> and </b>", sep = "");
            };
            cur.ch <- paste(cur.ch, cur.des, sep="");
        }
        choices <- c(choices, cur.ch);
    }

    choices;
})

##get checking data frame
get.weco.all <- reactive({
    if (is.null(userLog$results)) {
        return(NULL);
    }

    rst <- get.x();
    for (i in 1:length(userLog$results)) {
        rst <- cbind(rst, userLog$results[[i]]$weco);
    }

    colnames(rst) <- c(attr(get.x(), "name"),
                       paste("Check", 1:length(userLog$results), sep=""));

    data.frame(rst);
})
