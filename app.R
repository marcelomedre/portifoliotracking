#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(BatchGetSymbols)
library(GetTDData)
library(Quandl)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(tibble)

source("mykey.R")

####################################
##############   UI   ##############
####################################

ui <- dashboardPage(
        dashboardHeader(title = "Avaliação Histórica de Portifólio de Ações",
                        titleWidth = 1000),
        ## Sidebar content
        dashboardSidebar(
                width = 300,
                sidebarUserPanel("Marcelo Nobrega"),
                sidebarMenu(
                        menuItem("Sobre", tabName = "about", icon = icon("book")),
                        menuItem("Portifólio tracker", tabName = "portfolio", icon = icon("line-chart")),
                        menuItem("Disclaimer", tabName = "disclaimer", icon = icon("exclamation-triangle")),
                        menuItem("Reporte um bug ou faça uma sugestão", href = "https://github.com/marcelomedre/portifoliotracking", icon = icon("comment")),
                        menuItem("Código do App", href = "https://github.com/marcelomedre/portifoliotracking", icon = icon("github"))
                )
        ),
        dashboardBody(
                tabItems(
                        # First tab content
                        tabItem(tabName = "about",
                                fluidRow(
                                        box(),
                                        
                                        box()
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "portfolio",
                                box(
                                title = "Escolha até 5 empresas",
                                status = "primary", 
                                solidHeader = T,
                                h5("Digite os códigos em CAIXA ALTA"),
                                textInput(
                                        inputId = "asset1",
                                        label = "Código da Empresa",
                                        value = "ABEV3"),
                                textInput(
                                        inputId = "asset2",
                                        label = "",
                                        value = "BBAS3"),
                                textInput(
                                        inputId = "asset3",
                                        label = "",
                                        value = "ITUB3"),
                                textInput(
                                        inputId = "asset4",
                                        label = "",
                                        value = "MGLU3"),
                                textInput(
                                        inputId = "asset5",
                                        label = "",
                                        value = "VVAR3"),
                                dateRangeInput("port_dates1a",
                                               label = "Selecione o período:", 
                                               # default date shows past half year
                                               start = Sys.Date()-1825, 
                                               end = Sys.Date()-3),
                                height = 550, 
                                width = 6
                                ),
                                box(
                                        title = "Peso de cada empresa no Portifolio",
                                        status = "primary", 
                                        solidHeader = T,
                                        h5(""),
                                        textInput(
                                                inputId = "pesoasset1",
                                                label = "%",
                                                value = "20"),
                                        textInput(
                                                inputId = "pesoasset2",
                                                label = "",
                                                value = "20"),
                                        textInput(
                                                inputId = "pesoasset3",
                                                label = "",
                                                value = "20"),
                                        textInput(
                                                inputId = "pesoasset4",
                                                label = "",
                                                value = "20"),
                                        textInput(
                                                inputId = "pesoasset5",
                                                label = "",
                                                value = "20"),
                                        checkboxInput("BVSP", "Incluir índice Bovespa?", TRUE),
                                        checkboxInput("CDI", "Incluir CDI?", TRUE),
                                        checkboxInput("TD", "Incluir NTN-B Principal do Tesouro direto", TRUE),
                                        height = 550, 
                                        width = 6
                                ),
                               fluidRow(
                                       box(
                                               title = "Sua carteira",
                                               status = "primary",
                                               solidHeader = TRUE,
                                               DT::dataTableOutput("port_summary_table"),
                                               width = 12
                                       )
                               ),
                                
                                fluidRow(
                                        box(
                                                title="Retorno sobre o investimento", 
                                                status="success", 
                                                solidHeader = TRUE,
                                                plotOutput("graph1"), 
                                                height = 500, 
                                                width = 12
                                        )),
                               fluidRow(
                                       box(
                                               title="Resultado do Período para cada real investido", 
                                               status="success", 
                                               solidHeader = TRUE,
                                               plotOutput("graph2"), 
                                               height = 500, 
                                               width = 12
                                       )),
                               fluidRow(
                                       box(
                                               title="Risco x Retorno", 
                                               status="warning", 
                                               solidHeader = TRUE,
                                               plotOutput("graph3"), 
                                               height = 500, 
                                               width = 12
                                       ))
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "disclaimer",
                                h2("Disclaimer")
                        )
                )
        )
)

####################################
############   SERVER   ############
####################################

server <- function(input, output, session) {
        extract <- function(text){
                text <- gsub(" ", " ", text)
                split <- strsplit(text, ",", fixed = FALSE)[[1]]
                as.numeric(split)
        }
        
        # Portifolio table
        output$port_summary_table <- DT::renderDataTable({
                asset1 <- input$asset1
                asset2 <- input$asset2
                asset3 <- input$asset3
                asset4 <- input$asset4
                asset5 <- input$asset5
                pesoasset1 <- input$pesoasset1
                pesoasset2 <- input$pesoasset2
                pesoasset3 <- input$pesoasset3
                pesoasset4 <- input$pesoasset4
                pesoasset5 <- input$pesoasset5
                
                x <- c(asset1, asset2, asset3, asset4, asset5)
                y <- c(pesoasset1, pesoasset2, pesoasset3, pesoasset4, pesoasset5)
                DT::datatable(data.frame(x, y),
                              extensions = "Buttons",
                              colnames = c('Ticker', 'Percentual'),
                              options = list(
                                      lengthChange = FALSE,
                                      dom = "Blfrtip",
                                      buttons = c("copy", "csv", "excel", "pdf", "print")
                              ))
        })
        
        output$graph1 <- renderPlot({
        asset1 <- input$asset1
        asset2 <- input$asset2
        asset3 <- input$asset3
        asset4 <- input$asset4
        asset5 <- input$asset5
        tickers <- c(paste(asset1,"SA", sep = "."),
                     paste(asset2,"SA", sep = "."),
                     paste(asset3,"SA", sep = "."),
                     paste(asset4,"SA", sep = "."),
                     paste(asset5,"SA", sep = "."))
        #print(tickers)
        first.date = input$port_dates1a[1]
        last.date = input$port_dates1a[2]
        my.l <- BatchGetSymbols(tickers = tickers, 
                                first.date = first.date,
                                last.date = last.date)
        #print(my.l$df.control)
        df.stocks <- my.l$df.tickers[,c("price.adjusted", "ref.date", "ticker")]
        colnames(df.stocks) <- c("price", "ref.date", "ticker")

        # Getting BVSP data
        if(input$BVSP == TRUE){
                bvsp <- BatchGetSymbols(tickers = "^BVSP", 
                                        first.date = first.date,
                                        last.date = last.date)
                df.bvsp <- bvsp$df.tickers[,c("price.adjusted", "ref.date", "ticker")]
                colnames(df.bvsp) <- c("price", "ref.date", "ticker")
        }
                
        # Getting TD data
        if(input$TD == TRUE){
                download.TD.data(asset.codes = "NTN-B Principal")
                df.TD <- read.TD.files(maturity = "150824")
                # print(head(df.TD))
                idx <- (df.TD$ref.date >= first.date) & (df.TD$ref.date <= last.date)
                df.TD <- df.TD[idx,]
                df.TD$yield.bid <- NULL
                df.TD$matur.date <- NULL
                colnames(df.TD) <- c("ref.date", "price", "ticker")
        }
        
        # Getting TD data
        if(input$CDI == TRUE){
                my.key <- "vaxjQ8nvtX62qez6fhiS"
                Quandl.api_key(my.key)
                quandl.codes <- c("BCB/4389")
                df.CDI <- Quandl(quandl.codes,
                                 type = "raw",
                                 start_date = first.date,
                                 end_date = last.date)
                colnames(df.CDI) <- c("ref.date", "ret_year")
                df.CDI$ret_day <- (1+df.CDI$ret_year/100)^(1/252) - 1
                df.CDI <- df.CDI[order(df.CDI$ref.date),]
                df.CDI$price <- cumprod(1+df.CDI$ret_day)
                df.CDI$ticker <- "CDI"
                df.CDI$ret_year <- NULL
                df.CDI$ret_day <- NULL              
        }
        
        indices.df <- bind_rows(df.bvsp, df.TD, df.CDI)
        #print(head(indices.df))
        #Retornos Anuais
        my.ret <- indices.df %>%
                mutate(year = format(indices.df$ref.date, "%Y")) %>%
                group_by(ticker, year) %>%
                summarise(last.price = price[length(price)]) %>%
                mutate(ret = c(0,last.price[2:length(last.price)]/last.price[1:length(last.price)-1]-1)) %>%
                mutate(accu.ret = c(cumsum(ret))) 
        
        my.ret.st <- df.stocks %>%
                mutate(year = format(df.stocks$ref.date, "%Y")) %>%
                group_by(ticker, year) %>%
                summarise(last.price = price[length(price)]) %>%
                mutate(ret = c(0,last.price[2:length(last.price)]/last.price[1:length(last.price)-1]-1)) %>%
                mutate(accu.ret = c(cumsum(ret)))
        
        pesoasset1 <- input$pesoasset1
        pesoasset2 <- input$pesoasset2
        pesoasset3 <- input$pesoasset3
        pesoasset4 <- input$pesoasset4
        pesoasset5 <- input$pesoasset5
        weights <- as.numeric(c(pesoasset1, pesoasset2, pesoasset3, pesoasset4, pesoasset5))/100
        #print(weights)
        my.ret.st$accu.ret.pond <- my.ret.st$accu.ret*weights
        #print(head(my.ret.st))
        portfolio <- as.data.frame(
                my.ret.st %>%
                        group_by(year) %>%
                        mutate(retorno = sum(accu.ret.pond)) %>%
                        select(c(year, retorno)) %>%
                        distinct()%>%
                        pull())
        portfolio$year <- seq(from = (format(first.date, "%Y")), format(last.date, "%Y"), by = 1)
        portfolio$ticker <- "portfolio"
        colnames(portfolio) <- c("accu.ret", "year", "ticker")
        #print(head(portfolio))
        #print(head(my.ret))
        g1 <- ggplot()+
                geom_line(my.ret, mapping = aes(x = as.numeric(year), y = accu.ret, col = ticker), size = 1.5) +
                geom_line(portfolio, mapping = aes(x = as.numeric(year), y = accu.ret,  col = ticker), size = 1.5)+
                labs(x = "ANO", y = "Retorno Anualizado")+
                theme(legend.position = "bottom")+
                theme(legend.text = element_text(size = 12))+
                theme(legend.title = element_blank())+
                theme(axis.text=element_text(size=12, face = "bold"),
                      axis.title=element_text(size=14,face="bold"))
        g1
        ################################################ 

        })
        
        output$graph2 <- renderPlot({
                asset1 <- input$asset1
                asset2 <- input$asset2
                asset3 <- input$asset3
                asset4 <- input$asset4
                asset5 <- input$asset5
                tickers <- c(paste(asset1,"SA", sep = "."),
                             paste(asset2,"SA", sep = "."),
                             paste(asset3,"SA", sep = "."),
                             paste(asset4,"SA", sep = "."),
                             paste(asset5,"SA", sep = "."))
                #print(tickers)
                first.date = input$port_dates1a[1]
                last.date = input$port_dates1a[2]
                my.l <- BatchGetSymbols(tickers = tickers, 
                                        first.date = first.date,
                                        last.date = last.date)
                #print(my.l$df.control)
                df.stocks <- my.l$df.tickers[,c("price.adjusted", "ref.date", "ticker")]
                colnames(df.stocks) <- c("price", "ref.date", "ticker")
                
                # Getting BVSP data
                if(input$BVSP == TRUE){
                        bvsp <- BatchGetSymbols(tickers = "^BVSP", 
                                                first.date = first.date,
                                                last.date = last.date)
                        df.bvsp <- bvsp$df.tickers[,c("price.adjusted", "ref.date", "ticker")]
                        colnames(df.bvsp) <- c("price", "ref.date", "ticker")
                }
                
                # Getting TD data
                if(input$TD == TRUE){
                        download.TD.data(asset.codes = "NTN-B Principal")
                        df.TD <- read.TD.files(maturity = "150824")
                        # print(head(df.TD))
                        idx <- (df.TD$ref.date >= first.date) & (df.TD$ref.date <= last.date)
                        df.TD <- df.TD[idx,]
                        df.TD$yield.bid <- NULL
                        df.TD$matur.date <- NULL
                        colnames(df.TD) <- c("ref.date", "price", "ticker")
                }
                
                # Getting TD data
                if(input$CDI == TRUE){
                        Quandl.api_key(my.key)
                        quandl.codes <- c("BCB/4389")
                        df.CDI <- Quandl(quandl.codes,
                                         type = "raw",
                                         start_date = first.date,
                                         end_date = last.date)
                        colnames(df.CDI) <- c("ref.date", "ret_year")
                        df.CDI$ret_day <- (1+df.CDI$ret_year/100)^(1/252) - 1
                        df.CDI <- df.CDI[order(df.CDI$ref.date),]
                        df.CDI$price <- cumprod(1+df.CDI$ret_day)
                        df.CDI$ticker <- "CDI"
                        df.CDI$ret_year <- NULL
                        df.CDI$ret_day <- NULL              
                }
                
                indices.df <- bind_rows(df.bvsp, df.TD, df.CDI)
                #print(head(indices.df))
                #Retornos Anuais
                my.ret <- indices.df %>%
                        mutate(year = format(indices.df$ref.date, "%Y")) %>%
                        group_by(ticker, year) %>%
                        summarise(last.price = price[length(price)]) %>%
                        mutate(ret = c(0,last.price[2:length(last.price)]/last.price[1:length(last.price)-1]-1)) %>%
                        mutate(accu.ret = c(cumsum(ret))) 
                
                my.ret.st <- df.stocks %>%
                        mutate(year = format(df.stocks$ref.date, "%Y")) %>%
                        group_by(ticker, year) %>%
                        summarise(last.price = price[length(price)]) %>%
                        mutate(ret = c(0,last.price[2:length(last.price)]/last.price[1:length(last.price)-1]-1)) %>%
                        mutate(accu.ret = c(cumsum(ret)))
                
                pesoasset1 <- input$pesoasset1
                pesoasset2 <- input$pesoasset2
                pesoasset3 <- input$pesoasset3
                pesoasset4 <- input$pesoasset4
                pesoasset5 <- input$pesoasset5
                weights <- as.numeric(c(pesoasset1, pesoasset2, pesoasset3, pesoasset4, pesoasset5))/100
                #print(weights)
                my.ret.st$accu.ret.pond <- my.ret.st$accu.ret*weights
                #print(head(my.ret.st))
                portfolio <- as.data.frame(
                        my.ret.st %>%
                                group_by(year) %>%
                                mutate(retorno = sum(accu.ret.pond)) %>%
                                select(c(year, retorno)) %>%
                                distinct()%>%
                                pull())
                portfolio$year <- seq(from = (format(first.date, "%Y")), format(last.date, "%Y"), by = 1)
                portfolio$ticker <- "portfolio"
                colnames(portfolio) <- c("accu.ret", "year", "ticker")
                #print(head(portfolio))
                #print(head(my.ret))
                
                # 1 real investido nos índices
                my.l.indices <- tapply(X = my.ret$ret, 
                                       INDEX = my.ret$ticker,
                                       FUN = function(x) cumprod(c(x+1)))
                my.l.indices <- my.l.indices[unique(my.ret$ticker)]
                my.ret$acum.ret <- unlist(my.l.indices)
                # 1 real investido no portifólio
                my.l.port <- tapply(X = portfolio$accu.ret, 
                                    INDEX = portfolio$ticker,
                                    FUN = function(x) cumprod(c(x+1)))
                my.l.port <- my.l.port[unique(portfolio$ticker)]
                portfolio$acum.ret <- unlist(my.l.port)
                
                g2 <- ggplot()+
                        geom_line(my.ret, mapping = aes(x = as.numeric(year), y = acum.ret, col = ticker), size = 1.5)+
                        geom_line(portfolio, mapping = aes(x = as.numeric(year), y = acum.ret, col = ticker), size = 1.5)+
                        labs(x = "ANO", y = "Resultado do Período para cada Real investido")+
                        theme(legend.position = "bottom")+
                        theme(legend.title = element_blank())+
                        theme(legend.text = element_text(size = 12))+
                        theme(legend.title = element_blank())+
                        theme(axis.text=element_text(size=12, face = "bold"),
                              axis.title=element_text(size=14,face="bold"))
                g2
                ################################################ 
                
        })
        output$graph3 <- renderPlot({
                asset1 <- input$asset1
                asset2 <- input$asset2
                asset3 <- input$asset3
                asset4 <- input$asset4
                asset5 <- input$asset5
                tickers <- c(paste(asset1,"SA", sep = "."),
                             paste(asset2,"SA", sep = "."),
                             paste(asset3,"SA", sep = "."),
                             paste(asset4,"SA", sep = "."),
                             paste(asset5,"SA", sep = "."))
                #print(tickers)
                first.date = input$port_dates1a[1]
                last.date = input$port_dates1a[2]
                my.l <- BatchGetSymbols(tickers = tickers, 
                                        first.date = first.date,
                                        last.date = last.date)
                #print(my.l$df.control)
                df.stocks <- my.l$df.tickers[,c("price.adjusted", "ref.date", "ticker")]
                colnames(df.stocks) <- c("price", "ref.date", "ticker")
                
                # Getting BVSP data
                if(input$BVSP == TRUE){
                        bvsp <- BatchGetSymbols(tickers = "^BVSP", 
                                                first.date = first.date,
                                                last.date = last.date)
                        df.bvsp <- bvsp$df.tickers[,c("price.adjusted", "ref.date", "ticker")]
                        colnames(df.bvsp) <- c("price", "ref.date", "ticker")
                }
                
                # Getting TD data
                if(input$TD == TRUE){
                        download.TD.data(asset.codes = "NTN-B Principal")
                        df.TD <- read.TD.files(maturity = "150824")
                        # print(head(df.TD))
                        idx <- (df.TD$ref.date >= first.date) & (df.TD$ref.date <= last.date)
                        df.TD <- df.TD[idx,]
                        df.TD$yield.bid <- NULL
                        df.TD$matur.date <- NULL
                        colnames(df.TD) <- c("ref.date", "price", "ticker")
                }
                
                # Getting TD data
                if(input$CDI == TRUE){
                        my.key <- "vaxjQ8nvtX62qez6fhiS"
                        Quandl.api_key(my.key)
                        quandl.codes <- c("BCB/4389")
                        df.CDI <- Quandl(quandl.codes,
                                         type = "raw",
                                         start_date = first.date,
                                         end_date = last.date)
                        colnames(df.CDI) <- c("ref.date", "ret_year")
                        df.CDI$ret_day <- (1+df.CDI$ret_year/100)^(1/252) - 1
                        df.CDI <- df.CDI[order(df.CDI$ref.date),]
                        df.CDI$price <- cumprod(1+df.CDI$ret_day)
                        df.CDI$ticker <- "CDI"
                        df.CDI$ret_year <- NULL
                        df.CDI$ret_day <- NULL              
                }
                
                indices.df <- bind_rows(df.bvsp, df.TD, df.CDI)
                #print(head(indices.df))
                #Retornos Anuais
                my.ret <- indices.df %>%
                        mutate(year = format(indices.df$ref.date, "%Y")) %>%
                        group_by(ticker, year) %>%
                        summarise(last.price = price[length(price)]) %>%
                        mutate(ret = c(0,last.price[2:length(last.price)]/last.price[1:length(last.price)-1]-1)) %>%
                        mutate(accu.ret = c(cumsum(ret))) 
                
                my.ret.st <- df.stocks %>%
                        mutate(year = format(df.stocks$ref.date, "%Y")) %>%
                        group_by(ticker, year) %>%
                        summarise(last.price = price[length(price)]) %>%
                        mutate(ret = c(0,last.price[2:length(last.price)]/last.price[1:length(last.price)-1]-1)) %>%
                        mutate(accu.ret = c(cumsum(ret)))
                
                pesoasset1 <- input$pesoasset1
                pesoasset2 <- input$pesoasset2
                pesoasset3 <- input$pesoasset3
                pesoasset4 <- input$pesoasset4
                pesoasset5 <- input$pesoasset5
                weights <- as.numeric(c(pesoasset1, pesoasset2, pesoasset3, pesoasset4, pesoasset5))/100
                #print(weights)
                my.ret.st$accu.ret.pond <- my.ret.st$accu.ret*weights
                #print(head(my.ret.st))
                portfolio <- as.data.frame(
                        my.ret.st %>%
                                group_by(year) %>%
                                mutate(retorno = sum(accu.ret.pond)) %>%
                                select(c(year, retorno)) %>%
                                distinct()%>%
                                pull())
                portfolio$year <- seq(from = (format(first.date, "%Y")), format(last.date, "%Y"), by = 1)
                portfolio$ticker <- "portfolio"
                colnames(portfolio) <- c("accu.ret", "year", "ticker")
                #print(head(portfolio))
                #print(head(my.ret))
                
                # 1 real investido nos índices
                my.l.indices <- tapply(X = my.ret$ret, 
                                       INDEX = my.ret$ticker,
                                       FUN = function(x) cumprod(c(x+1)))
                my.l.indices <- my.l.indices[unique(my.ret$ticker)]
                my.ret$acum.ret <- unlist(my.l.indices)
                # 1 real investido no portifólio
                my.l.port <- tapply(X = portfolio$accu.ret, 
                                    INDEX = portfolio$ticker,
                                    FUN = function(x) cumprod(c(x+1)))
                my.l.port <- my.l.port[unique(portfolio$ticker)]
                portfolio$acum.ret <- unlist(my.l.port)
                
                my.tab <- my.ret %>%
                        group_by(ticker) %>%
                        summarise(mean.ret = mean(ret, na.rm = T),
                                  sd.ret = sd(ret, na.rm = T),
                                  sharpe = mean.ret/sd.ret)
                
                my.tab.port <- portfolio %>%
                        group_by(ticker) %>%
                        summarise(mean.ret = mean(accu.ret, na.rm = T),
                                  sd.ret = sd(accu.ret, na.rm = T),
                                  sharpe = mean.ret/sd.ret)
                
                
                g3 <- ggplot()+
                        geom_point(my.tab, mapping = aes(x = sd.ret, y = mean.ret, col = ticker, size = mean.ret)) +
                        geom_point(my.tab.port, mapping = aes(x = sd.ret, y = mean.ret, col = ticker, size = mean.ret))+
                        annotate("text", x = my.tab$sd.ret - 0.007, 
                                 y = my.tab$mean.ret + 0.01,
                                 label = my.tab$ticker) +
                        annotate("text", x = my.tab.port$sd.ret - 0.007, 
                                 y = my.tab.port$mean.ret + 0.01,
                                 label = my.tab.port$ticker) +
                        labs(x = "Risco (desvio padrão)", 
                             y = "Retorno Esperado médio")+
                        theme(legend.position = "none")+
                        theme(legend.title = element_blank())+
                        theme(legend.text = element_text(size = 12))+
                        theme(legend.title = element_blank())+
                        theme(axis.text=element_text(size=12, face = "bold"),
                              axis.title=element_text(size=14,face="bold"))
                g3
                ################################################ 
                
        })
        
}

shinyApp(ui, server)

