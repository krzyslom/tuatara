library(dplyr)
library(readr)
library(stringi)
library(glmnet)
library(knitr)
# library(pander)
library(shiny)
library(shinythemes)

dane <- read_csv("../dane.csv")
model <- get(load("../model.Rda"))
cvLasso <- get(load("../cvLasso.Rda"))
iMin <- which(cvLasso$lambda == cvLasso$lambda.min)
iSE <- which(cvLasso$lambda == cvLasso$lambda.1se)


dane <-
  dane %>%
  mutate(
    lokalizacja = lokalizacja %>%
      stri_replace_first_regex(", Warszawa", "") %>%
      as.factor(),
    do_wynajecia_przez = do_wynajecia_przez %>% as.factor(),
    parking = parking %>% as.factor(),
    liczba_pokoi = liczba_pokoi %>% as.factor(),
    rodzaj_nieruchomosci = rodzaj_nieruchomosci %>% as.factor(),
    liczba_lazienek = liczba_lazienek %>% as.factor(),
    palacy = palacy %>% as.factor(),
    przyjazne_zwierzakom = przyjazne_zwierzakom %>% as.factor()
  )

shinyApp(
  shinyUI(
    fluidPage(
      theme = shinytheme("darkly"),
      fluidRow(
        column(
          4,
          selectInput(
            "lokalizacja",
            "Lokalizacja:",
            dane$lokalizacja %>% levels()
          )
        ),
        column(
          4,
          selectInput(
            "do_wynajecia_przez",
            "Do wynajęcia przez:",
            dane$do_wynajecia_przez %>% levels()
          )
        ),
        column(
          4,
          selectInput(
            "rodzaj_nieruchomosci",
            "Rodzaj nieruchomości:",
            dane$rodzaj_nieruchomosci %>% levels()
          )
        )
      ),
      fluidRow(
        column(
          4,
          numericInput(
            "wielkosc",
            "Wielkość (m2):",
            dane$wielkosc %>% summary() %>% .[3],
            dane$wielkosc %>% min(),
            dane$wielkosc %>% max()
          )
        ),
        column(
          4,
          selectInput(
            "liczba_pokoi",
            "Liczba pokoi:",
            dane$liczba_pokoi %>% levels()
          )
        ),
        column(
          4,
          selectInput(
            "liczba_lazienek",
            "Liczba łazienek:",
            dane$liczba_lazienek %>% levels()
          )
        )
      ),
      fluidRow(
        column(
          4,
          selectInput(
            "parking",
            "Parking:",
            dane$parking %>% levels()
          )
        ),
        column(
          4,
          selectInput(
            "palacy",
            "Palący:",
            dane$palacy %>% levels()
          )
        ),
        column(
          4,
          selectInput(
            "przyjazne_zwierzakom",
            "Przyjazne zwierzakom:",
            dane$przyjazne_zwierzakom %>% levels()
          )
        )
      ),
      fluidRow(
        column(
          4,
          textAreaInput(
            "opis",
            "Opis:",
            readLines("../overlook.txt")
          )
        )
      ),
      fluidRow(
        column(
          8,
          div(tableOutput("propozycja"), style = "font-size:160%"),
          offset = 3
        )
      )
    )
  ),
  shinyServer(
    function(input, output, session) {
      wycena <-
        eventReactive(
          c(
            input$lokalizacja,
            input$do_wynajecia_przez,
            input$rodzaj_nieruchomosci,
            input$wielkosc,
            input$liczba_pokoi,
            input$liczba_lazienek,
            input$parking,
            input$palacy,
            input$przyjazne_zwierzakom,
            input$opis
          ),
          {
            df <-
              data_frame(
                cena = 1,
                lokalizacja = input$lokalizacja %>%
                  factor(dane$lokalizacja %>% levels()),
                do_wynajecia_przez = input$do_wynajecia_przez %>%
                  factor(dane$do_wynajecia_przez %>% levels()),
                liczba_pokoi = input$liczba_pokoi %>%
                  factor(dane$liczba_pokoi %>% levels()),
                rodzaj_nieruchomosci = input$rodzaj_nieruchomosci %>%
                  factor(dane$rodzaj_nieruchomosci %>% levels()),
                wielkosc = input$wielkosc,
                parking = input$parking %>%
                  factor(dane$parking %>% levels()),
                liczba_lazienek = input$liczba_lazienek %>%
                  factor(dane$liczba_lazienek %>% levels()),
                palacy = input$palacy %>%
                  factor(dane$palacy %>% levels()),
                przyjazne_zwierzakom = input$przyjazne_zwierzakom %>%
                  factor(dane$przyjazne_zwierzakom %>% levels()),
                liczba_wyrazow = input$opis %>% stri_count_words()
              )
            mat <- model.matrix(cena~.-1, df)
            pred <- predict(model, mat)
            data_frame(
              Model = c("Najmniejszy błąd", "Elastyczny"),
              `Sugerowana cena wynajmu` = c(
                paste0(
                  "<center>",
                  "<font color=\"#FF0000\"><b>",
                  pred[iMin+1] %>% round(),
                  "</b></font>",
                  " PLN",
                  "</center>"
                ),
                paste0(
                  "<center>",
                  "<font color=\"#00FF00\"><b>",
                  pred[iSE+1] %>% round(),
                  "</b></font>",
                  " PLN",
                  "</center>"
                )
              )
            )
          }
        )
      output$propozycja <-
        renderTable(
          {wycena()},
          align = "c",
          sanitize.text.function = function(x) x
        )
    }
  )
)