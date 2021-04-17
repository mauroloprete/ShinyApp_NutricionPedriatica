if(!require(pacman)) install.packages("pacman")

p_load(
  shiny,
  shinythemes,
  tidyverse,
  magrittr,
  DT,
  openxlsx,
  lubridate,
  readxl,
  shinyWidgets,
  ggrepel
)
load("Data.Rdata")
if (utils::packageVersion("ggmosaic") < "0.3.0") {
  require("devtools")
  devtools::install_github("haleyjeppson/ggmosaic")
} else {
  library(ggmosaic)
}


ninios %<>%
  rename(
    "Fuma" = 'EM12',
    "Depresión" = 'EM5_NE',
    "Hierro" = 'EM20_1',
    "Importancia" = "SI23_NE_1",
    "Obligatoriedad" = 'SI23_NE_2',
    'EfectosAdversos' = 'SI23_NE_3',
    'Informacion' = 'SI23_NE_4',
    'Recomendacion' = 'SI23_NE_5'
  )

ninios %<>%
  mutate(
    Fuma = recode(
      as.factor(Fuma),
      "1" = "Si",
      "2" = "No",
      "99" = "Ns/Nc"
    ),
    Hierro = recode(
      as.factor(Hierro),
      "1" = "Si",
      "2" = "No",
      "99" = "Ns/Nc"
    ),
    Depresión = recode(
      as.factor(Depresión),
      "1" = "Si",
      "2" = "No",
      "99" = "Ns/Nc"
    )
  )

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("Nutrición Pediatrica"),
  navbarPage(
    "Menú",
#Pantalla de Bienvenida
    tabPanel("Inicio",
             mainPanel(
               h1("¡Bienvenido!"),
               br(),
               h4(
                 "En esta página encontrarás algunos de los resultados de la Encuesta de Nutrición,Desarrollo Infantil y Salud (ENDIS)."
               ),
               h4(
                 "Además podrás también realizar el análisis de las medidas antropométricas en la sección calculadora si cuentas con una tabla con datos correspondientes a dichas medidas."
               ),
               br(),
               div(
                 "La herramienta admite hojas de cálculo en formato XLSX (Excel) las cuales deben contener la siguiente información"
               ),
               tags$ul(
                 tags$li("Nombre"),
                 tags$li("Fecha de Nacimiento"),
                 tags$li("Fecha de la Entrevista"),
                 tags$li("Sexo"),
                 tags$li("Peso"),
                 tags$li("Talla"),
                 tags$li("Perímetro Cefálico")
               ),
               br(),
               h4(
                 "Puede descargar la siguiente tabla para probar dicha herramienta:"
               ),
               tags$a(href = "https://1drv.ms/x/s!AjeTXjPRNDU8kYsIN0pt5l6wZ_Mqtw?e=5yFmNX", "Tabla")
             )),
    tabPanel(
      "Resultados ENDIS",
# En esta pestaña se mostraran los resultados de la ENDIS
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Estado de Salud de Niños",
          sidebarPanel(selectInput(
            "medida",
            "Medida a Explorar",
            c("Peso", "PC", "IMC") #Peso para la edad , perimetro cefálico y Indice de Masa corporal
          )),
          mainPanel(plotOutput("disp"))
        ),
        tabPanel(
          "Condiciones en el Embarazo",
          sidebarPanel(
            selectInput("med", "Medida a Explorar", c("Peso", "PC", "IMC")),
            selectInput(
              "variable",
              "Condiciones durante el embarazo",
              c("Fuma", "Depresión", "Hierro")
            )
          ),
          mainPanel(plotOutput("plot"))
        ),
        tabPanel(
          'Opinion sobre vacunas',
          sidebarPanel(
            selectInput("medvacu", "Medida a Explorar", c("Peso", "PC", "IMC")),
            selectInput(
              'vacunas',
              "Opiniones (Escala del 1 al 5)",
              c(
                'Importancia',
                'Obligatoriedad',
                'EfectosAdversos',
                'Informacion',
                'Recomendacion'
              )
            )
          ),
          mainPanel(plotOutput("Marimekko"))
        )
      )
    ),
    tabPanel(
#En esta pestaña el usuario podrá cargar su hoja de cálculo en Excel.
      "Calculadora",
      sidebarPanel(
        fileInput("datafile",
                  "Selecciona el archivo xlsx",
                  accept = c("xlsx")),
        uiOutput("Nombre"),
#Los selectores que se mostraran una vez que la persona suba el archivo
        uiOutput("Sexo"),
# Son UI output , por que se generan una vez que el usuario interactua , en este caso
        uiOutput("Nacimiento"),
#Con la calculadora.
        uiOutput("Medida"),
        uiOutput("Peso"),
        uiOutput("Altura"),
        uiOutput("Percefa"),
        actionButton("ok", "Procesar"),
#Para que no aparezcan errores en la pantalla , usamos un Action Button
#Únicamente las indicaciones se harán al presionarlo.
        downloadButton("tablasave", "Tabla") #Se generará una tabla procesada con todos los niños.
      ),
      mainPanel(
        flowLayout(uiOutput("Uimedidacalcu"), #Como nos interesa el analsis por niño , creamos un selector de niños
                   uiOutput("FiltrarCalcu")),
        br(),
        plotOutput("graficodis"),
#El gráfico de los percentiles
        dataTableOutput("Tabla") #Tabla donde muestra por niño seleccionado el estado en cada visita.
      )
    )
  )
)

server <- function(input, output) {
  #Endis :

  titulo <-
    reactive({
    #Hicimos un objeto reacitvo , ya que nos interesba explicitar que en la medida de pérmietro
    #Cefálico únicamente se incluian los niños menores a tres años.
    if (input$medida == "PC") {
      return("Mediciones en infantes de 0 a 3 años")
    }
    else {
      # En el caso que sea quiera ver el IMC o el peso , no tendrá titulo.
      return(NULL)
    }
  })

  filtradoReactivo <- reactive({
    if (input$medida == "PC") {
      #Para que no quede un espacio sin puntos , es decir apartir de los tres años ,
      #Filtramos la tabla.
      return(37)
    }
    else {
      return(60)
    }
  })

  output$disp <- renderPlot({
    ninios %>%
      filter(.data[[paste("Estado", input$medida, sep = "")]] != "Sin Información") %>% #Usamos paste , ya qué nos ayuda
    #una vez seleccionada una variable
    #Poder elegir su clasificación.
    filter(Edad_meses < filtradoReactivo()) %>%
      ggplot(aes(AniosenMeses,
                 .data[[input$medida]],
                 colour = .data[[paste("Estado", input$medida, sep = "")]])) +
      geom_point(alpha = 0.4) +
      scale_x_continuous() +
      facet_grid(~Sexo) +
      scale_color_manual(
        values = c(
          "Óptimo" = "#34BF00",
          "Atención" = "#FFFF00",
          "Peligroso" = "#FFA600",
          "Muy Peligroso" = "#FF0000",
          "Sin Información" = "black"
        )
      ) +
      labs(
        x = "Edad (en Años)",
        y = (paste(input$medida, collapse = " ")),
        title = paste(titulo()),
        color = (paste(
          "Situación según", input$medida, collapse = " "
        ))
      ) +
      theme(
        legend.position = "bottom",
        aspect.ratio = 1,
        title = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 15,
                                    color = "#2F0B3A"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
      )
  })


  #Condiciones durante el embarazo :
  nombre <- reactive({
    switch(
      input$variable,
      Fuma = "¿Fumó durante el embarazo?",
      Depresión = "¿Le diagnosticaron depresión durante el embarazo?",
      Hierro = "¿Tomó hierro durante el embarazo?"
    )
  })

  output$plot <- renderPlot({
    ninios %>%
      filter(.data[[paste("Estado", input$med, sep = "")]] != 'Sin Información') %>%
      filter(.data[[eval(input$variable)]] != "Ns/Nc") %>%
      group_by(.data[[eval(input$variable)]], Sexo, .data[[paste("Estado", input$med, sep = "")]]) %>%
      summarise(Conteo = n(), .groups = "drop_last") %>%
      mutate(prop = Conteo / sum(Conteo)) %>%
      select(-Conteo) %>%
      ggplot(aes(x = .data[[input$variable]], y = prop, fill = .data[[paste("Estado", input$med, sep = "")]])) +
      geom_bar(stat = "identity") +
      labs(x = paste(nombre()),
           y = "Porcentaje",
           fill = (paste(
             "Situación según", input$med, collapse = " "
           ))) +
      scale_y_continuous(labels = scales::percent) +
      theme(
        aspect.ratio = 1,
        legend.position = "bottom",
        title = element_text(size = 12),
        strip.text.x = element_text(size = 12, color = "#2F0B3A"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
      ) +
      scale_fill_manual(
        values = c(
          "Óptimo" = "#34BF00",
          "Atención" = "#FFFF00",
          "Peligroso" = "#FFA600",
          "Muy Peligroso" = "#FF0000",
          "Sin Información" = "black"
        )
      ) +
      facet_grid(~Sexo) +
      geom_text(aes(label = scales::percent(round(prop, 2))), position = position_stack(vjust = 0.5))
  })

  vacunareactiva <- reactive({
    switch(
      input$vacunas,
      Importancia = "Las vacunas son importantes para los niños",
      Obligatoriedad = 'Las vacunas obligatorias del MSP son beneficiosas',
      EfectosAdversos = 'Me preocupan los posibles efectos adversos de las vacunas',
      Informacion = 'Información oficial sobre MSP sobre vacunas confiables',
      Recomendacion = 'Vacuno a mi hijo si me lo recomienda el equipo de Salud'
    )
  })

  output$Marimekko <- renderPlot({
    ninios %>%
      filter_at(
        vars(
          'Importancia',
          'Obligatoriedad',
          'EfectosAdversos',
          'Informacion',
          'Recomendacion'
        ),
        all_vars(. < 99)
      ) %>%
      filter(.data[[paste("Estado", input$medvacu, sep = "")]] != 'Sin Información') %>%
      ggplot() +
      geom_mosaic(aes(x = product(!!sym(input$vacunas)), fill = !!sym(
        paste("Estado", input$medvacu, sep = '')
      ))) +
      labs(
        x = paste(vacunareactiva()),
        y = paste("Situación según", input$medvacu, collapse = " ")
      ) +
      theme(
        aspect.ratio = 1,
        legend.position = "bottom",
        title = element_text(size = 12),
        strip.text.x = element_text(size = 12, color = "#2F0B3A"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
      ) +
      scale_fill_manual(
        values = c(
          "Óptimo" = "#34BF00",
          "Atención" = "#FFFF00",
          "Peligroso" = "#FFA600",
          "Muy Peligroso" = "#FF0000",
          "Sin Información" = "black"
        )
      )
  })


  #Calculadora :

  #Carga los datos :

  filedata <- reactive({
    req(input$datafile)
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read_excel(infile$datapath)
  })

  #Para que la persona que suba su Excel e indique donde estan los nombres:
  output$Nombre <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }

    items = names(df)
    names(items) = items
    selectInput("selnombre",
                "Nombre:",
                items)

  })


  #Para que la persona que suba su Excel indique donde esta la columna con los sexos :

  output$Sexo <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }

    items = names(df)
    names(items) = items
    selectInput("selsexo",
                "Sexo:",
                items)
  })

  # Tablero para indicar donde estan las fechas de nacimiento

  output$Nacimiento <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }

    items = names(df)
    names(items) = items
    selectInput("selnaci",
                "Fecha de Nacimiento:",
                items)
  })

  #Aquí se indican la fecha de cuando se tomarón las medidas :

  output$Medida <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }

    items = names(df)
    names(items) = items
    selectInput("selmedi",
                "Fecha de cuando se tomarón las medidas:",
                items)
  })

  # Ahora con el peso (en kg)

  output$Peso <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }
    items = names(df)
    names(items) = items
    selectInput("selpeso",
                "Peso:",
                items)
  })

  # Altura en M :

  output$Altura <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }
    items = names(df)
    names(items) = items
    selectInput("selaltura",
                "Altura:",
                items)
  })

  #Périmetro Cefálico

  output$Percefa <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }
    items = names(df)
    names(items) = items
    selectInput("selpercef",
                "Périmetro Cefálico:",
                items)
  })


  # Aca se van a realizar las transformaciones que hicimos para nuestro trabajo :

  filedatatrans <- eventReactive(input$ok, {
    #Para que no existan errores, llamamos a todos los selectores y al ser entradas , son funciones.
    req(input$selnaci)
    req(input$selmedi)
    req(input$selsexo)
    req(input$selpeso)
    req(input$selaltura)
    df <- filedata() # Enmascaramos para poder usar Dplyr
    df %>%
      transmute(
        Nombre = .data[[input$selnombre]],
    #Que use el filtro que creamos para el niño.
        Fecha_nacimiento = ymd(.data[[input$selnaci]]),
    #Para establecerlo en formato fecha
        Fecha_entrevista = ymd(.data[[input$selmedi]]),
    #Fecha
        Dias = difftime(Fecha_entrevista, Fecha_nacimiento, units = "days") %>% #Lo calculamos en dias ya que la tablas
    #tienen sus recomendaciones de esta manera.
          as.numeric(),
    #Para que lo haga número para poder operar con él
        Edadenmeses = difftime(Fecha_entrevista, Fecha_nacimiento, units = "days") %>%
          as.numeric() %/% (365.25 / 12),
    #Ese factor es para llevar de dias a meses.
        Sexo = .data[[input$selsexo]],
    #Para facetear por sexo
        Peso = .data[[input$selpeso]],
        Altura = .data[[input$selaltura]],
        PC = .data[[input$selpercef]],
        IMC = (Peso) / (Altura) ^ 2
      ) %>%
      left_join(PuntajesZ) %>% #Incorporar la información de la OMS , la clave será dias y sexo.
    mutate(
        ZScorePeso = abs((Peso - MediaPeso) / DesvioPeso),
    #Al la hora de clasificar , nos interesa su desviación abs.
        ZScorePC = abs((PC - MediaCfaCabeza) / DesvioCfaCabeza),
        ZScoreIMC = abs((IMC - MediaMasaCorporal) / DesvioMasaCorporal),
        EstadoPeso = case_when(
    #Esta clasifciación fue basada en el carnet de los niños
          ZScorePeso > 3 ~ "Muy Peligroso",
          between(ZScorePeso, 2, 3) ~ "Peligroso",
          between(ZScorePeso, 1, 2) ~ "Atención",
          between(ZScorePeso, 0, 1) ~ "Óptimo",
          is.na(ZScorePeso) ~ "Sin Información"
        ),
        EstadoPC = case_when(
          ZScorePC > 3 ~ "Muy Peligroso",
          between(ZScorePC, 2, 3) ~ "Peligroso",
          between(ZScorePC, 1, 2) ~ "Atención",
          between(ZScorePC, 0, 1) ~ "Óptimo",
          is.na(ZScorePC) ~ "Sin Información"
        ),
        EstadoIMC = case_when(
          ZScoreIMC > 3 ~ "Muy Peligroso",
          between(ZScoreIMC, 2, 3) ~ "Peligroso",
          between(ZScoreIMC, 1, 2) ~ "Atención",
          between(ZScoreIMC, 0, 1) ~ "Óptimo",
          is.na(ZScoreIMC) ~ "Sin Información"
        ),
        Sexo = recode(Sexo, # Para que sea mas amigable el facetado
                      "1" = "Niño",
                      "2" = "Niña"),
        EstadoPeso = fct_relevel(
    #Para que tenga un orden coherente los niveles.
          EstadoPeso,
          "Muy Peligroso",
          "Peligroso",
          "Atención",
          "Óptimo"
        ),
        EstadoPC = fct_relevel(
          EstadoPC,
          "Muy Peligroso",
          "Peligroso",
          "Atención",
          "Óptimo"
        ),
        EstadoIMC = fct_relevel(
          EstadoIMC,
          "Muy Peligroso",
          "Peligroso",
          "Atención",
          "Óptimo"
        )
      )
  })

  #Aca comienza la visualización :

  #Vamos a dejar que la persona pueda elegir que quiere ver : input$medidacalcu es un select input igual que hicimos con el de la ENIDS

  # También queremos que se haga un analisis indiviual por niño se quiere


  output$Uimedidacalcu <- renderUI({
    req(filedatatrans())
    selectInput("medidacalcu",
                "Medida a Explorar",
                c("Peso", "PC", "IMC"))
  }) #Aca la persona puede seleccionar la variable , como ocurria con los resultados de la ENDIS.


  output$FiltrarCalcu <-
    renderUI({
    #Nos interesa las medidas individuales.
    req(input$selnombre)
    auxdf <- filedatatrans()
    selectInput("listaninos",
                  label = "Nombre del Niño",
                  choices = unique(auxdf["Nombre"]))
  })


  output$graficodis <- renderPlot({
    req(input$medidacalcu)
    req(input$listaninos)
    req(filedatatrans)
    auxdf <- filedatatrans()
    auxdf %>%
      left_join(PerfDesv, .) %>% #Esta tabla tiene los valores perfectamente desviados , la trayectoria de cada percentil
    filter(eval(input$medidacalcu) != "Sin Información") %>%
      filter(Nombre == paste(input$listaninos)) %>%
      filter(Medida == !!input$medidacalcu) %>%
      filter(QDesv %in% c(
        "+ SD1",
        "- SD1",
        "+ SD2",
        "- SD2",
        "+ SD3",
        "- SD3",
        "+ SD4",
        "- SD4"
      )) %>%
      ggplot(aes(Edadenmeses / 12, .data[[input$medidacalcu]], group = QDesv)) +
      geom_line(aes(Dias / 365, Valor),
                color = "grey",
                show.legend = F) +
      geom_point(aes(colour = .data[[paste("Estado", input$medidacalcu, sep = "")]])) +
      labs(x = "Edad (en Años)",
           y = (paste(input$medidacalcu, collapse = " ")),
           color = (paste(
             "Situación según", input$medidacalcu, collapse = " "
           ))) +
      geom_text(
        data = . %>%
          group_by(QDesv) %>%
          top_n(1, Valor) %>%
          select(QDesv, Valor),
        aes(
          label = QDesv,
          x = 5,
          y = Valor,
          color = QDesv
        ),
        angle = 45,
        show.legend = FALSE
      ) +
      scale_color_manual(
        values = c(
          "Óptimo" = "#34BF00",
          "Atención" = "#FFFF00",
          "Peligroso" = "#FFA600",
          "Muy Peligroso" = "#FF0000",
          "Sin Información" = "black",
          "+ SD1" = "#34BF00",
          "- SD1" = "#34BF00",
          "+ SD2" = "#FFFF00",
          "- SD2" = "#FFFF00",
          "+ SD3" = "#FFA600",
          "- SD3" = "#FFA600",
          "+ SD4" = "#FF0000",
          "- SD4" = "#FF0000"
        )
      ) +
      theme(
        aspect.ratio = 1,
        title = element_text(size = 15),
        axis.title = element_text(size = 12),
        axis.line = element_blank(),
        strip.text.x = element_text(size = 15,
                                    color = "#2F0B3A"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
      )
  },
  height = "auto")

  #Hay que sacar la leyenda de los desvios, no queda muy lindo y luego agregarle el desvio que corresponde con un geom_text

  output$Tabla <- renderDT({
    req(input$listaninos)
    df <- filedatatrans()
    df %>%
      filter(Nombre == paste(input$listaninos)) %>%
      select(Nombre, Dias, EstadoPeso, EstadoPC, EstadoIMC)
  })

  filedataexport <- reactive({
    dfe <- filedatatrans() #Enmascaramos
    dfe %>%
      select(-starts_with("Media"), - starts_with("Desvio")) #Queremos que la tabla solo tenga la información de sus niños
  })

  output$tablasave <- downloadHandler(
  #Para guardar la tabla.
    filename = function() {
      paste("Tabla", ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filedataexport(), file)
    }
  )
}

shinyApp(ui, server)