# Shiny App 

Shiny es un paquete desarrollado para el lenguaje de progrmación R. Shiny tiene como objetivo ser una herramienta para desarrollar 
dashboards interactivos para mostrar resultados a público en general.

# Nutrición Pediatrica 

La aplicación fue desarrollada como trabajo final de Nuevas Tecnologías para el Ánalisis Estadístico de Datos, materia de la Licenciatura en Estadística. 
Dentro de sus caracteristicas principales se encuentra la visualización de medidas antropometricas con datos de la ENDIS (Encuesta de Nutrición Desarrollo Infantil y Salud.)

También cuenta con un apartado para procesar repetir los ánalisis con una tabla de datos propia. 

# Uso de Calculadora : Nutrición Pediátrica

Para utilizar dicha herramienta deberá de contar con una hoja de cálculo en formato Excel (xlsx) y en cada columna deberá de tener la siguiente información :

    - Nombre :
    
    Esto hace posible el estudio individual de cada niño, es importante tener el nombre completo en una única columna y en formato texto.
    
    -  Sexo :
    
    En esta variable se deberá de indicar el género del niño , para varones "1" mientras que para niñas "2"
    
    -  Fecha de Nacimiento :
    
    Debe estar escrita en formato fecha , esto se debe a que las mediciones de la OMS se encuentran en días. Se debe ingresar a fecha de nacimiento de todo niño.
    
    -  Fecha de Entrevista :
    
    Esta fecha también deberá de encontrarse en formato fecha, la diferencia entre el nacimiento y entrevista será la utilizada para obtener el puntaje Z.
    Como se explicó anteriormente, haciendo uso de ambas se calcula la edad en días del infante.
    
    -  Peso :
    
    Medida que debe estar registrada en Kilogramos (Kg) para cualquier edad.
    
    -  Talla/Altura :
    
    Esta medida se utiliza para el cálculo del IMC que también es calculado por el especialista al momento de la medición. Las mediciones deben estar expresadas en {Metros (m)
    
    -  Perímetro Cefálico :
    
    Esta medida debe expresarse en centímetros (cm) y los decimales deberán de registrarse con ",".

En la sección calculadora encontrará un botón que al ser presionado abrirá el explorador de archivos de su computadora, en el mismo se deberá de indicar la ruta local.


Una vez realizado el proceso de carga, encontrará unos "selectores", en cada uno de ellos se le deberá indicar el nombre de la columna que contiene la variable correspondiente.


Una vez seleccionadas las variables necesarias para el análisis deberá de seleccionar procesar , al instante se desplegará una gráfica y una tabla.


También se puede descargar la tabla procesada en formato xlsx (formato compatible con Excel).