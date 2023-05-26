# Pronósticos Estacionales de Volúmenes y Caudales en Chile Central

Este proyecto se centra en la generación de pronósticos estacionales de volúmenes y caudales en la región central de Chile.

## Requisitos

Para utilizar este software, asegúrate de tener instalado lo siguiente:

- R, versión 4.0.0 o superior
- CDO (Operadores de Datos Climáticos), disponible en [la página del proyecto CDO](https://code.mpimet.mpg.de/projects/cdo)

## Dependencias de Paquetes de R

Se requieren los siguientes paquetes de R (descarga disponible en "base/Load_libraries.R"):

### Manipulación de Datos

- **data.table:** Extensión de `data.frame` para manipulación rápida de datos.
- **dplyr:** Un conjunto de herramientas para manipular eficientemente conjuntos de datos en R.
- **reshape2:** Para remodelar los datos de manera flexible.
- **tibble:** Una alternativa a los data frames que permite una sintaxis más limpia.

### Visualización de Datos

- **ggplot2:** Un sistema para crear gráficos basado en La Gramática de Gráficos.
- **gridExtra:** Proporciona funciones para organizar gráficos 'grid' (como ggplot2) en una página.
  
### Procesamiento de Fechas y Cadenas

- **lubridate:** Simplifica el manejo de fechas y horas.
- **stringr:** Facilita el trabajo con cadenas de texto.

### Importación/Exportación de Datos e Interacción Web

- **devtools:** Herramientas para el desarrollo de paquetes, incluyendo la capacidad de cargar paquetes.
- **ecmwfr:** Una interfaz a los servicios web de datos de ECMWF.
- **ncdf4:** Proporciona una interfaz de alto nivel de R a los archivos de datos netCDF (network Common Data Form).
- **magrittr:** Proporciona un operador de tubería para encadenar comandos.
- **glue:** Interpreta una cadena como código R, lo que permite el uso de nombres de variables en cadenas.
  
### Manejo de Datos Geoespaciales

- **sf:** Soporta 'simple features', una forma estandarizada de codificar datos vectoriales espaciales.
- **terra:** Para el manejo de datos espaciales y el procesamiento de SIG.
- **exactextractr:** Extracción rápida de conjuntos de datos de raster utilizando polígonos.

### Herramientas Estadísticas y de Aprendizaje Automático

- **caret:** Abreviatura de Classification And REgression Training, este paquete contiene funciones para simplificar el proceso de entrenamiento de modelos.
- **verification:** Proporciona utilidades para la verificación de pronósticos meteorológicos.
- **bestNormalize:** Encuentra la mejor transformación normalizadora para un vector.
- **MBC:** Corrección Multivariante del Sesgo de las Salidas de los Modelos Climáticos.
  
### Modelos y Herramientas Hidrológicas

- **TUWmodel:** Proporciona el TUWmodel, un modelo simple de precipitación-escorrentía.
- **airGR:** Suite de modelos hidrológicos para pasos de tiempo diarios y mensuales.
- **hydromad:** Evaluación y Desarrollo de Modelos Hidrológicos.
  
### Otros Paquetes de Utilidad

- **rlist:** Un conjunto de herramientas para trabajar con objetos de lista.
- **icesTAF:** Proporciona herramientas para el Marco de Evaluación Transparente de ICES.
- **reshape2:** Reestructura y agrega datos de manera flexible.
