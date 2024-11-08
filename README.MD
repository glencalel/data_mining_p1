## Universidad de San Carlos de Guatemala  
## Facultad de Ingeniería
## Escuela de Postgrado
## Introducción a la Minería de Datos
#### Glen Abra-ham Calel Robledo
## <center>Proyecto Parte 1</center>
___

El desarrollo de este proyecto tiene por objetivo implementar algoritmos que encuentren reglas de asociación de datos, tales como Apriori y FP-Growth. Además se requiere utilizar un metodo de agrupamiento que divida un conjunto de datos en cluster, este algoritmo es K-means. A continuación, se describen los pasos para ejecutar localmente el proyecto. 

> ### 1. Descarga de Archivos

La fuente de datos se encontrará en el __Instituto Nacional de Estadística Guatemala__, el tema a evaluar será *Salud*, primero deberá ingresar al siguiente [enlace](https://www.ine.gob.gt/estadisticas-hospitalarias/), como se observa en la siguiente imagen, debe dirigirse en la sección de __Bases de datos__.

![](/imagenes/1.png)

En esta sección se podrá seleccionar el año que se desea descargar y el periodo. Veamos el siguiente ejemplo para descargar el set de datos correspondiente al año 2020.

![](/imagenes/2.png)

Como se observa, se tienen distintos recursos, el principal que nos interesa está en el apartado de __Base de Datos__, para este caso se debe descargar el archivo de __Servicios externos__. Adicional, también se debe descargar el archivo que contiene el __Diccionario de Variables__ para conocer con mayor detalle el signficiado de columna en el set de datos y el valor representativo.

![](/imagenes/3.png)

Desde el año 2022 hasta el año 2018 se tienen la misma cantidad de columnas, estas se listan a continuación:

- Año
- Mes
- Sexo
- Pueblo de pertenencia
- Edad
- Período de Edad
- Departamento de residencia
- Municipio de residencia
- Tipo de Consulta
- Causa de atención

__*Nota__: el año 2017 se descartó debido a que 2 columnas son diferentes en comparación con los años 2022 - 2018, y los años 2016 - 2015 sí tienen las mismas columnas, pero debido al descarte del año 2017 más adelante unicamente se unen los años 2022-2018. Además se debe tomar en cuenta que a partir del año 2014 hasta el 2009 se tienen unicamente 9 columnas (No tienen la columna *Tipo de Consulta*).

En la siguiente imagen se puede observar de forma preliminar el contenido del diccionario.

![](/imagenes/4.png)

Después de haber descargado el archivo se debe nombrar el archivo con el año correspondiente para tener una mejor orientación del archivo descargado para luego realizar la carga de estos desde RStudio. Se deberá descargar desde el año 2022 hasta el 2009, después se colocarán en una carpeta nombrada __files__ en la raíz del disco, adicional en la misma carpeta crear otra carpeta con el siguiente nombre __results__, tal y como se muestra en la siguiente imagen.

![](/imagenes/5.png)


> ### 2. Librerias
El proyecto se ejecutará en R, para tener el ambiente preparado es necesario tener instalado el siguiente paquete, el cual se podrá descargar desde el siguiente [enlace](https://cran.r-project.org/bin/windows/Rtools/).

Adicional, es necesario instalar los siguientes paquetes:
- __arules__: Este paquete brinda las herramientas necesarias para representar, manipular y analizar grandes conjuntos de datos y patrones de transacciones. 
- __haven__: Este paquete permite importar archivos en formatos de SPSS (.sav) a R, convirtiendolos en objetos que pueden ser manipulables.
- __dplyr__: Este paquete permite la manipulación y operaciones con data frames.

```
# INSTALL LIBRARIES
install.packages("arules")
install.packages("haven")
install.packages("dplyr")

# LOAD LIBRARIES
library(haven)
library(arules)
library(dplyr)
```

> ### 4. Set de datos

Para cargar el set de datos se utilizará la función __read_sav__.

```
# LOAD ALL DATA BY YEAR
data_2022 <- read_sav('C:\\files\\2022.sav')
data_2021 <- read_sav('C:\\files\\2021.sav')
data_2020 <- read_sav('C:\\files\\2020.sav')
data_2019 <- read_sav('C:\\files\\2019.sav')
data_2018 <- read_sav('C:\\files\\2018.sav')
data_2017 <- read_sav('C:\\files\\2017.sav')
data_2016 <- read_sav('C:\\files\\2016.sav')
data_2015 <- read_sav('C:\\files\\2015.sav')
data_2014 <- read_sav('C:\\files\\2014.sav')
data_2013 <- read_sav('C:\\files\\2013.sav')
data_2012 <- read_sav('C:\\files\\2012.sav')
data_2011 <- read_sav('C:\\files\\2011.sav')
data_2010 <- read_sav('C:\\files\\2010.sav')
data_2009 <- read_sav('C:\\files\\2009.sav')
```

Después de haber cargado cada set de datos podemos observar el contenido de cada uno, por ejemplo:

![](/imagenes/6.png)

En esta imagen se puede observar cada una de las columnas que se mencionaron en el diccionario y el valor de cada etiqueta.

> ### 3. Limpieza de datos

Ya que el objetivo es analizar la mayor cantidad de años el objetivo es la unión de cada set de datos, donde sea posible, para obtener mejores resultados, si se hace así como tal se encontrarán los siguientes errores.

![](/imagenes/7.png)

La solución es la siguiente:
```
data_2019$PPERTENENCIA <- labelled(data_2019$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2019$DEPTORESIDEN <- labelled(data_2019$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2019$MUNIRESIDEN <- labelled(data_2019$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))

data_2018$PPERTENENCIA <- labelled(data_2018$PPERTENENCIA, labels = attr(data_2022$PPERTENENCIA, "labels"))
data_2018$DEPTORESIDEN <- labelled(data_2018$DEPTORESIDEN, labels = attr(data_2022$DEPTORESIDEN, "labels"))
data_2018$MUNIRESIDEN <- labelled(data_2018$MUNIRESIDEN, labels = attr(data_2022$MUNIRESIDEN, "labels"))
```

__labelled__ esta función permite asignar etiquetas descriptivas a los valores, como en este caso había un problema con el set de datos de 2018 - 2019 en comparación al set de datos 2020 - 2022, lo que se hizo fue convertir las columnas que tenían problemas PPERTENENCIA, DEPTORESIDEN y MUNIRESIDEN a labelled.

> ### 4. Observaciones finales
Antes de ejecutar el script por completo primero debemos unir los set de datos donde sea posible, en este caso se unirán los siguientes rangos de años:
- 2022 - 2018
- 2016 - 2015
- 2014 - 2009

Para realizar dicha acción se utilizará la función bind_rows, en esta se indican los set de datos que se desean unir y la variable a la cual se le asignará el resultado final.

```
data_2022_2018 <- bind_rows(data_2022, data_2021, data_2020, data_2019, data_2018)
```