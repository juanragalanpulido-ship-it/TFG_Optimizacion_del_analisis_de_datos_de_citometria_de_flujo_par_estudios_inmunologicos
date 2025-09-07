# TFG_Optimizacion_del_analisis_de_datos_de_citometria_de_flujo_para_estudios_inmunologicos

Datos y códigos de script empleados en el desarrollo del TFG

# Explicación

Para cada carpeta de cada archivo ConcatXXk ejecute el archivo MetricasPonderadas.R.
Este Script se ejecuta por cada algoritmo, por lo que deberá de cambiar el nombre de 
los archivos.csv a analizar (En la sección # 2 Leer datos)

Por otro lado, el archivo flowMeansXXk, FlowSOMXXk, XshiftXX y PhenographXXk debe de 
cambiarlos en la línea 218 para generar dichos archivos .csv que serán procesados por
el script Graficos.R para generar los heatmaps correspondientes y hacer el rank-sum.

Para emplear el script Graficos.R debe de crear una carpeta en la que debe de copiar 
los archivos flowMeansXXk, FlowSOMXXk, XshiftXX y PhenographXXk; además debe de crear 
una carpeta dentro donde almacenará los archivos.csv que contienen el coeficiente de
Jaccard. De igual manera, deberá crear una carpeta Tiempo donde almacenará el archivo 
Tiemposdeejecucion.csv.

Teniendo en cuenta estas instrucciones el script debe de funcionar correctamente. Revise
los paquetes necesarios cargados en cada archivo por si requier de instalación.

Los scripts se ejecutaron en la versión 4.5.1 de R y con Rtools45.
