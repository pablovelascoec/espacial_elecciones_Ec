# Este repositorio incluye información necesaria para realizar análisis espaciales con datos del CNE

Los análisis espaciales de las elecciones en Ecuador pueden realizarse a nivel de recinto electoral, zonas, parroquias, cantones y provincias. Todos los documentos disponibles para descarga corresponden a aquellos usados oficialmente por el órgano electoral CNE y las líneas de programación son propias.

## Minado de datos de mapas de la página del CNE

La carpeta de minado incluye un archivo Python que permite descargar las capas espaciales utilizadas por el CNE.
Para este minado, se requiere la dirección de la página de ArcGIS del CNE y solo se actualiza en el archivo según el nivel de análisis de interpes.

Para mayor facilidad, todos los archivos shapefile ya descargados se encentran disponibles en mi resitorio público en el siguiente link: https://drive.google.com/drive/folders/1BI5UzZp64pBgPe99aTW01zqm5IvfkjHH?usp=sharing

## Procesamiento de datos históricos del CNE

La carpeta de procesamiento incluye archivos R que transforman las bases de datos del CNE a archivos que presentan los resultados presidenciales a nivel de parroquia. Se juntan los candidatos menores y se procesa la información de tal manera que solo hay una fila por cada parroquia. Los archivos resultantes pueden ser emparejados con los shapefiles de parroquias según su código único.

Los resultados del CNE y las etiquetas de los datos necesarias para este procesamiento se encuentran en esta dirección: https://drive.google.com/drive/folders/1DA2YCqEzo4IZ-dyUsYV-qRJvUG1W3xDM?usp=sharing

## Ejemplos de resultados

Finalmente, la carpeta de ejemplos contiene imágenes de la cartografía temática que puede producirse a partir de la información aquí compartida, tanto a nivel de parroquia como a nivel de recinto electoral.

*Pequeña nota final: los resultados electorales de 2025 todavía no han sido publicados por el CNE debido a los reclamos en algunas actas. Apenas estas sean públicas, actualizaré este repositorio.*
