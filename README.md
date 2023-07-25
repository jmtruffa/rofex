Funciones para consumir api de rofex: 

endpoint: https://apicem.matbarofex.com.ar/api/v2/closing-prices

Por el momento disponibles:

- getRofexPosition
  Se consulta la/s posicion/es y devuelve una lista con un tibble con las posiciones y toda su info y otra tibble con las posiciones que fallaron
- getRofexOEM
  Devuelve, dada una posición con el formato "PRODMMYYYY", donde PROD puede ser DLR por ejemplo, MM = mes en dos posiciones y YYYY año en 4 posiciones.
  

