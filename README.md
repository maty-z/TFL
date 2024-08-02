# Selección automática de umbral para modelar paramétricamente la distribución de excesos
### Tesis de licenciatura 
Agosto 2024

Este repositorio contiene los scripts tanto para seleccionar el umbral como para reproducir los resultados presentados en la tesis.

## Introducción
En la tesis se propuso un método automático de detección de umbral para valores extremos de una distribución a partir de una muestra aleatoria de la misma, utilizando un modelo semiparamétrico en donde existe un umbral al partir del cual la distribución de excesos se distribuye como $\mathcal{E(\lambda)}$.

Próximamente se podrá consultar la tesis en la biblbioteca digital de la Facultad de Ciencias Exactas y Naturales de la Universidad de Buenos Aires para profundizar en el análisis de los resultados y los fundamentos teóricos que se encuentran detrás de la propuesta.

## Funcionamiento
Para estimar el umbral alcanza solo con poseer el **R** base, no es necesario ninguna librería extra.
Luego descargar el script [f_estimaciones.R](scr/f_estimaciones.R) dentro de la carpeta [src](scr). 

Utilizar la función **u_est** para calcular el umbral. Requiere como argumentos la muestra como un vector, y pasarle los parámetros, que determinan la función de penalización que utiliza el alogritmo internamente:
- $n$ (n_pen)
- $\alpha$ (alpha_pen)
- $c$

Los valores que se utilizaron en la tesis son: $1,\ 0.4,\ 0.8$ respectivamente.

Internamente el algoritmo busca minimizar:
``` math
PN(u) = \hat l(u) + c\frac{u^n}{|muestra|^\alpha}
```
Donde $|muestra|$ es el tamaño de la muestra, y
``` math
\widehat\lambda(u) = \frac{\sum\mathbf{I}_{X_i>u}}{\sum(X_i-u)\mathbf{I}_{X_i>u}}
```
``` math
\widehat F_u(y) = \frac{\sum\mathbf{I}_{X_i-u\leq y}\mathbf{I}_{X_i>u}}{\sum\mathbf{I}_{X_i>u}}
```
``` math
\widehat l(u) = \sup_{y\geq0} |\widehat F_u(y)-F_{\widehat\lambda(u)}|
```
Con $F_{\widehat\lambda}$ función de distribución acumulada de una variable aleatoria que se distribuye como $\mathcal{E}(\widehat\lambda)$.

### Ejemplo de uso
``` R
#  Importar las funciones que realizan la estimación:
source("./src/f_estimaciones.R")

# Si se quiere generar una muestra a partir del modelo utilizado en la tesis importar primero el siguiente código:
source("./src/f_generar_muestras.R")

# Generar muestra de tamaño 1000, con proporción de datos por debajo del umbral igual a 0.8, con distribución de excesos por encima del umbral se distribuyen con una exponenial de parámetro 1.
muestra <- generar_muestra.unif_exp(
    size = 1000, 
    p_umbral = 0.8, 
    u_min = 0, 
    u_max = 1, 
    exp_lambda = 1
    )

u.estimado <- u_est(
    muestra,
    n_pen = 1, 
    alpha_pen = 0.4, 
    c = 1
    )
```

En el script [generar_muestras.R](scr/generar_muestras.R) hay otras funciones que optimizan la generación de muestras, y también se agregó una función generadora de muestras que siguen un modelo Normal-Exponencial (o sea existe un umbral tal que datos por debajo del umbral se comportan como una normal y por encima como una exponencial), no se utilizó para la realización de la tesis.

## Reproducción de experimentos
Para automatizar la realización de las pruebas y el análisis de los resultados se realizaron los scripts cuyos nombres empiezan con `s_*` dentro de la carpeta [scr](scr/). Estos cuentan también con la posibilidad de exportar y guardar los distintos resultados tanto finales como intermedios. Se asume que el directorio de trabajo posee la siguiente estructura:
```
repo
    |_data
    |_experiments
    |_outputs
    |_outputs_charts
    |_outputs_cuantiles
    |_src
  ```

Dentro de este repositorio un experimento consiste en la realización de $n_replicaciones$ de un mismo escenario, esto es definir los parámetros del modelo y definir el tamaño de la muestra aleatoria, tomarla, y realizar las cuentas necesarias.
En este trabajo $n_replicaciones$ = 1000. Para ejecutar las pruebas basta con ejecutar el script `s_experimentos.R`. Dicho script permite la ejecución de múltiples escenarios con múltiples replicaciones modificando los parámetros de los escenarios según corresponda.
Para reproducir los resultados de la tesis buscar el archivo [resumen_experimentos.txt](resumen_experimentos.txt) el cual indica los parámetros de cada experimento inclusive la semilla utilizada para la generación de las muestras aleatorias.

### Estimación umbral y exponencial
Cada experimento va a generar las $n_replicaciones$, en este caso $1000$, muestras a partir de una semilla y las va a trabajar de manera matricial. Si está activada la opción de exportar, se van a almacenar en la carpeta _data_ (es necesario crearla). El script genera las (en este caso) 1000 muestras de un mismo tamaño y las va a ubicar por filas en una matriz. Y luego procede a hacer las cuentas y estimaciones, devolviendo finalmente un `data.frame` con la misma cantidad de filas y una columna por cada valor estimado y los parámetros del experimento. 

En este punto los valores estimados son:
- Umbral.
- Parámetro de la exponencial que modela la distribución de excesos considerando el umbral previamente estimado.

### Estimación cuantiles
La decisión de analizar y comparar los cuantiles fue realizada posteriormente a estos experimentos, y para no tener que hacer de nuevo todo el proceso se lo realizó en un script aparte.
