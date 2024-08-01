# Selección automática de umbral para modelar paramétricamente la distribución de excesos
### Tesis de licenciatura 
Agosto 2024

Este repositorio contiene los scripts tanto para seleccionar el umbral como para reproducir los resultados presentados en la tesis.

## Introducción
En la tesis se propuso un método automático de detección de umbral para valores extremos de una distribución a partir de una muestra aleatoria de la misma, utilizando un modelo semiparamétrico en donde existe un umbral al partir del cual la distribución de excesos se distribuye como $\mathcal{E(\lambda)}$.

Próximamente se podrá consultar la tesis en la biblbioteca digital de la Facultad de Ciencias Exactas y Naturales de la Universidad de Buenos Aires para profundizar en el análisis de los resultados y los fundamentos teóricos que se encuentran detrás de la propuesta.

## Funcionamiento
Para estimar el umbral alcanza solo con poseer el **R** base, no es necesario ninguna librería extra.
Luego descargar el script _f\_estimaciones.R_ dentro de la carpeta _src_. 

Utilizar la función **u_est** para calcular el umbral. Requiere como argumentos la muestra como un vector, y pasarle los parámetros, que determinan la función de penalización que utiliza el alogritmo internamente:
- $n$ (n_pen)
- $\alpha$ (alpha_pen)
- $c$

Los valores que se utilizaron en la tesis son: $1,\ 0.4,\ 0.8$ respectivamente.

Internamente el algoritmo busca minimizar:
$$
PN(u) = \hat l(u) + c\frac{u^n}{|muestra|^\alpha}
$$
Donde $|muestra|$ es el tamaño de la muestra, y
- $$
    \widehat\lambda(u) = \frac{\sum_{i=1}^n\mathbf{I}_{X_i>u}}{\sum_{i=1}^n(X_i-u)\mathbf{I}_{X_i>u}}
$$
- $$
    \widehat F_u(y) = \frac{\sum_{i=1}^n\mathbf{I}_{X_i-u\leq y}\mathbf{I}_{X_i>u}}{\sum_{i=1}^n\mathbf{I}_{X_i>u}}
$$
- $$
    \widehat l(u) = \sup_{y\geq0} |\widehat F_u(y)-F_{\widehat\lambda(u)}|
$$
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

En el script `generar_muestras.R` hay otras funciones que optimizan la generación de muestras, y también se agregó una función generadora de muestras que siguen un modelo Normal-Exponencial (o sea existe un umbral tal que datos por debajo del umbral se comportan como una normal y por encima como una exponencial), no se utilizó para la realización de la tesis.
