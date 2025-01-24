# Estimación de Varianzas utilizando Funciones Generalizadas de Varianza (GVF) en Encuestas de Hogares

Este repositorio contiene el código desarrollado para el análisis realizado en el estudio **"Estimation of Variances using the Generalized Variance Function: Labor and Population Indicators in the Colombian Household Survey 2022"**, donde se implementaron modelos GVF con datos de la GEIH 2022, validados posteriormente con datos de 2023.

## Estructura del Repositorio

### Carpetas
- **Estimaciones DANE-2022**: Contiene las bases de datos generadas al procesar los datos del DANE para el año 2022, incluyendo resultados intermedios y finales.
- **Estimaciones DANE-2023**: Almacena las bases de datos generadas para los datos de 2023, utilizadas principalmente para la validación de los modelos ajustados.

### Códigos
El flujo de trabajo está compuesto por 8 códigos principales, programados en R, que permiten procesar, consolidar y analizar los datos, así como ajustar y validar los modelos GVF. 

1. **`codigo_1.R`**: Este código se encarga de leer, procesar y consolidar las bases mensuales del DANE 2022. Se enriquecen las variables con etiquetas descriptivas y se integran datos auxiliares de departamentos. Los resultados de cada mes se exportan como archivos separados en formato `.csv`, que serán insumos para las etapas posteriores.

2. **`codigo_2.R`**: Combina las bases mensuales generadas en el código anterior en un único archivo consolidado (`base_geih_12.csv`). Este archivo sirve como insumo principal para las siguientes etapas del análisis.

3. **`codigo_3.R`**: Calcula indicadores laborales clave (como tasas de desempleo, ocupación y participación) utilizando la base consolidada. Los resultados, tanto a nivel nacional como para 13 ciudades, se exportan como archivos `.csv` separados.

4. **`codigo_4.R`**: Unifica las estimaciones mensuales generadas en `codigo_3.R` en un único archivo consolidado (`base_estimador_real.csv`), enriquecido con información adicional como periodicidad y nivel de análisis.

5. **`codigo_5.R`**: Genera estimaciones trimestrales a partir de los datos mensuales. Los resultados se guardan en archivos `.csv` y en un archivo Excel consolidado (`base_estimador_real_trimestral.xlsx`).

6. **`codigo_6.R`**: Integra las estimaciones mensuales y trimestrales en una única base consolidada (`base_final.csv`). Este archivo incluye las estimaciones necesarias para los análisis finales.

7. **`codigo_7.R`**: Ajusta modelos de regresión GVF para estimar errores estándar ajustados. Este código compara modelos teóricos y alternativos, evaluando su desempeño con métricas avanzadas como RMSE, MAE, MAPE y R². Los resultados ayudan a identificar el modelo más adecuado para representar la varianza.

8. **`codigo_8.R`**: Valida los modelos ajustados utilizando los datos de 2023. Este código compara las estimaciones de errores estándar ajustados con los valores reales empleando métricas como RMSE, MAPE, MAE y R². Los resultados permiten evaluar la precisión y el desempeño de los modelos ajustados en un conjunto de datos independiente.

## Descripción del Flujo de Trabajo

El procesamiento de los datos sigue una estructura secuencial que permite:
1. Leer y procesar los datos iniciales.
2. Generar estimaciones mensuales y trimestrales.
3. Ajustar modelos GVF para estimar varianzas y errores estándar.
4. Validar los modelos utilizando un conjunto de datos independiente.

El diseño modular de los códigos facilita su adaptación para otros dominios, variables o encuestas, permitiendo replicar o ajustar el análisis a nuevos objetivos.

### Ajuste y Evaluación de Modelos GVF (Detalles del Código 7)
En el **`codigo_7.R`**, se realiza el ajuste de los modelos GVF y el cálculo de medidas de ajuste. Este código se basa en la lógica y estructura empleadas en el estudio de la **CEPAL** realizado por Gutiérrez y Babativa-Márquez (2023), adaptándolas al contexto de la encuesta GEIH 2022. Se amplía la gama de modelos evaluados, integrando tanto las propuestas del estudio de la CEPAL como modelos de otros estudios, además de incluir nuevas variantes con variables categóricas. Asimismo, se incorporan métricas avanzadas como MAE, MAPE y NRMSE para evaluar el desempeño de los modelos ajustados.

## Requisitos Computacionales
El tiempo de procesamiento dependerá de los recursos computacionales disponibles. En un procesador **Ryzen 7 8000** con **64 GB de RAM**, el procesamiento completo dura aproximadamente 5 minutos.

## Referencia
Gutiérrez, A. y Babativa-Márquez, G. (2023). *Efectos de diseño para indicadores sociales en América Latina: función generalizada de varianza para estimadores directos provenientes de encuestas de hogares.* Series de la CEPAL.
