# Resiliencia climática de *Phaseolus vulgaris* L. mediante consorcios microbiales en el Corredor Seco Nicaragüense

## 📋 Descripción General

Este repositorio contiene los análisis estadísticos y económicos de un experimento científico sometido a la **Revista Agronomía Mesoamericana**. El estudio evalúa la eficacia de microorganismos promotores del crecimiento vegetal (PGPR) en mejorar la productividad y resiliencia climática del frijol común (*Phaseolus vulgaris* L.) bajo condiciones de estrés hídrico en la región del Corredor Seco nicaragüense.

## 🎯 Objetivo de la Investigación

Determinar el efecto de diferentes consorcios microbiales compuestos por:
- **Micorriza arbuscular**
- **Rhizobium** (bacteria fijadora de nitrógeno)
- **Trichoderma harzianum** (hongo benéfico)

...sobre variables de crecimiento y rendimiento de *Phaseolus vulgaris* L., con énfasis en aumentar la resiliencia ante sequías en el Corredor Seco.

## 📊 Metodología

### Diseño Experimental
- **Tipo de diseño:** Bloques Completos al Azar (BCA)
- **Número de bloques:** 4
- **Número de tratamientos:** 8
- **Total de parcelas:** 32
- **Estructura:** El diseño controla la variabilidad ambiental (gradiente de sombra) mediante bloques homogéneos

### Tratamientos Evaluados
1. Micorriza
2. Rhizobium
3. *Trichoderma harzianum*
4. Testigo (control)
5. Micorriza + *Trichoderma harzianum*
6. Rhizobium + *Trichoderma harzianum*
7. Micorriza + Rhizobium
8. Micorriza + Rhizobium + *Trichoderma harzianum*

## 📈 Variables Evaluadas

### Variables de Crecimiento
- **Altura de planta** (cm)
- **Número de hojas**
- **Diámetro del tallo** (mm)

### Variables de Productivas
- **Vainas por planta**
- **Granos por planta**
- **Rendimiento** (kg·ha⁻¹)

### Análisis Estadísticos
Todos los análisis incluyen:
- Coeficiente de variación (C.V.)
- Coeficiente de determinación (R²)
- Prueba de Levene (homogeneidad de varianzas)
- Prueba de Shapiro-Wilk (normalidad)
- Comparación de medias con letras de significancia

## 💰 Análisis Económico

El repositorio incluye análisis económicos que evalúan:
- **Costos variables** por hectárea (USD·ha⁻¹)
- **Ingresos netos** generados por tratamiento
- **Análisis de dominancia económica**
- **Tasa de Retorno Marginal (TRM)** para tratamientos económicamente viables

## 🗂️ Estructura del Repositorio

```
Iniculantes/
├── README.md                           # Este archivo
├── Iniculantes.Rproj                  # Proyecto RStudio
├── Diseño experimental.qmd            # Documento del diseño BCA
├── Analisis var crecimiento.qmd       # Análisis de variables de crecimiento
├── Analisis var_prod.qmd              # Análisis de variables productivas
├── Analisis_economico.qmd             # Análisis económico
├── scrip_analisis_var_crecimiento.R   # Script R para crecimiento
├── scrip_analisis_var_prod.R          # Script R para variables productivas
├── Análisis económico.R               # Script R para análisis económico
├── Supuestos_y_estadisticos.csv       # Tabla de supuestos estadísticos
├── Datos/
│   └── Var_Productivas.csv            # Base de datos de variables productivas
└── images/
    └── Diseño.png                      # Diagrama del diseño experimental
```

## 🛠️ Dependencias Técnicas

El proyecto utiliza **R** con las siguientes librerías principales:
- `agricolae` - Diseños experimentales y análisis de varianza
- `ggplot2` - Visualización de datos
- `tidyverse` - Manipulación y transformación de datos
- `readr` - Lectura de datos
- Librerías adicionales para análisis estadísticos y económicos

### Requisitos del Sistema
- R ≥ 3.6
- RStudio (recomendado)
- Paquetes listados en scripts individuales

## 📄 Archivos de Análisis

### 1. **Diseño experimental.qmd**
Define la estructura experimental BCA y la aleatorización de tratamientos dentro de cada bloque.

### 2. **Analisis var crecimiento.qmd**
Análisis estadístico completo de:
- Altura de planta
- Número de hojas
- Diámetro del tallo
Incluye pruebas de supuestos, ANOVA y comparación de medias.

### 3. **Analisis var_prod.qmd**
Evaluación de:
- Vainas por planta
- Granos por planta
- Rendimiento (kg·ha⁻¹)
Con visualizaciones y análisis de significancia entre tratamientos.

### 4. **Analisis_economico.qmd**
Análisis económico integral que incluye:
- Curva de dominancia de costos vs. ingresos
- Tasa de Retorno Marginal (TRM)
- Evaluación de viabilidad económica de tratamientos

## 📊 Visualizaciones Principales

El repositorio genera múltiples gráficos comparativos:
- **Gráficos de barras** para variables de crecimiento y productividad
- **Análisis de dominancia** que confronta costos variables con ingresos netos
- **Análisis de TRM** para determinar rentabilidad marginal de inversiones

## 📝 Notas Metodológicas

- El diseño BCA fue seleccionado para controlar la variabilidad espacial (gradiente de sombra)
- Los datos están estructurados por bloques para facilitar análisis espacialmente informados
- Todos los tratamientos microbianos fueron comparados contra un control (testigo)
- Los análisis económicos se basan en condiciones del Corredor Seco nicaragüense

## 📖 Documentación

Todos los análisis están documentados en formato Quarto (`.qmd`), permitiendo reproducibilidad completa:
- Código R visible y evaluable
- Resultados integrados con visualizaciones
- Documentación técnica accesible

## 🔄 Reproducibilidad

Para reproducir los análisis:

1. **Instalar dependencias:**
```R
install.packages(c("agricolae", "ggplot2", "tidyverse", "readr"))
```

2. **Abrir el proyecto en RStudio:**
```
File > Open Project > Iniculantes.Rproj
```

3. **Ejecutar documentos Quarto:**
```R
quarto::quarto_render("Diseño experimental.qmd")
quarto::quarto_render("Analisis var crecimiento.qmd")
# ... y así sucesivamente
```

## 📮 Contexto de Publicación

Esta investigación fue desarrollada como propuesta de artículo científico para:

**Revista:** Agronom��a Mesoamericana  
**Título del Manuscrito:** *Resiliencia climática de Phaseolus vulgaris L. mediante consorcios microbiales en el Corredor Seco Nicaragüense*  
**Enfoque:** Mejora de la productividad agrícola sostenible mediante microorganismos promotores del crecimiento bajo estrés hídrico

## ⚖️ Licencia

MIT

## 👤 Autor

**Víctor Hugo Rodríguez Salguera**

## 📧 Contacto y Consultas

Para preguntas sobre la investigación, análisis o reproducibilidad de resultados, contacte al autor a través del repositorio.

---

**Última actualización:** 28 de mayo de 2026  
**Estatus del manuscrito:** Bajo revisión en Agronomía Mesoamericana
