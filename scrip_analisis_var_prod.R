#Llamando a las librerias
library(car)
library(performance)
library(agricolae)
library(ggplot2)
library(stringr)
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)
library(ggplotify)
library(patchwork)

#Leyendo los datos
var_prod <- read.csv("C:/Analisis_inoculantes/Iniculantes/Datos/Var_Productivas.csv")


var_prod$Tratamiento <- as.factor(var_prod$Tratamiento)
var_prod$Bloques <- as.factor(var_prod$Bloques)

#Andeva, diagnostico y parametros del modelo para la variable Numero de vainas

modelo_sqrt_vainas <- aov(sqrt(Vainas_por_planta) ~ Tratamiento + Bloques, data = var_prod)
anova_sqrt_vainas <- anova(modelo_sqrt_vainas)
sw_test_sqrt_vainas <- shapiro.test(residuals(modelo_sqrt_vainas))
lev_test_sqrt_vainas <- leveneTest(sqrt(Vainas_por_planta) ~ Tratamiento, data = var_prod)
LSD_vainas <- LSD.test(modelo_sqrt_vainas, trt = "Tratamiento", group = TRUE, alpha = 0.05)        
qqnorm(sqrt(var_prod$Vainas_por_planta))
res_vainas <- residuals(modelo_sqrt_vainas)
Pred_vainas <- predict(modelo_sqrt_vainas)
df_res_vs_pr_vainas <- data.frame(Pred_vainas, res_vainas)
scatterplotMatrix(df_res_vs_pr_vainas)
scatterplot(Pred_vainas, res_vainas)

#Andeva, diagnostico y parametros del modelo para la variable Numero de granos por planta

modelo_granos <- aov(Granos_por_planta ~ Tratamiento + Bloques, data = var_prod)
anova_granos <- anova(modelo_granos)
sw_test_granos <- shapiro.test(residuals(modelo_granos))
lev_test_granos <- leveneTest(log(Granos_por_planta) ~ Tratamiento, data = var_prod)
LSD_granos <- LSD.test(modelo_granos, trt = "Tratamiento", group = TRUE, alpha = 0.05)        
qqnorm(var_prod$Granos_por_planta)
res_granos <- residuals(modelo_granos)
Pred_granos <- predict(modelo_granos)
df_res_vs_pr_granos <- data.frame(Pred_granos, res_granos)
scatterplotMatrix(df_res_vs_pr_granos)
scatterplot(Pred_granos, res_granos)

#Andeva, diagnostico y parametros del modelo para la variable Rendimiento

modelo_rend <- aov(Rendimiento ~ Tratamiento + Bloques, data = var_prod)
anova_rend <- anova(modelo_rend)
sw_test_rend <- shapiro.test(residuals(modelo_rend))
lev_test_rend <- leveneTest(Rendimiento ~ Tratamiento, data = var_prod)
LSD_rend <- LSD.test(modelo_rend, trt = "Tratamiento", group = TRUE, alpha = 0.05)        
qqnorm(var_prod$Rendimiento)
res_rend <- residuals(modelo_rend)
Pred_rend <- predict(modelo_rend)
df_res_vs_pr_rend <- data.frame(Pred_rend, res_rend)
scatterplotMatrix(df_res_vs_pr_rend)
scatterplot(Pred_rend, res_rend)

#Calculando estadisticos de los datos

cv_vainas <- round(100 * summary.lm(modelo_sqrt_vainas)$sigma / mean(sqrt(var_prod$Vainas_por_planta)), 2)
cv_granos <- round(100 * summary.lm(modelo_granos)$sigma / mean(var_prod$Granos_por_planta), 2)
cv_rend <- round(100 * summary.lm(modelo_rend)$sigma / mean(var_prod$Rendimiento), 2)


r2_vainas <- r2_vainas <- round(summary.lm(modelo_sqrt_vainas)$r.squared, 2)
r2_granos <- round((anova_granos$'Sum Sq'[1]+anova_granos$'Sum Sq'[2]) / sum(anova_granos$'Sum Sq'), 2)
r2_rend <- round((anova_rend$'Sum Sq'[1]+anova_rend$'Sum Sq'[2]) / sum(anova_rend$'Sum Sq'), 2)


# ==============================================================================
# CONSTRUYENDO UN MARCO DE DATOS (CORREGIDO)
# ==============================================================================

# 1. Extraer los resultados de cada variable manteniendo el nombre del tratamiento
df_vainas <- LSD_vainas$groups %>% 
        tibble::rownames_to_column(var = "trt") %>% 
        rename(letra_vainas = groups) # Mantiene la media 'sqrt.Vainas_por_planta.' y su letra

df_granos <- LSD_granos$groups %>% 
        tibble::rownames_to_column(var = "trt") %>% 
        select(trt, Granos_por_planta, letra_granos = groups)

df_rend <- LSD_rend$groups %>% 
        tibble::rownames_to_column(var = "trt") %>% 
        select(trt, Rendimiento, letras_rend = groups)

# 2. Unir de forma segura usando el nombre del tratamiento ('trt') como llave
df <- df_vainas %>%
        inner_join(df_granos, by = "trt") %>%
        inner_join(df_rend, by = "trt")


# --- El resto de vectores estadísticos se mantienen igual ---
Co_Var <- c(cv_vainas, cv_granos, cv_rend)
Coe_Det <- c(r2_vainas, r2_granos, r2_rend)
Shapiro_Wilk <- c(sw_test_sqrt_vainas$p.value, sw_test_granos$p.value, sw_test_rend$p.value)
Levene <- c(
        lev_test_sqrt_vainas$`Pr(>F)`[1],
        lev_test_granos$`Pr(>F)`[1],
        lev_test_rend$`Pr(>F)`[1]
)
p_values <- c(
        summary(modelo_sqrt_vainas)[[1]][["Pr(>F)"]][1],
        summary(modelo_granos)[[1]][["Pr(>F)"]][1],
        summary(modelo_rend)[[1]][["Pr(>F)"]][1]
)
Variables <- c("Vainas por planta", "Granos por planta (Log)", "Rendimiento")
df_stad <- data.frame(Variables, Co_Var, Coe_Det, Shapiro_Wilk, Levene, p_values)


#Graficando los datos
etiq_zz <- function(x) {
        ifelse(seq_along(x) %% 2 == 1,
               paste0(x, "\n"),
               paste0("\n", x))
}

#Graficando Numero de vainas por planta

graf_vainas <- ggplot(data = df) +
        geom_col(aes(x = trt, y = `sqrt(Vainas_por_planta)`),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = `sqrt(Vainas_por_planta)`, label = letra_vainas),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        scale_y_continuous(limits = c(0, 2.8),
                           breaks = seq(0, 2.8, by = 0.5)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"))+
        labs(x = "", y = "Número de vainas(Raiz cuadrada)") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
              axis.text.y  = element_text(size = 10, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))

print(graf_vainas)

#Graficando granos por planta

graf_granos <- ggplot(data = df) +
        geom_col(aes(x = trt, y = Granos_por_planta),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = Granos_por_planta, label = letra_granos),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        scale_y_continuous(limits = c(0, 27),
                           breaks = seq(0, 27, by = 5)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"))+
        labs(x = "Tratamientos", y = "Granos por planta") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
              axis.text.y  = element_text(size = 10, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))

print(graf_granos) 

#Graficando el rendimiento

graf_rend <- ggplot(data = df) +
        geom_col(aes(x = trt, y = Rendimiento),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = Rendimiento, label = letras_rend),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        scale_y_continuous(limits = c(0, 1100),
                           breaks = seq(0, 1100, by = 100)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"))+
        labs(x = "Tratamientos", y = expression(Rendimiento ~ (kg %.% ha^-1))
        ) +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
              axis.text.y  = element_text(size = 10, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))

print(graf_rend)   

#Convierte el data frame llamado "Supuestos_y_estadisticos" a una tabla en formato apa

# 1) Preparar la tabla

tabla_ap <- df_stad %>%
        mutate(across((ncol(.)-2):ncol(.), ~ sprintf("%.4f", as.numeric(.))))

# 2) Tema de tabla estilo APA (líneas horizontales mínimas)
tt <- ttheme_minimal(
        core = list(
                fg_params = list(fontfamily = "Arial", fontsize = 10),
                padding   = unit.c(unit(6, "pt"), unit(6, "pt"))
        ),
        colhead = list(
                fg_params = list(fontfamily = "Arial", fontsize = 10, fontface = "bold"),
                padding   = unit.c(unit(6, "pt"), unit(6, "pt"))
        )
)

tbl_grob <- tableGrob(tabla_ap, rows = NULL, theme = tt)

# (Opcional) agregar una línea horizontal extra bajo el encabezado:

# --- 1. Crear tu tabla base (sin líneas)
tbl_grob <- tableGrob(
        tabla_ap,              # <- tu data frame ya formateado
        rows = NULL,
        theme = ttheme_minimal(
                core = list(
                        fg_params = list(fontfamily = "Arial", fontsize = 10),
                        padding   = unit(c(6, 6), "pt")
                ),
                colhead = list(
                        fg_params = list(fontfamily = "Arial", fontsize = 10, fontface = "bold"),
                        padding   = unit(c(6, 6), "pt")
                )
        )
)

# --- 2. Agregar línea superior al encabezado
tbl_grob <- gtable_add_grob(
        tbl_grob,
        grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                             y0 = unit(1, "npc"), y1 = unit(1, "npc")),
        t = 1, l = 1, r = ncol(tbl_grob)
)

# --- 3. Agregar línea inferior al encabezado (fila 1)
tbl_grob <- gtable::gtable_add_grob(
        tbl_grob,
        grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                             y0 = unit(0, "npc"), y1 = unit(0, "npc")),
        t = 1, b = 1, l = 1, r = ncol(tbl_grob), z = Inf
)


# --- 4. Agregar línea inferior a la última fila
last_row <- nrow(tbl_grob)
tbl_grob <- gtable_add_grob(
        tbl_grob,
        grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"),
                             y0 = unit(0, "npc"), y1 = unit(0, "npc")),
        t = last_row, l = 1, r = ncol(tbl_grob)
)

# --- 5. Crear TÍTULO como grob estilo APA
titulo <- textGrob(
        "Parámetros y supuestos estadísticos",
        gp = gpar(fontface = "bold", fontsize = 12, fontfamily = "Arial")
)

# --- 6. Unir título + tabla
tabla_con_titulo <- arrangeGrob(
        titulo,
        tbl_grob,
        ncol = 1,
        heights = unit.c(unit(1.5, "lines"), unit(1, "null"))
)

# --- 7. Convertir en ggplot para usar en patchwork
gg_tab <- as.ggplot(tabla_con_titulo)
gg_tab

# 3) Convertir a “ggplot” para patchwork

graf_final <-
        (graf_vainas + graf_granos + graf_rend + wrap_elements(gg_tab)) +
        plot_layout(ncol = 2, nrow = 2) &
        theme_classic() &
        theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white", colour = NA),
                plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
        )

#Nota al pie
graf_final <- graf_final + plot_annotation(
        tag_levels = "A",
        caption = "NOTA: Barras con letras diferentes corresponden a tratamientos estadísticamente distintos según la prueba LSD de Fisher (α = 0.05).",
        theme = theme(
                plot.caption = element_text(size = 10, hjust = 0),  # hjust=0 = izquierda
                plot.margin  = margin(t = 1, r = 2, b = 2, l = 2),
                plot.tag     = element_text(size = 14, face = "bold"),
                plot_annotation(theme = theme(plot.margin = margin(4, 4, 4, 4))) # margen externo# margen exterior opcional
        )
)

graf_final


ggsave(
        filename = "graf_var_prod.png",
        plot     = graf_final,
        width    = 12,          
        height   = 8,          
        units    = "in",
        dpi      = 300,        
        device   = "png"
)

