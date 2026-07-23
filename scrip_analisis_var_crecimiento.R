#Llamando a las librerias
library(dplyr)
library(car)
library(agricolae)
library(broom)
library(ggplot2)
library(readr)
library(ggpubr)
library(stringr)
library(tibble)
library(gridExtra)
library(ggplotify)
library(grid)
library(gtable)
library(patchwork)
library(cowplot)
library(kableExtra)



#Leyendo los datos
var_crecimiento <- read.csv("C:/Analisis_inoculantes/Iniculantes/Datos/Var_Crecimiento.csv")


var_crecimiento$Tratamientos <- as.factor(var_crecimiento$Tratamientos)
var_crecimiento$Bloques <- as.factor(var_crecimiento$Bloques)

#Andeva, diagnostico y parametros del modelo para la variable altura

modelo_alt <- aov(Altura ~ Tratamientos + Bloques, data = var_crecimiento)
anova_alt <- anova(modelo_alt)
sw_test_alt <- shapiro.test(residuals(modelo_alt))
lev_test_alt <- leveneTest(Altura ~ Tratamientos, data = var_crecimiento)
LSD_alt <- LSD.test(modelo_alt, trt = "Tratamientos", group = TRUE, alpha = 0.05)        
qqnorm(var_crecimiento$Altura)
res_alt <- residuals(modelo_alt)
Pred_alt <- predict(modelo_alt)
df_res_vs_pr_alt <- data.frame(Pred_alt, res_alt)
scatterplotMatrix(df_res_vs_pr_alt)
scatterplot(Pred_alt, res_alt)

#Andeva, diagnostico y parametros del modelo para la variable Numero de hojas

modelo_hoj <- aov(Numero_de_hojas ~ Tratamientos + Bloques, data = var_crecimiento)
anova_hoj <- anova(modelo_hoj)
sw_test_hoj <- shapiro.test(residuals(modelo_hoj))
lev_test_hoj <- leveneTest(Numero_de_hojas ~ Tratamientos, data = var_crecimiento)
LSD_hoj <- LSD.test(modelo_hoj, trt = "Tratamientos", group = TRUE, alpha = 0.05)        
qqnorm(var_crecimiento$Numero_de_hojas)
res_hoj <- residuals(modelo_hoj)
Pred_hoj <- predict(modelo_hoj)
df_res_vs_pr_hoj <- data.frame(Pred_hoj, res_hoj)
scatterplotMatrix(df_res_vs_pr_hoj)
scatterplot(Pred_alt, res_hoj)

#Andeva, diagnostico y parametros del modelo para la variable Diametro del tallo

modelo_diam <- aov(Diametro ~ Tratamientos + Bloques, data = var_crecimiento)
anova_diam <- anova(modelo_diam)
sw_test_diam <- shapiro.test(residuals(modelo_hoj))
lev_test_diam <- leveneTest(Diametro ~ Tratamientos, data = var_crecimiento)
LSD_diam <- LSD.test(modelo_diam, trt = "Tratamientos", group = TRUE, alpha = 0.05)        
qqnorm(var_crecimiento$Diametro)
res_diam <- residuals(modelo_diam)
Pred_diam <- predict(modelo_diam)
df_res_vs_pr_diam <- data.frame(Pred_diam, res_diam)
scatterplotMatrix(df_res_vs_pr_diam)
scatterplot(Pred_diam, res_diam)

#Calculando estadisticos de los datos

cv_alt <- round(100 * summary.lm(modelo_alt)$sigma / mean(var_crecimiento$Altura), 2)
cv_hoj <- round(100 * summary.lm(modelo_hoj)$sigma / mean(var_crecimiento$Numero_de_hojas), 2)
cv_diam <- round(100 * summary.lm(modelo_diam)$sigma / mean(var_crecimiento$Diametro), 2)

r2_alt <- round((anova_alt$'Sum Sq'[1]+anova_alt$'Sum Sq'[2]) / sum(anova_alt$'Sum Sq'), 2)
r2_hoj <- round((anova_hoj$'Sum Sq'[1]+anova_hoj$'Sum Sq'[2]) / sum(anova_hoj$'Sum Sq'), 2)
r2_diam <- round((anova_diam$'Sum Sq'[1]+anova_diam$'Sum Sq'[2]) / sum(anova_diam$'Sum Sq'), 2)


#Construyendo un marco de datos
trt <- c(rownames(LSD_diam$groups))
letra_alt <- c(LSD_alt$groups)
letra_hoj <- c(LSD_hoj$groups)
letra_diam <- c(LSD_diam$groups)
df <- data.frame(trt, letra_alt, letra_diam, letra_hoj)

Co_Var <- c(cv_alt, cv_hoj, cv_diam)
Coe_Det <- c(r2_alt, r2_hoj, r2_diam)
Shapiro_Wilk <- c(sw_test_alt$p.value, sw_test_hoj$p.value, sw_test_diam$p.value)
Levene <- c(lev_test_alt$`Pr(>F)`[1], lev_test_hoj$`Pr(>F)`[1], lev_test_diam$`Pr(>F)`[1])
p_values <- c(
        summary(modelo_alt)[[1]][["Pr(>F)"]][1],
        summary(modelo_hoj)[[1]][["Pr(>F)"]][1],
        summary(modelo_diam)[[1]][["Pr(>F)"]][1]
)
Variables <- c(Altura = "Altura", Numero_de_hojas = "Número de hojas", Diametro = "Diámetro del tallo")
df_stad <- data.frame(Variables, Co_Var, Coe_Det, Shapiro_Wilk, Levene, p_values)



#Graficando los datos
etiq_zz <- function(x) {
        ifelse(seq_along(x) %% 2 == 1,
               paste0(x, "\n"),
               paste0("\n", x))
}


graf_alt <- ggplot(data = df) +
        theme_classic() +
        theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white", colour = NA))+
        geom_col(aes(x = trt, y = Altura),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = Altura, label = groups),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"))+
        labs(x = "", y = "Altura") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
                axis.text.y  = element_text(size = 10, face = "bold"),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold"))
        
print(graf_alt)

graf_hoj <- ggplot(data = df) +
        geom_col(aes(x = trt, y = Numero_de_hojas),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = Numero_de_hojas, label = groups.1),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        scale_y_continuous(limits = c(0, 20),
                breaks = seq(0, 20, by = 4)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"))+
        labs(x = "Tratamientos", y = "Número de hojas") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
              axis.text.y  = element_text(size = 10, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))
       
print(graf_hoj)       

graf_diam <- ggplot(data = df) +
        geom_col(aes(x = trt, y = Diametro),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = Diametro, label = groups.2),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        scale_y_continuous(limits = c(0, 6),
                           breaks = seq(0, 6, by = 1)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"),
              margin = margin(t = 6))+
        labs(x = "Tratamientos", y = "Diámetro") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
              axis.text.y  = element_text(size = 10, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))
        
print(graf_diam)

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
        (graf_alt + graf_hoj + graf_diam + wrap_elements(gg_tab)) +
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
                plot_annotation(theme = theme(plot.margin = margin(4, 4, 4, 4))) # margen externo# margen exterior opcional
        )
)

graf_final


ggsave(
        filename = "graf_var_crec.png",
        plot     = graf_final,
        width    = 12,          
        height   = 8,          
        units    = "in",
        dpi      = 300,        
        device   = "png"
)






