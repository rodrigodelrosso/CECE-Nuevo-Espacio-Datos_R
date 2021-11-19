##########################################################
#                ANÁLISIS DE DATOS USANDO R              #
#	                CONFERENCIAS CECE - UBA                #
#            FACULTAD DE CIENCIAS ECONÓMICAS             #
#              UNIVERSIDAD DE BUENOS AIRES               #
##########################################################

# Expositores: Martin Masci - Rodrigo Del Rosso
# Lunes 15 de Noviembre de 2021

## Disclaimer: Material construido a partir del libro 
## "Estadística para contadores y auditores con R"
## Disponible en: https://dspace.ups.edu.ec/bitstream/123456789/18100/1/Estadistica%20para%20contadores%20y%20auditores%20con%20R.pdf

## Reiniciar sesión
.rs.restartR()

## Limpiar la consola ##
rm(list = ls())


## CARGAR LIBRERÍAS
suppressPackageStartupMessages({
  library(dplyr)
  library(DescTools)
  library(xlsx)
  library(agricolae)
  library(tidyr)
  library(ggplot2)
  library(BSDA)
})

## CONFIGURACIÓN
options(scipen = 999) ## quitar la notación científica

## SETEAR RUTA DE TRABAJO
path = "D:/OneDrive - Facultad de Cs Económicas - UBA/Charlas/Charla CECE 2021/"

path = "D:/OneDrive - Facultad de Cs Económicas - UBA/Charlas/Charla CECE 2021/"
setwd(path)

#############################
## ESTADÍSTICA DESCRIPTIVA ##
#############################

big4size <- read.csv("cap2_big4_size.csv",
                     header = TRUE,
                     sep = ";",
                     dec = ",")
str(big4size)

big4size <- big4size %>%
  mutate(
    ROS = UTILIDAD/VTAS, ## Return on Sales
    ROE = UTILIDAD/PAT   ## Return on Equity
  )

str(big4size)

mean(big4size$ACTIVOS)
median(big4size$ACTIVOS)
Mode(big4size$ACTIVOS)

big4size %>%
  summarise(PROM.ACTIVOS = mean(ACTIVOS),
            PROM.UTILIDAD = mean(UTILIDAD),
            PROM.VTAS = mean(VTAS),
            MEDIAN.ACTIVOS = median(ACTIVOS),
            MEDIAN.UTILIDAD = median(UTILIDAD),
            MEDIAN.VTAS = median(VTAS)
)

quantile(big4size$ACTIVOS, 0.25)

quantile(big4size$ACTIVOS, c(0.25,0.50,0.75))

quantile(big4size$ACTIVOS, seq(0.1,0.9, by = 0.1))

big4size %>%
  summarise(RANGO.ACTIVOS = max(ACTIVOS/1000000) - min(ACTIVOS/1000000),
            VARM.ACTIVOS = var(ACTIVOS/1000000),
            DESVM.ACTIVOS = sd(ACTIVOS/1000000),
            n = n()
  ) %>%
  mutate(VARP.ACTIVOS = VARM.ACTIVOS*((n-1)/n),
         DESVP.ACTIVOS = sqrt(VARP.ACTIVOS)) %>%
  select(RANGO.ACTIVOS, VARM.ACTIVOS, DESVM.ACTIVOS, VARP.ACTIVOS, DESVP.ACTIVOS)

## OTRO EJEMPLO
audit_bolsa <- read.csv("audit_bolsa.csv",
                        header = TRUE,
                        sep = ";",
                        dec = ",")

tabla_firma <- audit_bolsa %>%
  group_by(FIRMA) %>%
  summarise(Frecuencia=n()) %>%
  mutate(Porcentaje = round(100*Frecuencia/sum(Frecuencia),2)
  ) %>%
  arrange(desc(Porcentaje))
print(tabla_firma)

tabla_firma = as.data.frame(tabla_firma)
write.xlsx(tabla_firma, "tablas.xlsx", sheetName = "firmas", row.names = FALSE)


h2 <- with(big4size,graph.freq(VTAS/1000000,plot=FALSE));
h2 = table.freq(h2)
h3 <- h2 %>%
  mutate(Clase = paste("[",Lower,",",Upper,")"),	
         "Marca de Clase"  =  Main,
         Frec. = Frequency,
         "Frec. Rel." = Percentage,
         "Frec. Acu." = CF,
         "Rel. Acu." = CPF )  %>%
  select(-c(1:7))

h3 = as.data.frame(h3)
write.xlsx(h3, "tablas.xlsx", sheetName = "frec_ventas", row.names = FALSE,append=TRUE)

## continuaremos desde acá
rank2018 = read.csv("Ranking2018Guayas.csv",
                    header=TRUE, 
                    sep=";",
                    dec=",")

ciudad.tama = rank2018 %>% 
  group_by(CIUDAD, TAMAÑO)%>%
  summarise(n=n())%>%
  spread(TAMAÑO, n) %>%
  replace(., is.na(.), 0)

ciudad.tama.porc = rank2018 %>% 
  group_by(CIUDAD, TAMAÑO)%>%
  summarise(Porc = round(100*n()/nrow(rank2018),2)) %>%
  spread(TAMAÑO, Porc) %>%
  replace(., is.na(.), 0)

tipo.tama = rank2018 %>% 
  group_by(TIPO, TAMAÑO)%>%
  summarise(n=n())%>%
  spread(TAMAÑO, n) %>%
  replace(., is.na(.), 0)

tipo.tama.porc = rank2018 %>% 
  group_by(TIPO, TAMAÑO)%>%
  summarise(Porc = round(100*n()/nrow(rank2018),2)) %>%
  spread(TAMAÑO, Porc) %>%
  replace(., is.na(.), 0)

IPC = read.csv("IPC.csv",header = TRUE, dec=",", sep = ";")
head(IPC)

IPC2 = IPC %>%
  gather(key = "MES", value = "IPC", 2:13)

head(IPC2)

IPC3 = IPC2 %>%
  spread(key = "MES", value = "IPC")

head(IPC3)


ggplot(big4size, aes(x= VTAS/1000000)) + 
  geom_histogram(bins=12, color= "red", fill="blue" ) + 
  theme_light()

ggplot(big4size, aes(x= VTAS/1000000)) + 
  geom_histogram(bins=12, color= "red",  fill="blue" ) + 
  xlab("Ventas en Millones de Dólares") + ylab("Frecuencia") +
  theme_light()

rank2018 = read.csv("Ranking2018Guayas.csv",header=TRUE, sep=";",dec=",")

ggplot(rank2018, aes(x=VENTAS/1000, fill=TAMAÑO)) + 
  geom_histogram(alpha=0.3, color="black",bins=10, binwidth = 150) +
  scale_x_continuous(breaks = seq(0,1350,150)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.background = element_rect(fill="white")) +
  xlab("Ventas en Miles") + ylab("Frecuencia")


ggplot(rank2018, aes(x=VENTAS/1000000)) + 
  geom_histogram(alpha=0.3, color="black",bins = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.background = element_rect(fill="white")) +
  xlab("Ventas en Miles") + ylab("Frecuencia") +
  facet_grid(. ~ TAMAÑO)

rank2018com = read.csv("Ranking2018Comercio.csv",header = T,sep=";",dec=",")
str(rank2018com)

tabla_reg <- rank2018com %>%
  group_by(REGIÓN) %>%
  summarise(Frecuencia=n()) %>%
  mutate(Porcentaje = round(100*Frecuencia/sum(Frecuencia),2)
  ) %>%
  arrange(desc(Porcentaje))
print(tabla_reg)

ggplot(rank2018com, aes(x=REGIÓN)) + 
  geom_bar(stat = "count",col="black",fill="white") +
  xlab("") + ylab("Frecuencia")

rank2018com$REGIÓN <- factor(rank2018com$REGIÓN,
                              levels = c("COSTA", "SIERRA", "ORIENTE", "GALAPAGOS"))

ggplot(rank2018com, aes(x=REGIÓN)) + 
  geom_bar(stat = "count",col="black",fill="white") +
  xlab("") + ylab("Frecuencia")

tama.reg = rank2018com %>% 
  group_by(TAMA, REGIÓN)%>%
  summarise(n=n())%>%
  spread(TAMA, n) %>%
  replace(., is.na(.), 0)

print(tama.reg)

ggplot(rank2018com, aes(x=REGIÓN,fill=TAMA)) + 
  geom_bar(stat = "count",position = "dodge") +
  xlab("") + ylab("Frecuencia")

ggplot(rank2018com, aes(x=REGIÓN,fill=TAMA)) + 
  geom_bar(stat = "count",position = "dodge") +
  scale_fill_discrete(name="Tamaño") + 
  xlab("") + ylab("Frecuencia")

ggplot(rank2018, aes(TAMAÑO, VENTAS/1000)) + 
  geom_boxplot() + xlab("TamaÃ±o de las empresas") +
  ylab("Ventas en Miles de DÃ³lares") +theme_light()

ggplot(subset(rank2018, TAMAÑO == "MICROEMPRESA"), aes(TAMAÑO, VENTAS/1000)) + 
  geom_boxplot() + xlab("") +
  ylab("Ventas en Miles de Dólares") +theme_light()

#############################
## ESTADÍSTICA INFERENCIAL ##
#############################

big4size <- big4size %>% 
  mutate(
    Big4 = ifelse(BIG4==1, "Sí","No")
  )

ggplot(big4size, aes(x=VTAS/1000, fill=Big4)) + 
  geom_histogram(alpha=0.3, color="black",bins=10, binwidth = 200000) +
  scale_x_continuous(breaks = seq(0,2000000,200000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Ventas en Miles") + ylab("Frecuencia")

ggplot(big4size, aes(Big4, VTAS/1000)) + 
  geom_boxplot() + xlab("Tipo de Firma") +
  ylab("Ventas en Miles de Dólares")

rank18com = read.csv("Ranking2018Comercio.csv",header=TRUE, dec=",", sep=";")

rank18com.Micro = rank18com %>%
  filter(TAMA == "MICROEMPRESA") %>%
  select(ACTIVO)

attach(rank18com.Micro)

media = mean(ACTIVO)
desviacion = sd(ACTIVO)

error  = qnorm(0.975)*desviacion/sqrt(nrow(rank18com.Micro))

menor = media - error
mayor = media + error
menor
mayor

error  = qnorm(0.99)*desviacion/sqrt(nrow(rank18com.Micro))
menor = media - error
mayor = media + error
menor
mayor

z.test(ACTIVO,sigma.x = desviacion)$conf.int

z.test(ACTIVO,sigma.x = desviacion, conf.level = 0.98)$conf.int

t.test(ACTIVO)$conf.int

t.test(ACTIVO, conf.level = 0.90 )$conf.int

rank18com = read.csv("Ranking2018Comercio.csv",header=TRUE,dec=",", sep=";")

x = nrow(rank18com[which(rank18com$TAMA == "GRANDE"), ])
n = nrow(rank18com)

prop.test(x,n)$conf.int

rank18com.Peq = rank18com %>%
  filter(TAMA == "PEQUEÑA") %>%
  select(ACTIVO)
act.Peq = rank18com.Peq$ACTIVO

rank18com.Micro = rank18com %>%
  filter(TAMA == "MICROEMPRESA") %>%
  select(ACTIVO)
act.Micro = rank18com.Micro$ACTIVO

z.test(x=act.Peq, sigma.x = sd(act.Peq), 
       y=act.Micro, sigma.y = sd(act.Micro))$conf.int

z.test(x=act.Peq, sigma.x = sd(act.Peq), 
       y=act.Micro, sigma.y = sd(act.Micro), 
       conf.level = 0.90)$conf.int

t.test(x=act.Peq, y=act.Micro, var.equal = FALSE,
       conf.level = 0.95)$conf.int

t.test(x=act.Peq, y=act.Micro, var.equal = FALSE,
       conf.level = 0.98)$conf.int

t.test(x=act.Peq, y=act.Micro, var.equal = TRUE,
       conf.level = 0.95)$conf.int

t.test(x=act.Peq, y=act.Micro, var.equal = TRUE,
       conf.level = 0.98)$conf.int

n = nrow(rank18com)
x1 = length(act.Micro)
x2 = length(act.Peq)

prop.test(c(x1,x2),c(n,n))$conf.int

prop.test(c(x1,x2),c(n,n), conf.level = 0.99)$conf.int


desviacion = sd(act.Micro)
z.test(act.Micro, mu = 40000, sigma.x = desviacion, conf.level = 0.95)

z.test(act.Micro, mu = 40000, sigma.x = desviacion,alternative = "less",  conf.level = 0.95)

t.test(act.Peq, mu = 250000,alternative = "greater",  conf.level = 0.95)

n = nrow(rank18com)
x1 = length(act.Micro)
prop.test(x1, n, p = 0.50, alternative = "greater",  conf.level = 0.95)

n = nrow(rank18com)
x2 = length(act.Peq)
prop.test(x2, n, p = 0.25, alternative = "less",  conf.level = 0.98)

z.test(x=act.Peq, sigma.x = sd(act.Peq), 
       y=act.Micro, sigma.y = sd(act.Micro),
       alternative = "greater", conf.level = 0.90)

t.test(x=act.Peq, y=act.Micro, var.equal = FALSE,
       conf.level = 0.95)

t.test(x=act.Peq, y=act.Micro, var.equal = TRUE,
       conf.level = 0.95)

construc = read.csv("rankingconstruccion.csv",header=TRUE, 
                    dec=",", sep=";")

manufact = read.csv("rankingmanufactura.csv",header=TRUE, 
                    dec=",", sep=";")

GRAN.CONSTRUC = nrow(construc[which(construc$TAMAÑO == "GRANDE"), ])
N.CONSTRUC = nrow(construc)

GRAN.MANUFACT = nrow(manufact[which(manufact$TAMAÑO == "GRANDE"), ])
N.MANUFACT = nrow(construc)

prop.test(x=c(GRAN.CONSTRUC,GRAN.MANUFACT),n=c(N.CONSTRUC,N.MANUFACT), 
          alternative = "greater")

###########################
## ANÁLISIS DE REGRESIÓN ##
###########################

set.seed(1.8)
Publicidad <- rnorm(100, mean=80, sd=10)
Ventas <- 30 + rnorm(100,mean=4,sd=0.3)*Publicidad + rnorm(100,mean = 0,sd=1)

cor(Publicidad, Ventas)

set.seed(1.8)
Publicidad <- rnorm(100, mean=80, sd=10)
Ventas <- 30 + rnorm(100,mean=4,sd=0.3)*Publicidad + rnorm(100,mean = 0,sd=1)

m1 = lm(Ventas ~ Publicidad)
summary(m1)

datos = read.csv("Ranking2018Comercio.csv",header=TRUE,sep=";", dec=",")

datos = datos %>%
  filter(VENTAS>0) %>%
  select(UTILIDAD,EMPLEADOS,VENTAS,TAMA)
attach(datos)

pairs(datos[,1:3])

m3 = lm(UTILIDAD ~ EMPLEADOS + VENTAS + TAMA)
summary(m3)

m4 = lm(UTILIDAD ~ -1 + EMPLEADOS + VENTAS + TAMA)
summary(m4)

datos =read.csv("cap2_big4_size.csv",header=TRUE,dec=",",sep=";")

ggplot(datos,aes(x=UTILIDAD, y=BIG4)) + geom_point()

ggplot(datos,aes(x=UTILIDAD, y=BIG4)) + geom_point() + 
  geom_smooth(method = "lm",se=FALSE)

notas = read.csv("notas.csv",header=TRUE,sep=";")

str(notas)

notas$NIVEL = as.factor(notas$NIVEL)

notas = notas %>%
  mutate(APRUEBA = ifelse(FINAL>=70,1,0))

str(notas)

m5 = glm(formula = APRUEBA ~ NIVEL, family = binomial, data = notas)

summary(m5)

m6 = glm(formula = APRUEBA ~ GENERO + NIVEL , family = binomial, data = notas)

summary(m6)

m7 = glm(formula = APRUEBA ~ -1 + GENERO + NIVEL + CARRERA , family = binomial, data = notas)

summary(m7)
