#https://livebook.datascienceheroes.com/exploratory-data-analysis.html

#1 Analyse de donn�es exploratoire
#1.1 Profiling, La voix des chiffres
#�tat de sant� de l'ensemble de donn�es :
#1 Obtention de m�triques telles que le nombre total de lignes, de colonnes, de types de donn�es, de z�ros et de valeurs manquantes
#2 Impact de chacun des �l�ments pr�c�dents sur diff�rentes analyses
#3 Comment filtrer et utiliser rapidement (et avec) les donn�es, les nettoyer
#Analyse univari�e en variable cat�gorielle :
#Fr�quence, pourcentage, valeur cumulative et trac�s color�s
#Analyse univari�e avec variables num�riques :
#1 Percentile, dispersion, �cart type, moyenne, valeurs sup�rieure et inf�rieure
#2 Centile vs quantile vs quartile
#3 Kurtosis, asym�trie, gamme inter-quartile, coefficient de variation
#4 Trac� des distributions
#�tude de cas compl�te bas�e sur �Data World� , pr�paration et analyse de donn�es


#df_status(data): Profilage de la structure du jeu de donn�es
#describe(data): Profilage num�rique et cat�gorique (quantitatif)
#freq(data): Profilage cat�gorique (quantitatif et graphique).
#profiling_num(data): Profilage de variables num�riques (quantitatif)
#plot_num(data): Profilage de variables num�riques (graphiques)

#1.1.1 �tat de sant� du jeu de donn�es
#La quantit� de z�ros, NA, Inf, les valeurs uniques ainsi que le type de donn�es peuvent conduire � un bon ou mauvais mod�le. Voici une approche pour couvrir la toute premi�re �tape de la mod�lisation de donn�es.

# Loading funModeling!
library(funModeling)
library(dplyr)
data(heart_disease)
#1.1.1.1 V�rification des valeurs manquantes, des z�ros, du type de donn�es et des valeurs uniques
# Profiling the data input
df_status(heart_disease)
#q_zeros: quantit� de z�ros ( p_zeros: en pourcentage)
#q_inf: quantit� de valeurs infinies ( p_inf: en pourcentage)
#q_na: quantit� de NA ( p_na: en pourcentage)
#type: facteur ou num�rique
#unique: quantit� de valeurs uniques

#1.1.1.2 Pourquoi ces m�triques sont-elles importantes?

#Z�ros : les variables avec beaucoup de z�ros peuvent ne pas �tre utiles pour la mod�lisation et, dans certains cas, elles peuvent biaiser consid�rablement le mod�le.
#NA : plusieurs mod�les excluent automatiquement les lignes avec NA ( for�t al�atoire par exemple). En cons�quence, le mod�le final peut �tre biais� en raison de plusieurs lignes manquantes en raison d'une seule variable. Par exemple, si les donn�es ne contiennent qu'une variable sur 100 avec 90% des NA, le mod�le sera en formation avec seulement 10% des lignes d'origine.
#Inf : Les valeurs infinies peuvent conduire � un comportement inattendu dans certaines fonctions de R.
#Type : Certaines variables sont cod�es sous forme de nombres, mais ce sont des codes ou des cat�gories et les mod�les ne les traitent pas de la m�me mani�re.
#Unique : Les variables factorielles / cat�gorielles avec un nombre �lev� de valeurs diff�rentes (~ 30) ont tendance � sur-adapter si les cat�gories ont une cardinalit� faible ( arbres de d�cision, par exemple).


#1.1.1.3 Filtrer les cas non d�sir�s

#Supprimer les variables avec un nombre �lev� de z�ros

# Profiling the Data Input
my_data_status=df_status(heart_disease, print_results = F)

# Removing variables with 60% of zero values
vars_to_remove=filter(my_data_status, p_zeros > 60)  %>% .$variable
vars_to_remove

# Keeping all columns except the ones present in 'vars_to_remove' vector
heart_disease_2=select(heart_disease, -one_of(vars_to_remove))
#Classement des donn�es par pourcentage de z�ros
arrange(my_data_status, -p_zeros) %>% select(variable, q_zeros, p_zeros)

#1.1.2 Profilage des variables qualitatives
#1.1.1.5 Obtenir d'autres statistiques communes: nombre total de lignes , nombre total de colonnes et noms de colonnes :
# Total rows
nrow(heart_disease)
# Total columns
ncol(heart_disease)
# Column names
colnames(heart_disease)
freq(data=heart_disease, input = c('thal','chest_pain'))

freq(data=heart_disease$thal, plot = FALSE, na.rm = TRUE)
freq(data=heart_disease, path_out='C:\\Users\\Hp\\Desktop\\Memoire_ITS4\\Livre Live Data Science')

library(Hmisc)

# Loading data from the book repository without altering the format
data_world=read.csv(file = "https://goo.gl/2TrDgN", header = T, stringsAsFactors = F, na.strings = "..")

# Excluding missing values in Series.Code. The data downloaded from the web page contains four lines with "free-text" at the bottom of the file.
data_world=filter(data_world, Series.Code!="")

# The magical function that keeps the newest values for each metric. If you're not familiar with R, then skip it.
max_ix<-function(d) 
{
  ix=which(!is.na(d))
  res=ifelse(length(ix)==0, NA, d[max(ix)])
  return(res)
}

data_world$newest_value=apply(data_world[,5:ncol(data_world)], 1, FUN=max_ix)

# Printing the first three rows
head(data_world, 3)

# Loading needed libraries
library(funModeling) # contains heart_disease data
library(minerva) # contains MIC statistic
library(ggplot2)
library(dplyr)
library(reshape2) 
library(gridExtra) # allow us to plot two plots in a row
options(scipen=999) # disable scientific notation
anscombe_data = 
  read.delim(file="https://goo.gl/mVLz5L", header = T)
cor_1 = cor(anscombe_data$x1, anscombe_data$y1)
cor_2 = cor(anscombe_data$x2, anscombe_data$y2)
cor_3 = cor(anscombe_data$x3, anscombe_data$y3)
cor_4 = cor(anscombe_data$x4, anscombe_data$y4)

plot_anscombe <- function(x, y, value, type)
{
  # 'anscombe_data' is a global variable, this is 
  # a bad programming practice ;)
  p=ggplot(anscombe_data, aes_string(x,y))  + 
    geom_smooth(method='lm', fill=NA) + 
    geom_point(aes(colour=factor(1), 
                   fill = factor(1)), 
               shape=21, size = 2
    ) + 
    ylim(2, 13) + 
    xlim(4, 19) + 
    theme_minimal() + 
    theme(legend.position="none") + 
    annotate("text", 
             x = 12, 
             y =4.5, 
             label = 
               sprintf("%s: %s", 
                       type, 
                       round(value,2)
               )
    )  
  
  return(p)
}

# plotting in a 2x2 grid
grid.arrange(plot_anscombe("x1", "y1", cor_1, "R2"), 
             plot_anscombe("x2", "y2", cor_2, "R2"), 
             plot_anscombe("x3", "y3", cor_3, "R2"), 
             plot_anscombe("x4", "y4", cor_4, "R2"), 
             ncol=2, 
             nrow=2)