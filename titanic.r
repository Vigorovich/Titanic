install.packages("dplyr")

install.packages("ggplot2")

install.packages("corrplot")

library("dplyr")

library("ggplot2")

library("corrplot")

titanic <- read.csv("train.csv")

# Explorer la structure des données :

lapply(titanic, class)


#Valeurs manquantes
missing <- which(is.na(titanic),arr.ind=TRUE)
dim(missing)
# Il manque l'age de 177 passagers

#Selection des variables que nous allons étudier
data <- select(titanic, Survived, Sex, Pclass, Age)
summary(data)

# Histogramme montrant la distribution de l'age des passagers
hist(data$Age, main="Age of Titanic passengers", xlab="Age")


# Voyons la distribution des passagers selon leur classe
data %>% group_by(Pclass) %>% summarise(sum(Pclass))
# 216 passagers en 1e classe, 368 en 2nd et 1473 en 3e classe

# Regardons le nombre de femmes et d'hommes
summarise(data, 'female_count' = sum(Sex=='female'),
          'male_count' = sum(Sex=='male'))
# 314 femmes and 577 hommes

# On transforme nos 3 variables en facteurs pour ggplot
data$Sex<-as.factor(data$Sex)
data$Pclass<-as.factor(data$Pclass)
data$Survived<-as.factor(data$Survived)

# Graphique représentant le nombre de passager qui a survecu (ou non) par rapport au sexe
ggplot(data) + 
  geom_bar(aes(x=Survived, fill=Sex)) + 
  labs(title = "Nombre de survivant par rapport au sexe",
       subtitle = "1 = survived")

# Graphique : Sexe des passagers  par rapport à leur classe
ggplot(data) + 
  geom_histogram(aes(x=Pclass, fill=Sex), stat="count") +
  labs(title = "Sexe des survivants pour chaque classe", x="Classe")

# Transformation de la variable age en tranches de 20
cAge <- cut(data$Age, breaks = c(0, 20, 40, 60, 80))

# On ajoute notre nouvlle variable à notre data frame
data <- mutate(data, cAge)

# On enleve les données manquantes
data_no_na <- na.omit(data)

# Graphique : Survie par rapport à l'age des passagers
ggplot(data_no_na) + 
  geom_histogram(aes(x=cAge, fill=Survived), stat="count") +
  labs(title = "Nombre de survivant par age",
       subtitle = "1 = survived",
       x="Age")

# Proportion de survie par sexe
data %>% group_by(Sex) %>% summarise(survived = sum(Survived==1),
                                    not_survived = sum(Survived==0),
                                    tot = survived + not_survived,
                                    survived_rate = survived / tot)


# Proportion de survie par classe
data %>% group_by(Pclass) %>% summarise(survived = sum(Survived==1),
                                       not_survived = sum(Survived==0),
                                       tot = survived + not_survived,
                                       survived_rate = survived / tot)

# Proportion de survie par age
data_no_na %>% group_by(cAge) %>% summarise(survived = sum(Survived==1),
                                       not_survived = sum(Survived==0),
                                       tot = survived + not_survived,
                                       survived_rate = survived / tot)

# Calcul du coefficient de corrélation entre nos variables
mcor <- cor(data.matrix(data_no_na, rownames.force = NA))

# Création d'une palette de couleur dégradé rouge et bleu
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))


# Matrice de corrélation utilisant la matrice préalablement calculé mcor
corrplot(mcor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         sig.level = 0.01, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

# Construction des tables de probabilitées de nos variables
S_P <- prop.table(table(data$Pclass, data$Survived), margin=2)
S_Sx <- prop.table(table(data$Sex, data$Survived))
S_Ca <- prop.table(table(data$cAge, data$Survived))
S <- prop.table(table(data$Survived))

prob_total <- function(Sex, Pclass, Age, S_Sx, S_P, S_Ca, s) {
  # Calcul la probabilité de survie (s=2) ou non (s=1) par rapport au profil du passager (sexe, age, classe)
  
  # On récupère les probas qui nous interressent pour utiliser la formule probabilités totales
  
  # S_Sx = table de probabilité de survie par sexe
  if (Sex == "female") {sex_prob <- S_Sx[1, s]}
  else if (Sex == 'male') {sex_prob <- S_Sx [2, s]}
  else {return ("warning, Sex variable is incorrect")}
  
  # S_Sx = table de probabilité de survie par classe
  if (Pclass == 1) {class_prob <- S_P[1, s]}
  else if (Pclass == 2) {class_prob <- S_P[2, s]}
  else if (Pclass == 3) {class_prob <- S_P[3, s]}
  else {return ("warning, Class variable is incorrect")
       }
     # Pour le test.csv on remplace Na par 0 sur l'âge
  # Une probabilité de 1 n'aura pas d'influence sur le calcul
  if (Age == 0) {age_prob <- 1}
  else if (Age <= 20) {age_prob <- S_Ca[1, s]}
  else if (Age <= 40) {age_prob <- S_Ca[2, s]} 
  else if (Age <= 60) {age_prob <- S_Ca[3, s]} 
  else if (Age <= 100) {age_prob <- S_Ca[4, s]} 
  else {return ("warning, Age variable is incorrect")}
  
  return (sex_prob * class_prob * age_prob)
    }
  


prob_prediction <- function(Sex, Pclass, Age, S_Sx, S_P, S_Ca, S) {
  # naive bayes classifier w/ Maximum a Posteriori Probability
  
  # donne la probabilité de survie d'un passager 
  # en fonction de son age, son sexe et la classe dans laquelle il voyageait
  
  # prend en arguments trois vecteur et les tables de probabilités nécéssaires
  # Sex: caracteres "female" ou "male"
  # Pclass: classe dans laquelle voyageait le passager; numeric
  # Age: numeric
  
  # retourne 1 si la probabilité de survie > 0.5 sinon 0
  
  survived <- prob_total(Sex, Pclass, Age, S_Sx, S_P, S_Ca, 1)
  not_survived <- prob_total(Sex, Pclass, Age, S_Sx, S_P, S_Ca, 2)
  
  if (survived / (survived + not_survived) > 0.5) {return (1)}
  else {return (0)}
}

# Une fois les probabilités calculés sur les données d'entrainement
# On va tenter de prédire si les passagers du fichier test vont survivre ou non

# N'ayant pas le jeu de données avec la survie, nous utiliserons celui de kaggle
# Ou nous pouroons uploader nos prédictions afin de déterminer les perfomances du modèle

test <- read.csv("test.csv")
test <- select(test, Sex, Age, Pclass, PassengerId)

# On remplace nos Na
test[is.na(test)] <- 0

# Création d'un df vide pour y mettre nos prédiction 
pred <- data.frame("PassengerId"=numeric(418),
                   "Survived"=numeric(418), 
                   stringsAsFactors = FALSE)
k <- 1

# On parcours notre dataset (de 419 lignes)
while (k < 419) {
  # On calcul nos prédiction pour chaque ligne
  c <- prob_prediction(Sex=test[k,1], 
                       Age=test[k,2], 
                       Pclass=test[k,3], 
                       S_Sx=S_Sx, 
                       S_P=S_P, 
                       S_Ca=S_Ca, 
                       S=S)
  # On met notre prédiction de survie dans un df
  pred[k, 2] <- c[1]
  # On met aussi l'ID du passager
  pred[k, 1] <- test[k, 4]
  k = k + 1
}

# On créée notre fichier pred.csv avec nos prédiction 
write.csv(pred,'pred.csv', row.names = FALSE)

#score: 0.76794


