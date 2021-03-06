#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout( position="right",
    sidebarPanel(h3("text html 3 commande"),
                 h4("text html 4 commande"),
                 
                 
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30),
       
                 h5("text html 5 commande"),
       
       h6("commande importer du text"),
       textInput("nom1","entrez votre nom",""),
       textInput("age1","entrez votre age","0"),
       
       # cr�er un bouton a cocher masculin Feminin
       h7("commande creer le bouton � cocher"),
       radioButtons("genre1","veuillez selectionner le genre",
                    list("Masculin","Feminin"), ""),
       
       # importer une valeur en cr�ant une bar qu'on peut glisser pour selectionner une valeur
       h8("commande afficher la bar et importer valeur"),
       sliderInput("bare1", "selectionne une valeur ou zom",
                   min=0, max=10, value=2, 
                   animate = TRUE , # pour mettre une puce pour d�filer
                   step = 0.5  # pour d�finir le pas
                   ),
       # pour s�lectionner un intervalle, on modifie la valeur par d�faut "value" par un couple de valeur :
       sliderInput("bare2_couple", "selectionne une valeur ou zom",
                   min=0, max=10, value=c(2,3)),
       
       # cr�er liste d�roulante
       h9("commande cr�er une liste d�roulante de pays benin, france, canada et belgique"),
       selectInput("liste_deroulante_choix_multiple_1", "selectionne un pays", 
                   c("benin", "france", "canada","belgique"),
                   selected = "france", # pour l'actualiser � une valeur par d�faut
                   selectize = FALSE, # pour changer l'aspect de la liste d�roulante
                   multiple=TRUE # question � choix multiple
                   )
                   
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel( h3(" RESULTAT des commandes H3"),
               h4(" RESULTAT des commandes H4"),
       
       # plotOutput() pour les graphs issu de renderPlot()        
       plotOutput("distPlot"),
       
       h5(" RESULTAT des commandes H5"),
       h6("affichage de text import� nom et age"),
       
       # textOutput() pour du text issu de rendretext()
       textOutput("nom1_de_sortie"),
       textOutput("age1_de_sortie"),
       
       h7("le bouton genre  coch�"),
       textOutput("genre1_de_sortie"),
       
       h8(" afficher la valeur importer de la bare"),
       textOutput("bare1_de_sortie"),
       # afficher la phrase :
       textOutput("bare1_de_sortie_phrase"),
       # afficher la phrase pour le couple intervalle :
       textOutput("bare2_couple_de_sortie_phrase"),
       
       h9("afficher les selaction de la liste d�roulante ou choix multiple de pays benin, france, canada et belgique"),
       textOutput("liste_deroulante_choix_multiple_1_couple_de_sortie")
       
       
    )
  )
))
