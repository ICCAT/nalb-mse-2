# ============================================================
# Script: translate_text.R
#
# Purpose:
#   Provide the translate_info() function, which adds
#   multilingual support (English, Spanish and French) to
#   the shinyFLBEIA input object. Replaces all descriptive
#   text fields with named lists containing the three
#   language versions, covering: MSE title and summary,
#   MP descriptions, OM factor and level descriptions,
#   time series labels, performance indicator descriptions,
#   and fleet descriptions.
#
# Inputs:
#   - my_object : shinyFLBEIA input list, as produced by
#                 Prepare_Shiny_Input.R, containing the
#                 English text fields to be translated
#   - title_es, title_fr     : MSE title in ES and FR
#                              (defined in Prepare_Shiny_Input.R)
#   - summary_es, summary_fr : MSE summary in ES and FR
#                              (defined in Prepare_Shiny_Input.R)
#
# Outputs:
#   - Returns the same shinyFLBEIA input object with all
#     descriptive text fields replaced by named lists of
#     the form list(en = ..., es = ..., fr = ...)
#
#
# Author: AZTI
# ============================================================


# Translate information in Shiny object:
# Three languages are required:
# en = English (mandatory)
# es = Spanish
# fr = French

translate_info = function(my_object) {

  # Title MSE -------------------------------------------------------------
  tmp_list = list()
  tmp_list$en = my_object$title
  tmp_list$es = title_es
  tmp_list$fr = title_fr
  # Replace main object:
  my_object$title = tmp_list
  
  # Summary MSE -------------------------------------------------------------
  tmp_list = list()
  tmp_list$en = my_object$summary
  tmp_list$es = summary_es
  tmp_list$fr = summary_fr
  # Replace main object:
  my_object$summary = tmp_list

  # MP Description ----------------------------------------------------------
  tmp_list = list()
  tmp_list$en = my_object$mp$metadata
  tmp_list$es = tmp_list$en
  tmp_list$es$Description = c('TAC a partir de HCR basado en modelo: F_tgt=0.8F_msy, B_thr=B_msy. TAC no podrá aumentar más de un 25 % ni disminuir más de un 20 % entre períodos de gestión consecutivos. El TAC no podrá superar las 50,000 t. Esto es equivalente al MP actual.',
                              'TAC a partir de HCR basado en modelo: F_tgt=0.8F_msy, B_thr=B_msy. Variación máxima del TAC del 10% entre períodos de gestión consecutivos. El TAC no puede superar las 50,000 t.',
                              'TAC a partir de HCR basado en modelo: F_tgt=F_msy, B_thr=B_msy. TAC no podrá aumentar más de un 25 % ni disminuir más de un 20 % entre períodos de gestión consecutivos. El TAC no podrá superar las 50,000 t.',
                              'TAC a partir de HCR basado en modelo: F_tgt=F_msy, B_thr=B_msy. Variación máxima del TAC del 10% entre períodos de gestión consecutivos. El TAC no puede superar las 50,000 t.',
                              'TAC es constante (42,000 t) si el índice combinado supera el valor de referencia. Variación máxima del TAC del 15% entre períodos de gestión consecutivos.',
                              'TAC basado en HCR empírica. Ponderación para obtener un índice combinado. Variación máxima del TAC del 10% entre períodos de gestión consecutivos. El TAC no puede superar las 50,000 t.',
                              'TAC basado en HCR empírica. Ponderación para obtener un índice combinado. TAC no podrá aumentar más de un 25 % ni disminuir más de un 20 % entre períodos de gestión consecutivos. El TAC no podrá superar las 50,000 t.' )
  tmp_list$fr = tmp_list$en
  tmp_list$fr$Description = c("TAC issu d’une HCR basée sur un modèle : F_tgt=0,8F_msy, B_thr=B_msy. Augmentation maximale du TAC de 25% et diminution maximale de 20% entre périodes de gestion consécutives. Le TAC ne peut dépasser 50 000 t. Ceci est équivalent à la MP actuelle.",
                              "TAC issu d’une HCR basée sur un modèle : F_tgt=0,8F_msy, B_thr=B_msy. Variation maximale du TAC de 10% entre périodes de gestion consécutives. Le TAC ne peut dépasser 50 000 t.",
                              "TAC issu d’une HCR basée sur un modèle : F_tgt=F_msy, B_thr=B_msy. Augmentation maximale du TAC de 25% et diminution maximale de 20% entre périodes de gestion consécutives. Le TAC ne peut dépasser 50 000 t.",
                              "TAC issu d’une HCR basée sur un modèle : F_tgt=F_msy, B_thr=B_msy. Variation maximale du TAC de 10% entre périodes de gestion consécutives. Le TAC ne peut dépasser 50 000 t.",
                              "Le TAC est constant (42 000 t) si l’indice combiné est supérieur à la valeur de référence. Variation maximale du TAC de 15% entre périodes de gestion consécutives.",
                              "TAC issu d’une HCR empirique. Pondération pour dériver l’indice combiné. Variation maximale du TAC de 10 % entre périodes de gestion consécutives. Le TAC ne peut dépasser 50 000 t.",
                              "TAC issu d’une HCR empirique. Pondération pour dériver l’indice combiné. Augmentation maximale du TAC de 25 % et diminution maximale de 20 % entre périodes de gestion consécutives. Le TAC ne peut dépasser 50 000 t."
)
  # Replace main object:
  my_object$mp$metadata = tmp_list

  # OM Factor Description ----------------------------------------------------------
  tmp_list = list()
  tmp_list$en = my_object$om$metadata$factor
  tmp_list$es = tmp_list$en
  tmp_list$es$Description = c("Conjunto de referencia.",
                              "Test de robustez: disminución de 20% en reclutamiento sin pesca en el periodo de proyección.",
                              "Test de robustez: incremento de 20% en reclutamiento sin pesca en el periodo de proyección.",
                              "Test de robustez: incremento de 20% en variabilidad de reclutamiento en el periodo de proyección.")
  tmp_list$fr = tmp_list$en
  tmp_list$fr$Description = c("Ensemble de référence.",
                              "Test de robustesse : diminution de 20 % du niveau de recrutement non exploité pendant la période de projection.",
                              "Test de robustesse : augmentation de 20 % du niveau de recrutement non exploité pendant la période de projection.",
                              "Test de robustesse : augmentation de 20 % de la variabilité du recrutement pendant la période de projection.")
  # Replace main object:
  my_object$om$metadata$factor = tmp_list  
  
  
  # OM Level Description ----------------------------------------------------------
  tmp_list = list()
  tmp_list$en = my_object$om$metadata$level
  tmp_list$es = tmp_list$en
  tmp_list$es$Description = c("Sin cambio en el peso de los datos.",
                              "Aumenta el peso de CPUE.",
                              "Aumenta el peso de composición por tallas.",
                              "Aumenta el peso de datos de edad a la talla.")
  tmp_list$fr = tmp_list$en
  tmp_list$fr$Description = c("Pas de changement dans la pondération des données.",
                              "Augmentez le poids de la CPUE.",
                              "Augmenter la répartition par taille.",
                              "Augmenter la pondération des données relatives à l'âge par rapport à la taille.")
  # Replace main object:
  my_object$om$metadata$level = tmp_list
  
  # TS Description:
  tmp_list = list()
  tmp_list$en = my_object$timeseries$metadata
  tmp_list$es = tmp_list$en
  tmp_list$es$Description = c("Biomasa desovante relativo a la biomasa desovante al MSY.", 
                                                   "Mortalidad por pesca relativo a la mortalidad por pesca al MSY.",
                                                   "Captura total permisible (toneladas).")
  tmp_list$fr = tmp_list$en
  tmp_list$fr$Description = c("Biomasse reproductrice par rapport à la biomasse reproductrice au MSY.", 
                                                   "Mortalité par pêche par rapport à la mortalité par pêche au MSY.",
                                                   "Capture totale autorisée (en tonnes).")
  # Replace main object:
  my_object$timeseries$metadata = tmp_list
  

  # Kobe: no need to translate ----------------------------------------------

  # Performance indicators --------------------------------------------------
  tmp_list = list()
  tmp_list$en = my_object$pi$metadata
  tmp_list$es = tmp_list$en
  tmp_list$es$Description = c('Valor mínimo de SB/SB_MSY.',
                              'Media de SB/SB_MSY.',
                              'Media de F/F_MSY.',
                              'Prob. Cuadrante verde de diagrama Kobe.',
                              'Prob. Cuadrante rojo de diagrama Kobe.',
                              'Prob. SB > 40%SB_MSY.',
                              'Prob. SB_MSY > SB > 40%SB_MSY.',
                              'TAC promedio (Corto plazo, 1-3 Años).',
                              'TAC promedio (Mediano plazo, 5-10 Años).',
                              'TAC promedio (Largo plazo, 15-25 Años).',
                              'Desviación estándar en TAC.',
                              'Cambio absoluto promedio en TAC.',
                              'Prob. Cambio de TAC (%) mayor a 10%.',
                              'Max. Cambio de TAC (%) entre periodos.')
  tmp_list$fr = tmp_list$en
  tmp_list$fr$Description = c('Valeur minimale de SB/SB_MSY.',
                              'Moyenne de SB/SB_MSY.',
                              'Moyenne de F/F_MSY.',
                              'Prob. Quadrant vert du diagramme de Kobe.',
                              'Prob. Quadrant rouge du diagramme de Kobe.',
                              'Prob. SB > 40%SB_MSY.',
                              'Prob. SB_MSY > SB > 40%SB_MSY.',
                              'TAC moyenne (Court terme, 1-3 Années).',
                              'TAC moyenne (À moyen terme, 5-10 Années).',
                              'TAC moyenne (À long terme, 15-25 Années).',
                              'Écart type des TAC.',
                              'Variation moyenne absolue des TAC.',
                              'Prob. Variation du TAC (%) supérieure à 10 %.',
                              "Max. Variation du TAC (%) d'une période à l'autre.")
  # Replace main object:
  my_object$pi$metadata = tmp_list
  
  # Fleets ------------------------------------------------------------------
  tmp_list = list()
  tmp_list$en = my_object$fleet$metadata
  tmp_list$es = tmp_list$en
  tmp_list$es$Description = c('Cebo (España, Francia)',
                                              'Cebo Islas (Portugal Madeira/Azores, España Canarias) para trimestres 1, 3, y 4',
                                              'Cacea (España, Francia) y Agalleras (Francia, Irlanda)',
                                              'Arrastre media agua (Francia, Irlanda)',
                                              'Japan palangre norte 30',
                                              'Japan palangre sur 30',
                                              'Taiwan palangre norte 30',
                                              'Taiwan palangre sur 30',
                                              'US y Canada palangre norte 30',
                                              'US palangre sur 30',
                                              'Venezuela palangre',
                                              'Banderas mixtas palangre (KR, PA, CHN)',
                                              'Otros palangre',
                                              'Otros artes de superficie',
                                              'Cebo Islas (Portugal Madeira/Azores, España Canarias) para trimestre 2')
  tmp_list$fr = tmp_list$en
  tmp_list$fr$Description = c('Bateau (Espagne, France)',
                                              'Bateau Îles (Portugal Madeira/Azores, Espagne Canaries) par trimestre 1, 3, et 4',
                                              'Cacea (Espagne, France) et Filet Maillant (France, Irlande)',
                                              'Traîne en eaux moyennes (France, Irlande)',
                                              'Japan palangre nord 30',
                                              'Japan palangre sud 30',
                                              'Taiwan palangre nord 30',
                                              'Taiwan palangre sud 30',
                                              'US et Canada palangre nord 30',
                                              'US palangre sud 30',
                                              'Venezuela palangre',
                                              'Drapeaux mixtes palangre (KR, PA, CHN)',
                                              'Autres palangre',
                                              'Autres autres engins de surface',
                                              'Bateau Îles (Portugal Madeira/Azores, Espagne Canaries) par trimestre 2')
  # Replace main object:
  my_object$fleet$metadata = tmp_list
  
  # Return object:
  return(my_object)
  
}
