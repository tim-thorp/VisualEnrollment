
# Carga de los fichero de datos
data_files <- load_files()

# Carga de los ficheros de traduccion
translations <- load_translations()

informaticaServer <- function(id, lang) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Valors reactius --------------------------------------------------------
      
      step <- reactiveValues(val=0)
      hoveredList <- reactiveValues(ass=character())
      clickedList <- reactiveValues(ass=character())
      descartaList <- reactiveValues(ass=character())
      recomendedList <- reactiveValues(ass=character())
      selectedList <- reactiveValues(ass=character())
      
      # JULIA 24/12/2022 parámetros de entrada
      # JULIÀ 24/10/2023 de moment ho desactivem
      #claveTutor <- reactive({
      #  if (is.null(get_query_param()$tutor)) {
      #    return("")
      #  }
      #  return(get_query_param()$tutor)
      #})
      
      dadesGrau <- reactive({
        if (is.null(input$grau)) {
          return(list(
            # JULIA 05/10/2024
            #x = NULL,
            dadesASS = NULL,
            idps = NULL,
            ASSTFM = NULL,
            tipologia = NULL,
            dadesEST = NULL, aepsEST=NULL, 
            noms = NULL,
            matr = NULL,
            Dsol = NULL, Dpop = NULL, Ddif = NULL, Dreq=NULL, Dabs=NULL
          ))
        }
        
        # dades del grau seleccionat
        
        g = input$grau
        x <- data_files[[paste0("recomanacions_", g)]]
        # por qué se repite tantas veces? !!!
        print(dim(x))
        # JULIA 17/11/23
        #x$grau=g
        
        # llegir AEPs (todas)
        aeps <- data_files[[paste0("aeps_", g)]]
        
        tipologia <- data_files[[paste0("tipologia_", g)]]
        noms <-  data_files[[paste0("noms_", g)]]
        
        # JULIA 27/12/2022 eliminar dadesASSsem
        dadesASS = data_files[[paste0("assignatures_", input$grau)]]        
        
        # Estudiants: filtrar per nombre de semestres i tope d'assignatures
        # matriculades
        # JULIA 23/12/2022 cambiar el filtro para cargar más estudiantes
        # JULIA 13/10/2023 cambiar para el nuevo formato
        #idps=unique(x[(x$V7+x$V9)<=20,'V1'])
        idps=unique(x$V1)
        idps=c("---", idps)
        
        # Solapaments i semestres
        solap1 <- data_files[[paste0("solap1_", g)]]
        solap2 <- data_files[[paste0("solap2_", g)]]
        if (input$sem1o2==1) {
          matr <- solap1[solap1$subject_code %in% selectedList$ass,]
        } else {
          matr <- solap2[solap2$subject_code %in% selectedList$ass,]
        }    
        
        # JULIÀ 18/10/2023 preparar aquí les matrius
        
        # JULIA 23/12/2022 leer la matriz Dsol (Dso1 / Dso2)    
        if (input$sem1o2==1) {
          temp <- data_files[[paste0("Dso1_", g)]]
        } else { 
          temp <- data_files[[paste0("Dso2_", g)]]
        }    
        Dsol = matrix(temp$sol, nrow=length(dadesASS$ass), ncol=length(dadesASS$ass), byrow=T)
        colnames(Dsol)=dadesASS$ass
        Dsol=Dsol/max(Dsol)
        temp=NULL
        
        # JULIA 23/12/2022 leer la matriz Dpop
        temp <- data_files[[paste0("Dpop_", g)]]
        Dpop = matrix(temp$pop, nrow=length(dadesASS$ass), ncol=length(dadesASS$ass), byrow=T)
        colnames(Dpop)=dadesASS$ass
        Dpop=Dpop/max(Dpop)
        temp=NULL
        
        # JULIA 23/12/2022 leer la matriz Ddif
        temp <- data_files[[paste0("Ddif_", g)]]
        Ddif = matrix(temp$dif, nrow=length(dadesASS$ass), ncol=length(dadesASS$ass), byrow=T)
        colnames(Ddif)=dadesASS$ass
        Ddif=Ddif/max(Ddif)
        temp=NULL
        
        # JULIA 24/12/2022 prerequisitos 
        temp <- data_files[[paste0("Dreq_", g)]]        
        Dreq = matrix(temp$req, nrow=length(dadesASS$ass), ncol=length(dadesASS$ass), byrow=T)
        colnames(Dreq)=dadesASS$ass
        Dreq=Dreq/max(Dreq)
        temp=NULL
        
        # JULIA 27/12/2022 ordenación semestral 
        temp <- data_files[[paste0("Dabs_", g)]]        
        Dabs = matrix(temp$abs, nrow=length(dadesASS$ass), ncol=length(dadesASS$ass), byrow=T)
        colnames(Dabs)=dadesASS$ass
        Dabs=Dabs/max(Dabs)
        temp=NULL
        
        # dadesEST
        dadesEST = NULL
        aepsEST = NULL
        if (!is.null(input$idp) && input$idp!="---") {
          # JULIA 05/10/2023
          # V1=userid, V3=sem, V4=rel, V11=ass, V12=nota
          dadesEST=x[x[,1]==input$idp,c(3,4,11,12)]
          colnames(dadesEST)=c('sem','rel','ass','nota')
          
          # la última nota de cada asignatura
          dadesEST=aggregate(dadesEST[,c('sem','rel','nota')],list(dadesEST$ass),function(x){return(x[length(x)])})
          colnames(dadesEST)=c('ass','sem','rel','nota')
          
          # convalidades: és com una nota especial (només les incorporades!)
          #browser()
          aepsEST=aeps[aeps$V1==input$idp & aeps$V4=="Reconeguda",c(2,4)]
          colnames(aepsEST)=c('ass','nota')
          
          # si té convalidades
          if (nrow(aepsEST)>0) {
            
            aepsEST$sem="N/A"
            aepsEST$rel=0
            aepsEST=aepsEST[,match(names(dadesEST),names(aepsEST))]
            
            # afegir les convalidades a les ja matriculades
            dadesEST=rbind(aepsEST,dadesEST)
          } else {
            aepsEST=NULL
          }
          # JULIA 29/08/2023 mantener el semestre absoluto
          #dadesEST$sem=NULL
        }
        
        # Asignatura TFM es la que té tipus T
        ASSTFM <- dadesASS %>% filter(type == "T") %>% pull(ass) 
        
        # Agrupem el resultat
        return (list(
          # JULIA 05/10/2024
          #x = x,
          idps = idps,
          ASSTFM = ASSTFM,
          tipologia = tipologia,
          dadesASS = dadesASS,
          dadesEST = dadesEST, aepsEST=aepsEST,
          noms = noms,
          matr = matr,
          Dsol = Dsol, Dpop = Dpop, Ddif = Ddif, Dreq = Dreq, Dabs = Dabs
        ))
      })  
      
      output$graf_hover_info <- renderUI({
        #print("graf_hover_info")
        hovered <- NULL
        if(!is.null(input$graf_hover)) hovered <- asignatura_posicio(input$graf_hover)
        if(is.null(hovered)) return (NULL)
        hoveredList$ass <- hovered$ass
        # print("hovered")
        # print(hovered)
        dadesGrau <- dadesGrau()
        hovered$tipologia <- asignatura_tipologia(lang, dadesGrau, hovered)
        hovered$name <- asignatura_nom(lang, dadesGrau, hovered)
        html = asignatura_hover(input, hovered)
        return(html)
      })
      
      
      # Gràfic del mapa asignatures -------------------------------------------------------
      
      output$grafic=renderPlot({
        dadesGrau = dadesGrau()
        # JULIA 05/10/2024
        #x = dadesGrau$x
        idps = dadesGrau$idps
        ASSTFM = dadesGrau$ASSTFM
        dadesASS =  dadesGrau$dadesASS
        dadesEST = dadesGrau$dadesEST
        
        #browser()
        
        # tots els parells d'assignatures ordenades per codi
        ass=sort(dadesASS$ass)
        # JULIÀ 17/10/2023 treure ass2
        #ass2=expand.grid(ass1 = ass, ass2 = ass)
        #if(nrow((ass2))==0) return(NULL)
        # print("ass2")
        # print(head(ass2))
        
        # JULIA 23/12/2022 adaptar la matriz de solapamientos
        if (is.null(dadesGrau$Dsol)) return(NULL)
        Dsol=dadesGrau$Dsol
        
        # JULIA 23/12/2022 adaptar la matriz de popularidad relativa
        if (is.null(dadesGrau$Dpop)) return(NULL)
        Dpop=dadesGrau$Dpop
        
        # JULIA 23/12/2022 adaptar la matriz de dificultad
        if (is.null(dadesGrau$Ddif)) return(NULL)
        Ddif = dadesGrau$Ddif
        
        # JULIA 24/12/2022 adaptar la matriz de prerequisitos
        if (is.null(dadesGrau$Dreq)) return(NULL)
        Dreq = dadesGrau$Dreq
        
        # JULIA 27/12/2022 adaptar la matriz con el plan de estudios 
        if (is.null(dadesGrau$Dabs)) return(NULL)
        Dabs=dadesGrau$Dabs
        
        # !!! RENDER PLOT
        # creació del gràfic, de moment és un plot
        totOK=!is.null(input$dificultat) &&
          !is.null(input$semestre) &&
          !is.null(input$requisit) &&
          !is.null(input$popularitat) &&
          !is.null(input$overlap)
        
        #print(paste("dificultat:", !is.null(input$dificultat)))
        #print(paste("semestre:", !is.null(input$semestre)))
        #print(paste("requisit:", !is.null(input$requisit)))
        #print(paste("popularitat:", !is.null(input$popularitat)))
        #print(paste("overlap:", !is.null(input$overlap)))
        #print(paste("idp:", !is.null(input$idp)))
        #print(paste("indicador:", !is.null(input$indicador)))
        #print(paste("totOk:", totOK))
        
        if (totOK) {
          dificultat <- input$dificultat
          semestre <- input$semestre
          requisit <- input$requisit
          popularitat <- input$popularitat
          overlap <- input$overlap 
          
          # JULIA 24/12/2022 normalizar los pesos
          WD=5.0
          
          resultat <<- matrix(
            # JULIA 23/12/2022 cambiar matrices
            (dificultat/WD)*Ddif +
              (overlap/WD)*Dsol + 
              (requisit/WD)*Dreq +
              (popularitat/WD)*Dpop +
              (semestre/WD)*Dabs,
            length(ass),length(ass))
          
          if (!is.null(resultat)) {
            # JULIA 23/12/2022 forzar una distancia mínima
            # añadimos un "pequeño" margen a los ceros
            set.seed(1)
            resultat=apply(resultat,c(1,2),function(x){ifelse(x>0,x,runif(1,0,0.1))})
            # JULIA 24/12/2022 forzar una separación para poder generar cuadrícula
            resultat=resultat+1
            
            # crear mapeado 2D de la matriz de distancias resultante
            # JULIA 07/01/2023 se podrían probar otros algoritmos
            q <- sammon(resultat, trace=F)
            #q <- isoMDS(resultat, k=2)
            
            # corrección manual tomando como referencia el TF
            # el TF debería quedar lo más a la derecha y arriba posible
            # !!!
            TF=which(ass==ASSTFM)
            r=range(q$points[,1])
            if ((q$points[TF,1]-r[1])/(r[2]-r[1])<0.5) {
              q$points[,1]=-q$points[,1]
            }
            r=range(q$points[,2])
            if ((q$points[TF,2]-r[1])/(r[2]-r[1])<0.5) {
              q$points[,2]=-q$points[,2]
            }
            
            q <- as.data.frame(q$points)
            q$ass=ass
            
            # afegir dades de les assignatures
            q <- merge(q,dadesASS,'ass',all.x=T)
            #q$TFM=as.factor(ifelse(q$ass==ASSTFM,0,0))
            
            # afegir dades estudiant
            if (!is.null(input$idp)) {
              if (input$idp!="---") {
                q <- merge(q,dadesEST,'ass',all.x=T)
                q[is.na(q$nota),'nota']=translate(lang, "Pending")
              } else {
                q$nota=translate(lang, "Pending")
              }
            } else {
              q$nota=translate(lang, "Pending")
            }
            
            # JULIA 30/12/2022 quitar left_join, aprovechando que todo está
            # ordenado
            q$full_name=dadesGrau$noms[[paste0("name_",lang)]]
            q$full_name=paste0(q$full_name," (",q$abrv,")")
            
            # JULIA 28/12/2022 esto va lento
            #asignatures <- tibble(
            #  ass = dadesGrau$noms[["ass"]],
            #  noms = dadesGrau$noms[[paste0("name_",lang)]]
            #)
            #q <- left_join (q, asignatures) %>%
            #  mutate(full_name = paste0(abrv, ": ", noms))
            
            # glimpse(q)
            
            # Controlar estats (Controlar estados)
            # JULIA 09/01/2023 cambiar full_name por abrv
            R1 <-  q %>% filter(ass %in% recomendedList$ass[1]) %>% pull(abrv) %>% paste0("R1 ", .)
            R2 <-  q %>% filter(ass %in% recomendedList$ass[2]) %>% pull(abrv) %>% paste0("R2 ", .)
            R3 <-  q %>% filter(ass %in% recomendedList$ass[3]) %>% pull(abrv) %>% paste0("R3 ", .)
            R4 <-  q %>% filter(ass %in% recomendedList$ass[4]) %>% pull(abrv) %>% paste0("R4 ", .)
            R5 <-  q %>% filter(ass %in% recomendedList$ass[5]) %>% pull(abrv) %>% paste0("R5 ", .)
            R6 <-  q %>% filter(ass %in% recomendedList$ass[6]) %>% pull(abrv) %>% paste0("R6 ", .)
            q <- q %>% 
              mutate(nota = ifelse(ass %in% recomendedList$ass[1], R1, nota)) %>%
              mutate(nota = ifelse(ass %in% recomendedList$ass[2], R2, nota)) %>%
              mutate(nota = ifelse(ass %in% recomendedList$ass[3], R3, nota)) %>%
              mutate(nota = ifelse(ass %in% recomendedList$ass[4], R4, nota)) %>%
              mutate(nota = ifelse(ass %in% recomendedList$ass[5], R5, nota)) %>%
              mutate(nota = ifelse(ass %in% recomendedList$ass[6], R6, nota)) %>% 
              mutate(nota = ifelse(ass %in% descartaList$ass, translate(lang, "Discarded"), nota)) %>% 
              mutate(nota = ifelse(ass %in% selectedList$ass, translate(lang, "Selected"), nota))
            
            if (!is.null(input$sem1o2)) {
              q <- q %>% 
                mutate(nota = ifelse(!bsem %in% c(0,input$sem1o2), translate(lang,"Not available"), nota))
            } else { 
              q <- q %>% 
                mutate(nota = ifelse(!bsem %in% c(0,1), translate(lang,"Not available"), nota))
            }
            
            
            # forzar y ordenar todas las notas posibles
            # incloure les convalidades (Reconeguda)
            q[q$nota %in% c('A','NO','EX','M'),'nota']=translate(lang, "Pass")
            q[q$nota %in% c('NP','SU'),'nota']=translate(lang, "Fail")
            q[q$nota %in% c('Reconeguda'),'nota']=translate(lang, "Transfer")
            
            q$nota=factor(q$nota,c(translate(lang, "Pass"),
                                   translate(lang, "Fail"),
                                   translate(lang, "Pending"),
                                   translate(lang, "Not available"),
                                   translate(lang, "Transfer"),
                                   translate(lang, "Discarded"),
                                   translate(lang, "Selected"),
                                   R1,R2,R3,R4,R5,R6))
            # JULIA 24/12/2022 cambio pendiente de la paleta de colores
            #pal <- c("#c5c4c4", "#FF7D87", "#9aebfd", "#fdf6f6", "#e0e0e0", "#4875fb", "#22b33b", "#4adb63", "#8cff8c", "#acffa3", "#cdffc1", "#e8ffe0")
            pal <- c("#73edff", "#FF7D87", "#c5c4c4", "#fdf6f6", "#D8F6FF", "#ccbb11", "#4875fb", "#22b33b", "#4adb63", "#8cff8c", "#acffa3", "#cdffc1", "#e8ffe0")
            
            # graf
            colorT="black"
            gg=ggplot(q, aes(x=V1,y=V2,label=abrv)) +
              theme_void() +
              theme(
                panel.background=element_rect(fill=NA, color=NA)
              )
            
            # si no hi ha cap indicador triat
            if (input$idp!="---" || is.null(input$indicador)) {
              # color de l'àrea segons la nota
              # JULIA 07/01/2023 cambiar position a right
              if (input$bubbles) {
                gg = gg +
                  geom_voronoi_tile(aes(x=V1,y=V2,fill=nota,group=-1L), colour="white",max.radius=input$bubbles) +
                  scale_fill_manual(values=pal, drop = F)+guides(colour="none")+theme(legend.position="right") +
                  labs(fill = translate(lang, "Subject status"))
              } else {
                gg = gg + 
                  geom_voronoi_tile(aes(x=V1,y=V2,fill=nota,group=-1L), colour="white") +
                  scale_fill_manual(values=pal, drop = F)+guides(colour="none")+theme(legend.position="right") +
                  labs(fill = translate(lang, "Subject status"))
              }
            } else {
              # color de l'àrea en funció de l'indicador
            }
            
            # afegir les assignatures
            q$quines=1
            if (!is.null(input$tipologia)) {
              q <- asignatura_quines(lang, dadesGrau, q, input)
            }
            if (!is.null(input$buscar) && str_length(input$buscar) > 0) {
              q <- asignatura_buscar(lang, dadesGrau, q, input)
            }
            
            # JULIA 20/12/2022 forzar que aparezcan todas las asignaturas
            # cuando se seleccionan las asignaturas a matricular no se
            # muestran los "centros" de las recomendadas, pero solo si la
            # selección de nombre de asignatura está vacía
            if (!is.null(input$tipologia) & is.null(input$buscar)) {
              if (input$tipologia==translate(lang, "All")) {
                q$quines=1
              }
            }
            
            qq = q[q$nota==translate(lang, "Pending") | q$quines==1,]
            # JULIA 07/01/2023 cambiar a 1 columna en vertical
            gg <- gg +
              geom_point(data = qq, aes(alpha=ifelse(quines==1,1,0.72)), shape = 19, size = 2, colour = colorT) +
              guides(colour="none") + 
              guides(alpha="none") +
              guides(fill=guide_legend(ncol=1)) +
              geom_text_repel(aes(alpha=ifelse(q$quines==1,1,0.72)),size=5,colour=colorT) +
              theme(
                legend.justification = "left",
                legend.direction = "vertical",
                legend.title = element_text(size = 15, face = "bold"),
                legend.text = element_text(size = 13)
              )
            
            # guardamos el mapa para hover, etc.
            #print("save qQ")
            qQ <<- q
            
            # show map
            gg
          }
        }
        
      }, height=600)
      
      
      
      
      # Gràfic del calendari --------------------------------------------------------------
      
      output$cal=renderPlot({
        dadesGrau = dadesGrau()
        lli=NULL
        matr=NULL
        a <- !is.null(selectedList$ass[1]) || 
          !is.null(selectedList$ass[2]) ||
          !is.null(selectedList$ass[3]) ||
          !is.null(selectedList$ass[4]) || 
          !is.null(selectedList$ass[5]) || 
          !is.null(selectedList$ass[6])
        workload <- if(!is.null(input$workload)){ input$workload } else{ 1 }
        inputsOK <- !is.null(input$sem1o2) && a
        if (!inputsOK) return(NULL)
        matr <- dadesGrau$matr
        if (nrow(matr)==0) return(NULL)
        
        # per cada activitat de cada assignatura nomes darrers N dies indicats 
        for (i in 1:nrow(matr)) {
          j=ncol(matr)
          
          # buscar el primer 1 per la dreta
          while (matr[i,j]==0) {
            j=j-1
          }
          # guardar els lliuraments
          lli=rbind(lli,data.frame(subject_code=matr[i,1],dia=j-4,load=1))
          
          # "saltar" els dies indicats
          j=j-workload
          
          # treure els 1 anteriors
          while ((matr[i,j]==1) && (j>4)) {
            matr[i,j]=0
            j=j-1
          }
        }
        
        # acumular les activitats de cada assignatura
        acum=aggregate(matr[,-(1:4)],list(matr$subject_code), sum)
        # print("colnames(acum)")
        colnames(acum)[1]='subject_code'
        
        # format long, recuperant el dia
        res=gather(acum,key='dia',value='load',-subject_code)
        res$dia=as.numeric(substring(res$dia,2))
        res=res[order(res$subject_code,res$dia),]
        quines=unique(res$subject_code)
        res$subject_code=factor(res$subject_code, levels=quines)
        
        # calculem la carrega mitjana per dia 
        perfil=aggregate(res$load,list(res$dia),sum)$x
        
        # OLD: Average daily activities;  load=round(mean(perfil)*100)/100
        load <- sum(perfil > 0)
        load_prop <- round(100 * (load / length(perfil)), 2)
        
        # OLD: Percentage of overlapping deadlines: load2=round((sum(perfil>1)/sum(perfil>0))*100)/100
        load2 <- sum(perfil > 1)
        load2_prop <- round(100 * (load2 / length(perfil)), 2)
        
        # Preparem dates
        inici_sem <- inici_semestres()
        # 09/02/2023 JULIA: no funciona para el segundo semestre !!!
        #inici <- if_else(input$sem1o2 == 1, inici_sem$sem1, inici_sem$sem2)
        inici <- inici_sem$sem1
        asignatures <- tibble(
          ass = dadesGrau$noms[["ass"]],
          # JULIA 30/12/2022 cambiar noms por full_name
          full_name = dadesGrau$noms[[paste0("name_",lang)]]
        ) %>% 
          left_join(dadesGrau$dadesASS, by = "ass") %>% 
          mutate(full_name = paste0(full_name," (",abrv,")"))
        res <- as_tibble(res) %>% 
          mutate(dia_semestre = (inici + dia)) %>% 
          left_join(asignatures, by = c("subject_code" = "ass"))
        lli <- as_tibble(lli) %>% 
          mutate(
            dia_semestre = (inici + dia),
            subject_code = factor(lli$subject_code, levels = quines),
          ) %>% 
          left_join(asignatures, by = c("subject_code" = "ass"))
        
        # fem el gràfic
        # print("dades gràfic")
        # print(res)
        # print("dades asignatures")
        # print(lli)
        gg <- res %>% ggplot(aes(x = dia_semestre, y = load)) +
          geom_col( width = 0.5, alpha = 0.28, fill = "#4875fb") +
          geom_col(data = lli, aes(x = dia_semestre, y = load), fill = "#4875fb", width = 0.72, alpha = 1) +
          scale_y_continuous(breaks = seq(0, 8)) +
          scale_x_date(
            name = "Dia",
            limits = c((inici), (inici + 135)),
            date_labels = "%d/%m", 
            date_breaks = "1 weeks",
            sec.axis = dup_axis(
              name = "Semana",
              labels = scales::date_format("%V")
            )
          ) +
          labs(
            fill = translate(lang, "Subjects:"), 
            x = NULL,
            y = translate(lang, "Number of concurrent activities")
          ) +
          theme(
            plot.title=element_text(size=24),
            legend.position="none",
            axis.text.x = element_text(size = rel(1.25)),
            panel.grid.minor = element_blank()
          )
        
        gg2 <- gg + 
          facet_wrap(c("full_name"), ncol = 1)
        gg <- gg + 
          labs(
            title=paste0(
              translate(lang, "Total number of activities"),": ",
              nrow(lli),"\n",
              translate(lang, "Average daily activities"),": ",
              load," (", load_prop,"%)\n",
              translate(lang, "Percentage of overlapping (days with more than 1 activity)"),": ",
              load2, " (", load2_prop,"%)\n"
            )
          )
        
        #print("numero_asignatures")
        #print(numero_asignatures(selectedList))
        
        (gg2 / gg) + plot_layout(heights = c(numero_asignatures(selectedList), 2))
        
      }, height = 800)
      
      
      # Taula de l'expedient de l'estudiant -----------------------
      output$expedient=renderTable({
        # JULIA 19/10/2023 afegir nom assignatura, capçalera amb idioma
        dadesGrauOUT = dadesGrau()
        if (!is.null(dadesGrauOUT$dadesEST)) {
          dadesGrauOUT$dadesEST=merge(dadesGrauOUT$dadesEST,dadesGrauOUT$noms,'ass')
          temp=dadesGrauOUT$dadesEST[,c('sem',paste0('name_',lang),'nota')]
          temp=temp[order(temp$sem),]
          # JULIÀ 07/11/2023 notas
          temp[temp$nota=="M",'nota']=translate(lang, "With Honors")
          temp[temp$nota=="EX",'nota']=translate(lang, "Excellent")
          temp[temp$nota=="NO",'nota']=translate(lang, "Good")
          temp[temp$nota=="A",'nota']=translate(lang, "Satisfactory")
          temp[temp$nota=="SU",'nota']=translate(lang, "Fail")
          temp[temp$nota=="NP",'nota']=translate(lang, "Withdrawal")
          # falta traduir les convalidacions
          temp[temp$nota=="Reconeguda",'nota']=translate(lang, "Transfer")
          # traduir els noms dels camps 
          colnames(temp)=c(translate(lang, "Semester"), translate(lang, "Subject"), translate(lang, "Mark"))
          # mostrar la taula tal qual
          temp
        }
      }, bordered=T, digits=0)
      
      
      # Resum de la matricula ------------------------------------
      output$uiEnrollment=renderUI({
        if(step$val < 4) {
          return()
        }
        ns <- session$ns
        dadesGrau <- dadesGrau()
        asignatures <- tibble(
          ass = dadesGrau$noms[["ass"]],
          # JULIA 30/12/2022 cambiar noms por full_name
          full_name = dadesGrau$noms[[paste0("name_",lang)]]
        ) %>% 
          left_join(dadesGrau$dadesASS, by = "ass") %>% 
          mutate(full_name = paste0(full_name," (",abrv,")"))
        descartaStr <- asignatures %>% 
          filter(ass %in% descartaList$ass) %>% 
          pull(full_name)
        recomanaStr <- asignatures %>% 
          filter(ass %in% recomendedList$ass) %>% 
          pull(full_name)
        seleccioStr <- asignatures %>% 
          filter(ass %in% selectedList$ass) %>% 
          pull(full_name)
        tagList(
          h2(translate(lang, "¡Gracias por utilizar Visual Enrollment!")),
          h3(translate(lang, "Tus preferencias y selección para la siguiente matricula")),
          tags$ol(
            tags$li(class="step0", translate(lang, "Discard"), p(paste(descartaStr, collapse = ", "))),
            tags$li(class="step1", translate(lang, "Preferences"),
                    div(
                      translate(lang,"Difficulty:"), paste0(input$dificultat, "/5"),
                      br(),
                      translate(lang, "Popularity:"),  paste0(input$popularitat, "/5"),
                      br(),
                      translate(lang, "Previous requirements:"),  paste0(input$requisit, "/5"),
                      br(),
                      translate(lang, "Overlaps between deadlines:"),  paste0(input$overlap, "/5"),
                      br(),
                      br(),
                    )
            ),
            tags$li(class="step2", translate(lang, "Recommendations"), p(paste(recomanaStr, collapse = ", "))),
            tags$li(class="step3", translate(lang, "Selection"), p(paste(seleccioStr, collapse = ", "))),
          ),
          actionButton(ns("screenshot"), translate(lang, "Download enrolment")),
          NULL
        )
      })
      
      
      # Cercar per tipologia o nom ----------------------------------------------------------------
      
      output$uiTipologia=renderUI({
        req(input$grau)
        ns <- session$ns
        dadesGrau = dadesGrau()
        tipologia <- dadesGrau$tipologia[[paste0("tipologia_",lang)]]
        tagList(
          selectInput(
            ns("tipologia"),
            translate(lang, "Highlight each type of subject"),
            choices = c(translate(lang, "All"), tipologia)
          )
        )
      })
      
      output$uiBuscar=renderUI({
        req(input$grau)
        ns <- session$ns
        dadesGrau = dadesGrau()
        # JULIA 10/01/2023 cambiar noms por full_name
        full_name <- dadesGrau$noms[[paste0("name_",lang)]]
        tagList(
          textInput(
            ns("buscar"),
            translate(lang, "Search subject"),
          )
        )
      })
      
      
      # Selectors dinamics admin ----------------------------------------------------------------
      
      output$uiGrau=renderUI({
        ns <- session$ns
        selectInput(
          ns("grau"),
          translate(lang, "Choose degree"),
          choices=setNames(
            c(
              "INFORMATICA",
              "DATASCIENCE"
            ),
            c(
              translate(lang,"Computer science degree"),
              translate(lang,"Applied data science")
            )
          ),
          selected=1
        )
      })
      
      
      output$uiSem=renderUI({
        ns <- session$ns
        selectInput(
          ns("sem1o2"),
          translate(lang, "Choose a semester:"),
          choices=c(1,2),
          selected=1
        )
      })
      
      # !!! JULIA 24/12/2022 filtrar los idps por tutor
      # falta decidir los idps por tutor y la clave de cada uno
      output$uiEst=renderUI({
        if(!is.null(input$grau)) {
          dadesGrau = dadesGrau()
          # JULIÀ 17/10/2023 per defecte la llista d'idps disponibles
          idps = dadesGrau$idps
          # JULIÀ 24/10/2023 de moment treiem lo dels tutors
          #claveTutor = claveTutor()
          #if (claveTutor=="1A2B3C4D5E6F") {
          #  idps = c("XXX")
          #}
          #if (claveTutor=="DEMOECTEL2023") {
          #  idps = c("00000000000000000000000000000000")
          #}
          
          ns <- session$ns
          selectInput(
            ns("idp"),
            translate(lang, "Choose a student:"),
            choices = idps,
            selected = input$idp
          )
        } else{
          NULL
        }
      })
      
      # Events: Click mapa -----------------------------------------------------------------
      
      observeEvent(input$graf_click, {
        #print("input$graf_click")
        if(!is.null(input$graf_click)) clicked <- asignatura_posicio(input$graf_click)
        if(!is.null(clicked)) clickedList$ass <- unique(c(clicked$ass, clickedList$ass))
        # JULIA 18/12/2022 afegir protecció 
        if (is.null(clicked)) return(NULL)
        #print(step$val)
        if(step$val == 0) {
          descartable <- !(clicked$ass %in% selectedList$ass) &&
            clicked$nota %in% c("SU", "NP", translate(lang, "Fail"), translate(lang, "Pending"), translate(lang, "Discarded"))
          if(descartable) { 
            if(length(descartaList$ass)>0 && clickedList$ass[[1]] %in% descartaList$ass) {
              # print(paste0("input$graf_click desconvalida clicked ",clickedList$ass[[1]]))
              descartaList$ass <- descartaList$ass[ !descartaList$ass %in% clickedList$ass[[1]] ]
            } else {
              # print(paste0("input$graf_click convalida clicked ",clickedList$ass[[1]]))
              descartaList$ass <- unique(c(clickedList$ass[[1]], descartaList$ass))
            }
          }
        }
        if(step$val > 1) {
          matriculable <- step$val != 0 && !(clicked$ass %in% descartaList$ass) 
          matriculable <- matriculable & (clicked$nota %in% c("SU", "NP", translate(lang, "Fail"), translate(lang, "Pending"), translate(lang, "Selected")))
          matriculable <- matriculable | str_detect(clicked$nota, "^(R1|R2|R3|R4|R5|R6)")
          
          if(matriculable){
            step$val <- 3
            session$sendCustomMessage(type = "steps",  message = paste0("step",step$val))
            session$sendCustomMessage(type = "show", message = ".widgets_cal")
            session$sendCustomMessage(type = "show", message = ".workload_asignatures")
            session$sendCustomMessage(type = "show", message = ".download_asignatures")
            session$sendCustomMessage(type = "show", message = ".cal_asignatures")
            if(length(selectedList$ass)>0 && clickedList$ass[[1]] %in% selectedList$ass) {
              #print(paste0("input$graf_click deselecciona clicked ",clickedList$ass[[1]]))
              selectedList$ass <- selectedList$ass[ !selectedList$ass %in% clickedList$ass[[1]] ]
            } else {
              #print(paste0("input$graf_click selecciona clicked ",clickedList$ass[[1]]))
              if(length(na.omit(selectedList$ass))==6){
                mostra_avis_traduit(lang, "Remove one of your selected subjects before adding a new subject.")
                return(NULL)
              }
              selectedList$ass <- unique(c(clickedList$ass[[1]], selectedList$ass))[1:6]
            }
          }
        }
      })
      
      
      # Events: Recommend -----------------------------------------------------------------
      
      observeEvent(input$recommend, {
        #print("recommend")
        dadesGrau = dadesGrau()
        ASSTFM = dadesGrau$ASSTFM
        step$val <- 2
        session$sendCustomMessage(type = "steps",  message = paste0("step",step$val))
        matriculables <- as_tibble(qQ) %>% 
          filter(nota %in% c("SU", "NP", translate(lang, "Fail"), translate(lang, "Pending"), translate(lang, "Selected")) | str_detect(nota, "^(R1|R2|R3|R4|R5|R6)"))
        
        # print("matriculables")
        # print(as_tibble(qQ))
        
        # recomendador basado en distancias
        if (input$recommender==translate(lang,"Distance")) {
          suspeses <- matriculables  %>% filter(nota %in% c("SU","NP", translate(lang, "Fail")))
          # JULIA: 03/10/2023
          #superades <- as_tibble(qQ) %>% filter(nota %in% c("A","NO","EX","M"))
          superades <- as_tibble(qQ) %>% filter(nota %in% c("A","NO","EX","M", translate(lang, "Pass")))	  
          convalidades <- as_tibble(qQ) %>% filter(nota %in% c(translate(lang, "Transfer")))
          tfm_df <- as_tibble(qQ) %>% filter(ass %in% c(ASSTFM))
          # JULIA 27/01/2023 corregir las asignaturas usadas para calcular distancias 
          #b_set <- case_when(
          #  length(superades$ass)>0 ~ list(superades),
          #  length(convalidades$ass)>0 ~ list(convalidades),
          #  TRUE ~ list(tfm_df)
          #)
          #b_set <- b_set[[1]]
          b_set <- rbind(superades, convalidades)
          # print("b_set")
          # print(b_set)
          b_n <- length(b_set$ass)
          dist = tibble(ass = matriculables$ass, d = 0)
          #print("recommender")
          # para cada asignatura matriculable
          for(A in matriculables$ass){
            dist_a = 0
            # acumular la distancia a todas las asignaturas ya superadas o convalidadas
            a_x <- matriculables[matriculables$ass==A,'V1']
            a_y <- matriculables[matriculables$ass==A,'V2']
            for(B in b_set$ass){
              #              a_x <- matriculables %>% filter(ass == A) %>% pull(V1)
              #              a_y <- matriculables %>% filter(ass == A) %>% pull(V2)
              #              b_x <- b_set %>% filter(ass == B) %>% pull(V1)
              #              b_y <- b_set %>% filter(ass == B) %>% pull(V2)
              b_x <- b_set[b_set$ass==B,'V1']
              b_y <- b_set[b_set$ass==B,'V2']
              
              # JULIA 18/11/2023 fòrmula errònia!!!
              d=sqrt((a_x-b_x)^2+(a_y-b_y)^2)
              
              # si se trata de una asignatura suspendida, "forzar" que sea
              # más probable recomendarla reduciendo la distancia
              if(A %in% suspeses){
                d=d/input$distancia_suspeses
              }
              dist_a <- dist_a + d
            }
            # guardar la distancia promedio calculada
            dist[dist$ass==A,'d']=dist_a/b_n
            #            dist <- dist %>% 
            #              mutate(d = if_else(ass == A, dist_a/b_n, d))
          }
          
          if(length(superades$ass)>0 | length(convalidades$ass)>0){
            arranged_dist <- dist %>% arrange(d)
          }else{
            arranged_dist <- dist %>% arrange(desc(d))
          }
          
          recomendedList$ass <- arranged_dist %>% 
            head(n = 6) %>%
            pull(ass)
          # print("dist")
          # print(dist %>% arrange(d))
        }
        
        # recomendador aleatorio
        if (input$recommender==translate(lang,"Random")) {
          recomendedList$ass <-
            matriculables %>%
            sample_n(6) %>%
            pull(ass)
        }
        
        # mostra_avis_traduit(lang, "The system has marked in shades of green the ranking of the 6 most recommended subjects for your enrollment.")
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
      })
      
      # Events: Previous, next, buscar, etc -----------------------------------------------------------------
      
      observeEvent(input$previous1, {
        clickedList$ass = character()
        descartaList$ass = character()
        selectedList$ass = character()
        recomendedList$ass = character()
        step$val <- 0
        session$sendCustomMessage(type = "steps",  message = paste0("step",step$val))
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
      })
      
      observeEvent(input$previous2, {
        selectedList$ass = character()
        recomendedList$ass = character()
        step$val <- 1
        session$sendCustomMessage(type = "steps",  message = paste0("step",step$val))
        session$sendCustomMessage(type = "show",  message = ".step-sliders")
        session$sendCustomMessage(type = "show",  message = ".step-recommend")
        session$sendCustomMessage(type = "hide",  message = ".selecciona_asignatures")
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
      })
      
      observeEvent(input$next_button, {
        step$val <- 1
        session$sendCustomMessage(type = "steps",  message = paste0("step",step$val))
      })
      
      observeEvent(input$tipologia, {
        updateTextInput(session, "buscar", value = "")
      })
      
      
      # Events: Descarregar i screenshot-----------------------------------------------------------------
      
      observeEvent(input$descargar, {
        step$val <- 4
        session$sendCustomMessage(type = "hide",  message = ".tabbable")
        session$sendCustomMessage(type = "hide",  message = ".well")
        session$sendCustomMessage(type = "hide",  message = ".steps")
        session$sendCustomMessage(type = "hide",  message = ".graf")
        session$sendCustomMessage(type = "hide",  message = ".cal_asignatures")
        session$sendCustomMessage(type = "hide", message = ".workload_asignatures")
        session$sendCustomMessage(type = "hide", message = ".download_asignatures")
      })
      
      observeEvent(input$screenshot, {
        shinyscreenshot::screenshot(filename = "visualenrollment")
      })
      
    }
  )
}
