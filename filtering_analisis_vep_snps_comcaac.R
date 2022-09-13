#script para filtrar vcf con anotaciones de mutaciones. 

#############ESTE ES EL PRIMER INPUT#####################
#Paulina

#ESPECIFICAMENTE ESTE ES EL DE LOS SNPs

#Libraries


library("dplyr")
library("vroom")
library("stringr")

#Reading file

vep_snps_seri <- vroom(file = "anotacion2_Seri.txt")


#Eliminar columnas que no sirven, y filtrar los codificantes

snps_seri_cols <- vep_snps_seri %>% 
  select(SYMBOL, Gene, Feature, BIOTYPE, EXON, 
         Consequence, cDNA_position, Protein_position, Amino_acids, 
         Codons, Existing_variation, CANONICAL, 
         SWISSPROT, UNIPROT_ISOFORM, ENSP, GIVEN_REF, USED_REF, 
         DOMAINS, AF, AFR_AF, AMR_AF, EAS_AF, EUR_AF, SOURCE)

#Quiero tener una lista unica de los rsIDs que encontro la herramienta de anotacion

seri_snps_rsid <- snps_seri_cols %>% 
  filter(str_detect(Existing_variation, "rs*")) %>% #para que sean solo los que tienen registro rsID
  select(Existing_variation) %>% 
  distinct()   #para que se eliminen los que son iguales

#Filtrando todos los genes que son missense, y que por INPUT son SNPs

genes_missense <- snps_seri_cols %>% 
  filter(Consequence %in% "missense_variant") 

#Quiero una lista de los genes missesnse SNPs

missense_snp <- genes_missense %>% 
  select(SYMBOL) %>% 
  unique()

#Filtrado para obtener transcritos canonicos

genes_missense_canonico <- genes_missense %>% ##pongo %in% por si tiene más de una consequence
  filter(CANONICAL == "YES") #solamente quiero los transcritos canonicos

#Para tener una lista de los genes missense con transcritos canonicos

missense_canonico <- genes_missense_canonico %>% 
  select(SYMBOL) %>% 
  unique()
  
#Filtrado para tener los transcritos canonicos que tienen reporte de entrada de la isoforma

genes_missense_isoformas <- genes_missense_canonico %>%  
  filter(!str_detect(UNIPROT_ISOFORM, "^-")) #que tengan una anotacion en la isoforma para 
  #conseguir la secuencia exacta
  
#Para saber cuantas variantes SNPs missense tengo en total

lista_variantes_missense <- genes_missense %>% 
  select(SYMBOL) %>% 
  unique()

#Quiero una lista de las variantes de UNIPROT y SWISSPROT

validacion <- c("Validado", "Validado", "Validado", "Validado","Validado","Validado", "Validado")

entradas_proteina <- genes_missense_isoformas %>% 
  select(SYMBOL, Protein_position, Amino_acids,
         UNIPROT_ISOFORM, SWISSPROT) %>% 
  mutate(validacion)

#Son 22 genes, de los cuales se filtraron a 7, que cuentan con las siguientes
#características: 
#1. Son SNPs
#2. Son variantes missense, por lo que son expresables
#3. Son el transcrito canonico
#4. Tienen un acceso del trascrito en UNIPROT, para tener la secuencia exacta

#OUTPUTS


#Los rsids rescatados

write.csv(seri_snps_rsid, 
          file = "rsid_Seri_anotacion2.csv")

#Los genes missense con sus anotaciones

write.csv(genes_missense, 
          file = "genes_missense_Seri.csv")

#Los genes missense, en su transcrito canonico

write.csv(genes_missense_canonico, 
          file = "genes_missense_canonico.csv")

##Transcritos canonicos que tienen reporte de entrada de la isoforma

write.csv(genes_missense_isoformas, 
          file = "genes_missense_isoforma.csv")

#Lista de las variantes missense

write.csv(lista_variantes_missense, 
          file = "lista_variantes_missense.csv")

#Lista de las entradas de Uniprot y SWissprot


write.csv(entradas_proteina, 
          file = "entradas_proteinas.csv",
          row.names = F)


########################comparando con los anteriores

a <- lista_variantes_missense$SYMBOL  ###Los 22 missense actuales

b <- c("AGRN", "PLA2G2F", "PUM1", "MCOLN3",
       "IL6R", "OR2T29", "DNHD1", "CEP290",
       "TRAV8-7", "HCN2", "PEAK3", "C19orf47", "ERCC2",
       "SIGLEC14", "ATAD2B", "ZSWIM2", "SLC37A1", "ATP2B2", 
       "LMOD3", "INPP4B", "MAGI2", "PDLIM2", "MROH5", "NXF5")  ##Los 24 missense anteriores


# Múltiples opciones
intersect(a, b)
b[b %in% a]
a[a %in% b]

#Elementos únicos de a
setdiff(a, b)
a[!a %in% b]
a[b %in% a]

#Elementos únicos de b

setdiff(b, a)
b[!b %in% a]
b[a %in% b]

