###Descripcion de la carpeta

analisis_vep_snps_seri2.R es un script de R que

1. Lee un INPUT en formato .txt de la anotacion de variantes SNP extraidas 
con la herramienta nf-100GMX-variant-summarizer
2. Filtra las variantes con mutaciones expresables no-sinonimas
3. Filtra segun los transcritos canonicos y detecta los que tienen un acceso directo del transcrito en UNIPROT

#Son 22 genes, de los cuales se filtraron a 7, que cuentan con las siguientes
#características: 
#1. Son SNPs expresables, variantes missenss, transcrito canonico, y tienen un
 acceso del trascrito en UNIPROT para tener la secuencia exacta

INPUT

anotacion2_Seri.txt

OUTPUTS

#Los rsids rescatados

rsid_Seri_anotacion2.csv

#Los genes missense con sus anotaciones

genes_missense_Seri.csV

#Los genes missense, en su transcrito canonico

genes_missense_canonico.csv

##Transcritos canonicos que tienen reporte de entrada de la isoforma

genes_missense_isoforma.csv

#Lista de las variantes missense

lista_variantes_missense.csv

#Lista de las entradas de SWISSPROT

entradas_swissprot.csv

#Lista de las entradas de Uniprot

entradas_uniprot.csv
