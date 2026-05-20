# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc


dados_sinasc <- read.csv("SINASC_2015.csv", sep = ";")
str(dados_sinasc)




# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK



dados_sinasc_1 =  dados_sinasc[,  c(1,4,5,6,7,12,13,14,15,19,21,22,23,24,35,38,44,46,48,59,60,61)]


# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 



UF = substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2)
dados_sinasc_2 = dados_sinasc_1[UF == "21",]
nrow(dados_sinasc_2)
write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)


# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

# Exportar o arquivo com o nome dados_sinasc_2.csv


# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK



table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)


# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA



dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] <- NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] <- NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] <- NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] <- NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] <- NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 9 | dados_sinasc_2$SEXO == 0] <- NA 
dados_sinasc_2$RACACOR[dados_sinasc_2$RACACOR == 9] <- NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] <- NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] <- NA
dados_sinasc_2$RACACORMAE[dados_sinasc_2$RACACORMAE == 9] <- NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] <- NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] <- NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] <- NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] <- NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] <- NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] <- NA
dados_sinasc_2$SEMAGESTAC[dados_sinasc_2$SEMAGESTAC == 99] <- NA


# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")



dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros"))

dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União consensual"))

dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))

dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Três ou mais"))

dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesáreo"))

dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))

dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))

dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I", "Fundamental II", "Médio", "Superior incompleto", "Superior completo"))

dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálica", "Pélvica ou podálica", "Transversa"))

dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5

# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator



# 1. Categorizando PESO, IDADE e APGAR5
dados_sinasc_2$F_PESO <- cut(dados_sinasc_2$PESO, breaks = c(-Inf, 2500, 4000, Inf), labels = c("Baixo peso", "Peso normal", "Macrossomia"), right = FALSE)

dados_sinasc_2$F_IDADE <- cut(dados_sinasc_2$IDADEMAE, breaks = c(-Inf, 15, 20, 25, 30, 35, 40, 45, 50, Inf), labels = c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"), right = FALSE)

dados_sinasc_2$F_APGAR5 <- cut(dados_sinasc_2$APGAR5, breaks = c(-Inf, 7, Inf), labels = c("Baixo", "Normal"), right = FALSE)

# 2. Criando variável de Peregrinação (Mãe teve o bebê em um município diferente de onde mora)
dados_sinasc_2$PERIG <- ifelse(dados_sinasc_2$CODMUNRES == dados_sinasc_2$CODMUNNASC, "Não", "Sim")

# 3. Criando variável agrupada para Estado Civil (Com ou Sem companheiro)
dados_sinasc_2$F_ESTCIV <- ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União consensual"), "Com companheiro", 
                                  ifelse(is.na(dados_sinasc_2$ESTCIVMAE), NA, "Sem companheiro"))


# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.



# Tarefa 9. Obter as frequências das categorias das variáveis qualitativas e medidas descritivas de variáveis quantitativas e salvar os resultados em novas variáveis.
# Exemplo: freq_SEXO = table(dados_sinasc_2$SEXO)   media_peso = mean(dados_sinasc_2$PESO)
# Medidas descritivas a serem calculadas para variáveis QUANTITATIVAS: P25, P50, P75, média e desvio-padrão. Atenção: usar na.rm = TRUE, quando necessário.


# Tarefa 10. Criar as colunas do novo banco de dados (de nome SINASC_UF.csv Exemplo: SINASC_RJ.csv) com base nas análises prévias, devendo as variáveis estar na ordem indicada abaixo
# ATENÇÃO aos nomes das variáveis e ordem das colunas
# 1. ANO: 2015  2. UFR (Estado de residência)   3. TN (total de nascimentos)   4. TNRC (total de nascimentos com registros completos, ou seja, sem NA em todas as variáveis do banco de dados)
# 5. TGI_15 (total de gestantes com idade inferior a 15 anos - F_IDADE)   6. TGI_15_19 (total de gestantes com idade >=15 e <=19 anos)
# 7: TGI_20_24 (total de gestantes com idade >=20 e <=24 anos)   8. TGI_25_29 (total de gestantes com idade >=25 e <=29 anos)
# 9: TGI_30_34 (total de gestantes com idade >=30 e <=34 anos)   10. TGI_35_39 (total de gestantes com idade >=35 e <=39 anos)
# 11: TGI_40_44 (total de gestantes com idade >=40 e <=44 anos)  12. TGI_45_49 (total de gestantes com idade >=45 e <=49 anos)
# 13: TGI_50 (total de gestantes com idade >=50)   14: TGIF (total de gestantes em idade fértil, idade >=15 e <=49 anos)
# 15: IM_P25 (percentil 25 da idade materna - IDADEMAE) 16: IM_P50 (percentil 50 da idade materna)   17: IM_P75 (percentil 75 da idade materna)
# 18. IM_MD (idade média materna)   19: IM_DP (desvio-padrão da idade materna)
# 20. EM_S (total de gestantes sem escolaridade, ESCMAE2010=0)   21: EM_FI (total de gestantes com escolaridade Fundamental I)
# 22. EM_FII (total de gestantes com escolaridade Fundamental II)   23. EM_M (total de gestantes com escolaridade Médio)   
# 24. EM_SI (total de gestantes com escolaridade Superior Incompleto)   25. EM_SC (total de gestantes com escolaridade Superior Completo) 
# 26. TGRC_B (total de gestantes da raça/cor branca - RACACORMAE)   27. TGRC_PT (total de gestantes da raça/cor preta)
# 28. TGRC_A (total de gestantes da raça/cor amarela)   29. TGRC_PD (total de gestantes da raça/cor parda)
# 30. TGRC_I (total de gestantes da raça/cor indígena)
# 31. TGSC (total de gestantes sem companheiro - ESTCIV)   32. TGCC (total de gestantes com companheiro)
# 33. TGPRI (total de gestantes primíparas - PARIDADE)     34. TGNPRI (total de gestantes não primíparas)
# 35. TGU (total de gestações única)   36. TGG (total de gestações gemelares)   37. TGD_22 (total de gestações com duração inferior a 22 semanas - GESTACAO)
# 38. TGD_22_27 (total de gestações com duração da gestação >=22 e <=27)   39. TGD_28_31 (total de gestações com duração da gestação >=28 e <=31)
# 40. TGD_32_36 (total de gestações com duração da gestação >=32 e <=36)   41. TGD_37_41 (total de gestações com duração da gestação >=37 e <=41)
# 42. TGD_42 (total de gestações com duração da gestação >=42)   43. TGD_PRT (total de gestações pre-termo, duração < 37 semanas)
# 44. TGD_AT (total de gestações a termo, duração >=37 e <=41)   45. TGD_PST  (total de gestações pós termo, duração >=42) 
# 46. DG_P25 (percentil 25 da duração da gestação - SEMAGESTAC)  47. DG_P50 (percentil 50 da duração da gestação)   
# 48. DG_P75 (percentil 75 da duração da gestação)   49. DG_MD (idade média da duração da gestação)   50. DG_DP (desvio-padrão da duração da gestação)
# 51. TKC_NR (total de consultas de pre-natal não realizado - KOTELCHUCK)   52. TKC_ID (total de consultas de pre-natal inadequado)
# 53. TKC_IT (total de consultas de pre-natal intermediário)   54. TKC_AD (total de consultas de pre-natal adequado)  
# 55. TKC_MAD (total de consultas de pre-natal mais que adequado)   56. TGPRG_S (total de gestantes que peregrinaram)  
# 57. TGPRG_N (total de gestantes que não peregrinaram)    58. TPV (total de partos vaginais)   59. TPC (total de partos cesáreos) 
# 60. TRAP_C (total de recém-nascidos na posição cefálica - TPAPRESENT)   61. TRAP_P (total de recém-nascidos na posição pélvica ou podálica)
# 62. TRAP_T (total de recém-nascidos na posição transversa)  63. TGROB_1 (total de gestantes do grupo de Robson 1 - TPROBSON)
# 64. TGROB_2 (total de gestantes do grupo de Robson 2)   65. TGROB_3 (total de gestantes do grupo de Robson 3)
# 66. TGROB_4 (total de gestantes do grupo de Robson 4)   67. TGROB_5 (total de gestantes do grupo de Robson 5)
# 68. TGROB_6 (total de gestantes do grupo de Robson 6)   69. TGROB_7 (total de gestantes do grupo de Robson 7)
# 70. TGROB_8 (total de gestantes do grupo de Robson 8)   71. TGROB_9 (total de gestantes do grupo de Robson 9)
# 72. TGROB_10 (total de gestantes do grupo de Robson 10)   
# 73. TNLOC_H (total de nascimentos em hospital)   74. TNLOC_ES (total de nascimentos em outros estabelecimentos de saúde)
# 75. TNLOC_D (total de nascimentos em domicílio)  76. TNLOC_O (total de nascimentos em outros locais) 
# 77. TNLOC_AI (total de nascimentos em aldeias indígenas)   
# 78. TRRC_B (total de recém-nascidos da raça/cor branca - RACACOR)   79. TRRC_PT (total de recém-nascidos da raça/cor preta)
# 80. TRRC_A (total de recém-nascidos da raça/cor amarela)   81. TRRC_PD (total de recém-nascidos da raça/cor parda)
# 82. TRRC_I (total de recém-nascidos da raça/cor indígena)  83. TRP_BP (total de recém nascidos com baixo peso - FPESO)
# 84. TRP_N (total de recém nascidos com peso normal)   85. TRP_M (total de recém nascidos com macrossomia)
# 86. PESO_P25 (percentil 25 do peso dos recém-nascidos - PESO)  87. PESO_P50 (percentil 50 do peso dos recém-nascidos)   
# 88. PESO_P75 (percentil 75 do peso dos recém-nascidos)  89. PESO_MD (peso médio dos recém-nascidos)   
# 90. PESO_DP (desvio-padrão dos pesos dos recém-nascidos)    91. TRPIG_P (total de recém-nascidos de GESTAÇÕES ÚNICAS com PIG) 
# 92. TRPIG_A (total de recém-nascidos de GESTAÇÕES ÚNICAS com AIG)   93. TRPIG_G (total de recém-nascidos de GESTAÇÕES ÚNICAS com GIG)
# 94: TRAPG5_B (total de recém-nascidos com Apgar5 baixo, ou seja, < 7)
# 95: TRAPG5_N (total de recém-nascidos com Apgar5 normal, ou seja, >= 7)   96. APG5_MD (Apgar5 médio dos recém-nascidos)   
# 97. APG5_DP (desvio-padrão dos Apgar5 dos recém-nascidos)   98. TRAC (total de recém-nascidos com anomalia congênita - IDANOMAL)
# 99. TRSAC (total de recém-nascidos sem anomalia congênita)


# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv



# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"


tabela_pig <- read.csv("Tabela_PIG_Brasil.csv", sep = ";")
dados_sinasc_2 <- merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC", "SEXO"), all.x = TRUE)
casos_validos <- dados_sinasc_2$GRAVIDEZ == "Única" & 
  !is.na(dados_sinasc_2$PESO) & 
  !is.na(dados_sinasc_2$SEMAGESTAC) & 
  !is.na(dados_sinasc_2$SEXO) &
  !is.na(dados_sinasc_2$PESO_P10) & 
  !is.na(dados_sinasc_2$PESO_P90)
dados_sinasc_2$F_PIG <- ifelse(!casos_validos, NA, # Se não for caso válido, recebe NA
                               ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                      ifelse(dados_sinasc_2$PESO > dados_sinasc_2$PESO_P90, "GIG", "AIG")))
dados_sinasc_2$F_PIG <- factor(dados_sinasc_2$F_PIG, levels = c("PIG", "AIG", "GIG"))


# TAREFAS 9 E 10:
library(dplyr)
calcular_resumo <- function(dados, nivel_info, codigo_ibge) {
  dados %>%
    summarise(
      # 1 a 3: Identificação
      ANO = 2015,
      NIVEL = nivel_info,
      CODMUNRES = codigo_ibge,
      
      # 4 a 6: Informações sobre nascimentos
      TN = n(),
      TNRC = sum(complete.cases(.)),
      TNRCR = sum(complete.cases(select(., 1:22))), # <-- AQUI ESTÁ A CORREÇÃO!
      
      # 7 a 16: Idade da gestante
      TGI_15 = sum(F_IDADE == "<15", na.rm = TRUE),
      TGI_15_19 = sum(F_IDADE == "15-19", na.rm = TRUE),
      TGI_20_24 = sum(F_IDADE == "20-24", na.rm = TRUE),
      TGI_25_29 = sum(F_IDADE == "25-29", na.rm = TRUE),
      TGI_30_34 = sum(F_IDADE == "30-34", na.rm = TRUE),
      TGI_35_39 = sum(F_IDADE == "35-39", na.rm = TRUE),
      TGI_40_44 = sum(F_IDADE == "40-44", na.rm = TRUE),
      TGI_45_49 = sum(F_IDADE == "45-49", na.rm = TRUE),
      TGI_50 = sum(F_IDADE == "50+", na.rm = TRUE),
      TGIF = sum(IDADEMAE >= 15 & IDADEMAE <= 49, na.rm = TRUE),
      
      # 17 a 21: Estatísticas da idade
      IM_P25 = quantile(IDADEMAE, 0.25, na.rm = TRUE),
      IM_P50 = quantile(IDADEMAE, 0.50, na.rm = TRUE),
      IM_P75 = quantile(IDADEMAE, 0.75, na.rm = TRUE),
      IM_MD = mean(IDADEMAE, na.rm = TRUE),
      IM_DP = sd(IDADEMAE, na.rm = TRUE),
      
      # 22 a 27: Escolaridade
      EM_S = sum(ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
      EM_FI = sum(ESCMAE2010 == "Fundamental I", na.rm = TRUE),
      EM_FII = sum(ESCMAE2010 == "Fundamental II", na.rm = TRUE),
      EM_M = sum(ESCMAE2010 == "Médio", na.rm = TRUE),
      EM_SI = sum(ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
      EM_SC = sum(ESCMAE2010 == "Superior completo", na.rm = TRUE),
      
      # 28 a 32: Raça/Cor da Mãe
      TGRC_B = sum(RACACORMAE == "Branca", na.rm = TRUE),
      TGRC_PT = sum(RACACORMAE == "Preta", na.rm = TRUE),
      TGRC_A = sum(RACACORMAE == "Amarela", na.rm = TRUE),
      TGRC_PD = sum(RACACORMAE == "Parda", na.rm = TRUE),
      TGRC_I = sum(RACACORMAE == "Indígena", na.rm = TRUE),
      
      # 33 a 36: Estado Civil e Paridade
      TGSC = sum(F_ESTCIV == "Sem companheiro", na.rm = TRUE),
      TGCC = sum(F_ESTCIV == "Com companheiro", na.rm = TRUE),
      TGPRI = sum(PARIDADE == 1, na.rm = TRUE), 
      TGNPRI = sum(PARIDADE == 0, na.rm = TRUE), 
      
      # 37 a 47: Gestações
      TGU = sum(GRAVIDEZ == "Única", na.rm = TRUE),
      TGG = sum(GRAVIDEZ == "Dupla" | GRAVIDEZ == "Três ou mais", na.rm = TRUE),
      TGD_22 = sum(GESTACAO == "Menos de 22 semanas", na.rm = TRUE),
      TGD_22_27 = sum(GESTACAO == "22 a 27 semanas", na.rm = TRUE),
      TGD_28_31 = sum(GESTACAO == "28 a 31 semanas", na.rm = TRUE),
      TGD_32_36 = sum(GESTACAO == "32 a 36 semanas", na.rm = TRUE),
      TGD_37_41 = sum(GESTACAO == "37 a 41 semanas", na.rm = TRUE),
      TGD_42 = sum(GESTACAO == "42 semanas e mais", na.rm = TRUE),
      TGD_PRT = sum(SEMAGESTAC < 37, na.rm = TRUE),
      TGD_AT = sum(SEMAGESTAC >= 37 & SEMAGESTAC <= 41, na.rm = TRUE),
      TGD_PST = sum(SEMAGESTAC >= 42, na.rm = TRUE),
      
      # 48 a 52: Estatísticas da gestação
      DG_P25 = quantile(SEMAGESTAC, 0.25, na.rm = TRUE),
      DG_P50 = quantile(SEMAGESTAC, 0.50, na.rm = TRUE),
      DG_P75 = quantile(SEMAGESTAC, 0.75, na.rm = TRUE),
      DG_MD = mean(SEMAGESTAC, na.rm = TRUE),
      DG_DP = sd(SEMAGESTAC, na.rm = TRUE),
      
      # 53 a 57: Pré-natal (Kotelchuck)
      TKC_NR = sum(KOTELCHUCK == "Não realizou pré-natal", na.rm = TRUE),
      TKC_ID = sum(KOTELCHUCK == "Inadequado", na.rm = TRUE),
      TKC_IT = sum(KOTELCHUCK == "Intermediário", na.rm = TRUE),
      TKC_AD = sum(KOTELCHUCK == "Adequado", na.rm = TRUE),
      TKC_MAD = sum(KOTELCHUCK == "Mais que adequado", na.rm = TRUE),
      
      # 58 a 64: Parto e Peregrinação
      TGPRG_S = sum(PEREGRINACAO == "Sim", na.rm = TRUE),
      TGPRG_N = sum(PEREGRINACAO == "Não", na.rm = TRUE),
      TPV = sum(PARTO == "Vaginal", na.rm = TRUE),
      TPC = sum(PARTO == "Cesáreo", na.rm = TRUE),
      TRAP_C = sum(TPAPRESENT == "Cefálica", na.rm = TRUE),
      TRAP_P = sum(TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
      TRAP_T = sum(TPAPRESENT == "Transversa", na.rm = TRUE),
      
      # 65 a 74: Grupos de Robson
      TGROB_1 = sum(TPROBSON == 1, na.rm = TRUE),
      TGROB_2 = sum(TPROBSON == 2, na.rm = TRUE),
      TGROB_3 = sum(TPROBSON == 3, na.rm = TRUE),
      TGROB_4 = sum(TPROBSON == 4, na.rm = TRUE),
      TGROB_5 = sum(TPROBSON == 5, na.rm = TRUE),
      TGROB_6 = sum(TPROBSON == 6, na.rm = TRUE),
      TGROB_7 = sum(TPROBSON == 7, na.rm = TRUE),
      TGROB_8 = sum(TPROBSON == 8, na.rm = TRUE),
      TGROB_9 = sum(TPROBSON == 9, na.rm = TRUE),
      TGROB_10 = sum(TPROBSON == 10, na.rm = TRUE),
      
      # 75 a 79: Local de Nascimento
      TNLOC_H = sum(LOCNASC == "Hospital", na.rm = TRUE),
      TNLOC_ES = sum(LOCNASC == "Outros estabelecimentos de saúde", na.rm = TRUE),
      TNLOC_D = sum(LOCNASC == "Domicílio", na.rm = TRUE),
      TNLOC_O = sum(LOCNASC == "Outros", na.rm = TRUE),
      TNLOC_AI = sum(LOCNASC == "Aldeia Indígena", na.rm = TRUE), 
      
      # 80 a 89: Bebê (Sexo, Cor, Peso)
      TRS_M = sum(SEXO == "Masculino", na.rm = TRUE),
      TRS_F = sum(SEXO == "Feminino", na.rm = TRUE),
      TRRC_B = sum(RACACOR == "Branca", na.rm = TRUE),
      TRRC_PT = sum(RACACOR == "Preta", na.rm = TRUE),
      TRRC_A = sum(RACACOR == "Amarela", na.rm = TRUE),
      TRRC_PD = sum(RACACOR == "Parda", na.rm = TRUE),
      TRRC_I = sum(RACACOR == "Indígena", na.rm = TRUE),
      TRP_BP = sum(F_PESO == "Baixo peso", na.rm = TRUE),
      TRP_N = sum(F_PESO == "Peso normal", na.rm = TRUE),
      TRP_M = sum(F_PESO == "Macrossomia", na.rm = TRUE),
      
      # 90 a 97: Estatísticas do Peso e PIG/AIG/GIG
      PESO_P25 = quantile(PESO, 0.25, na.rm = TRUE),
      PESO_P50 = quantile(PESO, 0.50, na.rm = TRUE),
      PESO_P75 = quantile(PESO, 0.75, na.rm = TRUE),
      PESO_MD = mean(PESO, na.rm = TRUE),
      PESO_DP = sd(PESO, na.rm = TRUE),
      TRPIG_P = sum(F_PIG == "PIG", na.rm = TRUE),
      TRPIG_A = sum(F_PIG == "AIG", na.rm = TRUE),
      TRPIG_G = sum(F_PIG == "GIG", na.rm = TRUE),
      
      # 98 a 103: APGAR e Anomalia
      TRAPG5_B = sum(F_APGAR5 == "Baixo", na.rm = TRUE),
      TRAPG5_N = sum(F_APGAR5 == "Normal", na.rm = TRUE),
      APG5_MD = mean(APGAR5, na.rm = TRUE),
      APG5_DP = sd(APGAR5, na.rm = TRUE),
      TRAC = sum(IDANOMAL == "Sim", na.rm = TRUE),
      TRSAC = sum(IDANOMAL == "Não", na.rm = TRUE)
    )
}

# ==========================================
# GERANDO OS DADOS AGREGADOS
# ==========================================

# 1. Resumo do Estado (Maranhão - 21)
resumo_estado <- calcular_resumo(dados_sinasc_2, "UF", 21)

# 2. Resumo por Município
resumo_municipios <- dados_sinasc_2 %>%
  group_by(CODMUNRES) %>%
  do(calcular_resumo(., "MUNICIPIO", .$CODMUNRES[1])) %>%
  ungroup()

# 3. Juntando Estado (linha 1) com Municípios (demais linhas)
SINASC_MA <- bind_rows(resumo_estado, resumo_municipios)

# 4. Exportando o arquivo final
write.csv(SINASC_MA, "SINASC_MA.csv", row.names = FALSE)

# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv
write.csv(SINASC_MA, "SINASC_MA.csv", row.names = FALSE)


# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"
# Fim da Etapa 1



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim
dados_sim <- read.csv("Mortalidade_Geral_2015.csv", sep = ";")
str(dados_sim)
dim(dados_sim)

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, 
# CODMUNRES, TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

dados_sim_1 <- dados_sim[, c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)]

# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de 
# CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF


# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645    
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv


# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"

dados_sim_2 <- subset(dados_sim_1, substr(as.character(CODMUNRES), 1, 2) == "21")
write.csv(dados_sim_2, "dados_sim_2.csv", row.names = FALSE)

# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR,
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

table(dados_sim_2$TIPOBITO, useNA = "ifany")
table(dados_sim_2$SEXO, useNA = "ifany")
table(dados_sim_2$RACACOR, useNA = "ifany")
table(dados_sim_2$TPMORTEOCO, useNA = "ifany")
table(dados_sim_2$OBITOGRAV, useNA = "ifany")
table(dados_sim_2$OBITOPUERP, useNA = "ifany")
table(dados_sim_2$CAUSABAS, useNA = "ifany")
table(dados_sim_2$TPOBITOCOR, useNA = "ifany")
table(dados_sim_2$MORTEPARTO, useNA = "ifany")

# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", 
# geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 9999 para NA

dados_sim_2$TIPOBITO[dados_sim_2$TIPOBITO == 9] <- NA
dados_sim_2$SEXO[dados_sim_2$SEXO == 9] <- NA
dados_sim_2$RACACOR[dados_sim_2$RACACOR == 9] <- NA
dados_sim_2$TPMORTEOCO[dados_sim_2$TPMORTEOCO == 9] <- NA
dados_sim_2$OBITOGRAV[dados_sim_2$OBITOGRAV == 9] <- NA
dados_sim_2$OBITOPUERP[dados_sim_2$OBITOPUERP == 9] <- NA
dados_sim_2$TPOBITOCOR[dados_sim_2$TPOBITOCOR == 9] <- NA
dados_sim_2$MORTEPARTO[dados_sim_2$MORTEPARTO == 9] <- NA
dados_sim_2$IDADE[dados_sim_2$IDADE >= 999] <- NA

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), labels = c("Fetal", "Não fetal")

# TIPOBITO (1: Fetal, 2: Não fetal)
dados_sim_2$TIPOBITO <- factor(dados_sim_2$TIPOBITO, levels = c(1, 2), labels = c("Fetal", "Não fetal"))
# SEXO (1: Masculino, 2: Feminino)
dados_sim_2$SEXO <- factor(dados_sim_2$SEXO, levels = c(1, 2), labels = c("Masculino", "Feminino"))
# RACACOR (1: Branca, 2: Preta, 3: Amarela, 4: Parda, 5: Indígena)
dados_sim_2$RACACOR <- factor(dados_sim_2$RACACOR, levels = c(1, 2, 3, 4, 5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
# TPMORTEOCO (Local de ocorrência)
dados_sim_2$TPMORTEOCO <- factor(dados_sim_2$TPMORTEOCO, levels = c(1, 2, 3, 4, 5), labels = c("Hospital", "Outro estabelecimento saúde", "Domicílio", "Via pública", "Outros"))
# OBITOGRAV, OBITOPUERP e TPOBITOCOR (1: Sim, 2: Não)
dados_sim_2$OBITOGRAV <- factor(dados_sim_2$OBITOGRAV, levels = c(1, 2), labels = c("Sim", "Não"))
dados_sim_2$OBITOPUERP <- factor(dados_sim_2$OBITOPUERP, levels = c(1, 2), labels = c("Sim", "Não"))
dados_sim_2$TPOBITOCOR <- factor(dados_sim_2$TPOBITOCOR, levels = c(1, 2), labels = c("Sim", "Não"))
# MORTEPARTO (1: Antes, 2: Durante, 3: Depois)
dados_sim_2$MORTEPARTO <- factor(dados_sim_2$MORTEPARTO, levels = c(1, 2, 3), labels = c("Antes", "Durante", "Depois"))

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE

library(dplyr)
dados_sim_2 <- dados_sim_2 %>%
  mutate(
    letra_causa = substr(as.character(CAUSABAS), 1, 1),
    # Idade em dias para óbitos neonatais (0-27 dias) [cite: 2, 14]
    idade_dias = case_when(
      substr(IDADE, 1, 1) %in% c("1", "2") ~ 0,
      substr(IDADE, 1, 1) == "3" ~ as.numeric(substr(IDADE, 2, 3)),
      TRUE ~ 999
    ),
    # Idade em anos para óbitos gerais e idade fértil [cite: 2, 14]
    idade_anos = case_when(
      substr(IDADE, 1, 1) == "4" ~ 0,
      substr(IDADE, 1, 1) == "5" ~ as.numeric(substr(IDADE, 2, 3)),
      TRUE ~ 0
    )
  )

# 2. Criando o banco SIM_MA com as 41 variáveis [cite: 2]
SIM_MA <- dados_sim_2 %>%
  group_by(CODMUNRES) %>%
  summarise(
    ANO = 2015,
    NIVEL = "MUNICIPIO",
    
    # Informações Gerais [cite: 2]
    TO = n(),
    TORC = sum(complete.cases(.), na.rm = TRUE),
    TORCR = sum(complete.cases(across(c(TIPOBITO, SEXO, RACACOR, IDADE))), na.rm = TRUE),
    TO_NN = sum(letra_causa %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_N  = sum(!letra_causa %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_CBI = sum(letra_causa %in% c("A", "B"), na.rm = TRUE),
    TO_CB_N = sum(letra_causa %in% c("C", "D"), na.rm = TRUE),
    TO_CB_C = sum(letra_causa == "I", na.rm = TRUE),
    TO_CB_R = sum(letra_causa == "J", na.rm = TRUE),
    TO_CB_O = sum(!letra_causa %in% c("A","B","C","D","I","J","V","W","X","Y"), na.rm = TRUE),
    TO_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TO_F = sum(SEXO == "Feminino", na.rm = TRUE),
    TO_F_IF = sum(SEXO == "Feminino" & idade_anos >= 15 & idade_anos <= 49, na.rm = TRUE),
    
    # Fetais e Neonatais [cite: 2, 9]
    TO_FT = sum(TIPOBITO == "Fetal", na.rm = TRUE),
    TO_NT = sum(TIPOBITO == "Não fetal" & idade_dias <= 27, na.rm = TRUE),
    TO_NT_P = sum(TIPOBITO == "Não fetal" & idade_dias <= 6, na.rm = TRUE),
    TO_NT_T = sum(TIPOBITO == "Não fetal" & idade_dias >= 7 & idade_dias <= 27, na.rm = TRUE),
    TO_PNT = sum(idade_dias >= 28 & idade_anos < 1, na.rm = TRUE),
    TO_MT_G = sum(OBITOGRAV == "Sim", na.rm = TRUE),
    TONT_B  = sum(TIPOBITO == "Não fetal" & idade_dias <= 27 & RACACOR == "Branca", na.rm = TRUE),
    TONT_PT = sum(TIPOBITO == "Não fetal" & idade_dias <= 27 & RACACOR == "Preta", na.rm = TRUE),
    TONT_A  = sum(TIPOBITO == "Não fetal" & idade_dias <= 27 & RACACOR == "Amarela", na.rm = TRUE),
    TONT_PD = sum(TIPOBITO == "Não fetal" & idade_dias <= 27 & RACACOR == "Parda", na.rm = TRUE),
    TONT_I  = sum(TIPOBITO == "Não fetal" & idade_dias <= 27 & RACACOR == "Indígena", na.rm = TRUE),
    
    # Informações Maternas [cite: 2, 33, 38]
    TO_MT = sum(OBITOGRAV == "Sim" | OBITOPUERP == "Sim", na.rm = TRUE),
    TO_MT_DG = sum(OBITOGRAV == "Sim", na.rm = TRUE),
    TO_MT_PT = sum(MORTEPARTO == "Durante", na.rm = TRUE),
    TO_MT_AB = sum(letra_causa == "O" & (as.numeric(substr(CAUSABAS, 2, 4)) %in% 3:7), na.rm = TRUE),
    TO_MT_42 = sum(OBITOPUERP == "Sim", na.rm = TRUE),
    TO_MT_43 = sum(TPOBITOCOR == "Sim", na.rm = TRUE),
    TO_MT_P = sum(OBITOGRAV == "Sim" | OBITOPUERP == "Sim", na.rm = TRUE),
    TO_MT_P_I = sum((OBITOGRAV == "Sim" | OBITOPUERP == "Sim") & idade_anos >= 15 & idade_anos <= 49, na.rm = TRUE),
    TO_MT_P_ES = sum((OBITOGRAV == "Sim" | OBITOPUERP == "Sim") & ESC2010 == 0, na.rm = TRUE),
    TO_MT_P_EFI = sum((OBITOGRAV == "Sim" | OBITOPUERP == "Sim") & ESC2010 == 1, na.rm = TRUE),
    TO_MT_P_EFII = sum((OBITOGRAV == "Sim" | OBITOPUERP == "Sim") & ESC2010 == 2, na.rm = TRUE),
    TO_MT_P_EM = sum((OBITOGRAV == "Sim" | OBITOPUERP == "Sim") & ESC2010 == 3, na.rm = TRUE),
    TO_MT_P_ESI = sum((OBITOGRAV == "Sim" | OBITOPUERP == "Sim") & ESC2010 == 4, na.rm = TRUE),
    TO_MT_P_ESC = sum((OBITOGRAV == "Sim" | OBITOPUERP == "Sim") & ESC2010 == 5, na.rm = TRUE)
  )

# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv
write.csv(SIM_MA, "SIM_MA.csv", row.names = FALSE)
# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main

#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
# 1. população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv  
# 2. população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv  
# 3. população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv
# 4. população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv

# A partir dos arquivos acima gere o banco de dados de nome SIDRA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPRE_T
# 5 POPRC_T
# 6 POPRC_M
# 7 POPRC_F
# 8 POPRC_15
# 9 POPRC_15_49
# 10 POPRC_50
# 11 POPRC_F_15
# 12 POPRC_F_15_49
# 13 POPRC_F_50

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

library(dplyr)

# 1. Leitura dos 4 arquivos
pop_est_2015 <- read.csv("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv", sep = ";")
pop_censo_sexo <- read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv", sep = ";")

# Lendo as tabelas de faixa etária (Estado e Municípios separados)
pop_censo_idade_uf <- read.csv("população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv", sep = ";")
pop_censo_idade_mun <- read.csv("população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv", sep = ";")

# 2. Unindo os dados de idade do Estado com os dos Municípios
pop_censo_idade_total <- bind_rows(
  pop_censo_idade_uf %>% select(CODMUNRES, F_IDADE, POP, POPF),
  pop_censo_idade_mun %>% select(CODMUNRES, F_IDADE, POP, POPF)
)

# 3. Agrupando as faixas etárias e somando
idade_agrupada <- pop_censo_idade_total %>%
  mutate(
    faixa = case_when(
      F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos") ~ "<15",
      F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", 
                     "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos") ~ "15_49",
      TRUE ~ "50+"
    )
  ) %>%
  group_by(CODMUNRES) %>%
  summarise(
    POPRC_15 = sum(POP[faixa == "<15"], na.rm = TRUE),
    POPRC_15_49 = sum(POP[faixa == "15_49"], na.rm = TRUE),
    POPRC_50 = sum(POP[faixa == "50+"], na.rm = TRUE),
    POPRC_F_15 = sum(POPF[faixa == "<15"], na.rm = TRUE),
    POPRC_F_15_49 = sum(POPF[faixa == "15_49"], na.rm = TRUE),
    POPRC_F_50 = sum(POPF[faixa == "50+"], na.rm = TRUE)
  )

# 4. Construindo o banco de dados SIDRA_UF (com as 13 variáveis)
SIDRA_UF <- pop_est_2015 %>%
  select(CODMUNRES, POPRE_T) %>%
  left_join(
    pop_censo_sexo %>% select(CODMUNRES, POPRC_T, POPRC_M, POPRC_F),
    by = "CODMUNRES"
  ) %>%
  left_join(idade_agrupada, by = "CODMUNRES") %>%
  mutate(
    ANO = 2015,
    NIVEL = ifelse(nchar(as.character(CODMUNRES)) == 2, "UF", "MUNICIPIO")
  ) %>%
  select(ANO, NIVEL, CODMUNRES, POPRE_T, POPRC_T, POPRC_M, POPRC_F, 
         POPRC_15, POPRC_15_49, POPRC_50, POPRC_F_15, POPRC_F_15_49, POPRC_F_50) %>%
  filter(!is.na(CODMUNRES))

# 5. Exportar o arquivo
write.csv(SIDRA_UF, "SIDRA_MA.csv", row.names = FALSE)


#####################################################################################################
# ETAPA 4: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos das Tarefa 1 a 3  abaixo

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
# 1. população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv  
# 2. população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv  
# 3. população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv
# 4. população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv

# A partir dos arquivos acima gere o banco de dados de nome SIDRA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPRE_T
# 5 POPRC_T
# 6 POPRC_M
# 7 POPRC_F
# 8 POPRC_15
# 9 POPRC_15_49
# 10 POPRC_50
# 11 POPRC_F_15
# 12 POPRC_F_15_49
# 13 POPRC_F_50

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

# Tarefa 2: Acesso aos bancos de dados do SINISA e obtenção da informação
# Escreva os comandos da Tarefa 2 estando na branch OUTROS# Leia o arquivo agua e esgoto - município - 2015.csv 
# A partir do arquivo acima gere o banco de dados de nome SINISA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPR_RA
# 5 POPR_RE

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SINISA"


# Tarefa 3: Acesso aos bancos de dados do ATLAS  e obtenção da informação
# Escreva os comandos da Tarefa 3 estando na branch OUTROS
# Leia os arquivos:
# 1. códigos dos municípios - 2010.csv      
# 2. IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# 3. IDHM - 2010 - municípios - Atlas Brasil.csv
# A partir do arquivo acima gere o banco de dados de nome ATLAS_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 IDHM_A
# 5 IDHM_CA
# 6 IDHM_CA_M
# 7 IDHM_CA_F

# Exporte o arquivo em formato CSV# Faça o commit com a mensagem "Script e dados TAREFA 3 - ATLAS"


############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

