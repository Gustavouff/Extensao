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

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

