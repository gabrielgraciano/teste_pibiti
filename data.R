dados_saude_alimentacao <- readr::read_delim("data/dados_saude_alimentacao.csv",
                                             delim = ";", escape_double = F, trim_ws = T,
                                             show_col_types = F) %>% 
  modify_at(c("Sexo", "Trabalha", "Relacionamento","Cozinha", "Come fora",
              "Culinária favorita", "Vegetais nas refeições",
              "Pratica exercícios", "Pratica esportes", "Toma vitaminas",
              "Álcool: Consumo mensal", "Álcool: Dose média", 
              "Consumo de tabaco"), as.factor)

var_factors <- sapply(dados_saude_alimentacao, is.factor)
var_numerics <- sapply(dados_saude_alimentacao, is.numeric) | 
  sapply(dados_saude_alimentacao, is.integer)
shortLevels <- sapply(dados_saude_alimentacao, nlevels) < 6 & 
  sapply(dados_saude_alimentacao, is.factor)


dados_paralisia <- read.csv( "data/dados_paralisia.csv", sep = ";")

exerc_idosos <- read.csv2( "data/exerc_idosos.csv", sep = ";") %>% 
  modify_at(c('sexo'), as.factor) %>% 
  mutate(sexo = fct_relevel(sexo, 'Masculina', 'Feminina'))

exerc_imc <- read.csv2( "data/exerc_imc.csv", sep = ";") %>% 
  modify_at(c('sexo'), as.factor) %>% 
  mutate(sexo = fct_relevel(sexo, 'Meninas', 'Meninos'))


example_dataframe <- as.data.frame(rbind(
  c("Sexo", "'Masculino', 'Feminino'", "Qualitativa Nominal"),
  c("Ano letivo", "1, 2, 3, 4...", "Quantitativa Discreta"),
  c("Peso (kg)", "61,2; 85; 119,3; ...", "Quantitativa Contínua"),
  c("Trabalho", "'Tempo Integral', 'Meio Período', 'Não trabalha'", "Qualitativa Ordinal"),
  c("Relacionamento", "'Solteiro', 'Em um relacionamento', 'Morando junto', ...", "Qualitativa Nominal"),
  c("Cozinha", "'Sempre', 'Quase todo dia', 'Nunca', ...", "Qualitativa Ordinal"),
  c("Come Fora", "'Sempre', 'Quase todo dia', 'Nunca', ...", "Qualitativa Ordinal"),
  c("Percepção de Saúde", "1, 2, 3, 4....", "Quantitativa Discreta"),
  c("Vegetais nas Refeições", "'Difícil', 'Um pouco', 'Regular'...", "Qualitativa ordinal"),
  c("Pratica exercícios", "'Sempre', 'Quase todo dia', 'Nunca', ...", "Qualitativa Ordinal"),
  c("Pratica esportes", "'Sim', 'Não'", "Qualitativa Nominal"),
  c("Toma Vitaminas", "'Sim', 'Não'", "Qualitativa Nominal"),
  c("Altura (m)", "1,75; 1,67; 1,68; ...", "Quantitativa Contínua"),
  c("Álcool: Consumo mensal", "'Não bebe', 'Raramente', 'Ocasional', 'Frequente', ...", "Qualitativa Ordinal"),
  c("Álcool: Dose média", "'Não bebe', 'Até 2 doses', '3 a 4 doses', ...", "Qualitativa Ordinal"),
  c("Consumo tabaco", "'Não fuma', 'Até 1 cigarro', 'Até 5 cigarros'", "Qualitativa ordinal"),
  c("Idade (anos)", "17, 21, 20,...", "Quantitativa Discreta"),
  c("HDL (mg/dL)", "50,76; 56,43; 56,87;", "Quantitativa Contínua"),
  c("LDL (mg/dL)", "84,93; 100,65; 101,47;", "Quantitativa Contínua"),
  c("Triglicérides (mg/dL)", "61,32; 55,4; 91,62;", "Quantitativa Contínua")
))
colnames(example_dataframe) <- c("Variável", "Possíveis Valores", "Tipo de Variável")

## Tabela de frequencias Exemplo 1

tab_frequencia_relacionamento <- as.data.frame(table(dados_saude_alimentacao$Relacionamento))
tab_frequencia_relacionamento$prop <- round(tab_frequencia_relacionamento$Freq/sum(tab_frequencia_relacionamento$Freq), digits = 3)
tab_frequencia_relacionamento$porc <- scales::percent(tab_frequencia_relacionamento$prop)
colnames(tab_frequencia_relacionamento) <- c("Relacionamento", "Frequência Absoluta", "Proporção", "Porcentagem")

tab_frequencia_relacionamento$Relacionamento <- as.character(tab_frequencia_relacionamento$Relacionamento)
tab_frequencia_relacionamento[4,] <- c(
  "Total", sum(tab_frequencia_relacionamento[,2]), "1.000", "1.0%"
)


##Tabela de frequencias Exemplo 3
freq_peso <- cut(dados_saude_alimentacao$Peso, breaks = c(45, 60, 75, 90, 105, 120))
tab_frequencia_peso <- as.data.frame(with(dados_saude_alimentacao, table(freq_peso, useNA = 'no')))
tab_frequencia_peso$prop <- round(tab_frequencia_peso$Freq/sum(tab_frequencia_peso$Freq), digits = 3)
tab_frequencia_peso$porc<- scales::percent(tab_frequencia_peso$prop) 
colnames(tab_frequencia_peso) <- c("Faixa de peso", "Frequência", "Proporção", "Porcentagem")
tab_frequencia_peso$`Faixa de peso` <- as.character(tab_frequencia_peso$`Faixa de peso`)
tab_frequencia_peso[dim(tab_frequencia_peso)[1]+1,] <- c(
  "Total", sum(tab_frequencia_peso[,2]), "1.000", "1.0%"
)


### Tabela de frequencias Exemplo 2

tab_frequencia_ano_letivo <- as.data.frame(table(dados_saude_alimentacao$`Ano letivo`))
tab_frequencia_ano_letivo$prop <- round(tab_frequencia_ano_letivo$Freq/sum(tab_frequencia_ano_letivo$Freq), digits = 3)
tab_frequencia_ano_letivo$porc <- scales::percent(tab_frequencia_ano_letivo$prop)
colnames(tab_frequencia_ano_letivo) <- c("Ano Letivo", "Frequência", 
                                         "Proporção", "Porcentagem")
tab_frequencia_ano_letivo$`Ano Letivo` <- 
  as.character(tab_frequencia_ano_letivo$`Ano Letivo`)
tab_frequencia_ano_letivo[5,] <- c(
  "Total", sum(tab_frequencia_ano_letivo[,2]), "1.000", "1.0%"
)


#Dicionário de mudanças nos nomes
nomes_exibidos <- c(
  'Sexo' = 'sexo',
  'Grupo' = 'grupo',
  'Idade' = 'idade',
  'Perda Auditiva' = 'perda_audit',
  'Distúrbio de Comunicação' = 'dist_comun',
  'Grau de Disfunção Motora Oral' = 'dmo',
  'Tempo líquido' = 'td_liquido',
  'Tempo pastoso' = 'td_pastoso',
  'Tempo sólido' = 'td_solido'
)

'entradas <- c(
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0
)' 


'# Criar um vetor vazio para armazenar os dados recuperados
dados_paralisia$perda_audit <- rep(NA, length(entradas))

# Preencher o vetor perda_audit com base nas entradas fornecidas
dados_paralisia$perda_audit[entradas == 0] <- 0
dados_paralisia$perda_audit[entradas == 1] <- 1
'

DicioAlimentacao <- rbind(c("Sexo", "Sexo do aluno: Masculino ou Feminino."),
                          c("Ano letivo", "Ano do curso em que o aluno se encontra. 1 é o primeiro ano e 4, o último."),
                          c("Peso", "Peso do aluno, em kg."),
                          c("Trabalha", "Variável que indica se o aluno trabalha ou não, e se trabalha, se é meio período ou integral."),
                          c("Relacionamento", "Estado de relacionamento do aluno."),
                          c("Cozinha", "Frequência em que o aluno costuma cozinhar, de nunca até sempre."),
                          c("Come fora", "Frequência em que o aluno come fora de casa, em algum restaurante."),
                          c("Percepção de saúde", "Como o aluno classifica, de 1 a 10, sua saúde, sendo 1 péssima e 10 excelente."), 
                          #Retirei culinária favorita - Gabriel
                          c("Vegetais nas refeições", "Frequência em que o aluno tem vegetais como parte de suas refeições"),
                          c("Pratica exercícios", "Frequência em que o aluno se exercita."),
                          c("Pratica esportes", "Se o aluno participa de algum esporte ou não."),
                          c("Toma vitaminas", "Se o aluno toma vitaminas."),
                          c("Altura", "Altura do aluno, em metros."),
                          c('Álcool: consumo mensal', 'Frequência de consumo de álcool por mês.'),
                          c('Álcool: dose', 'Dose de álcool por consumo.'),
                          #Não tenho certeza desse período - Gabriel
                          c('Consumo tabaco', 'Quantos cigarros o aluno consome em um dia.'),
                          c('Idade', 'Idade em anos completos.'),
                          c('HDL', 'Concentração de HDL em mg/dL.'),
                          c('LDL', 'Concentração de LDL em mg/dL.'),
                          c('Triglicérides', 'Concentração de triglicérides em mg/dL.')
                          
)

DicioAlimentacao <- as.data.frame(DicioAlimentacao)

DicioParalisia <- data.frame(
  "Variável" = c('sexo (Sexo)',
                 'idade (Idade)',
                 'grupo (Grupo)',
                 'perda_audit (Perda auditiva)',
                 'dist_comun (Distúrbio de Comunicação)',
                 'dmo (DMO)',
                 'td_liquido (Tempo líquido)',
                 'td_pastoso (Tempo pastoso)',
                 'td_solido (Tempo sólido)'),
  "Descrição" =  c('Menino ou menina',
                   'Idade em anos completos',
                   'Grupo das crianças por condição de saúde (SAN ou PC)',
                   'Existência ou não de perda auditiva',
                   'Existência ou não de distúrbio de comunicação',
                   'Grau de DMO (disfunção motora oral, compreendido entre 0 e 4)',
                   'Tempo, em segundos, para deglutição de 100 ml de 
                   suco de laranja',
                   'Tempo, em segundos, para deglutição de 140 g de
                   iogurte de morango homogêneo sem pedaços de fruta',
                   'Tempo, em segundos, para deglutição de 12 g de bolacha
                  recheada de chocolate')
)