rm(list = ls())
gc()
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(openxlsx)
excel_sheets("dados_recrutamento.xlsx")
pacientes <- read_excel("dados_recrutamento.xlsx", sheet = "Pacientes")

protocolos <- read_excel("dados_recrutamento.xlsx", sheet = "Protocolos")
protocolos <- protocolos %>%
  mutate(
    doenca = str_trim(doenca),
    doenca = str_remove(doenca, "(\\s*\\|\\s*)+$"),
    doenca_norm = str_to_lower(doenca),
    especialidade_norm = str_to_lower(especialidade)
  )
protocolos %>%
  filter(str_detect(doenca_norm, "\\|$"))
pacientes <- pacientes %>%
  mutate(
    diagnostico_norm = str_to_lower(diagnostico),
    especialidade_norm = str_to_lower(especialidade)
  )
protocolos <- protocolos %>%
  rename(
    especialidade_protocolo = especialidade,
    especialidade_norm_protocolo = especialidade_norm
  )

pacientes <- pacientes %>%
  rename(
    especialidade_paciente = especialidade,
    especialidade_norm_paciente = especialidade_norm
  )
triagem <- crossing(
  pacientes,
  protocolos
)
triagem <- triagem %>%
  mutate(
    match_especialidade =
      especialidade_norm_paciente == especialidade_norm_protocolo
  )
triagem <- triagem %>%
  mutate(
    match_doenca = str_detect(
      diagnostico_norm,
      doenca_norm
    )
  )
triagem_filtrada <- triagem %>%
  filter(
    match_especialidade,
    match_doenca
  )
triagem_final <- triagem_filtrada %>%
  select(
    paciente,
    idade,
    sexo,
    diagnostico,
    protocolo,
    titulo,
    centro,
    especialidade = especialidade_protocolo
  )
write.xlsx(
  triagem_final,
  file = "triagem_pacientes_protocolos.xlsx",
  overwrite = TRUE
)
triagem_final <- triagem_filtrada %>%
  select(
    paciente,
    diagnostico,
    doenca,
    match_especialidade,
    match_doenca,
    everything()
  )
write.xlsx(
  triagem_final,
  file = "triagem_pacientes_protocolos.xlsx",
  overwrite = TRUE
)
triagem_final <- triagem_filtrada %>%
  select(
    paciente,
    diagnostico,
    doenca,
    match_especialidade,
    match_doenca,
    everything()
  )
write.xlsx(
  triagem_final,
  file = "triagem_pacientes_protocolos2.xlsx",
  overwrite = TRUE
)

triagem_final <- triagem_filtrada %>%
  select(
    paciente,
    diagnostico,
    protocolo,
    doenca,
    match_especialidade,
    match_doenca,
  )
write.xlsx(
  triagem_final,
  file = "triagem_pacientes_protocolos3.xlsx",
  overwrite = TRUE
)
protocolos %>%
  mutate(
    pacientes_match = map(doenca_norm, ~
                            pacientes %>%
                            filter(str_detect(diagnostico_norm, .x)) %>%
                            pull(paciente)
    )
  )
install.packages("tidytext")
library(tidytext)

tokens_pac <- pacientes %>%
  select(paciente, diagnostico) %>%
  unnest_tokens(palavra, diagnostico)

tokens_prot <- protocolos %>%
  select(protocolo, doenca) %>%
  unnest_tokens(palavra, doenca)

intersecao <- tokens_pac %>%
  inner_join(tokens_prot, by = "palavra") %>%
  distinct(paciente, protocolo)
'relantionship = many-to-many'
install.packages("purrr")
library(purrr)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(tidytext)
library(openxlsx)
intersecao <- tokens_pac %>%
  inner_join(tokens_prot, by = "palavra") %>%
  distinct(paciente, protocolo)
'relantionship = many-to-many'

write.xlsx(
  intersecao,
  file = "intersecao_validacao_triagem.xlsx",
  overwrite = TRUE
)
protocolos %>%
  mutate(
    pacientes_match = map(doenca_norm, ~
                            pacientes %>%
                            filter(str_detect(diagnostico_norm, .x)) %>%
                            pull(paciente)
    )
  ) %>% 
  View()
match_invertido <- protocolos %>%
  mutate(
    pacientes_match = map(doenca_norm, ~
                            pacientes %>%
                            filter(str_detect(diagnostico_norm, .x)) %>%
                            pull(paciente)
    )
  )
write.xlsx(
  match_invertido,
  file = "match_invertido_validacao.xlsx",
  overwrite = TRUE
)
