rm(list = ls())

##################################################################################
#                  INSTALACAO E CARREGAMENTO DE PACOTES NECESSARIOS              #
##################################################################################
#Pacotes utilizados
set.seed(1)
package <<- function(){
  
  package.book <- c("rio", "tidyverse","tidymodels", "vip", "skimr", "patchwork","glmnet", "knitr", "olsrr")
  
  if(sum(as.numeric(!package.book %in% installed.packages())) != 0){
    instalador <- package.book[!package.book %in% installed.packages()]
    for(i in 1:length(instalador)) {
      install.packages(instalador, dependencies = T)
      break()}
    sapply(package.book, require, character = T) 
  } else {
    sapply(package.book, require, character = T) 
  }
  
};package()

##################################################################################
#                                   BASE DE DADOS                                #
##################################################################################

#Carregando a base de dados
database_geral <- import(file.choose())

database <- database_geral

#verificando dimensao e estrutura dos dados

glimpse(database) # estrutura
summary(database) # resumo detalhados das variáveis
count(database, desligado) #volumetria de 0 e 1
map_dbl(database, ~mean(is.na(.x))) #verificando se tem vazio na base
names(database) #nomes da coluna


#tratamento na base e criação de variavel para graficos

database <- database %>%
  mutate(
    desligado = as.factor(desligado),
    status = ifelse(desligado == 1, "Desligados", "Ativos"),
    acidente_trabalho = as.factor(acidente_trabalho),
    promocao_ultimos_5_anos = as.factor(promocao_ultimos_5_anos),
    salario = factor(salario, levels = c("baixo","medio","alto"))
  )

glimpse(database)# estrutura

# print(
#   summarytools::dfSummary(
#     database, graph.magnif = 0.75)) 

##################################################################################
#                                      DESCRITIVA                                #
##################################################################################

# Correlação

database %>% 
  select_if(is.numeric) %>% 
  cor(use = "pairwise.complete.obs") %>% #devido ter valor nulo usar o "use"
  ggcorrplot::ggcorrplot(hc.method = TRUE, 
                         type = "lower", 
                         lab = TRUE, 
                         lab_size = 3,
                         tl.cex = 8,
                         method="circle", 
                         colors = c("tomato2", "white", "springgreen3"), 
                         ggtheme=theme_bw)


# Análise de Performance versus Nível de Satisfação

database %>% 
  ggplot() +
  geom_point(aes(x = ultima_avaliacao, y = nivel_satisfacao, color = status)) +
  labs(x = "Avaliação de Performance", y = "Nível de Satisfação") +
  scale_color_manual(values = c( "light blue", "red"))

# salario por grupo

database <- database %>% 
  mutate(
    grupos = ifelse(nivel_satisfacao >= 0.75 & ultima_avaliacao >= 0.8, "Satisfeito | Produtivo",
                    ifelse(nivel_satisfacao <= 0.12 & ultima_avaliacao >= 0.8, "Insatisfeito | Produtivo",
                           ifelse(nivel_satisfacao > 0.5 & ultima_avaliacao < 0.8, "Satisfeito | Improdutivo",
                                  ifelse(nivel_satisfacao < 0.5 & ultima_avaliacao < 0.6, "Insatisfeito | Improdutivo", "Não analisar"))))
  )

database %>% 
  filter(!grupos == "Não analisar") %>% 
  group_by(salario, grupos, status) %>% 
  count(salario) %>%
  mutate(
    salario = case_when(
      salario == "baixo" ~ "Baixo",
      salario == "medio" ~ "Médio",
      TRUE ~ "Alto"
    ),
    salario = factor(salario, levels = c("Baixo", "Médio", "Alto")),
    frequencia = n,
    percentual = percent(n/14999, accuracy = 0.01)
  ) %>% 
  ggplot(aes(x = salario, y = grupos)) +
  geom_tile(aes(fill =  frequencia)) +
  geom_label(aes(x = salario, y = grupos, label = percentual)) +
  labs(x = "Salário", y = "Grupos", fill='Frequência') +
  facet_wrap(~status) +
  scale_fill_gradient(low = "blue", high = "light blue")

# analise de salario, grupo e atuacao em projetos

database %>% 
  mutate(
    salario = case_when(
      salario == "baixo" ~ "Baixo",
      salario == "medio" ~ "Médio",
      TRUE ~ "Alto"
    ),
    salario = factor(salario, levels = c("Baixo", "Médio", "Alto"))
    ) %>%
  ggplot(aes(x = ultima_avaliacao, y = nivel_satisfacao)) +
  geom_point(aes(color = status)) +
  labs(x = "Avaliação de Performance", y = "Nível de Satisfação") +
  scale_color_manual(values = c("light blue", "red")) +
  facet_grid(numero_projeto~salario)


##################################################################################
#                                      MODELAGEM                                 #
##################################################################################

#1.treino e teste

set.seed(100)
database_dividido <- initial_split(data= database, strata = desligado, prop = 0.75)
database_dividido

database_treino <- training(database_dividido)
database_teste <- testing(database_dividido)

#2.pre processamento

database_rec <-
  recipe(desligado ~., data = database_treino) %>%
  step_rm(grupos, status) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes())

a <- head(juice(prep(database_rec)))

ls("package:recipes") %>% .[str_starts(., "step_")] #todos os steps que pode usar

#3.especificacao do modelo

lr_mod <- 
  logistic_reg(
    penalty = tune(),
    mixture = 1
  ) %>%
  set_engine("glmnet")


#4.validacao cruzada (reamostragem) - evita overfit

folds <- vfold_cv(database_treino, v = 10)
folds

#5.workflow (une todos objetos do modelo)

lr_workflow <- workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(database_rec)

lr_workflow

#6.tungem

tunagem_lr_mod <- tune_grid(
  lr_workflow,
  resamples = folds,
  metrics = metric_set(roc_auc, accuracy, sens, spec)
)

collect_metrics(tunagem_lr_mod)

collect_metrics(tunagem_lr_mod, summarize = FALSE) %>%
  ggplot(aes(x= .estimate, y= id)) +
  geom_col() +
  facet_wrap( ~ .metric) +
  theme_bw()

#7.melhor hiperparametro

autoplot(tunagem_lr_mod)
show_best(tunagem_lr_mod, metric = "spec") %>% view()
melhor_hp <- select_best(tunagem_lr_mod, "spec")

#atualizacao workflow

lr_workflow <- lr_workflow %>%
  finalize_workflow(melhor_hp)

#lr_workflow$fit$actions$model$spec

#modelo final

modelo_fit <- last_fit(
  lr_workflow,
  database_dividido,
  metrics = metric_set(roc_auc, accuracy, sens, spec)
)


prob <- modelo_fit$.predictions[[1]] #probabilidades do modelo

# table(prob$desligado, prob$.pred_class) #matriz de confusao do modelo final

#ks default 0.5

#metricas do modelo

collect_metrics(modelo_fit)

#curva roc

collect_predictions(modelo_fit) %>% 
  roc_curve(desligado, .pred_0) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  labs(x = "1-Especificidade", y = "Sensibilidade") +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()
  
#autoplot()

#importancia das variaveis

vi(extract_model(modelo_fit$.workflow[[1]])) %>% 
  mutate(
    abs_importance = abs(Importance),
    Variable = fct_reorder(Variable, abs_importance)
  ) %>%
  ggplot(aes(x = abs_importance, y = Variable)) +
  geom_col(aes(fill = Sign)) +
  labs(x = "Importância", y = "Varáveis", fill='Sinal') +
  scale_fill_manual(values = c("red", "blue"))

exp(modelo_fit$.workflow[[1]]$fit$fit$fit$dev.ratio) #razao de chance

#modelo_fit$.workflow[[1]]$fit$fit$fit$beta #betas


#modelo final

modelo_rl_database <- fit(lr_workflow, data = database)

summary(modelo_rl_database)

anova(modelo_rl_database, test = "chisq")


tidy() #betas


#matriz de confusão

matriz_confusao <- modelo_rl_database %>% 
  predict(new_data = testing(database_dividido)) %>% 
  bind_cols(testing(database_dividido) %>% select(desligado)) %>% 
  mutate_all(as.factor) %>% 
  conf_mat(desligado, .pred_class)

matriz_confusao %>% 
  pluck(1) %>% 
  as_tibble() %>% 
  ggplot(aes(Prediction, Truth, alpha = n)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), color = 'white', alpha = 1, size = 8)

matriz_confusao %>% 
  summary() %>% 
  select(-.estimator) %>% 
  filter(.metric %in% c('precision', 'recall', 'f_meas',
                        'accuracy', 'spec', 'sens')) %>% 
  kable()

