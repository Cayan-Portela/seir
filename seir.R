require(nCov2019)
require(dplyr)
require(deSolve)

## Obtendo os dados
data = load_nCov2019()
data_global = data["global"] #extract global data
data_br = filter(data_global, country == 'Brazil')


Infected = data_br$cum_confirm
Day <- 1:(length(Infected))
N <- 211289547 # População do Brasil

# Valores populacao

X = Infected[1]          # infeccioso (infectados)
Y = 0                    # recuperados
Z = 2                    # expostos (2 ?)
W = N - (X + Z)          # suscetiveis



SEIR = function (current_timepoint, state_values, parameters){
  # criando variaveis de estado (local variables)
  S = state_values[1]        # suscetiveis
  E = state_values[2]        # expostos
  I = state_values[3]        # infecciosos
  R = state_values[4]        # recuperados
  
  with ( 
    as.list (parameters),     # parameters recebe variaveis nomeadas 
    {
      # calcula derivadas
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}

taxa_contato = 10                     # numero medio de contatos por dia
prob_transmisssao = 0.17               # probabilidade de transmissao
periodo_infeccao = 8                  # infectious period
periodo_latencia = 7                     # periodo de latencia (incubacao)

beta_value = taxa_contato * prob_transmisssao
gamma_value = 1 / periodo_infeccao
delta_value = 1 / periodo_latencia

Ro = beta_value / gamma_value

par_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value)
init_values = c (S = W/N, E = X/N, I = Y/N, R = Z/N)

tempo = 1:100
output = lsoda (init_values, tempo, SEIR, par_list)

# Plota estimativas
plot (S ~ time, data = output, type='b', col = 'blue')    
plot (E ~ time, data = output, type='b', col = 'pink')    
plot (I ~ time, data = output, type='b', col = 'red')    
plot (R ~ time, data = output, type='b', col = 'green')    


# Usando optim (?)
# Nao esta pronto.

# RSS <- function(parameters) {
#   out <- lsoda(init_values, Day, SEIR, par_list)
#   fit <- out[ , 4]
#   sum((Infected - fit)^2)
# }
# 
# Opt <- optim(c(0.5, 0.5, 0.5), RSS,
#              method = "L-BFGS-B",
#              lower = c(0, 0, 0), upper = c(2, 2, 2))
# 
# Opt$message
# ## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
# Opt$par



