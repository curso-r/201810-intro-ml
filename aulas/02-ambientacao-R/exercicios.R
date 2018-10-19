## R-base

# 1. Calculo o número de ouro no R.
# Dica: o número de ouro é dado pela expressão 1+√52.



# 2. Quais as diferenças entre NaN, NULL, NA e Inf? 
# Digite expressões que retornam cada um desses resultados.



# 3. Por que o código abaixo retorna erro? 
# Arrume o código para retornar o valor TRUE

x <- 4
if(x = 4) {
  TRUE
}

# 4. Usando if e else, escreva um código que retorne a string “número” caso o 
# valor seja da classe numeric ou integer; a string “palavra” caso o valor seja 
# da classe character; e NULL caso contrário.


# 5. Use a função runif() para escrever uma função que retorne um número 
# aleatório inteiro entre 0 e 10 (0 e 10 inclusive).


## Pipe %>%

# 1. Reescreva a expressão abaixo utilizando o %>%.
# Dica: utilize a função magrittr::divide_by(). 
# Veja o help da função para mais informações.

round(mean(sum(1:10)/3), digits = 1)



# 2. Reescreva o código abaixo utilizando o %>%.


x <- rnorm(100)

x.pos <- x[x>0]

media <- mean(x.pos)

saida <- round(media, 1)



# 3. Sem rodar, diga qual a saída do código abaixo. 
# Consulte o help das funções caso precise.

2 %>% 
  add(2) %>% 
  c(6, NA) %>% 
  mean(na.rm = T) %>% 
  equals(5)



## Importação

# Ler o material: https://www.curso-r.com/material/importacao/


## Manipulação

# Ler o material: https://www.curso-r.com/material/manipulacao/


## Gráficos

# Ler o material: https://www.curso-r.com/material/ggplot/