#####################################################################################################
# Universidade Federal de Pernambuco - UFPE                                                         #
# Graduação em Ciência da Computação - Em: Novembro/2018                                            #
# Disciplina: Estatistica E Probabilidade para Computação - ET586  - Profa. Dr. Renata Souza (rmcrs)#
# Discente: Juliano Cezar Teles Vaz (jctv)                                                          #
# Discente: João Victor da S. Lobão (jvsl2)                                                         #
#####################################################################################################
####################################      PROJETO II     ############################################
#####################################################################################################

#enconding="UFT8"   #ou usar o latin2 tb.
enconding="latin1"  #tentativas falhas de tentar corrigir os erros de acentuacao 

#####################################################################################################
# Inicio da questão (1) ---------------------------------
# Descarregue o arquivo .csv da planilha e imprima o dataframe obtido exatamente do jeito que ele se encontra.
db <- read.csv("C:\\Users\\teste\\Desktop\\Project-R\\amostra.csv")

titulos = db[1]
notas = db[2]
anos = db[3]

dados = data.frame(TITULOS = titulos, NOTAS = notas, ANOS = anos)#coloca os dados em um data frame

print(dados)#printa os dados

#####################################################################################################

# InÃ­cio da questÃ£o (2) ---------------------------------
# Encontre a mÃ©dia das notas (sem utilizar a funÃ§Ã£o pronta do R).

tamanhoDaAmostra = lengths(dados[1])#leva alem do tamanho o nome da coluna
unname(tamanhoDaAmostra, force = FALSE)#tira o nome da coluna


media = function(dados){
  i = 1
  m = 0
  for(i in 1:tamanhoDaAmostra){
    
    m = m + dados[i,2]
  }
  return(m/48)
 
}
MEDIA_ = media(dados)
cat("\nQuestão 1 - a Média é:", MEDIA_)

#####################################################################################################

# InÃ­cio da questÃ£o (3) ---------------------------------
# Encontre o desvio padrÃ£o das notas (sem utilizar a funÃ§Ã£o pronta do R).
  variancia = 0;
  
  desvio = function(dados){
    i = 1
    m = 0
    for(i in 1:tamanhoDaAmostra){
      
      m =  m + ((dados[i,2] - MEDIA_ ) * (dados[i,2] - MEDIA_ ))
    }
    
    variancia = m/tamanhoDaAmostra; 
    return(sqrt(variancia)) #desvio
    
    
  }
DESVIO_ = desvio(dados)
  cat("\nQuestao 2 - O Desvio padrão é: ", DESVIO_)
# Nessa questÃ£o, o problema da mÃ©dia foi resolvido da seguinte forma (...)

#####################################################################################################
# InÃ­cio da questÃ£o (4) ---------------------------------
# Encontre a moda das notas (sem utilizar a funÃ§Ã£o pronta do R).
# Nessa questÃ£o, o problema da mÃ©dia foi resolvido da seguinte forma (...)
#####################################################################################################

# InÃ­cio da questÃ£o (5) ---------------------------------
# FaÃ§a uma funÃ§Ã£o que retorna apenas os nomes dos filmes que possuem notas maiores ou iguais a seis (6).
# Nessa questÃ£o, o problema da mÃ©dia foi resolvido da seguinte forma (...)
#####################################################################################################

# InÃ­cio da questÃ£o (6) ---------------------------------
#FaÃ§a uma funÃ§Ã£o que retorna quantos filmes possuem notas abaixo de seis (6).
# Nessa questÃ£o, o problema da mÃ©dia foi resolvido da seguinte forma (...)
#####################################################################################################

# InÃ­cio da questÃ£o (7) ---------------------------------
# FaÃ§a uma funÃ§Ã£o que retorna o nome do filme com menor pontuaÃ§Ã£o e o nome do filme com maior pontuaÃ§Ã£o, nessa ordem. Por fim, faÃ§a um dataframe com os dois filmes encontrados com as colunas TÃTULO, NOTA, ANO assim como o original.
# Nessa questÃ£o, o problema da mÃ©dia foi resolvido da seguinte forma (...)
#####################################################################################################

# InÃ­cio da questÃ£o (8) ---------------------------------
# FaÃ§a uma funÃ§Ã£o que retorne o ano em que saÃ­ram mais filmes com notas maiores ou iguais a seis e meio (6,5).
# Nessa questÃ£o, o problema da mÃ©dia foi resolvido da seguinte forma (...)
#####################################################################################################

# InÃ­cio da questÃ£o (9) ---------------------------------
# FaÃ§a um histograma onde mostra a frequÃªncia de filmes com notas maiores ou iguais a seis de cada ano. NÃ£o esqueÃ§a de dar um tÃ­tulo e fazer ele de forma colorida, facilitando a visualizaÃ§Ã£o. 
# Nessa questÃ£o, o problema da mÃ©dia foi resolvido da seguinte forma (...)
#####################################################################################################
