#####################################################################################################
# Universidade Federal de Pernambuco - UFPE                                                         #
# Graduacao em Ciencia da Computacao - Em: Novembro/2018                                            #
# Disciplina: Estatistica E Probabilidade para Computacao - ET586  - Profa. Dr. Renata Souza (rmcrs)#
# Discente: Juliano Cezar Teles Vaz (jctv)                                                          #
# Discente: Joao Victor da S. Lobao (jvsl2)                                                         #
#####################################################################################################
####################################      PROJETO II     ############################################
#####################################################################################################

#enconding="UFT8"   #ou usar o latin2 tb.
enconding="latin1"  #tentativas falhas de tentar corrigir os erros de acentuacao 

#####################################################################################################
# Inicio da questÃ£o (1) ---------------------------------
# Descarregue o arquivo .csv da planilha e imprima o dataframe obtido exatamente do jeito que ele se encontra.
db <- read.csv("D:\\Users\\jctv\\Documents\\Project-R\\amostra.csv")

titulos = db[1] #variaveis globais auxiliares
notas = db[2]  #variaveis globais auxiliares
anos = db[3]   #variaveis globais auxiliares

dados = data.frame(TITULOS = titulos, NOTAS = notas, ANOS = anos)#coloca os dados em um data frame
cat("\nQuestao 1 - imprimindo o dataframe..:\n")
print(dados)#printa os dados

############################################QUESTAO 2################################################
# Encontre a media das notas (sem utilizar a funcao pronta do R).

tamanhoDaAmostra = lengths(dados[1])#leva alem do tamanho o nome da coluna
unname(tamanhoDaAmostra, force = FALSE)#tira o nome da coluna

media = function(dados){
  i = 1 #contador
  m = 0 #acumulador
  for(i in 1:tamanhoDaAmostra){    
    m = m + dados[i,2] #linhas "i" da coluna 2
  }
  return(m/48)
 
}
MEDIA_ = media(dados)
cat("\nQuestao 2 - a media eh:", MEDIA_)
cat("\n")

#####################################QUESTAO 3######################################################
# Encontre o desvio padrao das notas (sem utilizar a funcao pronta do R).
 
variancia = 0;
desvio = function(dados){
  i = 1 #contador
  m = 0 #acumulador que representa o somatorio
  for(i in 1:tamanhoDaAmostra){      
    m =  m + ((dados[i,2] - MEDIA_ ) * (dados[i,2] - MEDIA_ )) #parte da formula da variancia
  }

  variancia = m/(tamanhoDaAmostra - 1); #parte da formula da variancia
  return(sqrt(variancia)) #desvio eh raiz da variancia
}
DESVIO_ = desvio(dados)
  cat("\nQuestao 3 - O Desvio padrao eh: ", DESVIO_)
  cat("\n")
  
# Nessa questao, o problema da media foi resolvido da seguinte forma (...)
#####################################QUESTAO 4##########################################################
# Encontre a moda das notas (sem utilizar a funcao pronta do R).
  cat("\nQuestao 4 - A nota de maior moda eh:\n")
  selecaoModa = function(){
    filtro = dados
    filtro = dados[dados$NOTAS >= 0,][2]#faz filtragem na coluna de anos --> 3, ao colocar "ANOS" nao reconhece.
    
    tabela<-table(filtro)
    print(tabela[tabela == max(tabela)])
    cat("vezes\n")
  }
  
  selecaoModa();
    
  
##############################################QUESTAO 5#############################################
# Faça uma funcao que retorna apenas os nomes dos filmes que possuem notas maiores ou iguais a seis (6).

  cat("\nQuestao 5 - Os nomes dos filmes com nota maior ou igual a seis(6) sao:\n")
  selecaoNotaMaiorOuIgualAseis = function(){
    filmesComNotaMaiorOuIgualA6 = dados[dados$NOTAS >= 6,]["TITULOS"]#faz filtragem na coluna de titulos
    filmesComNotaMaiorOuIgualA6 = unname(filmesComNotaMaiorOuIgualA6, force = FALSE)#tira o nome da coluna
    print(filmesComNotaMaiorOuIgualA6, row.names = FALSE)
  }
  selecaoNotaMaiorOuIgualAseis()
  
#########################################QUESTAO 6###################################################
  cat("\nQuestao 6 - A quantidade de filmes com nota nota menor que seis(6) eh:\n")
#Faça uma funcao que retorna quantos filmes possuem notas abaixo de seis (6).
  notaMenorQueSeis  = function(dados){
    qtd = 0
    for ( i in 1:tamanhoDaAmostra){
      if(dados[i,2] < 6){
        qtd = qtd + 1;
      }
    }
    return(qtd)
  }
  saida2 = notaMenorQueSeis(dados); 
  cat(saida2) 
  cat("\n")
      

  ########################################QUESTAO 7#####################################################
  # Faça uma funcao que retorna o nome do filme com menor pontuacao e o nome do filme com maior pontuacao, nessa ordem.
  # Por fim, Faça um dataframe com os dois filmes encontrados com as colunas titulo, NOTA, ANO assim como o original.

  cat("\nQuestao 7 - Os filme com maior e menor pontuacao, respectivamente sao:\n")
  selecaoMenorEMaior = function(){
    filtro = dados
    filtro = dados[dados$NOTAS >= 0,]
    maximo = max(filtro[2]);
    
    minimo = min(filtro[2]);
    
    linhaDoMaximo = (which(grepl(maximo, dados$NOTAS)))
    linhaDoMinimo = (which(grepl(minimo, dados$NOTAS)))
    
    filmeComNotaMaior = dados[dados$NOTAS == maximo,]["TITULOS"]#faz filtragem na coluna de titulos
    filmeComNotaMaior= unname(filmeComNotaMaior, force = FALSE)#tira o nome da coluna
    #nomeMaior = print(filmeComNotaMaior, row.names = FALSE)
    print(filmeComNotaMaior, row.names = FALSE)
    nomeMaior <- as.character(dados$TITULOS[linhaDoMaximo])
    
    filmeComNotaMenor = dados[dados$NOTAS == minimo,]["TITULOS"]#faz filtragem na coluna de titulos
    filmeComNotaMenor= unname(filmeComNotaMenor, force = FALSE)#tira o nome da coluna
    #nomeMenor = print(filmeComNotaMenor, row.names = FALSE)
    print(filmeComNotaMenor, row.names = FALSE)
    nomeMenor <- as.character(dados$TITULOS[linhaDoMinimo])
        
    titulosNovo = c(nomeMenor, nomeMaior)
    notasNovo = c(filtro[linhaDoMinimo,2], filtro[linhaDoMaximo,2])
    anosNovo = c(filtro[linhaDoMinimo,3],filtro[linhaDoMaximo,3])
    cat("\nImprimindo o dataframe com esses dois filmes selecionados:\n")
    dataFrameNovo = data.frame(TITULOS = titulosNovo, NOTAS = notasNovo, ANOS = anosNovo)#coloca os dados em um data frame
    print(dataFrameNovo)
    
  }
  selecaoMenorEMaior()  
    
######################################QUESTAO 8#######################################################
# Faça uma funcao que retorne O ano em que sairam mais filmes com notas maiores ou iguais a seis e meio (6,5).
  cat("\nQuestao 8 - O ano em que saiu mais filmes com notas maiores ou iguais a seis e meio(6.5), eh:\n")
  selecaoAno = function(){
    
    filtro = dados
    filtro = dados[dados$NOTAS >= (6.5),][3]#faz filtragem na coluna de anos --> 3, ao colocar "ANOS" nao reconhece.
    
    tabela<-table(filtro)
    print(tabela[tabela == max(tabela)])
    cat("vezes\n")
    
   }
  selecaoAno()

###########################################QUESTAO 9##################################################
# Faça um histograma onde mostra a frequencia de filmes com notas maiores ou iguais a seis de cada ano. 
# Nao esqueca de dar um titulo e fazer ele de forma colorida, facilitando a visualizacao. 

  histog = function(){
    cat("\n\nQuestao 9 - Faça um histograma onde mostra a frequencia de filmes com notas maiores ou iguais a seis de cada ano\n\n")
    filtro = dados
    filtro = dados[dados$NOTAS >= (6),][3]#faz filtragem na coluna de anos --> 3, ao colocar "ANOS" nao reconhece.
    Filmes = c(filtro[,1])
    #print(Filmes)
    #tabela<-table(filtro)
    #print(tabela)
    hist(Filmes, main = "Filmes com nota maior ou igual a seis(6)", xlab = "Anos", ylab="Frequencia", col="darkblue", border="black");
  }
  histog();
#####################################################################################################
