# Inequalities in Basic Education in Brazil
Analysis of socioeconomic status and performance on national evaluation in elementary education. Some intuitive graphs and descriptive analysis about these variables.

## Análise da distribuição de desempenho médio condicionada ao Nível Socioeconômico médio das Escolas

Esta é uma análise bivariada muito informativa. Com ela podemos começar a compreender a relação entre origem social e desempenho escolar; em outras palavras, a dimensão socioeconômica das desigualdades educacionais.

Temos duas variáveis: i) proficiência/desempenho médio por Escola; ii) Indicador de nível socioeconômico (INSE)

# Proficiência/desempenho médio por Escola

A proficiência ou desempenho médio por Escola é um indicador importante do aprendizado dos alunos atendidos pela instituição. Trata-se de uma variável métrica contínua que varia na escala SAEB de 0-400. Para uma interpretação pedagógica da distribuição desta variável, consideramos os níveis de desempenho utilizadas na avaliação do Estado de São Paulo (SARESP), discutida por Soares (2018): abaixo do básico, básico, adequado e avançado. Para cada segmento da educação básica há níveis de interpretação pedagógica específicos, tal como nos quadros abaixo.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/niveis_LP.png?raw=true)
![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/niveis_MT.png?raw=true)

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/histograms_DesempLP.png?raw=true)
![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/histograms_DesempMT.png?raw=true)

Observamos que a proficiência tem uma distribuição próxima da normal, com parte significativa das escolas concentradas no nível básico em LP e MT. Poucos são os casos de escolas no nível adequado, e ainda menos no avançado.

É possível observar também um pequeno deslocamento dos casos para a direita de 2013-2019, o que representa um aumento em proficiência das escolas em geral. Apenas com a distribuição do desempenho podemos observamos uma a situação sensível em termos de habilidades escolares. São poucos os casos de escolas com desempenho médio no nível adequado, e nenhuma no nível avançado entre 2013 e 2017.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/density_Desemp2013-2019.png?raw=true)

# Indicador de Nível Socioeconômico

O indicador de Nível Socioeconômico (INSE, daqui em diante) é um constructo teórico operacionalizado em uma medida derivada dos itens do questionário socioeconômico dos alunos. Conforme metodologia construída por Alves e Soares (2009, 2011), foi também desenvolvido com base na Teoria de Resposta ao Item (TRI) e está disponibilizado nas bases oficiais do INEP. De acordo com os Alves e Soares (2009), o INSE segue um constructo de medida socioeconômica tradicional no escopo das pesquisas sobre estratificação, e possui correlação forte com outros indicadores como o Índice de Desenvolvimento Humano Municipal (IDHM) e a Renda Domiciliar Per Capita (RDPC) nos municípios. Por meio de uma análise de cluster por método hierárquico, os estudantes foram agrupados em sete níveis ordinais segundo os itens respondidos. Dos sete grupos, derivamos três categorias sintéticas de INSE: i) baixo, que agrega os grupos um e dois; ii) médio, que agrega os grupos três, quatro e cinco e; iii) alto, que agrega os grupos seis e sete.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/barplot_INSE.png?raw=true)

# Desempenho por Nível Socioeconômico

Agora veremos a correlação entre nível socioeconômico e desempenho médio. Em primeiro lugar, condicionamos a distribuição do desempenho pelo INSE para verificar o padrão de distanciamento ao longo do tempo.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/density_INSExDesemp.png?raw=true)
![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/density_INSExDesemp-MT.png?raw=true)


Os diagramas representam na caixa central os três intervalos interquartis (IQR’s) da distribuição de proficiência, isto é, os pontos 25%, 50% (mediana) e 75% da distribuição ordenada. As linhas estendidas fora da caixa representam 1,5 do IQR, e os triângulos representam as observações fora da linha, que estão muito distantes da concentração. As linhas horizontais tracejadas representam a interpretação pedagógica dos níveis de proficiência da escala SAEB. A distribuição da proficiência condicionada ao INSE apresenta uma escada da hierarquia constante entre as Escolas, o que indica o padrão de desigualdades persistentes entre os grupos. Somado ao padrão de desigualdades, estão os casos extremos de Escolas com baixo e médio INSE e com desempenho médio abaixo de 200 (LP) e 225 (MT). Essas Escolas estariam contextualizadas no problema discutido pelos teóricos da reprodução (BOURDIEU; PASSERON, 2018), uma vez que têm refletida na proficiência a condição socioeconômica dos estudantes. Segundo a escala de proficiência definida pelo SAEB, estes estudantes estão em uma condição crítica de aprendizado nas disciplinas de referência, sem o domínio de habilidades elementares, e por isso requerem atenção especial (INEP, 2019). Também chama a atenção as Escolas com resultados acima de 325 (LP) e 350 (MT) nessas mesmas categorias. Essas configuram casos particulares de Escolas que possivelmente obtêm êxito no controle do background socioeconômico dos estudantes, oferecendo condições institucionais de aprendizado independente da origem social, tratados pela literatura sobre Eficácia escolar.
Para comparar as distribuições de proficiência entre os grupos de NSE e mensurar a distância entre estes, calculamos a distância entre mediana da proficiência de LP e MT do grupo com INSE baixo em relação aos grupos de médio e alto NSE, representados na tabela abaixo.

Para analisar com maior detalhe as distâncias entre os grupos de nível socioeconômico nesse período, fizemos uma análise de diferença por pontos percentis da distribuição de proficiência condicionada ao INSE. Calculamos a diferença em cada ponto percentil entre os grupos de INSE alto e médio em relação ao baixo. Com isso, podemos observar não apenas as distâncias em média ou em mediana, mas em cada ponto percentil da distribuição de proficiência de cada grupo marcado pelo INSE.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/percentis_INSExDesempLP.png?raw=true)

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/percentis_INSExDesempMT.png?raw=true)
