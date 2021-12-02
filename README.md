# Inequalities in Basic Education in Brazil
Este é o repositório dedicado ao trabalho que estou realizando no mestrado em Sociologia e Antropologia. Com o título "Oportunidades de quê?: desigualdades na aquisição de habilidades na Educação Básica", investigo o efeito da origem social sobre o aprendizado, um dos recursos importantes que configuram as oportunidades educacionais. A análise está centrada nos níveis do aluno (indivíduo) e das escolas (aglomerados) do sistema nacional de educação, no segmento fundamental. Aqui disponibilizo todos os procedimentos da pesquisa para quem tiver interesse em replicar, revisar ou usar para avançar nos estudos sobre desigualdades de oportunidades educacionais e estratificação educacional.

# Escolas

## Análise da distribuição de desempenho médio condicionada ao Nível Socioeconômico médio das Escolas

Esta é uma análise bivariada muito informativa. Com ela podemos começar a compreender a relação entre origem social e desempenho escolar; em outras palavras, a dimensão socioeconômica das desigualdades educacionais.

Temos duas variáveis: i) proficiência/desempenho médio por Escola; ii) Indicador de nível socioeconômico (INSE)

# Proficiência/desempenho médio por Escola

A proficiência ou desempenho médio por Escola é um indicador importante do aprendizado dos alunos atendidos pela instituição. Trata-se de uma variável métrica contínua que varia na escala SAEB de 100-500. Para uma interpretação pedagógica da distribuição desta variável, consideramos os níveis de desempenho utilizadas na avaliação do Estado de São Paulo (SARESP), conforme indicado por Soares (2018), com as categorias: abaixo do básico, básico, adequado e avançado. Seguindo a escala SAEB, para cada segmento da educação básica há níveis de interpretação pedagógica específicos, tal como nos quadros abaixo.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/niveis_LP.png)
![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/niveis_MT.png)

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/histograms_DesempLP.png?raw=true)
![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/histograms_DesempMT.png?raw=true)

Observamos que a proficiência tem uma distribuição próxima da normal, com parte significativa das escolas concentradas no nível básico em LP e MT. Poucos são os casos de escolas no nível adequado, e ainda menos no avançado.

É possível observar também um pequeno deslocamento dos casos para a direita de 2013-2019, o que representa um aumento em proficiência das escolas em geral. Apenas com a distribuição do desempenho podemos observamos uma a situação sensível em termos de habilidades escolares. São poucos os casos de escolas com desempenho médio no nível adequado, e nenhuma no nível avançado entre 2013 e 2017.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/density_Desemp2013-2019.png?raw=true)

# Indicador de Nível Socioeconômico

O indicador de Nível Socioeconômico (INSE, daqui em diante) é um constructo teórico operacionalizado em uma medida derivada dos itens do questionário socioeconômico dos alunos. Conforme metodologia construída por Alves e Soares (2009, 2011), foi também desenvolvido com base na Teoria de Resposta ao Item (TRI) e está disponibilizado nas bases oficiais do INEP. De acordo com os Alves e Soares (2009), o INSE segue um constructo de medida socioeconômica tradicional no escopo das pesquisas sobre estratificação, e possui correlação forte com outros indicadores como o Índice de Desenvolvimento Humano Municipal (IDHM) e a Renda Domiciliar Per Capita (RDPC) nos municípios. Por meio de uma análise de cluster por método hierárquico, os estudantes foram agrupados em sete níveis ordinais segundo os itens respondidos. Dos sete grupos, derivamos três categorias sintéticas de INSE: i) baixo, que agrega os grupos um e dois; ii) médio, que agrega os grupos três, quatro e cinco e; iii) alto, que agrega os grupos seis e sete.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/barplot_INSE.png)

# Desempenho por Nível Socioeconômico

Agora veremos a correlação entre nível socioeconômico e desempenho médio. Em primeiro lugar, condicionamos a distribuição do desempenho pelo INSE para verificar o padrão de distanciamento ao longo do tempo.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/density_INSExDesemp.png)
![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/density_INSExDesemp-MT.png)

Vemos que há uma diferença significativa entre as distribuições de desempenho condicionadas ao INSE. As curvas de densidade segue o padrão de hierarquia social da condição socioeconômica. Outra representação que auxilia a compreender tal distância é o _boxplot_ ou diagramas de caixa.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/BoxplotLP%20-%20Proficiencia_NSE_13-19.png)
![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/BoxplotMT%20-%20Proficiencia_NSE-13-19.png)

Os diagramas representam na caixa central os três intervalos interquartis (IQR’s) da distribuição de proficiência, isto é, os pontos 25%, 50% (mediana) e 75% da distribuição ordenada. As linhas estendidas fora da caixa representam 1,5 do IQR, e os triângulos representam as observações fora da linha, que estão muito distantes da concentração. As linhas horizontais tracejadas representam a interpretação pedagógica dos níveis de proficiência da escala SAEB. A distribuição da proficiência condicionada ao INSE apresenta uma escada da hierarquia constante entre as Escolas, o que indica o padrão de desigualdades persistentes entre os grupos. Somado ao padrão de desigualdades, estão os casos extremos de Escolas com baixo e médio INSE e com desempenho médio abaixo de 200 (LP) e 225 (MT). Essas Escolas estariam contextualizadas no problema discutido pelos teóricos da reprodução (BOURDIEU; PASSERON, 2018), uma vez que têm refletida na proficiência a condição socioeconômica dos estudantes. Segundo a escala de proficiência definida pelo SAEB, estes estudantes estão em uma condição crítica de aprendizado nas disciplinas de referência, sem o domínio de habilidades elementares, e por isso requerem atenção especial (INEP, 2019). Também chama a atenção as Escolas com resultados acima de 325 (LP) e 350 (MT) nessas mesmas categorias. Essas configuram casos particulares de Escolas que possivelmente obtêm êxito no controle do background socioeconômico dos estudantes, oferecendo condições institucionais de aprendizado independente da origem social, tratados pela literatura sobre Eficácia escolar.

Para analisar com maior detalhe as distâncias entre os grupos de nível socioeconômico nesse período, fizemos uma análise de diferença por pontos percentis da distribuição de proficiência condicionada ao INSE. Calculamos a diferença em cada ponto percentil entre os grupos de INSE alto e médio em relação ao baixo. Com isso, podemos observar não apenas as distâncias em média ou em mediana, mas em cada ponto percentil da distribuição de proficiência de cada grupo marcado pelo INSE.

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/percentis_INSExDesempLP.png)

![alt text:](https://github.com/victorgalcantara/SAEB_educ_inequalities_schools/blob/main/graphs/percentis_INSExDesempMT.png)

# Alunos

Para o nível dos alunos, mobilizamos uma série de indicadores a partir do questionário contextual que preenchem junto com a avaliação nacional. Os indicadores que utilizamos são os capitais econômico e cultural - sendo o primeiro uma _proxy_ do Status Socioeconômico dos alunos -, um indicador de estrutura escolar, a identidade étnico-racial e o sexo - aqui limitado no sexo biológico informado pelo aluno.

## Indicadores

Para viabilizar a mensuração dos capitais a partir da posse dos itens considerados, padronizamos os itens em uma escala de 0-3, em que 0 é o extremo de não possuir nenhum item, e 3 é extremo oposto de possuir mais itens. A partir desta padronização, construímos nossos indicadores com a média de posse dos itens ponderada pelo poder de discriminação de cada item. 

O poder de discriminação dos itens que compõem o indicador de capital econômico foi calculado com base no Índice de Desenvolvimento Humano (IDH) do município em que se encontra a escola. Calculamos a diferença de posse de cada item entre os cinco municípios com maior e os cinco com menor IDH. Consideramos esta diferença como uma expressão da discriminação dos itens. Isso significa que, se a posse dos itens for igual para os municípios com alto e baixo IDH, este item tem baixo poder de discriminação. São cinco categorias de resposta para cada item, portanto, o diferencial de discriminação está entre 0 e 5.
Formalmente, estimamos nossos indicadores por:

<script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML' async></script>

$$ \sum_{k=1}^{N} x_{i}∙∆_{discriminação} $$

x_{i} = item
∆ = poder de discriminação do item
n = total de itens considerados
