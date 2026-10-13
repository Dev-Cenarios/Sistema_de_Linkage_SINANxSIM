# Sistema de Linkage SINAN + SIM

Aplicação em R/Shiny que realiza vinculação de registros (*record linkage*) entre o **SINAN** (Sistema de Informação de Agravos de Notificação) e o **SIM** (Sistema de Informação sobre Mortalidade), identificando quais casos notificados evoluíram para óbito. O usuário carrega os arquivos das duas bases (CSV ou Excel), a aplicação padroniza e limpa os dados, executa o linkage em 15 passagens sucessivas dentro de um banco [DuckDB](https://duckdb.org/) em memória e devolve as tabelas resultantes para download em um único ZIP. Pares de baixa certeza ("zona cinza") são apresentados em uma aba de revisão manual, onde o usuário confirma se cada par é ou não a mesma pessoa. A ferramenta é de uso transversal: dá suporte à vinculação de bases usada tanto em análises de tuberculose quanto de hanseníase dentro do Programa Cenários.

### [▶ Acessar o sistema](https://cenarios.unb.br/sistema-de-linkage-sinan-sim/)

O sistema está em funcionamento na página do Programa Cenários, onde pode ser usado direto no navegador. As bases enviadas são processadas num banco em memória, descartado ao fim da sessão (ver [Fonte de dados](#3-fonte-de-dados)).

> É a mesma lógica de linkage que o projeto usa para corrigir o desfecho dos casos e trazer os óbitos por TB sem notificação no painel [SINAN Territórios: Recife-PE](https://github.com/Dev-Cenarios/SINAN_Territorios_Recife_PE_TB). Veja também o [Sistema de Georreferenciamento SINAN × CNEFE](https://github.com/Dev-Cenarios/Sistema_de_Georreferenciamento_Geocenarios).

## Sobre o Projeto

O Programa Cenários, coordenado pelo Prof. Dr. Walter Massa Ramalho da Universidade de Brasília (UnB), é uma iniciativa científica e tecnológica que reúne pesquisadores, gestores de saúde e parceiros nacionais e internacionais para desenvolver soluções inovadoras no enfrentamento de doenças determinadas socialmente. Atualmente, o programa é composto por dois grandes eixos de pesquisa:

### 1. Hanseníase – Modelagem de Cenários de Risco

Cenários Hanseníase tem como objetivo identificar áreas de risco e cadeias invisíveis de transmissão da hanseníase no Brasil, com foco especial nas regiões metropolitanas de Brasília (DF), Recife (PE), Palmas (TO) e demais capitais. A proposta integra inteligência artificial generativa, georreferenciamento por setores censitários e rastreamento de contatos. Os resultados incluem um dashboard nacional, algoritmos de análise espacial e temporal, e relatórios técnicos para subsidiar ações estratégicas do SUS, com foco na redução de desigualdades e modernização da vigilância.

### 2. Tuberculose – Cenários de Risco e Determinantes Sociais

Cenários TB visa apoiar a eliminação da tuberculose como problema de saúde pública no Brasil e em países lusófonos africanos e latino-americanos. A pesquisa combina modelagem epidemiológica, vinculação de bases de dados (linkage) e estratificação de cenários para avaliar a situação dos municípios e orientar ações prioritárias. Entre as entregas previstas estão toolkits com árvores de decisão e IA, dashboards interativos, matrizes de indicadores e metodologias de microplanejamento integradas à atenção primária. Também serão promovidas parcerias com municípios e capacitações para gestores de saúde.

Este sistema — o **Sistema de Linkage SINAN + SIM** — é uma ferramenta transversal aos dois eixos, com foco na vinculação probabilística/determinística entre notificações (SINAN) e óbitos (SIM), etapa comum às análises tanto de tuberculose quanto de hanseníase.

[Saiba mais sobre o projeto!](https://cenarios.unb.br)

### Abordagem Comum

Ambos os projetos compartilham princípios e tecnologias:

* Uso de dados sociais, econômicos e epidemiológicos para subsidiar decisões.
* Georreferenciamento de casos para detalhamento espacial.
* Análise temporal para identificar tendências e rupturas.
* Ferramentas digitais abertas para garantir transparência e replicabilidade.
* Interação com gestores e oficinas de validação para alinhar resultados às necessidades locais.

Essa atuação integrada fortalece o SUS, promove equidade em saúde e posiciona o Brasil na vanguarda da aplicação de ciência de dados e inteligência artificial em saúde pública.

## Sumário

1. [Funcionalidades](#1-funcionalidades)
2. [Estrutura do projeto](#2-estrutura-do-projeto)
3. [Fonte de dados](#3-fonte-de-dados)
4. [Tecnologias e pré-requisitos](#4-tecnologias-e-pré-requisitos)
5. [Instalação](#5-instalação)
6. [Troubleshooting](#6-troubleshooting)
7. [Desenvolvedores](#7-desenvolvedores)
8. [Licença](#licença)

## 1. Funcionalidades

- **Upload de bases SINAN e SIM** (`.csv` ou `.xlsx`, até 500 MB cada) — ver [`app.R`](app.R), painel lateral.
- **Pré-processamento automático**: padronização de datas em múltiplos formatos, limpeza de nomes (acentos, preposições, termos como "IGNORADO"/"DESCONHECIDO"), padronização de sexo (M/F) e cálculo de idade — função `preprocessar_dados()` em [`app.R`](app.R).
- **Deduplicação da base SINAN** por similaridade de nome (Jaro-Winkler) antes do linkage — ver [`script_linkage_duckdb.txt`](script_linkage_duckdb.txt).
- **Linkage em 15 passagens sucessivas** dentro do DuckDB, de chaves mais restritivas (nome completo + nome da mãe + data de nascimento) até combinações mais flexíveis (fragmentos de nome, sexo, data de nascimento, bairro); a cada passagem os registros já pareados saem da fila e o restante segue adiante — ver [`script_linkage_duckdb.txt`](script_linkage_duckdb.txt).
- **Revisão manual da "zona cinza"** (fase 15): pares encontrados apenas por alta similaridade de nome (Jaro-Winkler > 0,97) com mesmo sexo e data de nascimento são exibidos em tabela paginada para o usuário marcar MATCH/NÃO MATCH — aba "Revisão manual" em [`app.R`](app.R).
- **Log de execução em tempo real**, com tempo gasto em cada etapa (leitura, pré-processamento, SQL) — aba "Resumo" em [`app.R`](app.R).
- **Download em ZIP** de todas as tabelas resultantes (bases limpas, deduplicada, duplicados, pares de revisão e linkage final) — botão "Baixar todos os arquivos" em [`app.R`](app.R).

## 2. Estrutura do projeto

```text
Sistema_de_Linkage_SINANxSIM/
├── app.R                       # Aplicação Shiny: UI, limpeza/pré-processamento dos dados e orquestração do linkage no DuckDB
├── script_linkage_duckdb.txt   # Script SQL executado no DuckDB: limpeza avançada, deduplicação e as 15 passagens de linkage
├── www/
│   └── cenarios_logo_full.jpeg # Logo do Programa Cenários exibida no cabeçalho da aplicação
├── LICENSE, LICENSE.md         # Licença MIT
└── README.md
```

O repositório não segue a estrutura de pacote R (sem `DESCRIPTION`, `NAMESPACE` ou pasta `R/`): é um app Shiny de arquivo único (`app.R`) mais um script SQL externo, executado em tempo de uso.

## 3. Fonte de dados

- **SINAN** (Sistema de Informação de Agravos de Notificação): casos notificados, com campos como número de notificação, data de notificação, dados do paciente e da mãe, bairro e endereço de residência.
- **SIM** (Sistema de Informação sobre Mortalidade): declarações de óbito, com data e causa básica do óbito, dados do falecido e endereço.
- **Granularidade**: registro individual (nível de caso/óbito).
- **Periodicidade**: não há agendamento automático ou conexão fixa a uma base externa — cada execução processa os arquivos que o usuário carrega manualmente na sessão.
- **Restrições**: as bases contêm dados pessoais sensíveis (nome, nome da mãe, endereço), sujeitos à LGPD. Nenhum dado é distribuído junto ao repositório; o processamento ocorre em banco DuckDB em memória, descartado ao final da sessão (`dbdir = ":memory:"`, com desconexão explícita ao término do processamento).

## 4. Tecnologias e pré-requisitos

- **[R](https://www.r-project.org/) / [Shiny](https://shiny.posit.co/)** — não há `DESCRIPTION` nem `renv.lock` no repositório, portanto a versão do R não está fixada.
- **Pacotes R**, carregados via `pacman::p_load()` e `library()` em [`app.R`](app.R) (sem versões fixadas):
  `tidyverse`, `lubridate`, `readr`, `readxl`, `stringr`, `tidyr`, `digest`, `stringi`, `base64enc`, `glue`, `DBI`, `duckdb`, `DT`, `shinythemes`, `shinyjs`, `data.table` (leitura de CSV), `zip` (geração do ZIP de saída).
- **[DuckDB](https://duckdb.org/)** — motor analítico embutido em que roda o script SQL de linkage. O script não usa extensões do DuckDB: a `postgres_scanner` está desativada no início de [`script_linkage_duckdb.txt`](script_linkage_duckdb.txt), porque os dados chegam por upload e a instalação da extensão travava o processamento em servidores sem acesso ao repositório de extensões do DuckDB. O script limita o DuckDB a 16 GB de memória e 2 threads (`PRAGMA` no início do arquivo).
- **Pré-requisitos de ambiente**: não há `Dockerfile`, `.env.example` nem variáveis de ambiente documentadas no repositório — a aplicação não depende de banco de dados externo nem de credenciais de API para rodar.

## 5. Instalação

```r
# 1. Clonar o repositório
git clone https://github.com/Dev-Cenarios/Sistema_de_Linkage_SINANxSIM.git
cd Sistema_de_Linkage_SINANxSIM

# 2. Instalar as dependências (não há renv.lock; instalação manual via pacman)
install.packages("pacman")
pacman::p_load(tidyverse, lubridate, readr, readxl, stringr, tidyr, digest,
                stringi, base64enc, glue, DBI, duckdb, DT,
                shinythemes, shinyjs, data.table, zip)

# 3. Rodar a aplicação a partir da raiz do repositório
shiny::runApp(".")
```

Ao carregar os arquivos SINAN e SIM na interface e clicar em "Preparar Dados", a aplicação lê `script_linkage_duckdb.txt` do diretório de trabalho — por isso é necessário executar o `runApp()` a partir da raiz do repositório clonado.

## 6. Troubleshooting

- **"Erro ao processar os dados" na aba Resumo**: a mensagem traz o erro original do R. Um formato de data fora do esperado é causa comum: a aplicação avisa quando mais de 5% das datas de nascimento (`DTNASC` no SIM, `DT_NASC` no SINAN) viram `NA` na conversão — confira o log de execução para identificar o campo.
- **Upload de arquivo grande falha**: o limite de upload é de 500 MB por arquivo (`shiny.maxRequestSize`, no início do `app.R`); para bases maiores, aumente esse valor antes de rodar a aplicação.
- **Falta de memória em bases muito grandes**: o script limita o DuckDB a 16 GB de memória e 2 threads (`PRAGMA memory_limit` e `PRAGMA threads`, no início de `script_linkage_duckdb.txt`); ajuste ao servidor.

## 7. Desenvolvedores

- **Eduardo Schneider Arruda** — colaborador do Projeto Cenários, Engenheiro de Computação. Autor e mantenedor do sistema de linkage; trabalha à frente do georreferenciamento usado nos painéis. Contato: geoprocenarios@gmail.com

## Licença

[MIT](LICENSE.md) — © 2026 Projeto Cenários.
