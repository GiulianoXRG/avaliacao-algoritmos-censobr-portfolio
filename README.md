# censobr: o Censo em poucas linhas de R

Apresentação do pacote R **censobr** para a disciplina de Algoritmos (Ciências Atuariais, UFRN).

**Autores:** Giuliano Carvalho e Gabriel

## Arquivos

| Arquivo | O que é |
|---|---|
| `censobr_piramide_RN.R` | Script principal: pirâmide etária do RN com dados do Censo 2022 |
| `apresentacao/Apresentacao_censobr_v2.pptx` | Slides da apresentação (versão atual) |
| `apresentacao/Apresentacao_censobr_v2_reserva.pdf` | Cópia em PDF dos slides, para usar se o PowerPoint falhar |

## Como rodar

1. Instale os pacotes:

   ```r
   install.packages(c("censobr", "dplyr", "ggplot2"))
   ```

2. Coloque o arquivo `microdados_RN_2022.zip` na mesma pasta do script.
3. Abra `censobr_piramide_RN.R` e rode seção por seção.
4. Cada gráfico fecha ao apertar **Enter** no console.

> **Os microdados não estão no repositório.** São microdados de acesso controlado do IBGE e arquivos grandes; o `.gitignore` bloqueia `*.zip` e `*.parquet`.
