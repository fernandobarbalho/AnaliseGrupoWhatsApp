library(dplyr)
library(tidytext)
library(wordcloud)
library(tidyr)
library(scales)
library(ggplot2)
library(igraph)
library(ggraph)
library(widyr)

funcao_contabil<- PCASP_trabalho[,c(1,15,10)]

texto_df <- dplyr::data_frame(classe = funcao_contabil$CLASSE,conta= funcao_contabil$NOME_COMPLETO, texto =funcao_contabil$FUNÇÃO)


analise_contabil<-texto_df %>% unnest_tokens(palavra, texto)

analise_contabil <- analise_contabil %>%
                      anti_join(data_frame(palavra = stopwords::stopwords("pt")))

analise_contabil %>% count(palavra, sort = TRUE)

analise_contabil %>%
  count(palavra, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(palavra = reorder(palavra, n)) %>%
  ggplot(aes(palavra, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

analise_contabil %>%
  count(palavra)%>%
  with(wordcloud(palavra,n,max.words = 100))


frequencia<-analise_contabil %>% 
  count(classe,palavra) %>%
  group_by(classe) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(classe, proportion) %>%
  gather(classe, proportion,'2':'8' )


ggplot(frequencia, aes(x = proportion, y = `1`, color = abs(`1` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = palavra), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~classe, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "1", x = NULL)


texto_df <- dplyr::data_frame(classe = funcao_contabil$CLASSE,conta= funcao_contabil$NOME_COMPLETO, texto =funcao_contabil$FUNÇÃO)

analise_contabil <- texto_df %>%
  unnest_tokens(palavra,texto) %>%
  count(classe, palavra, sort = TRUE) %>%
  ungroup()

total_palavras <- analise_contabil %>%
          group_by(classe) %>%
  summarize(total=sum(n))


analise_contabil <- left_join(analise_contabil, total_palavras)


ggplot(analise_contabil, aes(n/total, fill = classe)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~classe, ncol = 2, scales = "free_y")

freq_by_rank <- analise_contabil %>%
            group_by(classe) %>%
            mutate(rank = row_number(),
            'frequencia_termo' = n/total)

freq_by_rank %>% 
  ggplot(aes(rank, `frequencia_termo`, color = classe)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(frequencia_termo) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, frequencia_termo, color = classe)) + 
  geom_abline(intercept = -0.1899, slope = -1.2858, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

analise_contabil <- analise_contabil %>%
  bind_tf_idf(palavra, classe, n)

analise_contabil

analise_contabil %>%
  select(-total) %>%
  arrange(desc(tf_idf))


analise_contabil %>%
    arrange(desc(tf_idf)) %>%
    mutate(palavra = factor(palavra, levels = rev(unique(palavra)))) %>% 
    group_by(classe) %>% 
    top_n(15) %>% 
    ungroup %>%
    ggplot(aes(palavra, tf_idf, fill = classe)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~classe, ncol = 2, scales = "free") +
    coord_flip()

stop_words_pcasp <- unique(c(unique(analise_contabil$palavra[analise_contabil$idf==0]), stopwords::stopwords("pt")))

analise_contabil_bigrams <- dplyr::data_frame(classe = funcao_contabil$CLASSE, texto =funcao_contabil$FUNÇÃO) %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2)

analise_contabil_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- analise_contabil_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words_pcasp) %>%
  filter(!word2 %in% stop_words_pcasp)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


analise_contabil_secoes <- dplyr::data_frame(classe = funcao_contabil$CLASSE, texto =funcao_contabil$FUNÇÃO) %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_pcasp)

# count words co-occuring within sections
word_pairs <- analise_contabil_secoes %>%
  pairwise_count(word, section, sort = TRUE)


# we need to filter for at least relatively common words first
word_cors <- analise_contabil_secoes %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors %>%
  filter(item1 %in% c("patrimonial", "orçamento", "fiscal", "seguridade")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)

word_cors %>%
  filter(correlation > .65) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


library(dplyr)

x <- c(5, 1, 3, 2, 2, NA)
row_number(x)
min_rank(x)
dense_rank(x)
percent_rank(x)
cume_dist(x)


fab <- dplyr::data_frame(classe = funcao_contabil$CLASSE, texto =funcao_contabil$FUNÇÃO) %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stop_words_pcasp)

