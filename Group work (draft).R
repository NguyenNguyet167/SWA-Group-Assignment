# ================================================================
# COMP3020 SOCIAL WEB ANALYTICS - COMPLETE PROJECT R SCRIPT
# Topic: FIFA World Cup 2026 (Bluesky Data)
# Author: Nguyễn Minh Nguyệt & Nguyễn Phúc Gia Thuỵ
# ================================================================

# ================================================================
# SETUP: INSTALL & LOAD PACKAGES
# ================================================================
needed <- c('dplyr', 'ggplot2', 'lubridate', 'httr', 'jsonlite', 'tidytext', 'stringr', 'tidyr', 'atrrr', 'igraph', 'Matrix', 'cluster')
for (p in needed) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidytext)
library(stringr)
library(tidyr)
library(atrrr)
library(igraph)

# ================================================================
# QUESTION 1: TREND & TIME SERIES ANALYSIS
# ================================================================

# 1. Read dataset
df <- read.csv(file.choose(), stringsAsFactors = FALSE)
df$date <- as.Date(df$created_at)

# a) Subtopic Classification (VAR & Refereeing)
keywords <- "var|referee|ref|penalty|foul|red card|yellow card"
df$subtopic <- ifelse(grepl(keywords, df$text, ignore.case = TRUE), "Subtopic", "No-Subtopic")
print(table(df$subtopic))

# b) Chi-square Test (Before vs After July 19, excluding event day)
df_test <- df %>% 
  filter(date != as.Date("2026-07-19")) %>% 
  mutate(period = ifelse(date < as.Date("2026-07-19"), "Before", "After"))

contingency_tab <- table(df_test$subtopic, df_test$period)
cat("Contingency Table:\n")
print(contingency_tab)

chisq_res <- chisq.test(contingency_tab)
cat("Chi-Square Test Results:\n")
print(chisq_res)

# c) Tính Twitter's Chi-square (Log-Likelihood ratio G^2) ---
row_totals <- rowSums(contingency_tab)
col_totals <- colSums(contingency_tab)
total <- sum(contingency_tab)
expected <- outer(row_totals, col_totals) / total

# Công thức Log-Likelihood chuyên dụng cho văn bản (Twitter's Chi-square)
twitter_chisq <- 2 * sum(contingency_tab * log(contingency_tab / expected), na.rm = TRUE)
cat("Twitter's Chi-square score (G^2):", twitter_chisq, "\n")

# d) Linear Regression Model on Daily Post Counts
daily_counts <- df %>% 
  filter(!is.na(date)) %>% 
  count(date, name = "post_count")

daily_counts$day_numeric <- as.numeric(daily_counts$date - min(daily_counts$date, na.rm = TRUE)) + 1

model <- lm(post_count ~ day_numeric, data = daily_counts)
cat("Linear Regression Summary:\n")
print(summary(model))

# Plot Linear Trend
p1 <- ggplot(daily_counts, aes(x = date, y = post_count)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Daily Post Counts for FIFA World Cup 2026",
       x = "Date", y = "Number of Posts") +
  theme_minimal()
ggsave("Q1_Linear_Trend.png", p1, width = 8, height = 5)
cat("Saved Q1_Linear_Trend.png\n")


# ================================================================
# QUESTION 2: TEXT MINING & TOPIC DISCOVERY
# ================================================================

# --- Q2(a): Identify Top 3 Posts (Cập nhật để in đầy đủ Text, URL, Likes, Reposts) ---
cat("\n--- Running Q2(a): Identify Top 3 Posts ---\n")

dat <- df
# Kiểm tra định dạng ngày của cột created_at
dat$date_str <- substr(as.character(dat$created_at), 1, 10)

# Lọc theo khung thời gian yêu cầu (13-18 và 20-25 tháng 7, loại bỏ 19/07)
q2 <- dat[dat$date_str >= '2026-07-13' & dat$date_str <= '2026-07-25' &
            dat$date_str != '2026-07-19' & !is.na(dat$date_str), , drop = FALSE]

# Kiểm tra xem có dòng dữ liệu nào thỏa mãn không
cat("Số lượng bài post tìm thấy trong khoảng thời gian lọc:", nrow(q2), "\n")

if(nrow(q2) > 0) {
  q2$reply_count <- suppressWarnings(as.numeric(q2$reply_count))
  q2$like_count <- suppressWarnings(as.numeric(q2$like_count))
  q2$repost_count <- suppressWarnings(as.numeric(q2$repost_count))
  
  # Xếp hạng theo reply, nếu trùng reply thì xét đến likes rồi reposts
  q2 <- q2[order(-q2$reply_count, -q2$like_count, -q2$repost_count, na.last = TRUE), , drop = FALSE]
  top3 <- head(q2, 3)
  top3$thread_id <- paste0('Thread_', seq_len(nrow(top3)))
  
  # Tạo hàm lấy URL chuẩn của Bluesky post
  post_url <- function(uri, handle) {
    if (is.na(uri) || !nzchar(uri) || is.na(handle) || !nzchar(handle)) return(NA_character_)
    paste0('https://bsky.app/profile/', handle, '/post/', sub('^.*/', '', uri))
  }
  top3$post_url <- mapply(post_url, top3$uri, top3$author_handle, USE.NAMES = FALSE)
  
  # --- IN KẾT QUẢ RA CONSOLE THEO ĐÚNG YÊU CẦU ĐỀ BÀI ---
  for(i in seq_len(nrow(top3))) {
    cat(paste0("\n--------------------------------------------------\n"))
    cat(paste0(">>> ", top3$thread_id[i], " <<<\n"))
    cat(paste0("Author Handle : ", top3$author_handle[i], "\n"))
    cat(paste0("Post URL      : ", top3$post_url[i], "\n"))
    cat(paste0("Reply Count   : ", top3$reply_count[i], "\n"))
    cat(paste0("Like Count    : ", top3$like_count[i], "\n"))
    cat(paste0("Repost Count  : ", top3$repost_count[i], "\n"))
    cat(paste0("Post Text     : ", top3$text[i], "\n"))
  }
  cat(paste0("\n--------------------------------------------------\n"))
} else {
  cat("LỖI: Không tìm thấy bài post nào trong khoảng thời gian trên! Hãy kiểm tra lại định dạng cột 'created_at' trong file CSV của nhóm.\n")
}

# Fetch replies via public Bluesky API
api <- 'https://public.api.bsky.app/xrpc/app.bsky.feed.getPostThread'
val <- function(x, fallback = NA_character_) {
  if (is.null(x) || length(x) == 0) return(fallback)
  as.character(x[[1]])
}

walk_replies <- function(node, thread_id, root_uri, root_url) {
  output <- list()
  visit <- function(n) {
    if (is.null(n$replies) || length(n$replies) == 0) return(invisible(NULL))
    for (child in n$replies) {
      if (is.null(child$post)) next
      p <- child$post
      uri <- val(p$uri)
      handle <- val(p$author$handle)
      output[[length(output) + 1L]] <<- data.frame(
        thread_id = thread_id, root_uri = root_uri, root_url = root_url,
        reply_uri = uri, reply_url = post_url(uri, handle),
        author_handle = handle, text = val(p$record$text),
        created_at = val(p$record$createdAt),
        like_count = suppressWarnings(as.numeric(val(p$likeCount))),
        stringsAsFactors = FALSE
      )
      visit(child)
    }
    invisible(NULL)
  }
  visit(node)
  if (length(output)) dplyr::bind_rows(output) else NULL
}

replies_list <- list()
for (i in seq_len(nrow(top3))) {
  id <- top3$thread_id[i]
  uri <- top3$uri[i]
  cat('Fetching', id, ':', uri, '\n')
  result <- tryCatch({
    response <- httr::GET(api, query = list(uri = uri, depth = 1000, parentHeight = 0), httr::timeout(30))
    httr::stop_for_status(response)
    jsonlite::fromJSON(httr::content(response, as = 'text', encoding = 'UTF-8'), simplifyVector = FALSE)
  }, error = function(e) e)
  
  if (!inherits(result, 'error') && !is.null(result$thread$post)) {
    rr <- walk_replies(result$thread, id, uri, top3$post_url[i])
    if (!is.null(rr) && nrow(rr)) replies_list[[id]] <- rr
  }
  Sys.sleep(1)
}

replies <- if (length(replies_list)) dplyr::bind_rows(replies_list) else data.frame()
if (nrow(replies)) replies <- dplyr::distinct(replies, thread_id, reply_uri, .keep_all = TRUE)
write.csv(replies, 'Q2_All_Replies.csv', row.names = FALSE, fileEncoding = 'UTF-8')
cat("Saved Q2_All_Replies.csv with", nrow(replies), "replies.\n")

# Tiền xử lý văn bản & tính tần suất từ
clean_replies <- replies %>%
  filter(!is.na(text), nzchar(trimws(text))) %>%
  mutate(clean_text = str_to_lower(text),
         clean_text = str_replace_all(clean_text, 'https?://\\S+', ' '),
         clean_text = str_replace_all(clean_text, '@[a-zA-Z0-9._-]+', ' '),
         clean_text = str_replace_all(clean_text, '[^a-z\\s]', ' '),
         clean_text = str_squish(clean_text))

tokens <- clean_replies %>%
  select(thread_id, clean_text) %>%
  unnest_tokens(word, clean_text) %>%
  anti_join(tidytext::stop_words, by = 'word') %>%
  filter(str_detect(word, '^[a-z]{2,}$'),
         !word %in% c('amp', 'http', 'https', 'www', 'bsky', 'com', 'rt'))

word_freq <- tokens %>% count(thread_id, word, name = 'frequency') %>% arrange(thread_id, desc(frequency))
top15_words <- word_freq %>% group_by(thread_id) %>% slice_max(frequency, n = 15, with_ties = FALSE) %>% ungroup()
write.csv(top15_words, 'Q2_Top15_Word_Frequencies.csv', row.names = FALSE)

# Vẽ biểu đồ Top 15 từ vựng
p_freq <- top15_words %>%
  mutate(word = reorder_within(word, frequency, thread_id)) %>%
  ggplot(aes(x = word, y = frequency)) +
  geom_col(fill = '#3273a8') +
  scale_x_reordered() + coord_flip() +
  facet_wrap(~thread_id, scales = 'free_y') +
  labs(title = 'Top 15 words in replies for each thread', x = 'Word', y = 'Frequency') +
  theme_minimal()
ggsave('Q2_Top15_Word_Frequencies.png', p_freq, width = 12, height = 8, dpi = 300)

print(p_freq)

# ================================================================
# QUESTION 3: CLUSTERING & VISUALISATION
# ================================================================

# --- Q3(a): Corpus Preparation & TF-IDF Vectorization ---
cat("\n--- Running Q3(a): TF-IDF Vectorization ---\n")
replies_q3 <- replies %>% mutate(doc_id = row_number(), text = as.character(text))

cleaned_q3 <- replies_q3 %>%
  filter(!is.na(text), nzchar(trimws(text))) %>%
  mutate(clean_text = str_to_lower(text),
         clean_text = str_replace_all(clean_text, 'https?://\\S+', ' '),
         clean_text = str_replace_all(clean_text, '@[a-zA-Z0-9._-]+', ' '),
         clean_text = str_replace_all(clean_text, '[^a-z\\s]', ' '),
         clean_text = str_squish(clean_text))

tokens_q3 <- cleaned_q3 %>%
  select(doc_id, clean_text) %>%
  unnest_tokens(word, clean_text) %>%
  anti_join(tidytext::stop_words, by = 'word') %>%
  filter(str_detect(word, '^[a-z]{2,}$'),
         !word %in% c('amp', 'http', 'https', 'www', 'bsky', 'com', 'rt'))

tfidf <- tokens_q3 %>%
  count(doc_id, word, name = 'n') %>%
  bind_tf_idf(word, doc_id, n) %>%
  filter(tf_idf > 0)

dtm <- tfidf %>% cast_sparse(doc_id, word, tf_idf)
mat <- as.matrix(dtm)
row_norms <- sqrt(rowSums(mat^2))
mat <- mat / row_norms
valid_ids <- as.integer(rownames(mat))


# --- Q3(b): Optimal K Selection via Average Silhouette Width ---
cat("\n--- Running Q3(b): Silhouette Width K Selection ---\n")
set.seed(42)
max_k <- min(6L, nrow(mat) - 1L)
distances <- dist(mat, method = 'euclidean')
ks <- 2:max_k
sil_scores <- sapply(ks, function(k) {
  fit <- kmeans(mat, centers = k, nstart = 50, iter.max = 100)
  if (length(unique(fit$cluster)) < 2) return(NA_real_)
  mean(cluster::silhouette(fit$cluster, distances)[, 'sil_width'])
})
k_selection <- data.frame(K = ks, avg_silhouette = sil_scores)
print(k_selection)
best_k <- k_selection$K[which.max(k_selection$avg_silhouette)]
cat("Selected K:", best_k, "\n")


# --- Q3(c): K-means Clustering Model ---
cat("\n--- Running Q3(c): K-Means Clustering ---\n")
set.seed(42)
final_fit <- kmeans(mat, centers = best_k, nstart = 100, iter.max = 100)
assignments <- data.frame(doc_id = valid_ids, cluster = factor(final_fit$cluster)) %>%
  left_join(replies_q3 %>% select(doc_id, thread_id, text), by = 'doc_id')
print(table(assignments$cluster))


# --- Q3(d): PCA 2D Dimensionality Reduction & Visualization ---
cat("\n--- Running Q3(d): PCA Visualization ---\n")
pca <- prcomp(mat, center = TRUE, scale. = FALSE)
coords <- data.frame(doc_id = valid_ids, PC1 = pca$x[, 1], PC2 = pca$x[, 2]) %>%
  left_join(assignments, by = 'doc_id')

var_explained <- 100 * (pca$sdev^2 / sum(pca$sdev^2))
p_cluster <- ggplot(coords, aes(x = PC1, y = PC2, color = cluster, shape = thread_id)) +
  geom_point(size = 3, alpha = 0.85) +
  labs(title = 'Clustering of Bluesky Replies (TF-IDF + K-means)',
       subtitle = paste('K =', best_k, '| Colours = clusters; shapes = original threads'),
       x = sprintf('PC1 (%.1f%% variance)', var_explained[1]),
       y = sprintf('PC2 (%.1f%% variance)', var_explained[2]),
       color = 'Cluster', shape = 'Original thread') +
  theme_minimal()
ggsave('Q3_Cluster_PCA.png', p_cluster, width = 10, height = 7, dpi = 300)


# --- Q3(e): Cluster vs. Original Thread Cross-Tabulation & Interpretation ---
cat("\n--- Running Q3(e): Cross-Tabulation & Interpretation ---\n")
cross_tab <- as.data.frame.matrix(table(assignments$thread_id, assignments$cluster))
print(cross_tab)
write.csv(cross_tab, 'Q3_Thread_Cluster_Table.csv', row.names = TRUE)

# ================================================================
# QUESTION 4: NETWORK ANALYSIS (BLUESKY)
# ================================================================
cat("\n--- RUNNING QUESTION 4 ---\n")

# Authenticate Bluesky (replace with your handle and app password)
auth(user = "mnguyet05.bsky.social", password = "niwh-auko-ivtu-geoy")

# Identify top 3 users by post count in dataset
top3 <- df %>% count(author_handle, sort = TRUE) %>% head(3)
print(top3)
target_users <- top3$author_handle

edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

for (user in target_users) {
  message("Fetching network for: ", user)
  
  # Fetch followers
  tryCatch({
    foll <- get_followers(user, limit = 50)
    if (nrow(foll) > 0) {
      edges <- rbind(edges, data.frame(from = foll$actor_handle, to = user))
    }
  }, error = function(e) message("Error fetching followers for ", user))
  Sys.sleep(3)
  
  # Fetch follows
  tryCatch({
    following <- get_follows(user, limit = 50)
    if (nrow(following) > 0) {
      edges <- rbind(edges, data.frame(from = user, to = following$actor_handle))
    }
  }, error = function(e) message("Error fetching following for ", user))
  Sys.sleep(3)
}

edges <- edges %>% filter(!is.na(from) & !is.na(to))
net <- graph_from_data_frame(edges, directed = TRUE)

# Save Network Plot
png("Q4_Network_Graph.png", width = 800, height = 800)
plot(net, 
     vertex.size = 3, 
     vertex.label = ifelse(V(net)$name %in% target_users, V(net)$name, NA),
     vertex.label.color = "red",
     vertex.label.cex = 1.2,
     edge.arrow.size = 0.3,
     main = "Bluesky Network: Top 3 Active Users")
dev.off()
cat("Saved Q4_Network_Graph.png\n")

# Centrality Metrics
in_deg <- degree(net, mode = "in")
out_deg <- degree(net, mode = "out")
pr <- page_rank(net)$vector

network_stats <- data.frame(
  User = V(net)$name,
  InDegree = in_deg,
  OutDegree = out_deg,
  PageRank = pr
)

cat("\nTop 1 In-Degree:\n"); print(network_stats %>% arrange(desc(InDegree)) %>% head(1))
cat("Top 1 Out-Degree:\n"); print(network_stats %>% arrange(desc(OutDegree)) %>% head(1))
cat("Top 1 PageRank:\n"); print(network_stats %>% arrange(desc(PageRank)) %>% head(1))

