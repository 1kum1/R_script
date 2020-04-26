# 傾向スコアを用いたマッチング

library(tidyverse)

メモ
・デフォルトマッチングの仕様
　傾向スコアによるマッチングの際は、傾向スコアの近さだけ見るので距離の定義の違いは特に気にせずによさそう
・1対1マッチングのRスクリプト
・1対1マッチングのシード固定方法
・1対1マッチングのアルゴリズム

・IPW推定量による因果効果推定のRスクリプト
・IPW推定量による因果効果推定のアルゴリズム


# 参考情報 --------------------------------------------------------------------
# 統計的因果推論に関するスライドとRのサンプルコード
# http://kamonohashiperry.com/archives/531

# 岩波DS vol3 サポートページ、サンプルコード
# https://sites.google.com/site/iwanamidatascience/vol-3/vol3-ingasuiron
# https://github.com/iwanami-datascience/vol3

# 高知工科大学
# https://yukiyanai.github.io/jp/classes/econometrics2/contents/R/matching.html

#Matchingパッケージを呼び出す
library(Matching)

# サンプルデータを呼び出す
# データの説明
# https://www.rdocumentation.org/packages/Matching/versions/4.9-6/topics/lalonde
data(lalonde)

#データの確認
head(lalonde)

#ロジスティック回帰モデルを用いて傾向スコアを推定する。
#re78(78年実質賃金)に対する効果を調べたいので、説明変数から除外しておく。
#treatはCMなどを見せたかどうかのバイナリーデータ。
logi <- glm(treat~., data=lalonde[, -9], family = binomial)

#因果効果を計算するために、マッチングを推定する。
#今回の効果を見るためのre78をYに指定して、CMなどを見せたかどうかのtreatをTrに指定する。
#共変量としてロジスティック回帰の予測値logi$fittedを用いている。
nsw1 <- Match(Y = lalonde$re78, Tr=lalonde$treat, X =logi$fitted)

summary(nsw1)

#マッチングのペアを確認する。
lalonde2 <- lalonde
lalonde2$id <- 1:nrow(lalonde2)
lalonde2$score <- logi$fitted

pair.df <- cbind(lalonde2[nsw1$index.treated, c("id","score")],
                 lalonde2[nsw1$index.control, c("id", "score")])

names(pair.df) <- c("t.id", "t.score", "c.id", "c.score")

head(pair.df)

# マッチングに用いられたユニークなサンプルの確認
# treated
# idごとに、マッチングに使われている回数を確認
ggplot(data = pair.df, aes(x = t.id)) + 
  geom_histogram()

# 重複して使われている回数の分布を確認
pair.df.treated <- pair.df %>% 
  dplyr::group_by(t.id) %>% 
  dplyr::summarise(count = n())
ggplot(data = pair.df.treated, aes(x = count)) + 
  geom_histogram()
summary(pair.df.treated$count)


# control
# idごとに、マッチングに使われている回数を確認
ggplot(data = pair.df, aes(x = c.id)) + 
  geom_histogram()

# 重複して使われている回数の分布を確認
pair.df.control <- pair.df %>% 
  dplyr::group_by(c.id) %>% 
  dplyr::summarise(count = n())
ggplot(data = pair.df.control, aes(x = count)) + 
  geom_histogram()
summary(pair.df.control$count)



#キャリパーマッチング（ペアが特定の距離以上になる時はマッチングしないマッチング）
nsw2 <- Match(Y =lalonde2$re78, Tr=lalonde2$treat, X =logi$fitted, caliper = T)
summary(nsw2)








