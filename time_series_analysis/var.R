setwd("./github/time_series_analysis")


# 参考 ----------------------------------------------------------------------
# サポートページ
# https://logics-of-blue.com/tsa-ssm-book-support/
# 
# サンプルコード
# https://github.com/logics-of-blue/book-tsa-ssm-foundation/tree/master/book-data



# library -----------------------------------------------------------------
library(urca)
library(fpp)
library(vars)
library(ggplot2)
library(ggfortify)


# 分析の対象となるデータ -----------------
# 四半期ごとのアメリカの個人消費・個人収入の増加率データ
usconsumption

# 図示
autoplot(usconsumption, facets = T)

# 単位根検定
# 消費の単位根検定
# 帰無仮説:単位根あり
summary(ur.df(usconsumption[, "consumption"], type = "drift"))
# type = "drift"で定数項のあるADF検定
# 統計量が棄却点を超えるので、単位根なし

# 収入の単位根検定
summary(ur.df(usconsumption[, "income"], type = "drift"))
# 統計量が棄却点を超えるので、単位根なし

# 相互相関係数
# データのラグを取りつつ、2つの変数の相関係数を順に求めていったもの
autoplot(
  ccf(
    usconsumption[, "consumption"],
    usconsumption[, "income"],
    plot = F
  )
)
# 同時点から前後5時点ほどの間に高い相関がみられる


# RによるVARモデル -----------------
# 次数の選択
# ARIMAモデルと同じくAICが最小となる次数を選択する
select_result <- VARselect(usconsumption, lag.max = 10, type="const")
# lag.max = 10で10次までを順に調べていく
# type="const"で定数項のあるVARモデルを対象としている
select_result

# AICが最小となる次数の確認
select_result$selection[1]

# モデル化
var_bestorder <- VAR(
  y = usconsumption, #データ
  type = "const", # 定数項あり
  p = select_result$selection[1] # 次数
)

summary(var_bestorder)
# 最後に残差の分散共分散行列と相関行列が出力されている
# VARモデルは同時刻における残差同士に相関を持つことがある


# VARモデルによる予測 -----------------
predict(var_bestorder, n.ahead = 4) # 4時点先までの予測

# 予測結果の図示
# 8時点先までの予測結果
autoplot(
  predict(var_bestorder, n.ahead = 8), 
  #ts.colour = 1,
  #predict.colour = 1, 
  predict.linetype = 'dashed'
)


# Grangerの因果性検定 -----------------
# 収入が消費に与える影響
causality(var_bestorder, cause = "income")
# $GrangerがGranger因果性検定の結果
# p-value=0.212であり、p値が0.05よりも大きいのでGrangerの因果があるとは言えないという結果
# $InstantはGrangerの瞬時因果性と呼ばれる因果の検定結果
# 同時刻における収入と消費の影響を検定したもの
# こちらはp-value=8.285e-05であり、瞬時因果性があるという結果
# 残差同士には関連性があるということ

# 消費が収入に与える影響
causality(var_bestorder, cause = "consumption")
# Grangeの因果性もGrangerの瞬時因果性もともに有意

# Grangerの因果は「消費→収入」方向に存在していることが示された
# その逆は有意なGrangeの因果が見られなかった


# インパルス応答関数 -----------------
# インパルス応答関数を求める
# 消費が急に増える(1時点進むインパクトを受ける)と、収入はどのくらいラグをが開いた後に増減するのかを調べる
irf_consumption <- irf(
  var_bestorder, # 推定されたVARモデル
  impulse = "consumption", # 変動元となる変数
  response = c("consumption", "income"), # 変化の結果をみたい変数
  n.ahead = 12, # 何時点先までをシミュレートするのか
  boot = T # ブートストラップによる信頼区間を表示するのか
)
# デフォルトで ortho=T となっており、「直交化インパルス応答関数」を求めるという意味


# インパルス応答関数の図示
plot(irf_consumption)

# 分散分解
# 相手のデータの影響をどれほど受けているかを調べる
plot(fevd(var_bestorder, n.ahead = 12))
# 収入は、最初は消費の影響が少ないものの、1年ほど経つと25%ほどの影響を受けていることがわかる



