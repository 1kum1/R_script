# 時系列分析用
# 整理できたらgithubに上げておく

setwd("/Users/ikumi/Dropbox/R_script/github/time_series_analysis")


# 参考 ----------------------------------------------------------------------
# サポートページ
# https://logics-of-blue.com/tsa-ssm-book-support/
# 
# サンプルコード
# https://github.com/logics-of-blue/book-tsa-ssm-foundation/tree/master/book-data


# library -----------------------------------------------------------------
library(forecast)
library(xts)
library(ggplot2)
library(ggfortify)
library(urca) # 単位根検定(KPSS)に使う
library(tseries)


# 時系列データ ts型 --------------------------------------------------------------
ts_sample <- ts(1:36, start=c(2000,1), freq=12) #2000年1月始まり。freq=12で月単位のデータ
ts_sample

# 四半期データ
ts_freq4 <- ts(c(1,4,7,3,9,2,5,3), start=c(2000,1), freq=4)
ts_freq4

# 行名と列名を指定してmatrixを作成
mat_with_name <- matrix(
  c(1,2,3,4,5,6,7,8),
  ncol = 4,
  byrow = T,
  dimnames = list(c("row1","row2"), c("col1","col2","col3","col4"))
)
mat_with_name

# 多変量時系列データ
# 多変量時系列では引数にmatrixやdataframeを指定する。列名があればそのまま引き継がれる
ts_multi <- ts(mat_with_name, start=c(2000,1), freq=12)
ts_multi

# 要素の抽出
# window関数で特定の期間だけ抽出できる
window(ts_freq4, start=c(2000,2), end=c(2001,1))

# R組み込みのデータ
# イギリスの交通事故死傷者数
head(Seatbelts[,], n = 3)

# 特定の列のみ抽出
Seatbelts[, "front"]

# 複数の列を抽出
Seatbelts[, c("front", "PetrolPrice")]

# 特定の月のみ抽出
subset(Seatbelts[, "front"], month = 3)


# 時系列データ xts型 --------------------
# ts型は便利だが日単位のデータが扱いにくい欠点がある
# xts型データは行名として日付を指定したmatrixを引数に入れるのが楽
xts_sample <- as.xts(matrix(
  c(1,2,3,4,5),
  dimnames = list(
    c("2000-01-01","2000-01-02","2000-01-03","2000-01-04","2000-01-05")
  ),
  ncol = 1
))
xts_sample

# 要素の抽出
xts_sample["2000-01-01"]

# ある日付以降
xts_sample["2000-01-02::"]

# 範囲指定
xts_sample["2000-01-02::2000-01-04"]


# ファイルからのデータ読み込み --------------------
file_data <- read.csv("input/5-2-1-timeSeries.csv")
file_data

## その他のファイル読み込み方法
# 対話形式でファイルを選ぶ
file_data_2 <- read.csv(file.choose())
# Excelで開いたデータをクリップボードから読み込む
file_data_3 <- read.delim("clipboard")

# data.frame型で読み込まれている
class(file_data)

# xtsへの変換
# いったんread.zooをかませてから読み込む
file_data_xts <- as.xts(
  read.zoo(file_data)
)
file_data_xts


# 図示 --------------------
plot(
  Seatbelts[, "front"],
  main="イギリスの交通事故死傷者数(前席)",
  xlab="年",
  ylab="死傷者数"
)


# ggplot2を使った図示
autoplot(
  Seatbelts[, "front"],
  main="イギリスの交通事故死傷者数(前席)",
  xlab="年",
  ylab="死傷者数"
)


# 単位根検定 --------------------
# KPSS検定の実行
# 単位根がないという帰無仮説を調べる
summary(ur.kpss(log(Seatbelts[, "front"])))

# 差分をとる回数を調べる
# forecastのndiffs関数を使うと差分を取る回数を調べられる
ndiffs(log(Seatbelts[, "front"]))



# ■■■RによるARIMAモデル■■■ ------------------------------------------------------------
# 分析の対象となるデータ -----------------
# R組み込みのデータ
# イギリスの交通事故死傷者数
Seatbelts

# 前方座席の死傷者数のみ抽出
front <- Seatbelts[, "front"]
front

# 個数や人数といったデータは対数変換してからモデル化するとうまくいく傾向がある らしい
# 対数変換 -----------------
# 対数系列
log_front <- log(front)

# 図示
# {forecast}の関数。データ系列と相関係数(ACF)、偏相関係数(PACF)のコレログラムが作成できる
ggtsdisplay(log_front, main="対数系列")


# 差分系列の作成方法 -----------------
# ラグをとる方法
# 原系列
front
# ラグをとった
# 時点が1つ未来にずれる
lag(front, -1)

# 差分系列
#ラグをとったデータを原系列から引く
front - lag(front, -1)

# diff関数を使う方法
diff(front, lag=1)

# 対数差分系列
log_diff <- diff(log_front)

# 図示
ggtsdisplay(log_diff, main="対数差分系列")


# 季節差分系列の作成方法 -----------------
# 参考 季節ごとのグラフ
ggsubseriesplot(front)

# 頻度
# 1年におけるデータの頻度を確認
frequency(front)
diff(front, lag=frequency(front))

# 対数差分系列に、さらに季節差分をとる
seas_log_diff <- diff(log_diff, lag=frequency(log_diff))

# 図示
ggtsdisplay(seas_log_diff, main="季節差分系列")
# 12ヶ月単位の自己相関が大きく残ったままとなっている
# 季節階差をとったからといって、季節の影響をすべて取り除けるわけではない とのこと


# 自己相関とコレログラム -----------------
# 自己相関の数値そのものを得たい時
acf(seas_log_diff, plot=F, lag.max=12)
pacf(seas_log_diff, plot=F, lag.max=12)


# コレログラム
autoplot(
  acf(seas_log_diff, plot=F), 
  main="対数系列のコレログラム"
)


# 訓練データとテストデータに分ける -----------------
# 対数変換
Seatbelts_log <- Seatbelts[,c("front", "PetrolPrice", "law")]
Seatbelts_log[,"front"] <- log(Seatbelts[,"front"])
Seatbelts_log[,"PetrolPrice"] <- log(Seatbelts[,"PetrolPrice"])
Seatbelts_log

# 訓練データとテストデータに分ける
head(Seatbelts_log)
train <- window(Seatbelts_log, end=c(1983,12))
test <- window(Seatbelts_log, start=c(1984,1))

# 説明変数だけ切り出す
petro_law <- train[, c("PetrolPrice", "law")]


# ARIMAモデルの推定 -----------------
model_sarimax <- Arima(
  y = train[, "front"], # 応答変数の指定
  order = c(1, 1, 1), # SARIMAの(p,d,q)(P,D,Q)における(p,d,q)の次数を設定
  seasonal = list(order = c(1, 0, 0)), # 季節成分(P,D,Q)の次数を設定
  xreg = petro_law # 説明変数の指定
)
# なお、この段階での次数は暫定的なものである

model_sarimax


# 差分系列とARIMAの次数の関係 -----------------
# 差分系列
# ARIMA(1,0,0)
Arima(
  y = log_diff, order =c(1, 0, 0),
  include.mean = F # 定数項を含めない
)
# 差分系列に対してARIMA(1,0,0)を推定したもの
# これは、実質ARIMA(1,1,0)

Arima(
  y = log_front, order =c(1, 1, 0)
)

# 季節差分系列
# 対数差分系列に対して更に季節差分を入れたデータにARIMA(1,0,0)を適用
Arima(
  y = seas_log_diff, order =c(1, 0, 0),
  include.mean = F
)
# このモデルは実質ARIMA(1,1,0)(0,1,0)である

Arima(
  y = log_front, order =c(1, 1, 0),
  seasonal = list(order = c(0, 1, 0))
)


# 自動モデル選択auto.arima関数 -----------------

# ◇◇◇◇注意◇◇◇◇
# 以下の関数を実行する前にお使いのPCのコア数を確認してください。
# num.coresの値は、お使いのPCに合わせて変更してください
sarimax_petro_law <- auto.arima(
  y = train[, "front"], 
  xreg = petro_law,
  ic = "aic", # AICを使ってモデル選択するという指定
  max.order = 7, # SARIMA(p,d,q)(P,D,Q)におけるp+q+P+Qの最大値 ここが大きいほど複雑なモデルを探索することになる
  stepwise = F, # 計算量をケチらない
  approximation = F, # 計算量をケチらない
  parallel = T, # 並列化演算を実行。計算に時間がかかる場合はTが良い
  num.cores = 2 #自分のPCのコア数
)
# 5minかかる

# bestモデル
sarimax_petro_law


# 定常性・反転可能性のチェック -----------------
# AR項
abs(polyroot(c(1,-coef(sarimax_petro_law)[c("ar1", "ar2")])))
# MA項
abs(polyroot(c(1,coef(sarimax_petro_law)[c("ma1", "ma2", "ma3")])))
# Seasonal AR項
abs(polyroot(c(1,-coef(sarimax_petro_law)[c("sar1","sar2")])))


# 残差のチェック -----------------

# 残差の自己相関のチェック
# 帰無仮説は「残差に自己相関がない」
checkresiduals(sarimax_petro_law)

# 残差の正規性の検定
# 帰無仮説は「正規分布に従う」
# {tseries}
jarque.bera.test(resid(sarimax_petro_law))


# ARIMAによる予測 -----------------
petro_law_test <- test[, c("PetrolPrice", "law")]
sarimax_f <- forecast(
  sarimax_petro_law, 
  xreg = petro_law_test, # 外生変数。法律が施行されたかどうかの01
  h = 12, # 12時点先まで予測する
  level = c(95, 70) # 95%, 70%予測区間も併せて出力する
)
sarimax_f
autoplot(sarimax_f, predict.colour=1, main = "ARIMAによる予測")
# この予測には「将来の石油価格がわかっている前提で予測を出している」という問題がある

# 過去の石油価格の平均値を使う
petro_law_mean <- data.frame(
  PetrolPrice=rep(mean(train[, "PetrolPrice"]),12),
  law=rep(1, 12)
)
sarimax_f_mean <- forecast(sarimax_petro_law, xreg=petro_law_mean)
autoplot(sarimax_f_mean, predict.colour=1, main = "ARIMAによる予測")

# 直近の石油価格を使う
petro_law_tail <- data.frame(
  PetrolPrice=rep(tail(train[, "PetrolPrice"], n=1),12),
  law=rep(1, 12) # 法律が施行されたかどうかの01データ
)
sarimax_f_tail <- forecast(sarimax_petro_law, xreg=petro_law_tail)
autoplot(sarimax_f_tail, predict.colour=1, main = "ARIMAによる予測")


# ナイーブ予測 -----------------
# 過去の平均値を予測値として使う
# {forecast}meanf()
naive_f_mean <- meanf(train[, "front"], h = 12) # hは予測時点。今回は12時点先まで予測するということ
naive_f_mean
mean(train[, "front"])

# 過去の最新の値を、予測値として使う
# {forecast}rwf()
naive_f_latest <- rwf(train[, "front"], h = 12) # hは予測時点。今回は12時点先まで予測するということ
naive_f_latest
tail(train[, "front"], n = 1)


# 予測の評価 -----------------

# SARIMXのRMSE
sarimax_rmse <- sqrt(
  sum((sarimax_f$mean - test[, "front"])^2) / 
    length(sarimax_f$mean)
)

sarimax_rmse


# 未来の石油価格がわかっている前提の予測
accuracy(sarimax_f, x=test[, "front"])
# Training setが訓練データの当てはめ精度
# Test setがテストデータの予測精度

# テストデータのRMSEのみを抽出
accuracy(sarimax_f, x=test[, "front"])["Test set", "RMSE"]



# 石油価格の平均値を使用
accuracy(sarimax_f_mean, x=test[, "front"])["Test set", "RMSE"]

# 直近の石油価格を使用
accuracy(sarimax_f_tail, x=test[, "front"])["Test set", "RMSE"]


# ナイーブ予測① 過去の平均値
accuracy(naive_f_mean, x=test[, "front"])["Test set", "RMSE"]

# ナイーブ予測①  直近の値
accuracy(naive_f_latest, x=test[, "front"])["Test set", "RMSE"]

# 石油価格の平均値を使用した予測値のRMSEがナイーブ予測を上回っているので、過去実績をそのまま使うよりは良い予測と言えそう







