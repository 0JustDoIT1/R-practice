#상관계수
x <- c(85,98,70,90,78)
y <- c(92,89,76,88,84)
cor(x,y,method = "spearman") # 델타
cor( x,y, method = "pearson") # 로우
cor(x,y,method = "kendal") #타우
cor.test(x,y) #상관분석
#귀무가설은 상관계수가 0이다.
# 대립가설은 상관계수가 0이 아니다.
# p-value = 0.107: 귀무가설을 기각할 수 없다. 두 데이터는 서로 상관이 없다.

#공분산 = ((xi - xbar) (yi-ybar) + ........... ) / (n-1)
(datax = 1:5)
(datay = 2:6)
var(datax) #2.5
var(datay) #2.5
sum(datax - mean(datax))/ (length(datax) -1)
#공분산.
sum((datax - mean(datax))  * (datay - mean(datay))) / (length(datax) -1 )
# 공분산을 표준편차로 정규화 - > 상관계수.
(sum((datax - mean(datax))  * (datay - mean(datay))) / (length(datax) -1 )) / (sd( datax) * sd( datay))


cov(1:5, 2:6) # covariance 공분산 2.5
cor(1:5, 2:6) # 1 완전 상관/ 1이 아닐 땐 정적 상관.
cor(1:5, c(3,3,3,3,3)) # 표준편차가 0이다 = NA 값. 0 상관이 없음
cor(1:5, 5:1) # 완전 부적 상관

(m <- matrix(c(1:10, (1:10)^2), ncol=2))
print(m)
cor(m, method= "pearson")  # 상관행렬.
cor(m, method = "spearman") # 순서.
cor(m, method = "kendall") #순서쌍( +, -) 0인 경우는 제외.


###
(a<- c(1:10))
(b <- (1:10)^2)
cov(a,b)

#문제 : 공분산을 수식으로 구하시오
res <- sum((a- mean(a) * (b-mean(b))/ (length(a)-1))
           # 문제 : 상관계수를 수식으로 구하시오
           (corelation_val <- res/(sd(a) * sd(b)))
           
           
           
           
           ###
           #상관행렬은 수치데이터 적용.
           library(dplyr)
           data(iris)
           head(iris)
           cor(iris$Sepal.Width, iris$Sepal.Length)
           (m_cor <- cor(iris[,1:4]))
           #install.packages("pheatmap")
           
           library(pheatmap)
           (m_cor <- cor(iris[,1:4]))
           pheatmap(m_cor, cutree_rows = 4) #상관행렬 -color
           #계층적으로 유사한 변수끼리 묶어서 표현하고 있다.
           cor(iris[, -5])
           ir <- iris %>%  select (-(Species)) # Species열만 제외한다.
           cor(ir)
           iris %>%  select(-(Species)) %>%  cor()
           symnum(cor(iris[,1:4]))
           
           
           
           ###
           #피어스 상관계수, 코사인 유사도
           
           #install.packages("lsa")
           library(lsa)
           
           x<- c(2,4,6,8,10)
           y <- c(1,3,5,7,9)
           (pearson_r <- cor(x,y,method = "pearson"))
           (x %*% y ) / (norm(as.matrix(x)) * norm(as.matrix(y))
                         (x %*% y) / (sqrt (sum(x^2)) * sqrt(sum(y^2)))
                         (cos_raw <- cosine(t(rbind(x,y))))
                         
                         
                         ###
                         product <- read.csv("product2.csv", encoding = "UTF-8", header= TRUE)
                         head(product)
                         summary(product)
                         colnames(product)<- c("제품_친밀도", "제품_적절성", "제품_만족도")
                         head(product)
                         cor(product, method="pearson")
                         
                         #제품_만족도와 상관이 높은 것은: 제품_적절성 : 0.8 이상이 되면 - 다중공선성.
                         #비슷한 정보를 담고있다 => 종속변수에 double 영향을 미친다 (이중영향).
                         
                         
                         
                         ###
                         #install.packages("corrgram")
                         library(corrgram)
                         corrgram(product)
                         corrgram(product, upper.panel=panel.conf)
                         
                         #install.packages("PerformanceAnalytics")
                         library(PerformanceAnalytics)
                         chart.Correlation(product, histogram = , pch ="+")
                         
                         
                         
                         ###
                         library(tidyverse)
                         head(airquality)
                         str(airquality)
                         airquality_1 <-
                           airquality %>%
                           select_if(is.numeric)
                         head(airquality_1)
                         airquality_1<-
                           airquality_1 %>%
                           select( - c(Month, Day))
                         cor(airquality_1)
                         (sum(is.na(airquality_1)))
                         (airquality_2 <- na.omit(airquality_1))
                         (airquality_cor <- cor(airquality_2))
                         par(mfrow = c(1,1))
                         
                         
                         install.packages("corrplot")
                         library(corrplot)
                         plot(airquality_2) #일반함수
                         corrplot(airquality_cor, method= "circle")
                         corrplot(airquality_cor, method = "square")
                         col <- colorRampPalette(c("darkblue", "white", "darkorange"))(20)
                         col
                         heatmap(x = airquality_cor, col = col, symm = TRUE)
                         
                         
                         install.packages("psych")
                         library(psych)
                         pairs.panels(airquality_cor, bg=c("red", "yellow","blue"),
                                      pch = 21,
                                      main ="airquality", hist.col= "red")
                         
                         
                         
                         ###
                         #mat
                         a <- c(4.0, 4.2, 3.9, 4.3, 4.1 )
                         b <- c(2.0, 2.1, 2.0, 2.1, 2.2 )
                         c <- c(0.60, 0.59, 0.58, 0.62, 0.63)
                         (mat <- matrix(c(a,b,c ), nrow=5,byrow=F ))
                         cor(mat)#자동으로 상관행렬이 된다. 3*3
                         
                         #eigen 분해
                         (eigvector <- eigen(cor(mat)))
                         names(eigvector)
                         # "values": 고유치
                         # "vectors" : 고유벡터
                         eigvector$values #축 방향으로의 크기를 의미.
                         eigvector$vectors # 내적하려면 0이 되어야함.
                         # 축 : 3개 => 데이터에서 찾은 새로운 축. --> 더 보기 쉽다.
                         
                         eigvector$vectors[,1] %*% eigvector$vectors[,2] #0
                         eigvector$vectors[,1] %*% eigvector$vectors[,3] #0
                         eigvector$vectors[,2] %*% eigvector$vectors[,3] #0
                         eigvector$vectors[1, ] %*% eigvector$vectors[2,] #0
                         eigvector$vectors[1, ] %*% eigvector$vectors[3,] #0
                         eigvector$vectors[2, ] %*% eigvector$vectors[3,] #0
                         
                         #단위행렬.
                         eigvector$vectors %*% t(eigvector$vectors)
                         #대각선이 1이고 나머지는 다 0임 = 정방행렬이면서 직교행렬, 전치행렬이 역행렬임.
                         #전치를 했다 = 역행렬을 했다.
                         #행렬은 나눗셈이 없다 = 역행렬을 구해서 곱해주면 = 그것이 나눗셈 !
                         
                         
                         
                         ###
                         # PCA -> scale 문제
                         # PCA는 분산기반 모델 -> 분산이 큰 것이 중요한 변수.
                         # 데이터 자체의 사이즈가 다르다 : 키(170), 체중(60)
                         
                         #고유값 분해, 특이행렬분해
                         # 고유값 분해를 할 때 -> 정방행렬
                         # 특이행렬분해(비정방, 정방)
                         # SVD(singular value decoposition) 으로 구현 : prcomp
                         
                         str(USArrests) # 4개의 변수
                         head(USArrests)
                         pairs(USArrests, panel=panel.smooth, main="USArrests data")
                         #PCA 분석.
                         prcomp(USArrests)
                         prcomp(USArrests, scale= TRUE) #분산이 중요.
                         prcomp( ~ Murder + Assault + Rape, data= USArrests, scale= TRUE)
                         #표준편차의 크기.
                         plot(prcomp(USArrests))
                         (fit <- prcomp(USArrests, scale = TRUE))
                         names(fit)
                         plot(fit, type="lines")
                         #새로운 축의 표준편차(분산의 크기)
                         #분산의 누적이 전체의 85% 이상이면 종속변수 설명.
                         fit$sdev
                         fit$x
                         fit$center
                         #고유벡터.
                         fit$rotation
                         #biplot
                         #얼마나 중요한 영향을 미치는지 시각화 하는 것.
                         biplot(prcomp(USArrests, scale= TRUE))
                         fit$rotation[, c(1,2,3)] #마지막 축은 제외했다는 것.
                         as.matrix(USArrests) %*% fit$rotation[, c(1,2)]
                         # 새로운 축에 표현된 데이터가 되고 ( noise가 정리된 데이터)
                         
                         
                         
                         
                         ###
                         # install.packages("FactoMineR")
                         # install.packages("factoextra")
                         
                         
                         library(FactoMineR)
                         library(factoextra)
                         data(iris)
                         res.pca <- prcomp(iris[, -5], scale= TRUE)
                         names (res.pca)
                         get_eig(res.pca)
                         fviz_eig(res.pca , addlabels = TRUE, ylim = c(0,85))
                         # elbow line 팔이 뻗어 나가는 것 처럼 생김.
                         fviz_eig(res.pca , geom = "line")
                         fviz_pca_ind(res.pca)
                         fviz_pca_ind(res.pca , label = "none", habillage = iris$Species)
                         # ggplot 기반 변수 대입이 가능.
                         p <- fviz_pca_ind(res.pca, label = "none", habillage = iris$Species,
                                           addEllipses = TRUE, ellipse.level= 0.95)
                         print(p)
                         fviz_pca_var(res.pca)#PCA 했을 때 변수의 영향력( 제 1 주성분과 제 2주성분)
                         
                         
                         
                         ###
                         #문제 :
                         #mtcars 데이터에 대해서 주성분 분석을 하고 biplot으로
                         # 제1 주성분과 제 2 주성분에 미친 영향을 분석해 보시오.
                         
                         str(mtcars)
                         data<- mtcars
                         #종속변수 포함 여부
                         # 일반적으로 종속변수와 독립변수는 상관이 있어야 좋음.
                         # 분석 시에는, 독립변수의 관계 속에서 다중공선성을 찾아야 한다. ( 중요도를 찾아야 한다.)
                         # PCA 주성분 분석
                         # 하는 이유: 차원 축소
                         (pr <- prcomp(mtcars[, -1], scale = TRUE)) # mpg를 제외함.
                         summary(pr)
                         #주성분 선택.
                         screeplot(pr,type = c("barplot", "lines"), max(10, length(pr)))
                         get_eig(pr)
                         pr$x # score 새로운 정직교 축에 데이터를 표현.
                         biplot(pr)
                         
                         num_pcs<-3
                         (pca_data <- pr$x [, 1 : num_pcs]) #주성분 3개.
                         data <- cbind(mtcars$mpg, pca_data) # 4 -> 종속변수 1 + 독립변수 3.
                         str(data)
                         colnames(data) <- c("mpg", " PC1", "PC2", "PC3") # 10차원 -> 3차원원
                         (data <- as.data.frame (data))
                         #PCA변환한 데이터를 이용해서 모델 생성.
                         #새로운 데이터는 반드시 PCA로 변환 후 입력.
                         
                         regression_model <- lm(mpg ~., data= data) #.(점) -> 나머지를 의미.
                         summary(regression_model)
                         
                         #회귀식 만들기.
                         (mpg_hat <- -2.2813 * PC1 + 0.1163 * PC2 + -1.2993 * PC3 + 20.0906)
                         new_data <- data.frame( #예측값 => 주성분 값으로 표현.
                           mpg = c(20.0,15.0),
                           cyl = c(4,8),
                           disp = c(120.0, 380.0),
                           hp = c(100.0, 200.0),
                           drat = c(4.0, 3.0),
                           wt = c(2.5, 4.0),
                           qsec = c(18.0, 16.0),
                           vs = c(0,0),
                           am= c(1,0),
                           gear = c(4,3),
                           carb= c(2,4)
                         )
                         new_data_pcs <- predict ( pr, newdata= new_data) #주성분 값으로 변환.
                         print(new_data_pcs [, 1:3])
                         # 회귀 예측모델 -> PCA 를 이용해서 차원축소 된 데이터 입력력
                         predictes_mpg <- predict(regression_model, mewdata = as.data.frame(new_data_pcs[, 1:3]))
                         
                         
                         ###
                         #위 데이터를 가지고,
                         #차원 축소 후 복원 데이터와 원본의 비교 해보기.
                         
                         # mtcars 중에서 mpg, disp, hp, wt만 골라낸 이유 -> 범주형 데이터 제거.
                         df <- mtcars [ , c("mpg","disp", "hp","wt")]
                         
                         #주성분 분석에서 scale : 분산 사이즈를 동일하게 비교.
                         X <- scale(df)
                         
                         pca <- prcomp(X, center = FALSE, scale. = FALSE)
                         
                         #rotation 의 의미 : 고유벡터.
                         W <- pca$rotation
                         
                         #마지막 2개는 제거.
                         k <- 2
                         
                         Wk <- W [, 1:k]
                         
                         Z <- X %*% Wk # -> 32X4 %*% 4X2 => 32X2 차원축소.
                         X_hat <- Z %*% t(Wk) # 2X4 : 전치는 행과 열을 교환. 32X2 2X4 => 32X4
                         #요소연산
                         (error <- X - X_hat)
                         (mse <- mean(error^2))
                         
                         head(X)
                         head(X_hat)
                         head(error)
                         plot(
                           X[,1],
                           X_hat[,1],
                           xlab ="원본데이터",
                           ylab ="재구성 데이터",
                           main = " Original vs Reconstructed"
                           
                         )
                         abline (0,1, col= "red", lwd =2)
                         # 빨간선 위에 있지 않고 흩어져 있음 = 원본데이터와 재구성 데이터가 다르기때문.
                         
                         
                         
                         
                         
                         
                         ################### FA( factor analysis) ########################
                         library(psych)
                         library(GPArotation)
                         
                         #6개의 변수.
                         v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
                         v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
                         v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
                         v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
                         v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
                         v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
                         
                         #데이터 프레임.
                         (med.data <- cbind (v1,v2,v3,v4,v5,v6))
                         head(med.data)
                         summary(med.data)
                         cor(med.data)
                         
                         #  principal 주성분 분석.
                         med.factor <- principal(med.data, rotate = "none")
                         
                         # "loadings" : 고유치 * 고유벡터 => 고유벡터에 기여한 정도.
                         # "scores" : prcomp.x 고유벡터의 축에 재표현 된 원본 데이터.
                         names(med.factor)
                         
                         op <- par (mfrow = c(1,1))
                         
                         #고유치.
                         plot(med.factor$values, type = "b")
                         
                         #요인분석 : 요인을 2개로 해라.
                         med.Varimax = principal(med.data , nfactors =2, rotate = "varimax")
                         biplot(med.Varimax)
                         
                         # 회전방법 : varimax, oblimin
                         # 회전을 하는 이유
                         # 주성분 분석의 목적 -> 분산 최대화 ( 주성분 추출)
                         # 요인 분석의 목적 -> 범주화 해서 -> 요인 (차원축소)
                         # 고유치 + 고유벡터가 만들어짐.  -> 고유벡터는 직교 함. -> 직교하는 축에 데이터를 표현 -> 데이터와 맞지 않음.
                         stats.fact <- factanal(med.data, factors =3,
                                                rotation = "oblimin", scores = "regression")
                         biplot(stats.fact$scores[, 1:2], stats.fact$loadings[, 1:2])