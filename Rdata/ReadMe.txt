Rdata資料夾裡都是所有分析裡會用到的data frame

1. data.Rda : 從MySQL中select出來經過篩選的資料

ex:篩選非停產, 評論數>25的產品, 膚質不等於na, 評分不等於0 ,價格>10的資料, 即random forest.R 中下面這句 code select出來的data
data = dbGetQuery(con, "SELECT * FROM prodinfo p INNER JOIN finaleff f ON p.proid = f.proid where count>25 and skin!='na' and score>0 and DATE NOT LIKE '%停產%' and price>10;")

2. data_all.Rda : 從MySQL中select出膚質不是na且評分>0的資料
data_all = dbGetQuery(con, "SELECT * FROM prodinfo p INNER JOIN finaleff f ON p.proid = f.proid where skin!='na' and score>0;")

3. effset.Rda : 將data.Rda的資料經過資料轉換後篩選所需的column來做後續分析的data frame, 即random forest.R 中下面這句 code select出來的data

effset = subset(data,select=c(proid,protype,price,skintype,age.deg,score,A:AD,AE:BG))
