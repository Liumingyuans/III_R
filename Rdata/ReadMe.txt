Rdata��Ƨ��̳��O�Ҧ����R�̷|�Ψ쪺data frame

1. data.Rda : �qMySQL��select�X�Ӹg�L�z�諸���

ex:�z��D����, ���׼�>25�����~, ���褣����na, ����������0 ,����>10�����, �Yrandom forest.R ���U���o�y code select�X�Ӫ�data
data = dbGetQuery(con, "SELECT * FROM prodinfo p INNER JOIN finaleff f ON p.proid = f.proid where count>25 and skin!='na' and score>0 and DATE NOT LIKE '%����%' and price>10;")

2. data_all.Rda : �qMySQL��select�X���褣�Ona�B����>0�����
data_all = dbGetQuery(con, "SELECT * FROM prodinfo p INNER JOIN finaleff f ON p.proid = f.proid where skin!='na' and score>0;")

3. effset.Rda : �Ndata.Rda����Ƹg�L����ഫ��z��һݪ�column�Ӱ�������R��data frame, �Yrandom forest.R ���U���o�y code select�X�Ӫ�data

effset = subset(data,select=c(proid,protype,price,skintype,age.deg,score,A:AD,AE:BG))
