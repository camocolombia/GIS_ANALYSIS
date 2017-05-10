require(stringdist);require(psych)

a<-"United States of America"
b<-"United States of Mexico"
c<-"United States"
d<-"United States of America"
e<-"USA"

stAB<-stringsim(a, b, method=c("jw"),p=0.1);stAB2<-stringsim(a, b, method=c("soundex"));
stAB3<-stringsim(a, b, method=c("jaccard"),q=2);stAB4<-stringsim(a, b, method=c("dl"));
stAB5<-stringsim(a, b, method=c("lv"));stAB6<-stringsim(a, b, method=c("osa"));
stAB7<-stringsim(a, b, method=c("lcs"));stAB8<-stringsim(a, b, method=c("cosine"),q=2);
stAB9<-stringsim(a, b, method=c("qgram"),q=2);stAB10<-stringsim(a, b, method=c("jw"));
# 1-
stMEDIAN_AB<-median(c(stAB,stAB2,stAB3,stAB4,stAB5,stAB6,stAB7,stAB8,stAB9,stAB10))
stMEAN_AB<-mean(c(stAB,stAB2,stAB3,stAB4,stAB5,stAB6,stAB7,stAB8,stAB9,stAB10))


stAC<-stringsim(a, c,method=c("jw"), p=0.2);stAC2<-stringsim(a, c, method=c("soundex"));
stAC3<-stringsim(a, c, method=c("jaccard"),q=2);stAC4<-stringsim(a, c, method=c("dl"));
stAC5<-stringsim(a, c, method=c("lv"));stAC6<-stringsim(a, c, method=c("osa"));
stAC7<-stringsim(a, c, method=c("lcs"));stAC8<-stringsim(a, c, method=c("cosine"),q=2);
stAC9<-stringsim(a, c, method=c("qgram"),q=2);stAC10<-stringsim(a, c, method=c("jw"));

# 1-
stMEDIAN_AC<-median(c(stAC,stAC2,stAC3,stAC4,stAC5,stAC6,stAC7,stAC8,stAC9,stAC10))
stMEAN_AC<-mean(c(stAC,stAC2,stAC3,stAC4,stAC5,stAC6,stAC7,stAC8,stAC9,stAC10))


stAD<-stringsim(a, d,method=c("jw"), p=0.2);stAD2<-stringsim(a, d, method=c("soundex"));
stAD3<-stringsim(a, d, method=c("jaccard"),q=2);stAD4<-stringsim(a, d, method=c("dl"));
stAD5<-stringsim(a, d, method=c("lv"));stAD6<-stringsim(a, d, method=c("osa"));
stAD7<-stringsim(a, d, method=c("lcs"));stAD8<-stringsim(a, d, method=c("cosine"),q=2);
stAD9<-stringsim(a, d, method=c("qgram"),q=2);stAD10<-stringsim(a, d, method=c("jw"));
# 1-
stMEDIAN_AD<-median(c(stAD,stAD2,stAD3,stAD4,stAD5,stAD6,stAD7,stAD8,stAD9,stAD10))
stMEAN_AD<-mean(c(stAD,stAD2,stAD3,stAD4,stAD5,stAD6,stAD7,stAD8,stAD9,stAD10))



stBE<-stringsim(b, e,method=c("jw"), p=0.2);stBE2<-stringsim(b, e, method=c("soundex"));
stBE3<-stringsim(b, e, method=c("jaccard"),q=2);stBE4<-stringsim(b, e, method=c("dl"));
stBE5<-stringsim(b, e, method=c("lv"));stBE6<-stringsim(b, e, method=c("osa"));
stBE7<-stringsim(b, e, method=c("lcs"));stBE8<-stringsim(b, e, method=c("cosine"),q=2);
stBE9<-stringsim(b, e, method=c("qgram"),q=2);stBE10<-stringsim(b, e, method=c("jw"));

# 1-
stMEDIAN_BE<-median(c(stBE,stBE2,stBE3,stBE4,stBE5,stBE6,stBE7,stBE8,stBE9,stBE10))
stMEAN_BE<-mean(c(stBE,stBE2,stBE3,stBE4,stBE5,stBE6,stBE7,stBE8,stBE9,stBE10))


stCE<-stringsim(c, e,method=c("jw"), p=0.2);stCE2<-stringsim(c, e, method=c("soundex"));
stCE3<-stringsim(c, e, method=c("jaccard"),q=2);stCE4<-stringsim(c, e, method=c("dl"));
stCE5<-stringsim(c, e, method=c("lv"));stCE6<-stringsim(c, e, method=c("osa"));
stCE7<-stringsim(c, e, method=c("lcs"));stCE8<-stringsim(c, e, method=c("cosine"),q=2);
stCE9<-stringsim(c, e, method=c("qgram"),q=2);stCE10<-stringsim(c, e, method=c("jw"));

# 1-
stMEDIAN_CE<-median(c(stCE,stCE2,stCE3,stCE4,stCE5,stCE6,stCE7,stCE8,stCE9,stCE10))
stMEAN_CE<-mean(c(stCE,stCE2,stCE3,stCE4,stCE5,stCE6,stCE7,stCE8,stCE9,stCE10))







quantile((st_F[1,5:14]))


st_F<-as.data.frame(matrix(ncol = 14,nrow = 5))

colnames(st_F)<-c("STRING_A","STRING_B","MEDIAN_SIM","MEAN_SIM","Jaro-Winkiler","Soundex","Jaccard","Damerau-Levensthein","Levenshtein","Optimal String Alignment","Longest common substring","Cosine","Q-gram","Jaro")

st_F[1,1]<-a;st_F[1,2]<-b
st_F[2,1]<-a;st_F[2,2]<-c
st_F[3,1]<-a;st_F[3,2]<-d
st_F[4,1]<-b;st_F[4,2]<-e
st_F[5,1]<-c;st_F[5,2]<-e


st_F[1,3]<-stMEDIAN_AB;st_F[1,4]<-stMEAN_AB
st_F[2,3]<-stMEDIAN_AC;st_F[2,4]<-stMEAN_AC
st_F[3,3]<-stMEDIAN_AD;st_F[3,4]<-stMEAN_AD
st_F[4,3]<-stMEDIAN_BE;st_F[4,4]<-stMEAN_BE
st_F[5,3]<-stMEDIAN_CE;st_F[5,4]<-stMEAN_CE


st_F[1,5]<-stAB;st_F[1,6]<-stAB2;st_F[1,7]<-stAB3;st_F[1,8]<-stAB4;st_F[1,9]<-stAB5;st_F[1,10]<-stAB6;st_F[1,11]<-stAB7;st_F[1,12]<-stAB8;st_F[1,13]<-stAB9;st_F[1,14]<-stAB10;
st_F[2,5]<-stAC;st_F[2,6]<-stAC2;st_F[2,7]<-stAC3;st_F[2,8]<-stAC4;st_F[2,9]<-stAC5;st_F[2,10]<-stAC6;st_F[2,11]<-stAC7;st_F[2,12]<-stAC8;st_F[2,13]<-stAC9;st_F[2,14]<-stAC10
st_F[3,5]<-stAD;st_F[3,6]<-stAD2;st_F[3,7]<-stAD3;st_F[3,8]<-stAD4;st_F[3,9]<-stAD5;st_F[3,10]<-stAD6;st_F[3,11]<-stAD7;st_F[3,12]<-stAD8;st_F[3,13]<-stAD9;st_F[3,14]<-stAD10
st_F[4,5]<-stBE;st_F[4,6]<-stBE2;st_F[4,7]<-stBE3;st_F[4,8]<-stBE4;st_F[4,9]<-stBE5;st_F[4,10]<-stBE6;st_F[4,11]<-stBE7;st_F[4,12]<-stBE8;st_F[4,13]<-stBE9;st_F[4,14]<-stBE10
st_F[5,5]<-stCE;st_F[5,6]<-stCE2;st_F[5,7]<-stCE3;st_F[5,8]<-stCE4;st_F[5,9]<-stCE5;st_F[5,10]<-stCE6;st_F[5,11]<-stCE7;st_F[5,12]<-stCE8;st_F[5,13]<-stCE9;st_F[5,14]<-stCE10


