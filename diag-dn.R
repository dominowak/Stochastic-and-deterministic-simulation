#install.packages('truncnorm')
#library('truncnorm')

konsmed<-function() {
  set.seed(1234)
  c1<-round((sort(c(runif(120,0,180),runif(60,180,360),runif(30,360,480)))),2) #czas przyjœcia posortowany po kolei
  z<-210 #pacjeci
  c2<-round((rtruncnorm(n=z, a=3, b=8, mean=7, sd=3)),2)  #czas wypelniania dokumentow przed badaniem
  c3<-round((rtruncnorm(n=z, a=3, b=10, mean=8, sd=3)),2)   #czas trwania badania
  c4<-c1+c2 #planowane rozpoczêcie badania
  c5<-c4+c3  #czas/godzina zakoñczenia badania/pobytu w przychodni
  c6<-c4-c1    #po jakim czasie od wypelnienia dokumentow rzeczywiste rozpoczêcie badania
  c7<-c5-c1    #ca³kowity czas pobytu w przychodni
  stanowisko<-c(0,0,0) #liczba stanowisk medycznych
  kolejka<-rep(NA,z)   #rzeczywista d³ugoœæ kolejki
  wkons<-rep(NA,z)  #wolni konsultanci medyczni
  
  for (i in 1:z) {
    stanowisko[which.min(stanowisko)]<-c4[i] #rzecziwsty czas rozpoczecia bdadania dla i pacjenta przydzielane jest do stanowsika wedlug pierwszego wolnego stanowiska
    sum(stanowisko<c4[i])->wkons[i] #wolni konsultanci s¹ sum¹ stanowisk, których czas jest mniejszy od czasu rozpocz¹cia badania
    
    if (c4[i]<stanowisko[which.min(stanowisko)]) {
       stanowisko[which.min(stanowisko)]>c4[i]
       c5[i]<-c3[i]+c4[i] 
      }
     if (i<z) {
      kolejka[i]<-sum(c4[(i+1):z]<stanowisko[which.min(stanowisko)])
     }
    else {
      kolejka[i]<-0
      }
  }
  
  #oczekuj¹ca na badanie przez pierwsze 3 godziny d³uzej niz 7 minut
  odp1<-round(sum(c6[1:120]>7),2)
  #sredni czas oczekiwania na badanie przez pierwsze 3 godziny
  odp2<-round(mean(c6[1:120]),2)
  #oczekuj¹ca na badanie przez drugie 3 godziny d³uzej niz 7 minut
  odp3<-round(sum(c6[121:180]>7),2)
  #sredni czas oczekiwania na badanie przez drugie 3 godziny
  odp4<-round(mean(c6[121:180]),2)
  #oczekiwanie na stanowiska przez ostatnie 2 godziny d³u¿ej ni¿ 7 minut
  odp5<-round(sum(c6[181:210]>7),2)
  #sredni czas oczekiwania na badanie przez ostatnie 2 godziny
  odp6<-round(mean(c6[181:210]),2)
  #œredni czas pobytu w przychodni
  odp7<-round(mean(c7),2)
  #srednia liczba osób w kolejce
  odp8<-mean(kolejka)
  #suma ile razy kunsultanci byli wolni przez ca³y dzieñ pracy
  odp9<-sum(wkons)/8
  #srednia liczba wolnych konsultanow w drugiej godzinie pracy 
  odp10<-mean(wkons[60<c4&c4<120])
  #srednia liczba wolnych konsultanow w pi¹tej godzinie pracy 
  odp11<-mean(wkons[240<c4&c4<300])
  #srednia liczba wolnych konsultanow w siódmej godzinie pracy 
  odp12<-mean(wkons[360<c4&c4<420])
  return<-(c(odp1,odp2,odp3,odp4,odp5,odp6,odp7,odp8,odp9,odp10,odp11,odp12))
}

#symulacja przerowadzona dla 8 godzinnego dnia pracy, 5 dni w tygodniu przez 1 miesi¹c -> 4 tygodnie

symulacja<-8*60*5*4
wnk<-matrix(NA,symulacja,12)
for (i in 1:symulacja) wnk[i,]<-konsmed()
wyniki<-apply(wnk, 2, mean)
wynikisym <- data.frame(mierzona_wielkosc = c('Na badanie przez pierwsze 3 godziny, d³uzej niz 7 minut, oczekuje: ',
                                              'Œredni czas oczekiwania na badanie przez pierwsze 3 godziny: ',
                                              'Na badanie przez drugie 3 godziny, d³uzej niz 7 minut, oczekuje: ',
                                              'Œredni czas oczekiwania na badanie przez drugie 3 godziny: ',
                                              'Na badanie przez ostatnie 2 godziny, d³u¿ej ni¿ 7 minut, oczekuje: ',
                                              'Œredni czas oczekiwania na badanie przez ostatnie 2 godziny: ',
                                              'Œredni czas pobytu w przychodni: ',
                                              'Œrednia liczba osób w kolejce: ',
                                              'Suma ile razy kunsultanci byli wolni przez ca³y dzieñ pracy: ',
                                              'Œrednia liczba wolnych konsultanow w drugiej godzinie pracy: ',
                                              'Œrednia liczba wolnych konsultanow w pi¹tej godzinie pracy: ',
                                              'Œrednia liczba wolnych konsultanow w siódmej godzinie pracy: '),
                        wynik_pomiaru = c(odp1,odp2,odp3,odp4,odp5,odp6,odp7,odp8,odp9,odp10,odp11,odp12), 
                        jednostka = c('osób','minuty','osób','minuty','osób','minuty','minuty','osób','[-]','[-]','[-]','[-]'))
wynikisym$wynik_pomiaru<-format(round(wynikisym$wynik_pomiaru,3),nsmall=3)
print(wynikisym)

rys1<-barplot(c6[1:120],xlab='pierwsze 120 pacjentów',ylab='czas oczekiwania pacjentów przez pierwsze 3 godziny [minuty]',names.arg = c(1:120))
rys2<-barplot(c6[121:180],xlab='kolejne 60 pacjentów',ylab='czas oczekiwania pacjentów przez drugie 3 godziny [minuty]',names.arg = c(121:180))
rys3<-barplot(c6[181:210],xlab='ostatnie 30 pacjentów',ylab='czas oczekiwania pacjentów przez ostatnie 2 godziny [minuty]',names.arg = c(181:210))

histogram_nr_1<-hist(c7[1:120],xlab='przedzia³ iloœci ca³kowitego czasu spêdzonego w przychodni [minuty]', ylab='iloœæ osób ')
histogram_nr_2<-hist(c7[121:180],xlab='przedzia³ iloœci ca³kowitego czasu spêdzonego w przychodni [minuty]', ylab='iloœæ osób ')
histogram_nr_3<-hist(c7[181:210],xlab='przedzia³ iloœci ca³kowitego czasu spêdzonego w przychodni [minuty]', ylab='iloœæ osób ')



