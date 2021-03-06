#install.packages('truncnorm')
#library('truncnorm')

konsmed<-function() {
  set.seed(1234)
  c1<-round((sort(c(runif(120,0,180),runif(60,180,360),runif(30,360,480)))),2) #czas przyj�cia posortowany po kolei
  z<-210 #pacjeci
  c2<-round((rtruncnorm(n=z, a=3, b=8, mean=7, sd=3)),2)  #czas wypelniania dokumentow przed badaniem
  c3<-round((rtruncnorm(n=z, a=3, b=10, mean=8, sd=3)),2)   #czas trwania badania
  c4<-c1+c2 #planowane rozpocz�cie badania
  c5<-c4+c3  #czas/godzina zako�czenia badania/pobytu w przychodni
  c6<-c4-c1    #po jakim czasie od wypelnienia dokumentow rzeczywiste rozpocz�cie badania
  c7<-c5-c1    #ca�kowity czas pobytu w przychodni
  stanowisko<-c(0,0,0) #liczba stanowisk medycznych
  kolejka<-rep(NA,z)   #rzeczywista d�ugo�� kolejki
  wkons<-rep(NA,z)  #wolni konsultanci medyczni
  
  for (i in 1:z) {
    stanowisko[which.min(stanowisko)]<-c4[i] #rzecziwsty czas rozpoczecia bdadania dla i pacjenta przydzielane jest do stanowsika wedlug pierwszego wolnego stanowiska
    sum(stanowisko<c4[i])->wkons[i] #wolni konsultanci s� sum� stanowisk, kt�rych czas jest mniejszy od czasu rozpocz�cia badania
    
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
  
  #oczekuj�ca na badanie przez pierwsze 3 godziny d�uzej niz 7 minut
  odp1<-round(sum(c6[1:120]>7),2)
  #sredni czas oczekiwania na badanie przez pierwsze 3 godziny
  odp2<-round(mean(c6[1:120]),2)
  #oczekuj�ca na badanie przez drugie 3 godziny d�uzej niz 7 minut
  odp3<-round(sum(c6[121:180]>7),2)
  #sredni czas oczekiwania na badanie przez drugie 3 godziny
  odp4<-round(mean(c6[121:180]),2)
  #oczekiwanie na stanowiska przez ostatnie 2 godziny d�u�ej ni� 7 minut
  odp5<-round(sum(c6[181:210]>7),2)
  #sredni czas oczekiwania na badanie przez ostatnie 2 godziny
  odp6<-round(mean(c6[181:210]),2)
  #�redni czas pobytu w przychodni
  odp7<-round(mean(c7),2)
  #srednia liczba os�b w kolejce
  odp8<-mean(kolejka)
  #suma ile razy kunsultanci byli wolni przez ca�y dzie� pracy
  odp9<-sum(wkons)/8
  #srednia liczba wolnych konsultanow w drugiej godzinie pracy 
  odp10<-mean(wkons[60<c4&c4<120])
  #srednia liczba wolnych konsultanow w pi�tej godzinie pracy 
  odp11<-mean(wkons[240<c4&c4<300])
  #srednia liczba wolnych konsultanow w si�dmej godzinie pracy 
  odp12<-mean(wkons[360<c4&c4<420])
  return<-(c(odp1,odp2,odp3,odp4,odp5,odp6,odp7,odp8,odp9,odp10,odp11,odp12))
}

#symulacja przerowadzona dla 8 godzinnego dnia pracy, 5 dni w tygodniu przez 1 miesi�c -> 4 tygodnie

symulacja<-8*60*5*4
wnk<-matrix(NA,symulacja,12)
for (i in 1:symulacja) wnk[i,]<-konsmed()
wyniki<-apply(wnk, 2, mean)
wynikisym <- data.frame(mierzona_wielkosc = c('Na badanie przez pierwsze 3 godziny, d�uzej niz 7 minut, oczekuje: ',
                                              '�redni czas oczekiwania na badanie przez pierwsze 3 godziny: ',
                                              'Na badanie przez drugie 3 godziny, d�uzej niz 7 minut, oczekuje: ',
                                              '�redni czas oczekiwania na badanie przez drugie 3 godziny: ',
                                              'Na badanie przez ostatnie 2 godziny, d�u�ej ni� 7 minut, oczekuje: ',
                                              '�redni czas oczekiwania na badanie przez ostatnie 2 godziny: ',
                                              '�redni czas pobytu w przychodni: ',
                                              '�rednia liczba os�b w kolejce: ',
                                              'Suma ile razy kunsultanci byli wolni przez ca�y dzie� pracy: ',
                                              '�rednia liczba wolnych konsultanow w drugiej godzinie pracy: ',
                                              '�rednia liczba wolnych konsultanow w pi�tej godzinie pracy: ',
                                              '�rednia liczba wolnych konsultanow w si�dmej godzinie pracy: '),
                        wynik_pomiaru = c(odp1,odp2,odp3,odp4,odp5,odp6,odp7,odp8,odp9,odp10,odp11,odp12), 
                        jednostka = c('os�b','minuty','os�b','minuty','os�b','minuty','minuty','os�b','[-]','[-]','[-]','[-]'))
wynikisym$wynik_pomiaru<-format(round(wynikisym$wynik_pomiaru,3),nsmall=3)
print(wynikisym)

rys1<-barplot(c6[1:120],xlab='pierwsze 120 pacjent�w',ylab='czas oczekiwania pacjent�w przez pierwsze 3 godziny [minuty]',names.arg = c(1:120))
rys2<-barplot(c6[121:180],xlab='kolejne 60 pacjent�w',ylab='czas oczekiwania pacjent�w przez drugie 3 godziny [minuty]',names.arg = c(121:180))
rys3<-barplot(c6[181:210],xlab='ostatnie 30 pacjent�w',ylab='czas oczekiwania pacjent�w przez ostatnie 2 godziny [minuty]',names.arg = c(181:210))

histogram_nr_1<-hist(c7[1:120],xlab='przedzia� ilo�ci ca�kowitego czasu sp�dzonego w przychodni [minuty]', ylab='ilo�� os�b ')
histogram_nr_2<-hist(c7[121:180],xlab='przedzia� ilo�ci ca�kowitego czasu sp�dzonego w przychodni [minuty]', ylab='ilo�� os�b ')
histogram_nr_3<-hist(c7[181:210],xlab='przedzia� ilo�ci ca�kowitego czasu sp�dzonego w przychodni [minuty]', ylab='ilo�� os�b ')



