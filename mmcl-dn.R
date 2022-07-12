  #install.packages('truncnorm')
  #library('truncnorm')

konsmed2<-function() {
  set.seed(1234)
  c1<-round((sort(c(runif(120,0,180),runif(60,180,360),runif(30,360,480)))),2) #czas przyjœcia posortowany po kolei
  z<-210 #pacjeci

  c2<-round((rtruncnorm(n=z, a=3, b=8, mean=7, sd=3)),2)  #czas wypelniania dokumentow przed badaniem
  c3<-round((rtruncnorm(n=z, a=3, b=10, mean=8, sd=3)),2)   #czas trwania badania
 
  #liczba konsultantów medycznych
  c<-3
  #miejsca w poczekalni (za³o¿enie o nieskoñczonoej d³ugosci kolejki - liczba 40 jest wystarczaj¹ca)
  l<-z-c
  
  d<-c+l+1
  Q <- c()
  
  #intensywnosc naplywu zgloszen uœredniona na 8h dzieñ pracy
  lambda<-rep((z/8),d)
  #intensywnoœæ obs³ugi zg³oszenia - tutaj wypenianie dokumentów i badanie to obs³uga zg³oszenia
  ui<-c(c3+c2,d)
  #obciazenie systemu
  roo<-c()
  
  for (i in 1:d){
    roo[i]<-(lambda[i]/ui[i])
  }
  ro<-rep(mean(roo),d)
  
  #Q
  for (i in 1:d) {
    if (i-1<=1){
      
      Q[i] = 1
      
      
    }  
    else if (i<=c){ 
      Q[i] = c(ro[i]^(i-1)/factorial(i-1))
      
    } 
    
    else {
      Q[i] = c(((c^c)/factorial(c))*((ro[i]/c)^(i-1)))
      
    }
  }
  
  #Wyznaczenie prawdopodobien´stw stanów
  p <- c()
  for (i in 1:d) {
    
    if (i == 1) {
      p[i]=(1/(sum(Q[i:d])))
      
    }
    else {
      p[i] = (p[1]*Q[i])
      
    }
  }
  
  #œrednia liczba zg³oszeñ w kolejce
  sumv <-c()
  for (i in 1:(d-1)) {
    if (i > c){
      sumv[i] =  (i*p[i+1])
    }
  }
  
  sumav<-sumv[-(1:(c))]
  v<-sum(sumav)
  
  
  #œrednia liczba zg³oszeñ na stanowisku obs³ugi
  suml1 <-c()
  suml2 <-c()
   
  for (i in 1:d){
    if (i<c){
      suml1[i] = (p[i]*(i-1))
    }
  }
 
   for (i in 1:d){
    if (i>=c) {
      suml2[i] =  (c*p[i])
    }
  }
  
  ll1<-suml1[-1]
  ll2<-sum(suml2[-(1:(c-1))])
  
  lz<-sum(p[2]+ll1+ll2)
 
  #Œrednia liczba zg³oszeñ w systemie
   sumn <-c()
  for (i in 1:(length(p)-1)) {
    sumn[i] =  (i*p[i+1])
  }
  
  n<-sum(sumn)
  
  #œrednie obci¹¿enie systemu
  odp1<-mean(ro[1:d])
  #Prawdopodobienstwo, ¿e 3 konsultantów bêdzie zajêtych a kolejka bêdzie pusta
  odp2<-p[4]
  #Prawdopodobienstwo, ¿e 3 konsultantów bêdzie zajêtych a w kolejce bêdzie 1 osoba
  odp3<-p[5]
  #Prawdopodobienstwo, ¿e 3 konsultantów bêdzie zajêtych a w kolejce bêdzie 6 osób
  odp4<-p[10]
  #Prawdopodobienstwo blokady systemu
  odp5<-p[d-1]
  #Prawdopodobienstwo obs³ugi zg³oszeñ w systemie
  odp6<-((100-p[d])/100)
  #Œrednia liczba zg³oszeñ znajduj¹ca siê w kolejce
  odp7<-v
  #Œrednia liczba zg³oszeñ na stanowisku obs³ugi
  odp8<-lz
  #Œrednia liczba zg³oszeñ w systemie
  odp9<-n
  #Œredni czas oczekiwania w kolejce
  odp10 <- mean((v/lambda)*60)
  #Œredni czas obs³ugi zg³oszenia
  odp11<-mean((lz/lambda)*60)
  #Œredni czas pobytu zg³oszenia w kolejce
  odp12<-mean((n/lambda)*60)

  return<-(c(odp1,odp2,odp3,odp4,odp5,odp6,odp7,odp8,odp9,odp10,odp11,odp12))
  
}

#symulacja przerowadzona dla 8 godzinnego dnia pracy przychodzni, 5 dni w tygodniu przez 1 miesi¹c -> 4 tygodnie
symulacja<-8*60*5*4
wnk<-matrix(NA,symulacja,12)
for (i in 1:symulacja) wnk[i,]<-konsmed2()
wyniki<-apply(wnk, 2, mean)
wynikisym <- data.frame(mierzona_wielkosc = c('Œrednie obci¹¿enie systemu: ',
                                              'Prawdopodobieñstwo, ¿e 3 konsultantów bêdzie zajêtych, a kolejka bêdzie pusta: ',
                                              'Prawdopodobieñstwo, ¿e 3 konsultantów bêdzie zajêtych, a w kolejce bêdzie 1 osoba: ',
                                              'Prawdopodobieñstwo, ¿e 3 konsultantów bêdzie zajêtych, a w kolejce bêdzie 6 osób: ',
                                              'Prawdopodobieñstwo blokady systemu: ',
                                              'Prawdopodobieñstwo obs³ugi zg³oszeñ w systemie: ',
                                              'Œrednia liczba zg³oszeñ znajduj¹ca siê w kolejce: ',
                                              'Œrednia liczba zg³oszeñ na stanowisku obs³ugi: ',
                                              'Œrednia liczba zg³oszeñ w systemie: ',
                                              'Œredni czas oczekiwania w kolejce: ',
                                              'Œredni czas obs³ugi zg³oszenia: ',
                                              'Œredni czas pobytu zg³oszenia w kolejce: '),
                        wynik_pomiaru = c(odp1,odp2,odp3,odp4,odp5,odp6,odp7,odp8,odp9,odp10,odp11,odp12), 
                        jednostka = c('[-]','[-]','[-]','[-]','[-]','[-]','[-]','[-]','[-]','minuty','minuty','minuty'))
wynikisym$wynik_pomiaru<-format(round(wynikisym$wynik_pomiaru,5),nsmall=5)
print(wynikisym)

rys1.1<-barplot(p[1:d],xlab='prawdopodobieñstwa stanów w zakresie [1:d]',ylab='zakres prawdopodobieñstw [-]',names.arg = c(1:d))

