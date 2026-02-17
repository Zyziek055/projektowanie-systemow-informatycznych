#zadanie1
wartosc_przyszla = function(kapital, stopa, lata) {
  ans =  kapital*(1+stopa)^lata
  return(ans)
}


print(wartosc_przyszla(1000,0.05,  5))

#zadanie2
przelicz_walute = function(kwota, kurs=4.32){
  return (kwota*kurs)
}

print(przelicz_walute(100, 4.5))

#zadanie3
kostka = function(n){
  return(sample(seq(1,6), n, replace = TRUE))
}

print(kostka(5))

#zadanie4
kalkulator = function(a, b, operacja){
  switch(operacja,
         "+"={
           return(a+b)
         },
         "-"={
           return(a-b)
         },
         "/"={
           return(a/b)
           },
         "*"={
           return(a*b)
         }
  )
}

print(kalkulator(20,2,"+"))
print(kalkulator(20,2,"/"))
print(kalkulator(20,2,"*"))
print(kalkulator(20,2,"-"))

#zadanie5
przyznaj_nagrode = function(){
  nagroda = kostka(1)
  switch(nagroda,
         return("Brak nagrody"),
         return("Brak nagrody"),
         return("Brak nagrody"),
         return("Nagrodwa standardowa"),
         return("Nagrodwa standardowa"),
         return("Super bonus")
  )
}

print(przyznaj_nagrode())

#zadanie6
ocena_kredytowa = function(dochod, zadluzenie){
  procent = zadluzenie/dochod
  if (procent < 0.3) {
    return("Kredyt przyznany")
  } else if (procent < 0.5) {
    return("Weryfikacja")
  } else {
    return("Odrzucony")
  }
}
print(ocena_kredytowa(10000, 2000))
print(ocena_kredytowa(10000, 4000))
print(ocena_kredytowa(10000, 6000))

#zadanie7
podatek_belki = function(przychod, koszt, typ_aktywa){
  zysk = przychod - koszt
  if (zysk < 0) {
    return(0)
  }
  
  podatek <- switch(
    typ_aktywa,
    "akcje"={0.19},
    "obligacje"={0.19},
    "kryptowaluty"={
      if (zysk < 85528) 0.18 else 0.32
    }
  )
  
  return(zysk*(1-podatek))
}
podatek_belki(150000, 50000, "kryptowaluty")
podatek_belki(1500, 1000, "lokata")

#zadanie8
typ_gospodarstwa = function(dochod, wydatki, dzieci, miasto){
  if (wydatki > dochod) {
    return("Trudna sytuacja")
  } else if (dzieci >= 2){
    return("Normalna sytaucaj")
  } else if(wydatki <= 0.8*dochod & miasto == "male") {
    return("Stabilna sytuacja")
  }
  
  return("brak kategorii")
}

typ_gospodarstwa(4000, 4500, 1, "duze")
typ_gospodarstwa(5000, 4500, 2, "duze")
typ_gospodarstwa(5000, 3500, 0, "male")
