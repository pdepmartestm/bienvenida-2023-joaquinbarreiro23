
esVocal x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u' || x=='A' || x == 'E' || x == 'I' || x == 'O' || x == 'U'

estaEntre numIz numMed numDer = numIz < numMed && numMed < numDer
--El segundo parametro es el numero que se va a comparar

potEsMayorCero pot = pot > 0
potencia num 0 = 1
potencia num pot = if potEsMayorCero pot
    then num * potencia num (pot-1)
    else  1 / potencia num (-pot)

  




