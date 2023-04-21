import System.Win32 (xBUTTON1, COORD (yPos), zeroMemory)
esMultiploTres x = mod x 3 == 0


esMultiploDe x y = mod y x == 0

potencia x 0 = 1
potencia num pot = num * potencia num (pot-1)
cubo x = potencia x 3 

areaRectangulo base altura = base* altura

numberToMessage :: Int -> String
numberToMessage n
    | n < 0     = "El número es negativo"
    | n == 0    = "El número es cero"
    | otherwise = "El número es positivo"

divisible400 x = mod 400 x == 0
divisible100no4 x = mod x 4 == 0 &&  mod 100 x /= 0

--ejercio 10
maximo x y z 
    | x < y && y < z = z
    | z < x && x < y = y 
    | otherwise = x

minimo x y z
    | x < y && x < z = x
    | y < x && y < z = y
    | otherwise = z

dispersion x y z = maximo x y z - minimo x y z 

diasRaros x y z
    | dispersion x y z < 30 = "el dia es parejo"
    | dispersion x y z > 100 = "el dia es loco"
    | otherwise = "el dia es normal"

-- suponiendo que la altura se pasa en cm
pesoPino altura
    | altura <= 3 = altura * 3 * 100
    | otherwise = ((altura - 3) * 2 + 3 * 3) * 100

esPesoUtil peso = peso >= 400 && peso <= 1000

sirvePino altura = esPesoUtil(pesoPino altura)

data Persona = Persona String Int deriving Show

santiago = Persona "santiago" 23

nombre (Persona _nombre _edad) = _nombre
edad (Persona _nombre _edad) = _edad

fst3 (a,b,c) = a
snd3 (a,b,c) = b
trd3 (a,b,c) = c

doble x = x * 2
triple x = x * 3

aplicar (funcion1,funcion2) num = (funcion1 num,funcion2 num)

cuentaBizarra (num1, num2)
    | num1 > num2 = num1 + num2
    | num2 > num1 +  10 = num2 - num1
    | num2 > num1  = num1 * num2 
    | otherwise = 0

--esPar a = (mod a 2 == 0, a)

duplicarPar (bool, a)
    | bool = a * 2
    | otherwise = a

sumarUno (bool,a)
   | bool = a 
   | otherwise = a + 1

--calcular (a, b) = (duplicarPar (esPar a), sumarUno(esPar b))

