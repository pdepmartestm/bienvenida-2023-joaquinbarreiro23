
data Auto = UnAuto {
    marca::String,
    modelo:: Int,
    kilometraje:: Float } deriving (Show, Eq)

ferrari,fitito, reno :: Auto
ferrari = UnAuto "ferrari" 1990 100
fitito = UnAuto "fiat" 1960 1000000
reno = UnAuto "renault" 2023 0

data Persona = Persona{
    nombre :: String,
    pagaImpuestos :: Bool,
    autos :: [Auto]
} deriving(Show,Eq)

juan,lara,joaco,pedro,sofia :: Persona
juan = Persona "juan" False [reno,ferrari]
lara = Persona "lara" True [fitito,ferrari]
joaco = Persona "joaco" True [fitito,reno,ferrari]
sofia = Persona "juan" False [fitito,reno]
pedro = Persona "pedro" True [ferrari,ferrari,ferrari] 

esImportado "ferrari" = True
esImportado "fiat" = False
esImportado "renault" = True

calcularValor unAuto
   | marca unAuto == "ferrari" = 1000000 - kilometraje unAuto * 0.5 + 100000 --100000 por importacion
   | marca unAuto == "renault" = 500000 - kilometraje unAuto * 0.3  + 50000  --50000 por importacion
   | otherwise = 200000 - kilometraje unAuto * 0.1

personas = [juan,lara,joaco,sofia,pedro]
calcularValorAutos persona = sum(map calcularValor (autos persona))
esHonestoMillonario persona = calcularValorAutos persona >= 1000000 && pagaImpuestos persona
cantMillonariosHonestos = length(filter id (map esHonestoMillonario personas))




















