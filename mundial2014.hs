data Jugador = CJugador {
    nombre :: String,
    edad :: Int,
    promedioGol :: Float,
    habilidad :: Int,
    cansancio :: Float
}deriving(Show,Eq)

martin = CJugador "Martin" 26 0.0 50 35.0
juan = CJugador "Juancho" 30 0.2 50 40.0
maxi = CJugador "Maxi Lopez" 27 0.4 68 30.0

jonathan = CJugador "Chueco" 20 1.5 80 99.0
lean = CJugador "Hacha" 23 0.01 50 35.0
brian = CJugador "Panadero" 21 5 80 15.0

garcia = CJugador "Sargento" 30 1 80 13.0
messi = CJugador "Pulga" 26 10 99 43.0
aguero = CJugador "Aguero" 24 5 90 5.0

type Equipo = (String,Char,[Jugador])
nombreEquipo (nombre,_,_) = nombre
grupoEquipo (_,grupo,_) = grupo
jugadoresEquipo (_,_,lista) = lista

equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero]) 

--Punto 1 

figuras (_,_,jugadores) = filter (\jugador-> habilidad jugador > 75 && promedioGol jugador > 0) jugadores

--Punto 2
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFarandulero (_,_,jugadores) = any (\jugador-> elem (nombre jugador) jugadoresFaranduleros) jugadores

--Punto 3

dificil :: [Equipo] -> Char -> [Jugador]
dificil equipos grupo = concatMap (\equipo -> cumplenCondiciones (jugadoresEquipo equipo)) (pertenecenGrupo equipos grupo)

cumplenCondiciones :: [Jugador] -> [Jugador]
cumplenCondiciones jugadores = filter (\jugador -> esFigura jugador && esJoven jugador && not(esFarandulero jugador)) jugadores

pertenecenGrupo :: [Equipo]-> Char -> [Equipo]
pertenecenGrupo equipos grupoParametro = filter (\equipo -> grupoEquipo equipo == grupoParametro ) equipos

esFarandulero :: Jugador -> Bool
esFarandulero jugador = any (\farandulero -> nombre jugador == farandulero) jugadoresFaranduleros

esFigura :: Jugador -> Bool
esFigura jugador = habilidad jugador > 75 && promedioGol jugador > 0

esJoven :: Jugador -> Bool
esJoven jugador = edad jugador < 27

--Punto 4

jugarPartido::Equipo->Equipo
jugarPartido equipo = (nombreEquipo equipo, grupoEquipo equipo, map modificarJugador $ jugadoresEquipo equipo)

modificarJugador jugador
    | not(esFarandulero jugador) && esJoven jugador && esFigura jugador = jugador{cansancio = 50}
    | esJoven jugador = jugador{cansancio = cansancio jugador * 1.1}
    | not(esJoven jugador) && esFigura jugador = jugador{cansancio = cansancio jugador + 20}
    | otherwise = jugador{cansancio = cansancio jugador * 2}

--Punto 5

quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs 


ganador::Equipo->Equipo->Equipo
ganador e1 e2 
    |sumaPromedios e1 > sumaPromedios e2 = jugarPartido e1
    |otherwise = jugarPartido e2
    where sumaPromedios eq =sum $ take 11 $ map promedioGol $ quickSort (\x y-> cansancio x < cansancio y) $ jugadoresEquipo eq

--Punto 6

listEquipos = [equipo1,losDeSiempre,restoDelMundo]

ganadorTorneo::[Equipo]->Equipo
ganadorTorneo = foldl1 ganador

ganadorTorneoRecursivo [equipo] = equipo
ganadorTorneoRecursivo (equipo1:equipo2:equipos) = ganadorTorneoRecursivo (ganador equipo1 equipo2:equipos)

--Punto 7

--esFigura jugador = habilidad jugador > 75 && promedioGol jugador > 0

figuraDelCampeon equipos = map nombre (take 1 (filter esFigura (jugadoresEquipo (ganadorTorneo equipos))))

--Teorico

--en todos lados use orden superior, no cree ninguna

--al tener infinitos jugadores no se podria jugar un partido ya que hay que modificarle la energia a cada jugador


