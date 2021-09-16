module Library where
import PdePreludat

-- Aquí va la solución del parcial. ¡Éxitos!

data Unidad = Unidad{
  tipo :: String,
  nivelDeAtaque :: Number,
  herramientas :: [Herramienta]
}deriving (Show, Eq,Ord)

data Herramienta = Herramienta{
  unidadDeAtaque :: Number
}deriving (Show, Eq,Ord)

type SistemaDeDefensa = Ciudad -> Ciudad
data Ciudad = Ciudad{
  nombreCiudad :: String,
  nivelDeDefensa :: Number,
  batallonDeDefensa :: [Unidad], 
  sistemaDeDefensas:: [SistemaDeDefensa]
}deriving (Show, Eq,Ord)

-----Punto 1 ------------------------------------------------
--a------
unidadesGrosas :: Ciudad -> [String]
unidadesGrosas= map tipo.filter (esUnidadGrosa 160).batallonDeDefensa

esUnidadGrosa:: Number->Unidad -> Bool 
esUnidadGrosa cantidad = (>cantidad).nivelDeAtaque

--b------
ataquePoderoso :: Ciudad -> Bool 
ataquePoderoso= all (esUnidadGrosa 100).take 3.batallonDeDefensa

--c------
nivelTotalHerramientas :: Unidad -> Number
nivelTotalHerramientas = sum.map unidadDeAtaque.herramientas

-----Punto 2 ------------------------------------------------

poderOfensivo :: Unidad -> Number 
poderOfensivo unidad 
   | ((>5).length.herramientas)unidad = 100 + sumatoriaDeNiveles unidad
   | ((== "caballeria").tipo)unidad = 2 * sumatoriaDeNiveles unidad
   | otherwise = sumatoriaDeNiveles unidad

sumatoriaDeNiveles unidad =nivelDeAtaque unidad + nivelTotalHerramientas unidad   

-----Punto 3 ------------------------------------------------

--sobreviveAlAtaque batallondeAtaque batallonDeDefensa 
sobreviveAlAtaque :: [Unidad] -> [Unidad] -> Bool
sobreviveAlAtaque [] _ = True
sobreviveAlAtaque _ [] = False 
sobreviveAlAtaque (uAtaque1:uAtaque) (uDefensa1:uDefensa) 
  | nivelDeAtaque uAtaque1 > nivelDeAtaque uDefensa1 = 
    sobreviveAlAtaque (uAtaque1:uAtaque)  uDefensa
  | otherwise= sobreviveAlAtaque uAtaque (uAtaque1:uAtaque) 

-----Punto 4 ------------------------------------------------
--a------
muralla :: Number -> SistemaDeDefensa 
muralla altura = agregarPrefijo.incrementarNivelDeDefensa ( 3 * altura)

agregarPrefijo ciudad = ciudad {
  nombreCiudad = "La gran ciudad de" ++ nombreCiudad ciudad
}

incrementarNivelDeDefensa ::Number -> SistemaDeDefensa
incrementarNivelDeDefensa cantidad ciudad = ciudad {
  nivelDeDefensa = nivelDeDefensa ciudad + cantidad 
}
--b------
torresDeVigilancia:: SistemaDeDefensa
torresDeVigilancia ciudad = incrementarNivelDeDefensa 40 ciudad
--c------
centroDeEntrenamiento :: Number -> SistemaDeDefensa
centroDeEntrenamiento valor ciudad = incrementarNivelDeDefensa 10 ciudad {
  batallonDeDefensa = map (aumentarNivelDeAtaque valor) (batallonDeDefensa ciudad)
}

aumentarNivelDeAtaque :: Number -> Unidad -> Unidad
aumentarNivelDeAtaque cantidad  unidad = unidad {
  nivelDeAtaque = nivelDeAtaque unidad + cantidad
} 
--c------

instalarBancosEnPlazas :: SistemaDeDefensa
instalarBancosEnPlazas ciudad = ciudad

-----Punto 5 ------------------------------------------------
poderDefensivo ::Ciudad -> Number
poderDefensivo ciudad= (nivelDeDefensa.foldr ($) ciudad.sistemaDeDefensas)ciudad


persepolis = Ciudad {
  nombreCiudad = " Persepolis",
  nivelDeDefensa = 10,
  batallonDeDefensa= [caballeria,arquero,bomba,catapulta],
  sistemaDeDefensas = [muralla 5, centroDeEntrenamiento 15, torresDeVigilancia]

}


-----Punto 6 ------------------------------------------------
esSobrevivienteAlAtaque :: Ciudad -> [Unidad] -> Bool  
esSobrevivienteAlAtaque ciudad batallondeAtaque = (sobreviveAlAtaque batallondeAtaque (batallonDeDefensa ciudad)) ||  (poderDefensivoEsSuperior ciudad batallondeAtaque)

poderDefensivoEsSuperior :: Ciudad -> [Unidad] -> Bool
poderDefensivoEsSuperior ciudad batallon = (poderDefensivo ciudad) > poderDeAtaqueBatallon batallon

poderDeAtaqueBatallon :: [Unidad] -> Number
poderDeAtaqueBatallon = sum.map nivelDeAtaque

-----Punto 7 ------------------------------------------------
batallon1 = [caballeria,arquero]
batallon2 = [catapulta,bomba]


caballeria = Unidad{
  tipo = "caballeria",
  nivelDeAtaque = 200,
  herramientas = [hacha, espada]
}

arquero= Unidad{
  tipo = "arquero",
  nivelDeAtaque = 180,
  herramientas = [espada]
}
catapulta = Unidad{
  tipo = "catapulta",
  nivelDeAtaque = 42,
  herramientas = [hacha]
}
bomba = Unidad{
  tipo = "bomba",
  nivelDeAtaque = 150,
  herramientas = [hacha, espada]
}

hacha = Herramienta{
  unidadDeAtaque = 10
}

espada = Herramienta{
  unidadDeAtaque = 15
}

infinitasUnidades :: Unidad -> [Unidad] 
infinitasUnidades unidad = unidad:(infinitasUnidades unidad)

tabriz = Ciudad { 
nombreCiudad = "Tabriz",
nivelDeDefensa = 80,
batallonDeDefensa  = infinitasUnidades caballeria, 
sistemaDeDefensas = []
}

-- En el caso de solicitar los tipos de la unidades grosas.... al estar el batallon compuesto con infinitasUnidades la funcion no termina nunca de evaluar,entonces no converge a un valor  sino que diverge ya que quiere evaluar toda la lista infinita para mostrar el resultado por pantalla pero nunca termina de evaluar. 

-- En el caso de evaluar la funcion de ataquePoderoso con la lista infinita, es posible ya que solo toma las primeras 3 unidades de la lista infinita => Converge gracias a la evaluacion diferida, el take permite que solo se evaluen los tres primeros elementos de la lista infinita, el resto de lo elementos no necesitan ser evaluados.
