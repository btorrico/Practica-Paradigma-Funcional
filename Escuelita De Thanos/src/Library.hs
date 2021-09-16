module Library where
import PdePreludat

-------------- Escuelita De Thanos---------------------

------Punto1-----------------------------------------------
data Guantelete = Guantelete{
 material :: String,
 gemas :: [Gema]
} deriving (Show)

type Gema = Personaje -> Personaje

data Personaje = Personaje{
  edad :: Number,
  energia :: Number,
  habilidades :: [String],
  nombre :: String,
  planeta :: String
}deriving (Show)

type Universo = [Personaje]

ironMan = Personaje {
  edad= 75,
  energia = 82,
  habilidades = ["Bailar"],
  nombre = "Tony",
  planeta = "Jupiter"
}

spiderMan = Personaje {
  edad= 18,
  energia = 95,
  habilidades = ["Bailar", "Saltar"],
  nombre = "Peter",
  planeta = "Tierra"
}

drStrange = Personaje {
  edad= 90,
  energia = 45,
  habilidades = ["Desaparecer", "Saltar"],
  nombre = "Dr",
  planeta = "Tierra"
}

{-Por ejemplo si tenemos un universo en el cual existen ironMan, drStrange, groot y wolverine, solo quedan los dos primeros que son ironMan y drStrange. Si además de los 4 personajes estuviera viudaNegra, quedarían también ironMan y drStrange porque se considera la división entera.-}

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete = ((&& esDeUru guantelete).gemasCompletas)guantelete

gemasCompletas :: Guantelete -> Bool
gemasCompletas guantelete = ((==6).length.gemas)guantelete

esDeUru :: Guantelete -> Bool  
esDeUru guantelete = ((=="uru").material) guantelete

reducirALaMitad universo = take (div (habitantes universo) 2) universo

habitantes universo = length universo
---- Alt 96 =  `` ` ` comillas
chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo 
  | guanteleteCompleto guantelete = reducirALaMitad universo
  | otherwise = universo 


------Punto2-----------------------------------------------

{-Resolver utilizando únicamente orden superior.-}

-- ● Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex universo = (any (esPendex)) universo

esPendex personaje = ((<45).edad)personaje

-- ● Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.

energiaTotal :: Universo -> Number 
energiaTotal universo = (sum.map energia.personajesConMuchasHabilidades) universo 

personajesConMuchasHabilidades :: Universo -> [Personaje]
personajesConMuchasHabilidades universo = (filter (tieneMasDeUnaHabilidad))universo

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad personaje = ((>1).length.habilidades)personaje 

--------------Segunda Parte ------------------------------
------Punto3-----------------------------------------------
quitarEnergia ::  Number ->  Personaje-> Personaje
quitarEnergia cantidadAQuitar personaje = personaje{
  energia = energia personaje - cantidadAQuitar
} 
-- ● La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.

laMente ::  Number ->Personaje-> Personaje
laMente  cantidad personaje = quitarEnergia cantidad personaje

-- ● El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. Además le quita 10 puntos de energía.
elAlma :: String -> Personaje -> Personaje
elAlma habilidad personaje = (quitarEnergia 10. eliminarHabilidad habilidad) personaje 

eliminarHabilidad :: String -> Personaje-> Personaje
eliminarHabilidad habilidad personaje = personaje {
  habilidades = filter (/= habilidad)  $ habilidades personaje
}


-- ● El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.

elEspacio ::   String ->Personaje-> Personaje
elEspacio  planeta personaje= (quitarEnergia 20.cambiarDePlaneta planeta)personaje 

cambiarDePlaneta :: String -> Personaje-> Personaje
cambiarDePlaneta nuevoPlaneta personaje= personaje{
  planeta = nuevoPlaneta
}

-- ● El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).

elPoder :: Personaje -> Personaje
elPoder personaje = personaje {
 energia = 0,
 habilidades = quitarHabilidades  $ habilidades personaje
}

quitarHabilidades :: [String] -> [String]
quitarHabilidades habilidades  | tienePocasHabilidades habilidades = []
                               | otherwise = habilidades 

tienePocasHabilidades :: [String] -> Bool
tienePocasHabilidades habilidades = ((<=2).length) habilidades

{- ●  El tiempo que reduce a la mitad la edad de su oponente pero como no está
permitido pelear con menores, no puede dejar la edad del oponente con menos de
18 años. Considerar la mitad entera, por ej: si el oponente tiene 50 años, le
quedarán 25. Si tiene 45, le quedarán 22 (por división entera). Si tiene 30 años, le deben quedar 18 en lugar de 15. También resta 50 puntos de energía.-}

elTiempo :: Personaje ->Personaje
elTiempo oponente = (quitarEnergia 50.reducirEdad) oponente

reducirEdad personaje = personaje{
  edad = (max 18 .div (edad personaje)) 2
} 

-- ● La gema loca que permite manipular el poder de una gema y la ejecuta 2 veces contra un rival.

gemaLoca :: (Personaje -> Personaje) -> Personaje -> Personaje
gemaLoca gema personaje = (gema.gema)personaje

------Punto4-----------------------------------------------

{-Dar un ejemplo de un guantelete de goma con las gemas tiempo, alma
que quita la habilidad de “usar Mjolnir” y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.-}

guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete{
  material = "goma",
  gemas = [elTiempo,elAlma "usar Mjolnir",gemaLoca (elAlma "programación en Haskell")]
} 

------Punto5-----------------------------------------------

{-No se puede utilizar recursividad. Generar la función utilizar que dado una lista de gemas y un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.-}

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaDeGemas enemigo = foldr ($) enemigo listaDeGemas 

------Punto6-----------------------------------------------

{-Resolver utilizando recursividad. Definir la función gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima.-}

--gemaMasPoderosa :: Guantelete -> Persona -> Gema
--gemaMasPoderosa guantelete persona  

gemasGuantelete :: Guantelete-> Personaje -> Gema
gemasGuantelete guantelete persona = perdidaMasGrandeDeEnergia (gemas guantelete) persona

perdidaMasGrandeDeEnergia :: [Gema] -> Personaje -> Gema
--Caso Base:
perdidaMasGrandeDeEnergia [gema] _ = gema

perdidaMasGrandeDeEnergia (gema1:gema2:gemas) persona 
  | energiaPerdida gema1 persona > energiaPerdida gema2 persona =perdidaMasGrandeDeEnergia (gema1:gemas) persona
  | otherwise = perdidaMasGrandeDeEnergia (gema2:gemas) persona 

energiaActual gema persona = energia.gema $ persona

energiaPerdida gema persona = energia persona - energiaActual gema persona 

{- Punto 7 evaluación diferida -}
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" $ infinitasGemas elTiempo

punisher:: Personaje 
punisher = Personaje 38 350.0  ["Disparar con de todo","golpear"] "The Punisher" "Tierra" 

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher

-- Justificacion:
-- En el caso : gemaMasPoderosa punisher guanteleteDeLocos -> La funcion no termina nunca de evaluar,entonces no converge a un valor  sino que diverge ya que quiere evaluar toda la lista infinita para mostrar el resultado por pantalla pero nunca termina de evaluar.

-- En este caso : -- usoLasTresPrimerasGemas guanteleteDeLocos punisher -> Converge gracias a la evaluacion diferida, el take permite que solo se evaluen los tres primeros elementos de la lista infinita, el resto de lo elementos no necesitan ser evaluados.

