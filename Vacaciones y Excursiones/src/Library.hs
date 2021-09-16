module Library where
import PdePreludat

data Turista = Turista{
  nivelDeCansancio :: Number,
  nivelDeEstress :: Number,
  viajaSolo :: Bool,
  idiomas :: [String]
} deriving (Show)

julian = Turista{
  nivelDeCansancio = 95 ,
  nivelDeEstress = 100,
  viajaSolo = False,
  idiomas = ["Frances" , "Ingles", "Portugues"]
}
-- Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
type Excursion = Turista -> Turista

irALaPlaya :: Turista -> Turista 
irALaPlaya turista 
  | viajaSolo turista = disminuirCansancio 5 turista
  | otherwise = disminuirStress 1 turista

disminuirCansancio :: Number ->Turista -> Turista
disminuirCansancio cantidad turista = turista { 
  nivelDeCansancio = nivelDeCansancio turista - cantidad
}

disminuirStress :: Number -> Turista -> Turista
disminuirStress cantidad turista = turista{
  nivelDeEstress = nivelDeEstress turista - cantidad
}

--Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia.

apreciarElemento :: String -> Turista -> Turista
apreciarElemento elemento turista = (disminuirStress (length elemento)) turista

--Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.

hablarUnIdioma :: String -> Turista -> Turista
hablarUnIdioma idioma turista = agregarIdioma idioma turista { viajaSolo = True }

agregarIdioma :: String ->Turista -> Turista 
agregarIdioma idioma turista = turista {
   idiomas = idioma : (idiomas turista)
 }

--Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.

caminar :: Number->Turista -> Turista 
caminar minutosCaminata turista = turista {
  nivelDeCansancio = nivelDeCansancio turista + intensidadDeCaminata minutosCaminata,
  nivelDeEstress = nivelDeEstress turista - intensidadDeCaminata minutosCaminata 
 } 

intensidadDeCaminata :: Number -> Number
intensidadDeCaminata minutosCaminados =  ((* 1).(/4))minutosCaminados 


--- Paseo en barco: depende de cómo esté la marea
       --- si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
       --- si está moderada, no pasa nada.
       --- si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.

paseoEnBarco :: Marea->Turista -> Turista
paseoEnBarco marea turista 
   | fuerte marea =  (aumentarCansancio 10.(aumentarStress 6)) turista       
   | tranquila marea = (hablarUnIdioma "aleman".apreciarElemento "mar".caminar 10) turista 
   | otherwise = turista 
   

data Marea = Marea{
  fuerte :: Bool,
  moderada :: Bool,
  tranquila :: Bool
}

aumentarStress :: Number ->Turista -> Turista
aumentarStress cantidad turista = turista{
  nivelDeEstress = nivelDeEstress turista + cantidad
}
aumentarCansancio :: Number ->Turista -> Turista
aumentarCansancio cantidad turista = turista{
  nivelDeCansancio = nivelDeCansancio turista + cantidad
}


--1. Crear un modelo para los turistas y crear los siguientes tres ejemplos:
--a. Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
ana = Turista{
  nivelDeCansancio = 0,
  nivelDeEstress = 21,
  viajaSolo = False,
  idiomas = ["Español"]
}

--b. Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress.

beto = Turista{
  nivelDeCansancio = 15,
  nivelDeEstress = 15,
  viajaSolo = True,
  idiomas = []
}
cathi = Turista{
  nivelDeCansancio = 15,
  nivelDeEstress = 15,
  viajaSolo = True,
  idiomas = ["Aleman", "Catalan"]
}

--2. Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga falta modificar las funciones existentes. Además:

--a. Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.

hacerExcursion :: Excursion->Turista -> Turista
hacerExcursion excursion turista = disminuirStress (0.1 * nivelDeEstress turista) (excursion turista)

{-
hacerExcursion :: Excursion ->Turista -> Turista
hacerExcursion excursion turista =  (disminuirStress (div 10 100) .excursion) turista
-}
-----------------------------------------------------------------
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun ::  (Turista -> Number) -> Excursion -> Turista -> Number
deltaExcursionSegun indice excursion turista = deltaSegun indice (hacerExcursion excursion turista) turista  

------------------------------------------------------------------
--Usar la función anterior para resolver cada uno de estos puntos:
--i. Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
excursionEducativa :: Excursion -> Turista -> Bool
excursionEducativa excursion turista = ((>0).deltaExcursionSegun (length.idiomas) excursion) turista 

--ii. Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.

excursionesDesestresantes :: Turista ->[Excursion]-> [Excursion] 
excursionesDesestresantes turista excursiones = filter (esExcursionDesestresante turista) excursiones

esExcursionDesestresante :: Turista ->Excursion-> Bool 
esExcursionDesestresante  turista excursion = ((>=3).deltaExcursionSegun (nivelDeEstress) excursion) turista


-- 3. Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones llamados tours. Un tour se compone por una serie de excursiones.

--- Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, y finaliza con una salida con gente local que habla "melmacquiano".

type Tour = [Excursion]

tourCompleto :: [Excursion]
tourCompleto = [caminar 20,apreciarElemento "cascada",caminar 40, irALaPlaya, hablarUnIdioma "melmacquiano"]

--- Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.

ladoB:: Marea -> Excursion -> [Turista -> Turista]
ladoB marea excursion = [paseoEnBarco marea, excursion, caminar 120]

-- Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra isla: si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.

islaVecina :: Marea -> Tour
islaVecina marea = [paseoEnBarco marea, excursionTour marea,paseoEnBarco marea]

excursionTour ::Marea -> Turista -> Turista
excursionTour marea turista | fuerte marea = apreciarElemento "lago" turista
                            | otherwise = irALaPlaya turista

--Modelar los tours para:
--a. Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades como cantidad de excursiones tenga el tour, y luego realizar las excursiones en orden.

hacerUnTour :: Tour -> Turista -> Turista
hacerUnTour tour turista = (aumentarStress (length tour).foldr ($) turista) tour

--b. Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla.

propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente  tour turista= any (esConvincente tour)turista

esConvincente :: Turista -> Tour -> Bool
esConvincente turista tour = (any (dejaAcompaniado turista) . excursionesDesestresantes turista)tour

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista excursion = (not . viajaSolo .hacerExcursion excursion)turista

--c. Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour.
--La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.

efectividadDelTour :: Tour -> [Turista] -> Number
efectividadDelTour  tour turistas = (sum.map espiritualidadRecibida.turistasConvencidos tour) turistas 

turistasConvencidos :: Tour -> [Turista] -> [Turista]
turistasConvencidos  tour turistas= filter (flip esConvincente tour) turistas

espiritualidadRecibida :: Turista -> Number 
espiritualidadRecibida turista = nivelDeEstress turista + nivelDeCansancio turista

--Implementar y contestar en modo de comentarios o pruebas por consola

--a. Construir un tour donde se visiten infinitas playas.2

playaInfinita :: Excursion -> [Excursion]
playaInfinita playa = playa : playaInfinita playa


--b. ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.

--Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
--Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.

--c. ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.

--No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.

{-
------------------------------------SOLUCION ALTERNATIVA:---------------------- 


-- 1)
type Idioma = String

data Turista = Turista
  { cansancio :: Int
  , stress    :: Int
  , solitario :: Bool
  , idiomas   :: [Idioma]
  } deriving Show

-- TODO: Cambiar
ana :: Turista
ana =
  Turista { cansancio = 0 , stress = 20, solitario = False, idiomas = ["espaniol"] }

beto :: Turista
beto =
  Turista { cansancio = 15, stress = 15, solitario = True, idiomas = ["aleman"] }

cathi :: Turista
cathi =
  Turista { cansancio = 15, stress = 15, solitario = True, idiomas = ["aleman", "catalan"] }

--
cambiarStress delta turista = turista {stress = stress turista + delta}

cambiarStressPorcentual porciento turista =
  cambiarStress (div (porciento * stress turista) 100) turista

cambiarCansancio delta turista = turista {cansancio = cansancio turista + delta}

aprenderIdioma idioma turista = turista {idiomas = idioma : idiomas turista}

acompaniado turista = turista {solitario = False}

-- 2)
type Excursion = Turista -> Turista

playa :: Excursion
playa turista
  | solitario turista = cambiarCansancio (-5) turista
  | otherwise = cambiarStress (-1) turista

apreciar :: String -> Excursion
apreciar algo = cambiarStress (-length algo)

salirConGente :: Idioma -> Excursion
salirConGente idioma = acompaniado . aprenderIdioma idioma

caminar :: Int -> Excursion
caminar mins = cambiarStress (-intensidad mins) . cambiarCansancio (intensidad mins)

intensidad mins = div mins 4

data Marea
  = Tranquila
  | Moderada
  | Fuerte

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Tranquila = acompaniado
paseoEnBarco Moderada  = id
paseoEnBarco Fuerte    = cambiarCansancio 10 . cambiarStress 6


-- a)
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = cambiarStressPorcentual (-10) . excursion

-- b)
deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun f turista excursion =
  deltaSegun f (hacerExcursion excursion turista) turista

-- c)
esEducativa :: Turista -> Excursion -> Bool
esEducativa turista = (> 0) . deltaExcursionSegun (length . idiomas) turista 

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista


-- 3)
type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciar "cascada", caminar 40, playa, salidaLocal]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina mareaVecina = [paseoEnBarco mareaVecina, excursionEnIslaVecina mareaVecina, paseoEnBarco mareaVecina]

--
excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina Fuerte = apreciar "lago"
excursionEnIslaVecina _  = playa

salidaLocal :: Excursion
salidaLocal = salirConGente "melmacquiano"

-- a)
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour =
  foldl (flip hacerExcursion) (cambiarStress (length tour) turista) tour

-- b)
propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (dejaAcompaniado turista) . excursionesDesestresantes turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . solitario . flip hacerExcursion turista

-- c)
efectividad :: Tour -> [Turista] -> Int
efectividad tour = sum . map (espiritualidadAportada tour) . filter (flip esConvincente tour)

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada tour = negate . deltaRutina tour

deltaRutina :: Tour -> Turista -> Int
deltaRutina tour turista =
  deltaSegun nivelDeRutina (hacerTour turista tour) turista

nivelDeRutina :: Turista -> Int
nivelDeRutina turista = cansancio turista + stress turista


-- 4)
-- a)
playasEternas :: Tour
playasEternas = salidaLocal : repeat playa

-}