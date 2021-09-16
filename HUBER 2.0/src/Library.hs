module Library where
import PdePreludat

------------------HUBER 2.0------------------------------------

------ Punto 1 -----------------------------------------------------------

type CondicionViaje = Viaje -> Bool

data Chofer = Chofer{
  nombre :: String,
  kilometraje :: Number,
  viajes :: [Viaje],
  condicion :: CondicionViaje
} deriving (Show,Eq,Ord)

data Cliente = Cliente {
  nombreCliente:: String,
  localidad :: String
}deriving (Show,Eq,Ord)

data Viaje = Viaje{
  fecha :: String,
  cliente :: Cliente,
  costo :: Number
}deriving (Show,Eq,Ord)

------ Punto 2 -----------------------------------------------------------

cualquierViaje :: Viaje -> Bool
cualquierViaje _ = True

costoSuperior :: Viaje -> Bool
costoSuperior viaje = ((>200).costo)viaje

nombreConMuchasLetras :: Number -> Viaje -> Bool
nombreConMuchasLetras cantidad viaje = ((>=cantidad).length.nombreCliente.cliente)viaje 

clientesQueNoVivenEn :: String -> Viaje -> Bool 
clientesQueNoVivenEn zona viaje = ((/=zona).localidad.cliente)viaje 

------ Punto 3 -----------------------------------------------------------
-- a-----
cliente1 = Cliente { nombreCliente = "Lucas", localidad = "Victoria"}
--b------
viaje1 = Viaje{fecha = "20/04/2017", cliente = cliente1 , costo = 150}

chofer1 = Chofer{ nombre = "Daniel", kilometraje = 23500,viajes = [viaje1] , condicion = clientesQueNoVivenEn "Olivos" }

chofer2 = Chofer{ nombre = "Alejandra", kilometraje = 180000,viajes = [] , condicion = cualquierViaje }

------ Punto 4 -----------------------------------------------------------
puedeTomarViaje :: Viaje -> Chofer -> Bool 
puedeTomarViaje viaje chofer = (condicion chofer)viaje

------ Punto 5 -----------------------------------------------------------
liquidacion:: Chofer -> Number
liquidacion chofer = (sum.map costo.viajes)chofer

--Alternativa
liquidacion':: Chofer -> Number
liquidacion' chofer = (foldr ((+).costo) 0.viajes) chofer

------ Punto 6 -----------------------------------------------------------
--a----
hacerUnViaje:: Viaje -> [Chofer]-> [Chofer]
hacerUnViaje viaje choferes = filter (puedeTomarViaje viaje) choferes

--b----
choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) 
  |cantidadDeViajes chofer1 < cantidadDeViajes chofer2 = choferConMenosViajes (chofer1:choferes)
  | otherwise = choferConMenosViajes(chofer2 :choferes)

cantidadDeViajes :: Chofer ->Number
cantidadDeViajes chofer = (length.viajes)chofer 

--c-----
efectuarElViaje :: Viaje -> Chofer -> Chofer
efectuarElViaje viaje chofer = chofer{
  viajes = viaje : viajes chofer
}

------ Punto 7 -----------------------------------------------------------
--a-------
viajeNito = Viaje{fecha = "11/03/2017", cliente = cliente1 , costo = 50}

repetirViaje viaje = viaje : repetirViaje viaje

viajeInfinito :: [Viaje]
viajeInfinito = repetirViaje viajeNito

nito = Chofer{ nombre = "Nito Infy", kilometraje = 70000,viajes = viajeInfinito , condicion = nombreConMuchasLetras 3 }

--b---------
-- No, para calcular la liquidacion de nito tendriamos que modificar la funcion de liquidacion para poder tomar solo una cierta cantidad de viajes ya que de otro modo convergeria al intentar sumar costos de la lista infinita de viajes

--ejemplo :  liquidacion chofer = (sum.map costo.take 10.viajes)chofer
--calcularia solo el costo de los 10 primeros viajes de la lista infinita de viajes

---- b 
-- liquidacionChofer nito ... no termina nunca!!
-- c pero 
-- puedeTomarViaje (Viaje (2,5,2017) lucas 50) nito
-- True
-- porque no involucra a la lista de viajes

------ Punto 8 -----------------------------------------------------------
gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3




