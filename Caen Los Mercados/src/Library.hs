module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


data Accion = Accion {
   simbolo :: String,
   precios :: [Number]
} deriving Show

-- Punto 1

data Titulo = Titulo{
  simboloTitulo::String,
 cantidad::Number,
 precio::Number
} deriving Show

data Usuario = Usuario{
 efectivo::Number,
 titulos::[Titulo]
} deriving Show

-- Dos funciones auxiliares usadas en varias puntos para no repetir codigo:

elSimboloDeLaAccionEs::String->Accion->Bool
elSimboloDeLaAccionEs simboloAccion = (==simboloAccion).simbolo

elSimboloDelTituloEs::String->Titulo->Bool
elSimboloDelTituloEs simboloAccion = (==simboloAccion).simboloTitulo


--

-- Punto 2.i

mapCondicional::(a->a)->(a->Bool)->[a]->[a]
mapCondicional transformacion condicion = map (transformarSiCumple condicion transformacion)


transformarSiCumple::(a->Bool)->(a->a)->a->a
transformarSiCumple condicion transformacion x
 | condicion x = transformacion x
 | otherwise = x

{-
Pregunta: ¿de qué tipo tiene que ser la transformación para que esta función tenga sentido? ¿Por qué?

Respuesta:

La transformación tiene que ser del mismo tipo de los elementos de la lista,
porque el resultado de map es una lista y una lista admite solamente elementos de un mismo tipo de datos.
Entonces como la transformación puede aplicarse o no según la condición, el resultado de la transformación
tiene que ser del tipo original de la lista, ya que los elementos transformados y los no transformados tienen que estar en una misma lista
-}


-- Punto 2.ii

encontrar::(a->Bool)->[a]->a
encontrar condicion = head.filter condicion

-- Punto 2.iii

cuantasTieneDe::String->Usuario->Number
cuantasTieneDe simboloAccion = cantidad.encontrar (elSimboloDelTituloEs simboloAccion).titulos

-- Punto 3.i

nuevoPrecioAccion::Number->Accion->Accion
nuevoPrecioAccion precioAccion accion = accion{precios = precioAccion:(precios accion)}

-- Punto 3.ii

nuevoPrecio::String->Number->[Accion]->[Accion]
nuevoPrecio simboloAccion precioAccion = mapCondicional (nuevoPrecioAccion precioAccion) (elSimboloDeLaAccionEs simboloAccion)

-- Punto 3.iii

precioActual::Accion->Number
precioActual = head.precios 

-- Punto 4

estadoActual::[Accion]->Usuario->Number
estadoActual acciones = sum.map (diferencia acciones).titulos 

diferencia::[Accion]->Titulo->Number
diferencia acciones titulo = precio titulo - (precioActual.encontrar (elSimboloDeLaAccionEs (simboloTitulo titulo)) $ acciones)

-- Punto 5

pagarDividendos::String->Number->[Usuario]->[Usuario]
pagarDividendos simboloAccion dividendos = mapCondicional (pagar dividendos) (tieneLaAccion simboloAccion)

pagar::Number->Usuario->Usuario
pagar dinero usuario = usuario{efectivo = efectivo usuario + dinero}

tieneLaAccion::String->Usuario->Bool
tieneLaAccion simboloAccion = (>0).cuantasTieneDe simboloAccion

-- Punto 6

rescateFinanciero::[Accion]->[Usuario]->[Usuario]
rescateFinanciero acciones = mapCondicional (rescatar) (noLeEstaYendoBien acciones)

rescatar::Usuario->Usuario
rescatar = pagar 1000

noLeEstaYendoBien::[Accion]->Usuario->Bool
noLeEstaYendoBien acciones = (<(-50000)).estadoActual acciones

-- Punto 7

venta::Accion->Number->Usuario->Usuario
venta accion cantidadAccionesVendidas  = cobrarAcciones accion cantidadAccionesVendidas.bajarCantidadAcciones accion cantidadAccionesVendidas

cobrarAcciones::Accion->Number->Usuario->Usuario
cobrarAcciones accion cantidadAccionesVendidas = pagar (precioActual accion * cantidadAccionesVendidas)

bajarCantidadAcciones::Accion->Number->Usuario->Usuario
bajarCantidadAcciones accion cantidadAccionesVendidas usuario = usuario{titulos= mapCondicional (\titulo -> titulo{cantidad = cantidad titulo - cantidadAccionesVendidas}) (elSimboloDelTituloEs (simbolo accion)) $ titulos usuario}

-- Punto 8.i

porcentajeDeGanancia::Accion->Number
porcentajeDeGanancia accion = (/primerMedicion accion).(*100).precioActual $ accion

primerMedicion::Accion->Number
primerMedicion = last.precios


-- Punto 8.ii

mayorGanancia::Accion->Accion->Accion
mayorGanancia unaAccion otraAccion
 | porcentajeDeGanancia unaAccion > porcentajeDeGanancia otraAccion = unaAccion
 | otherwise = otraAccion

-- Punto 8.iii

laMejorAccion::[Accion]->Accion
laMejorAccion = foldl1 mayorGanancia

-- Punto 9

funcionQueNoDebeSerNombrada x y = (>= x) . foldr (flip y 10) x

{-

El tipo de funcionQueNoDebeSerNombrada es:
Ord b => b -> (a -> Number -> b -> b) -> [a] -> Bool

b es el tipo del parametro x
(a -> Number -> b -> b) es el tipo del parametro y
[a] es el tipo de la lista que recibe el foldr, que esta como parametro implicito o point free

x es el valor semilla que toma el foldr
flip y 10 es la funcion que usa el foldr para plegar la lista

el tipo del valor semilla es el tipo que toma como segundo parametro y que devuelve la funcion del foldr.
el primer parametro de la funcion del foldr es del mismo tipo de la lista.
por lo tanto flip y 10 tiene que ser a->b->b

flip y seria de tipo Number->a->b->b, al hacer la aplicacion parcial flip y 10
queda el tipo de que tiene que tener la funcion del foldr (a->b->b)

como flip da vuelta los parametros, y flip y esta aplicada con un 10 (que es Number) primero a un elemento de la lista (de tipo a),
la funcion y recibe primero un elemento del tipo de la lista del foldr (a) 
y despues un Number, 
por eso el tipo de y queda (a -> Number -> b -> b).

Finalmente, el tipo de x (b) tiene que ser Ord porque se aplica (>=) a x,
y el resultado de toda la funcion es lo que devuelve el (>=x), o sea, Bool

-}