module Library where
import PdePreludat

--------------------FMI ----------------------------
-----Punto 1a-------------------------------------------------------------------

data Pais = Pais{
  ingresoPerCapita :: Number,
  pASectorPublico :: Number,
  pASectorPrivado :: Number,
  recursosNaturales :: [String],
  deuda :: Number
} deriving (Show,Eq,Ord)

-----Punto 1b-------------------------------------------------------------------

--Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s, la población activa del sector público es de 400.000, la población activa del sector privado es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.

namibia :: Pais
namibia = Pais {
  ingresoPerCapita= 4140,
  pASectorPublico= 400000,
  pASectorPrivado= 65000,
  recursosNaturales= ["mineria", "ecoturismo"],
  deuda = 50000000 }

 ---- Alternativa 1.b Generar al pais Namibia
--namibia :: Pais
--namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50 
----------------------------------------------------------
argentina = Pais{
  ingresoPerCapita = 4000000,
  pASectorPublico = 300,
  pASectorPrivado = 150,
  recursosNaturales = ["agua", "petroleo","sal"],
  deuda = 8000000
}
-----Punto 2--------------------------------------------------------------------
--prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)

type Receta = Pais -> Pais 

prestamo :: Number -> Pais -> Pais
prestamo cantidadAPrestar pais = pais{
  deuda = deuda pais + div (150 * cantidadAPrestar) 100 -- 150% -> 1.5 => cantidadAPrestar * 1.5

}


----------------------------------------------------------
--prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)

reduccion :: Number -> Pais -> Pais 
reduccion cantidad  pais 
    |superaPuestosDeTrabajo pais = (disminuirIngresoPerCapita 20.reduccionSectorPublico cantidad) pais
    | otherwise = disminuirIngresoPerCapita 15 pais

superaPuestosDeTrabajo pais = ((>100).pASectorPublico) pais

reduccionSectorPublico:: Number -> Pais -> Pais
reduccionSectorPublico cantidadAReducir pais = pais{
  pASectorPublico = pASectorPublico pais - cantidadAReducir 
}

disminuirIngresoPerCapita cantidad pais = pais{
  ingresoPerCapita = ingresoPerCapita pais -  div (cantidad * (ingresoPerCapita) pais) 100
}

---------------------------------------------------------------
--Darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 millones de dólares la deuda que el país mantiene con el FMI pero también deja momentáneamente sin recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.

cederUnRecurso :: String -> Pais -> Pais
cederUnRecurso recurso pais = (disminuirDeuda 2000000 .cederRecurso recurso) pais

cederRecurso :: String -> Pais -> Pais 
cederRecurso recurso pais = pais{
  recursosNaturales = filter (/=recurso) (recursosNaturales pais)
}

disminuirDeuda :: Number -> Pais -> Pais
disminuirDeuda cantidad pais = pais{
  deuda = deuda pais - cantidad
}

----------------------------------------------------------
--establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado por su población activa, sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.

blindaje :: Pais -> Pais 
blindaje pais = reduccionSectorPublico 500 pais {
 deuda = ((`div` 2).productoBrutoInterno) pais 
}


productoBrutoInterno :: Pais -> Number
productoBrutoInterno pais = ingresoPerCapita pais * (pASectorPublico pais + pASectorPrivado pais)


-----Punto 3a-------------------------------------------------------------------

--Modelar una receta que consista en prestar 200 millones, y darle a una empresa X la explotación de la “Minería” de un país.

receta1 :: [Receta] 
receta1 = [prestamo 200000000, cederUnRecurso "mineria"] 

-----Punto 3b-------------------------------------------------------------------
--Ahora queremos aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). Justificar cómo se logra el efecto colateral.

aplicarReceta :: Pais -> [Receta] -> Pais
aplicarReceta pais recetas= foldr ($) pais recetas 

-----Punto 4a-------------------------------------------------------------------

--Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre sus riquezas naturales.

puedenZafar :: [Pais] -> [Pais] 
puedenZafar paises = filter (tienePetroleo) paises

tienePetroleo :: Pais -> Bool
tienePetroleo pais = (elem ("petroleo").recursosNaturales) pais

-----Punto 4b-------------------------------------------------------------------
--Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
totalDeuda :: [Pais] -> Number 
totalDeuda paises = foldr ((+).deuda) 0 paises

--Alternativa 
totalDeuda' :: [Pais] -> Number 
totalDeuda' paises = (sum.map deuda)paises

-----Punto 4c-------------------------------------------------------------------
--Indicar en dónde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver el requerimiento.

-----Punto 5 -------------------------------------------------------------------

--Debe resolver este punto con recursividad: dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor. Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per cápita por la población activa (privada y pública).

estaOrdenada :: Pais -> [Receta] -> Bool
estaOrdenada pais [receta] = True

estaOrdenada pais (receta1:receta2:recetas) 
   | (productoBrutoInterno.receta1) pais <=(productoBrutoInterno.receta2) pais = True && estaOrdenada pais (receta2:recetas)
   | otherwise = False


-- Punto 6 -------------------------------------------------
-- Si un país tiene infinitos recursos naturales, modelado con esta función
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

--    a) ¿qué sucede con la función 4a? 
--    b) ¿y con la 4b?
--    Justifique ambos puntos relacionándolos con algún concepto.
pruebaInfinita1 = puedenZafar [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
--no termina nunca, porque quiere buscar "Mineria" entre los recursos

pruebaInfinita2 = totalDeuda [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
--se puede porque al no evaluar los recursos solamente suma deuda
-- relacionado con evaluacion diferida, solo se evalua lo que se necesita

----------------------SOLUCION ALTERNATIVA--------------------------------------
{-
-- Parcial FMI
-- Punto 1 - 2 puntos global
-- 1.a Representar al TAD Pais 
type Recurso = String

data Pais = Pais {
    ingresoPerCapita :: Float,
    activosPublico :: Int,
    activosPrivado :: Int,
    recursosNaturales :: [Recurso],
    deuda :: Float
} deriving (Eq, Show)

-- 1.b Generar al pais Namibia
namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

-- Punto 2 - 4 puntos
-- Prestarle plata
type Estrategia = Pais -> Pais

prestarPlata :: Float -> Estrategia
prestarPlata cuanto pais = pais {
    deuda = deuda pais + cobrarIntereses cuanto
}

cobrarIntereses :: Float -> Float
cobrarIntereses cuanto = cuanto * 1.5

-- 2.b 
-- Reducir x puestos de trabajo del sector publico
reducirPuestos :: Int -> Estrategia
reducirPuestos cantidadPuestos pais = pais {
    activosPublico = activosPublico pais - cantidadPuestos,
    ingresoPerCapita = ingresoPerCapita pais * (1 - reduccionIngreso cantidadPuestos)
}

reduccionIngreso :: Int -> Float
reduccionIngreso cantidadPuestos | cantidadPuestos > 100 = 0.2
                                 | otherwise             = 0.15

-- 2.c 
-- Darle a una empresa afin la explotacion de alguno de los recursos
explotar :: Recurso -> Estrategia
explotar recurso pais = pais {
    recursosNaturales = quitarRecurso recurso $ recursosNaturales pais,
    deuda = deuda pais - 20
}

quitarRecurso :: Recurso -> [Recurso] -> [Recurso]
quitarRecurso recurso recursos = filter (/= recurso) recursos

-- 2.d
blindaje :: Estrategia
blindaje pais = (prestarPlata (pbi pais * 0.5) . reducirPuestos 500) pais

pbi :: Pais -> Float
pbi pais = ingresoPerCapita pais * fromIntegral (poblacionActiva pais) 
-- el fromIntegral no es importante

poblacionActiva :: Pais -> Int 
poblacionActiva pais = activosPrivado pais + activosPublico pais

-- Punto 3 - 2 puntos
-- a) Modelar una receta que consista en prestar 200 millones, y darle a una empresa X
-- la explotación de la Minería de un país.
type Receta = [Estrategia]

receta :: Receta
receta = [prestarPlata 2000, explotar "Mineria"]

-- b) Aplicar la receta del punto 3.a al país Namibia (creado en el punto 1.b). 
-- Justificar cómo se logra el efecto colateral.
aplicarReceta :: Receta -> Pais -> Pais
-- opción con foldl + lambda
-- aplicarReceta receta pais = foldl (\pais estrategia -> estrategia pais) pais receta
-- opción con foldr + $
aplicarReceta receta pais = foldr ($) pais receta

-- Punto 4 - 3 puntos
-- 4.a) Conocer los países que pueden zafar, que son aquellos que tienen "Petróleo" entre sus riquezas naturales.
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter $ elem "Petroleo" . recursosNaturales

-- 4.b) Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
totalDeuda :: [Pais] -> Float
totalDeuda = foldr ((+) . deuda) 0 

-- Punto 5 - 2 puntos
-- dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, 
-- en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor
estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado pais [receta] = True
estaOrdenado pais (receta1:receta2:recetas) 
     = revisarPBI receta1 pais <= revisarPBI receta2 pais && estaOrdenado pais (receta2:recetas)  
     where revisarPBI receta = pbi . aplicarReceta receta

-}