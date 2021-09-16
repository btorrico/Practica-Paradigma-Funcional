
----------------------------------SAMURAI JACK--------------------------
module Library where
import PdePreludat

data Elemento = UnElemento { 
tipo :: String,
ataque :: (Personaje-> Personaje),
defensa :: (Personaje-> Personaje) 
}deriving (Show,Eq,Ord)


data Personaje = UnPersonaje { 
nombre :: String,
salud :: Number,
elementos :: [Elemento],
anioPresente :: Number 
} deriving (Show,Eq,Ord)

--------Punto 1 --------------------------------------------------
--a----
mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje{
  anioPresente = anio
}
--b----
meditar :: Number -> Personaje -> Personaje
meditar cantidad personaje = personaje{
  salud = salud personaje + div cantidad 2
}
--c----
causarDanio :: Number -> Personaje -> Personaje
causarDanio cantidad personaje = personaje{
  salud = max 0 (salud personaje - cantidad)
}
--------Punto 2 --------------------------------------------------
--a----
esMalvado :: Personaje -> Bool
esMalvado personaje = (any (esDeTipo "Maldad").elementos) personaje

esDeTipo :: String -> Elemento -> Bool
esDeTipo palabra elemento =  ((==palabra).tipo)elemento
--b----
danioQueProduce :: Elemento ->  Personaje -> Number 
danioQueProduce  elemento personaje = salud personaje - (salud.ataque elemento)personaje
--c----
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje enemigos =
  filter (esEnemigoMortal personaje) enemigos

esEnemigoMortal personaje enemigo =
  (any (tieneAtaqueMortal personaje) . elementos) enemigo


tieneAtaqueMortal personaje elemento =
  (estaMuerto . ataque elemento) personaje

estaMuerto  = ((==0).salud)

--------Punto 3 --------------------------------------------------
--a----
-- Punto 3
noHacerNada = id

--concentracion :: Number -> Elemento
---concentracion nivelDeConcentracion = UnElemento { 
--tipo = "Magia",
--ataque = noHacerNada,
--defensa = (\personaje -> iterate meditar personaje !! nivelDeConcentracion)
--}
               -- equivalente con composición y aplicación parcial para:
               -- defensa = (\personaje -> iterate meditar personaje !! nivelDeConcentracion) }
               -- otra versión super interesante:
               -- defensa = foldr1 (.) (replicate nivelDeConcentracion meditar)
               -- por ejemplo (concentracion 3) resultaría en meditar.meditar.meditar

esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad unEsbirro

unEsbirro :: Elemento
unEsbirro = UnElemento "Maldad" (causarDanio 1) noHacerNada

--jack = UnPersonaje { nombre = "jack",salud = 300, elementos = [concentracion 3, katanaMagica] , anioPresente = 200}


katanaMagica = UnElemento{
tipo="Magia", 
defensa = causarDanio 1000,
 ataque = id }

aku anio saludInicial = UnPersonaje {
nombre = "Aku",
salud = saludInicial,
anioPresente = anio,
elementos = [portalAlFuturoDesde anio]
}
portalAlFuturoDesde anio = UnElemento "Magia" (mandarAlAnio anioFuturo) (aku anioFuturo.salud)
  where anioFuturo = anio + 2800