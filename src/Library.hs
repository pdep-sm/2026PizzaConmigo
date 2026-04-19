module Library where
import PdePreludat

-- Punto 1.a
data Pizza = Pizza {
    ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
} deriving (Show, Eq)

-- Punto 1.b
grandeDeMuzza :: Pizza
grandeDeMuzza = Pizza ["salsa", "muzzarella", "oregano"] 8 350

-- Punto 2
nivelDeSatisfaccion :: Pizza -> Number
nivelDeSatisfaccion pizza
    | (elem "palmitos" . ingredientes) pizza = 0
    | calorias pizza < 500 = ((*80) . length . ingredientes) pizza
    | otherwise = ((*80) . length . ingredientes) pizza / 2

-- Punto 3
valor :: Pizza -> Number
valor pizza = ( (* tamanio pizza) . (*120) . length . ingredientes) pizza

{- Vemos que tenemos lógica repetida y HAY QUE CORREGIRLO -}