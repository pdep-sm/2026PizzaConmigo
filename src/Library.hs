module Library where
import PdePreludat

{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ semilla    []    = semilla
foldl f semilla (x : xs) = foldl f (f semilla x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs 

foldr :: (b -> a -> a) -> a -> [b] -> a
foldr _ semilla    []    = semilla
foldr f semilla (x : xs) = f x (foldr f semilla xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f xs = foldr f (last xs) (init xs) 

flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' f x y = f y x
-}

-- Punto 1.a
data Pizza = Pizza {
    ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
} deriving (Show, Eq)

-- Punto 1.b
grandeDeMuzza :: Pizza
grandeDeMuzza = Pizza { 
    ingredientes = ["salsa", "muzzarella", "oregano"],
    tamanio = 8,
    calorias = 350
}

-- Punto 2
nivelDeSatisfaccion :: Pizza -> Number
nivelDeSatisfaccion pizza
    | (elem "palmito" . ingredientes) pizza = 0
    | calorias pizza < 500 = ((*80) . length . ingredientes) pizza
    | otherwise = ((*80) . length . ingredientes) pizza / 2

-- Punto 3
valor :: Pizza -> Number
valor pizza = ( (* tamanio pizza) . (*120) . length . ingredientes) pizza

{- Vemos que tenemos lógica repetida y HAY QUE CORREGIRLO -}

-- Punto 4.a
type Ingrediente = String

nuevoIngrediente :: Ingrediente -> Pizza -> Pizza
nuevoIngrediente ingrediente pizza = 
    agregar ingrediente . 
    sumarCalorias (length ingrediente * 2)
    $ pizza

agregar :: Ingrediente -> Pizza -> Pizza
agregar ingrediente pizza = 
    pizza {
        ingredientes = ingrediente : ingredientes pizza
    }

sumarCalorias :: Number -> Pizza -> Pizza
sumarCalorias cantidad pizza = 
    pizza {
        calorias = calorias pizza + cantidad
    }

-- Punto 4.b
agrandar :: Pizza -> Pizza
agrandar pizza = pizza { tamanio = min 10 $ tamanio pizza + 2 }

-- Punto 4.c
mezcladita :: Pizza -> Pizza -> Pizza
mezcladita pizza1 pizza2 = 
    pizza2 {
        ingredientes = sacarRepetidos $ ingredientes pizza1 ++ ingredientes pizza2,
        calorias = calorias pizza2 + calorias pizza1 / 2
    } -- Esta versión no nos gusta, es poco cohesiva

mezcladita' :: Pizza -> Pizza -> Pizza
mezcladita' pizza1 pizza2  = 
    quitarIngredientesRepetidos .   -- Esto lo agregamos nosotros, no lo pusimos en clase
    sumarCalorias (calorias pizza1 / 2) . 
    foldr agregar pizza2 . 
    ingredientes 
    $ pizza1

mezcladita'' :: Pizza -> Pizza -> Pizza
mezcladita'' pizza1 pizza2  = 
    quitarIngredientesRepetidos .   -- Esto lo agregamos nosotros, no lo pusimos en clase
    sumarCalorias (calorias pizza1 / 2) . 
    foldl (flip agregar) pizza2 . 
    ingredientes 
    $ pizza1

quitarIngredientesRepetidos :: Pizza -> Pizza
quitarIngredientesRepetidos pizza =
    pizza { ingredientes = sacarRepetidos $ ingredientes pizza }

sacarRepetidos :: Eq a => [a] -> [a]    -- Todavía no vimos estos tipos, viene más adelante
sacarRepetidos [] = []
sacarRepetidos (x : xs) = x : filter (/= x) xs

-- Punto 5
satisfaccionPedido :: [Pizza] -> Number
satisfaccionPedido = sum . map nivelDeSatisfaccion 

-- Punto 6
type Pizzeria = [Pizza] -> [Pizza]

-- Punto 6.a
pizzeriaLosHijosDePato :: Pizzeria
pizzeriaLosHijosDePato pizzas = map (nuevoIngrediente "palmito") pizzas

-- Punto 6.b
pizzeriaElResumen :: Pizzeria
pizzeriaElResumen [pizza] = [pizza] -- Acá habían faltado los corchetes a la derecha
pizzeriaElResumen pizzas = zipWith mezcladita pizzas $ tail pizzas

pizzeriaElResumen' :: Pizzeria
pizzeriaElResumen' [pizza] = [pizza] -- También faltaban los corchetes
pizzeriaElResumen' (p:ps) = zipWith mezcladita (p:ps) ps