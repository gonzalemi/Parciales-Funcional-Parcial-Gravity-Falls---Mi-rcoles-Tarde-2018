module Library where
import PdePreludat

-- Ej 1 
type Criterio = Persona -> Bool
type Peligrosidad = Int -> Int
type AsuntoPendiente = Persona -> Bool
type Enfrentamiento = Criatura -> Persona ->  Persona

data Persona = Persona {
    edad :: Int,
    items :: [String],
    experiencia :: Int
} deriving(Show)

data Criatura = Criatura {
    peligrosidad :: Int,
    criterioParaDesaherce :: Criterio
} deriving(Show)

peligrosidadSiempreDetras :: Peligrosidad
peligrosidadSiempreDetras _ = 0

criterioSiempreDetras :: Criterio
criterioSiempreDetras _ = False

peligrosidadGnomo :: Peligrosidad
peligrosidadGnomo = (^2)

criterioGnomo :: Criterio
criterioGnomo persona = (elem "soplador de hojas") . items $ persona

peligrosidadFantasma :: Peligrosidad
peligrosidadFantasma = (*20)

criterioFantasma :: AsuntoPendiente -> Criterio
criterioFantasma asuntoPendiente persona = asuntoPendiente persona  

condicionFantasmaEj persona = ((elem "soplador de hojas") . items $ persona) && ((>=13) . edad) persona 

-- Ej 2

calcularExperiencia :: Persona -> Criatura -> Int
calcularExperiencia persona criatura 
     | criterioParaDesaherce criatura persona = peligrosidad criatura
     | otherwise = 1

enfrentar :: Enfrentamiento
enfrentar criatura persona  = persona {
    experiencia = experiencia persona + (calcularExperiencia persona criatura)
}

-- Ej 3

siempreDetrasEj = Criatura {
    peligrosidad = 0,
    criterioParaDesaherce = criterioSiempreDetras
}


gnomoEj = Criatura {
    peligrosidad = peligrosidadGnomo 10,
    criterioParaDesaherce = criterioGnomo 
}

fantasmaEj :: Criatura
fantasmaEj = Criatura {
    peligrosidad = peligrosidadFantasma 3,
    criterioParaDesaherce = criterioFantasma $ condicionFantasmaEj
}

condicionFantasmaEj2 = (> 10) . experiencia 

fantasmaEj2 :: Criatura
fantasmaEj2 = Criatura {
    peligrosidad = peligrosidadFantasma 1,
    criterioParaDesaherce = criterioFantasma $ condicionFantasmaEj
}

personaEdad13 = Persona {
    edad = 15,
    items = [],
    experiencia = 1
}

personaEdad12 = Persona {
    edad = 12,
    items = [],
    experiencia = 1
}

personaConSoplador = Persona {
    edad = 15,
    items = ["soplador de hojas", "flauta", "espada"],
    experiencia = 1
}

enfrentamientoMultiple = (enfrentar fantasmaEj2) . (enfrentar fantasmaEj) . (enfrentar gnomoEj) 
    . (enfrentar siempreDetrasEj)

--- Segunda Parte
-- 1)
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf f1 f2 (x: xs) (y:ys) 
    | (not.f2) y = (y : zipWithIf f1 f2 (x:xs) ys)
    | otherwise = (f1 x y : zipWithIf f1 f2 xs ys)  

zipWithIf _ _ [] ys = ys
zipWithIf _ _ xs [] = []

-- 2.a)

abecedario :: [Char]
abecedario = ['a' , 'b' , 'c' , 'd','e','f','g','h','i','j','k','l','m','n', 'o','p','q','r','s','t','u','v','w','x','y','z']

mayorA :: Char -> [Char]
mayorA letra = filter(>= letra) abecedario

menorA :: Char -> [Char]
menorA letra = filter(< letra) abecedario

abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = mayorA letra ++ menorA letra

-- 2.b)
abecedariosZip :: Char -> [(Char, Char)]
abecedariosZip lClave = zip(abecedarioDesde lClave) abecedario

letraClave :: (Char, Char) -> Char
letraClave = snd

buscar :: Char -> [(Char, Char )]-> Char 
buscar letra = letraClave.head.filter(\(x, y) -> x == letra)

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra lClave lIncognita = buscar lIncognita $ abecedariosZip lClave

-- 2.c)

esLetra :: Char -> Bool
esLetra letra = elem letra abecedario

cesar :: Char -> [Char] -> [Char]
cesar lClave texto = zipWithIf2(desencriptarLetra) (esLetra) (abecedarioDesde lClave) texto

-- Tuve que crear esta funcion porque sino Cesar no desencriptaba ok
zipWithIf2 :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
zipWithIf2 f1 f2 (x: xs) (y:ys) 
    | (not.f2) y = (y : zipWithIf2 f1 f2 (x:xs) ys)
    | otherwise = (f1 x y : zipWithIf2 f1 f2 (x:xs) ys)  


zipWithIf2 _ _ [] ys = ys
zipWithIf2 _ _ xs [] = []


--- Bonus

filtrarTexto :: [Char] -> [Char]
filtrarTexto texto = filter(\l -> elem l abecedario) texto

zipTexto :: [Char] -> [Char] -> [(Char, Char)]
zipTexto clave texto = zip (filtrarTexto texto) $ (concat.repeat) clave

vigenere :: [Char] -> [Char] -> [Char]
vigenere clave texto = zipWithIf desencriptarLetra esLetra (map letraClave $ zipTexto clave texto) texto