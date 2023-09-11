-- ejercicio 1
-- a)
--data Carrera = Matematica | Fisica | Computacion | Astronomia

-- b)
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

{-ghci> titulo Computacion
"Licenciatura en Ciencias de la Computacion"-}

{-ghci> titulo Fisica
"Licenciatura en Fisica"-}

-- c)
--data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si 

-- d)
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

{-ghci> cifradoAmericano Do
'C'-}

{-ghci> cifradoAmericano Sol
'G'-}

-- ejercicio 2 
-- a)
-- agrego en deriving la clase Bounded para poder usarla en la funcion minimoElemento'
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si
  deriving (Eq, Ord, Bounded, Show)

data Carrera = Matematica | Fisica | Computacion | Astronomia
  deriving (Eq, Ord, Bounded, Show)

{-ghci> Do <= Re
True-}

{-ghci> Matematica == Computacion
False-}

-- ejercicio 3
-- a)
minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:y:xs) | x <= y = minimoElemento (x:xs)
                        | otherwise = minimoElemento (y:xs)

{-ghci> minimoElemento [1,2,3,4,5]
1-}

{-ghci> minimoElemento [Re, Mi, Fa]
Re-}

{-ghci> minimoElemento [Fisica, Computacion, Astronomia]
Fisica-}

-- b)
minimoElemento' :: (Ord a, Bounded a,Show a) => [a] -> a
minimoElemento' [] = maxBound  
minimoElemento' (x:xs) = min x (minimoElemento' xs) 

{-ghci> minimoElemento' ([1,2,3,4,5] :: [Int])
1-}

{-ghci> minimoElemento' ([Sol, La, Re, Mi, Fa] :: [NotaBasica])
Re-}

{-ghci> minimoElemento' ([Computacion, Fisica, Astronomia] :: [Carrera])
Fisica-}

-- c)
-- ?????????????????????????????????????????????????

-- ejercicio 4
-- a)
-- Sinonimos de tipo
type Altura = Int
type NumCamiseta = Int
-- Tipos algebraicos sin parámetros (aka enumerados)
data Zona = Arco | Defensa | Mediocampo | Delantera
data TipoReves = DosManos | UnaMano
data Modalidad = Carretera | Pista | Monte | BMX
data PiernaHabil = Izquierda | Derecha
-- Sinónimo
type ManoHabil = PiernaHabil

data Deportista = Ajedrecista | Ciclista Modalidad | Velocista Altura | Tenista TipoReves ManoHabil Altura | Futbolista Zona NumCamiseta PiernaHabil Altura

-- b) Ciclista :: Modalidad -> Deportista

-- c)
contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas ((Velocista _):xs) = 1 + contar_velocistas xs
contar_velocistas (_:xs) = contar_velocistas xs

{-ghci> contar_velocistas [Velocista 180, Ajedrecista, Velocista 178]
2-}

{-ghci> contar_velocistas [Ciclista BMX, Ajedrecista]
0-}

-- d)
contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] _ = 0
contar_futbolistas ((Futbolista zona _ _ _):xs) z = contarFutbolistaEnZona zona z + contar_futbolistas xs z
contar_futbolistas (_ : xs) z = contar_futbolistas xs z

-- Función auxiliar para contar futbolistas en una Zona z
contarFutbolistaEnZona :: Zona -> Zona -> Int
contarFutbolistaEnZona Arco Arco = 1
contarFutbolistaEnZona Defensa Defensa = 1
contarFutbolistaEnZona Mediocampo Mediocampo = 1
contarFutbolistaEnZona Delantera Delantera = 1
contarFutbolistaEnZona _ _ = 0

{-ghci> contar_futbolistas [Futbolista Arco 1 Derecha 187, Ajedrecista, Futbolista Delantera 9 Derecha 190] Arco
1-}

{-ghci> contar_futbolistas [Futbolista Delantera 10 Derecha 177, Ajedrecista, Futbolista Delantera 9 Derecha 190] Delantera
2-}

-- e)
contar_futbolistas' :: [Deportista] -> Zona -> Int
contar_futbolistas' deportistas zona = length (filter (contarFutbolistaEnZona' zona) deportistas)

-- Instanciamos Eq para poder determinar si un Futbolista esta en la zona propuesta
instance Eq Zona where
  Arco == Arco = True
  Defensa == Defensa = True
  Mediocampo == Mediocampo = True
  Delantera == Delantera = True
  _ == _ = False

-- Función auxiliar para verificar si un futbolista está en la zona específica
contarFutbolistaEnZona' :: Zona -> Deportista -> Bool
contarFutbolistaEnZona' z (Futbolista zona _ _ _) = z == zona 
contarFutbolistaEnZona' z _= False

{-ghci> contar_futbolistas' [Futbolista Mediocampo 16 Izquierda 169, Ajedrecista, Futbolista Defensa 2 Derecha 195] Mediocampo
1
ghci> contar_futbolistas' [Futbolista Defensa 6 Izquierda 193, Ajedrecista, Futbolista Defensa 2 Derecha 195] Mediocampo
0
ghci> contar_futbolistas' [Futbolista Defensa 6 Izquierda 193, Ajedrecista, Futbolista Defensa 2 Derecha 195] Defensa
2-}

-- ejercicio 5
-- a)
sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11

{-ghci> sonidoNatural Re
2
ghci> sonidoNatural Sol
7-}

-- b)
data Alteracion = Bemol | Natural | Sostenido

-- c)
data NotaMusical = Nota NotaBasica Alteracion 

-- d)
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota notabasica Natural) = sonidoNatural notabasica
sonidoCromatico (Nota notabasica Bemol) = sonidoNatural notabasica - 1
sonidoCromatico (Nota notabasica Sostenido) = sonidoNatural notabasica + 1

{-ghci> sonidoCromatico (Nota Do Sostenido)
1
ghci> sonidoCromatico (Nota Fa Bemol)
4
ghci> sonidoCromatico (Nota Sol Natural)
7-}

-- e)
instance Eq NotaMusical 
  where 
    nota1 == nota2 = sonidoCromatico(nota1) == sonidoCromatico (nota2) 

{-ghci> (Nota Do Sostenido) == (Nota Do Natural) 
False
ghci> (Nota Mi Sostenido) == (Nota Fa Natural) 
True-}

-- f)
instance Ord NotaMusical 
  where 
    nota1 <= nota2 = sonidoCromatico(nota1) <= sonidoCromatico (nota2) 

{-ghci> (Nota Mi Sostenido) <= (Nota Fa Bemol) 
False
ghci> (Nota Do Natural) <= (Nota Do Sostenido) 
True-}

-- ejercicio 6
-- a)
primerElemento ::[a] -> Maybe a
primerElemento [] = Nothing 
primerElemento (x:xs) = Just x

{-ghci> primerElemento [1,2,3]
Just 1

ghci> primerElemento "abcde"
Just 'a'-}

-- ejercicio 7
data Cola = VaciaC | Encolada Deportista Cola deriving (Show)

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing  -- Si la cola está vacía, devolvemos Nothing
atender (Encolada _ resto) = Just resto  -- Si hay deportistas encolados, devolvemos la cola restante

