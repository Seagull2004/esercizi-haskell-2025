import Control.Monad.Trans.Cont (shift)
import Data.IntMap (insert)
-- WARNING:
-- gli esercizi qui sotto riportati sono tratti dal pdf:
-- ~/vault/01 - PROJECTS/2526-1 LINGUAGGI DI PROGRAMMAZIONE/materiale/esercizi_haskell/EserciziProgrammazioneHaskell.pdf
--
---------------------------------------------------------------------------------
-- argomento 1: NUMERI
-- Si ricordi che si dispone di varie funzioni aritmetiche polimorfe nel Prelude, come 
-- (+) , (*) :: ( Num a ) = > a -> a -> a 
-- ( div ) :: ( Integral a ) = > a -> a -> a
--
-- difficoltà: base

-- (1)
-- Si scriva la funzione fattoriale. Si verifichi il funzionamento calcolando 10000!
factV1 :: Int -> Int
factV1 0 = 1
factV1 n = n * factV1 (n - 1)

factV2 :: Int -> Int
factV2 n = foldl (*) 1 [1..n]

factV3 :: Int -> Int
factV3 n = product [1..n]

-- (2)
-- Si scriva la funzione \binom n k , combinazioni di k elementi su n.
-- note:
--  bormula binomio n su k:  n! / (k! * (n-k)!)
binomV1 :: Int -> Int -> Int
binomV1 n k = factV1 n `div` (factV1 k * factV1 (n - k))

binomV2 :: Int -> Int -> Int
binomV2 n k = foldl (*) 1 [(n-k+1)..n] `div` factV1 k

binomV3 :: Int -> Int -> Int
binomV3 n k = product [(n-k+1)..n] `div` factV1 k

-- (3)
-- Si scriva una funzione che calcoli una lista con tutte le combinazioni su n elementi. 
-- Si usi opportunamente map :: ( a -> b ) -> [ a ] -> [ b ]
-- note:
--  combinazioni di n elementi ~ insieme delle parti dei numeri da 1 a n
--  usare: map :: (a -> b) -> [a] -> [b]
combV1 :: Int -> [[Int]]
combV1 n = combV1Aux [1..n]
  where
    combV1Aux :: [Int] -> [[Int]]
    combV1Aux []     = [[]]
    combV1Aux (x:xs) = combV1Aux xs ++ map (x:) (combV1Aux xs)

combV2 :: Int -> [[Int]]
combV2 0 = [[]]
combV2 n = combV2 (n - 1) ++ map (n:) (combV2 (n - 1))


---------------------------------------------------------------------------------
-- argomento 2: LISTE
-- Si ricordi che si dispone di varie funzioni del Prelude, come 
-- foldr :: (a - >b - > b ) -> b -> [ a ] -> b 
-- che accumula, a partire da un opportuno elemento neutro, tutti gli elementi di una lista applicando un operatore binario da destra a sinistra 
-- foldr f z [ x1 , x2 ,... , xn ] = ( x1 ‘f ‘ ( x2 ‘f ‘ ... ( xn ‘f ‘ z )...))
--
-- difficoltà: 1..4 base
--             5    fold for beginners
--             6..  fold & co

-- (1)
-- Scrivere una funzione che data una lista ne costruisce una rimuovendo gli elementi di posizione pari (si conti partendo da 1).
removeEvenPositionV1 :: [Int] -> [Int]
removeEvenPositionV1 []     = []
removeEvenPositionV1 (x:xs) = x : removeEvenPositionV1_aux xs
  where
    removeEvenPositionV1_aux :: [Int] -> [Int]
    removeEvenPositionV1_aux []     = []
    removeEvenPositionV1_aux (x:xs) = removeEvenPositionV1 xs

removeEvenPositionV2 :: [Int] -> [Int]
removeEvenPositionV2 xs = fst (foldr step ([], length xs) xs)
  where
    step :: Int -> ([Int], Int) -> ([Int], Int)
    step x (acc, pos) = 
      if even pos
        then (acc, pos - 1)
        else (x : acc, pos - 1)

removeEvenPositionV3 :: [Int] -> [Int]
removeEvenPositionV3 xs = fst (foldl step ([], 1) xs)
  where
    step :: ([Int], Int) -> Int -> ([Int], Int)
    step (acc, nr) x =
      if even nr
        then (acc, nr + 1)
        else (acc ++ [x], nr + 1)

-- (2)
-- Scrivere una funzione che calcola la somma degli elementi di posizione dispari di una lista.
sumOddPositionV1 :: [Int] -> Int
sumOddPositionV1 xs = fst (foldl sumOdd (0, 1) xs)
  where
    sumOdd :: (Int, Int) -> Int -> (Int, Int)
    sumOdd (partialSum, i) x =
      if even i
        then (partialSum, i + 1)
        else (partialSum + x, i + 1)

-- (3)
-- Scrivere il QuickSort (polimorfo).
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++ [x] ++ quickSort [y | y <- xs, x <= y]

-- (4)
-- Scrivere una funzione che calcola i 2 minori elementi dispari di una lista (se esistono). Ad esempio minOdd([2,3,4,6,8,7,5]) riduce a (3,5)
minOddV1 :: [Int] -> [Int]
minOddV1 [] = []
minOddV1 xs = minOddAux xs []
  where
    minOddAux :: [Int] -> [Int] -> [Int]
    minOddAux [] odds = odds
    minOddAux (x:xs) odds
      | even x                                      = minOddAux xs odds
      | odd x && null odds                          = minOddAux xs [x]
      | odd x && length odds == 1 && x < odds!!0    = minOddAux xs [x]
      | odd x && length odds == 1 && x >= odds!!0   = minOddAux xs [odds!!0, x]
      | odd x && length odds == 2 && x < odds!!0    = minOddAux xs [x, odds!!0]
      | odd x && length odds == 2 && x < odds!!1    = minOddAux xs [odds!!0, x]
      | otherwise                                   = minOddAux xs odds


minOddV2 :: [Int] -> [Int]
minOddV2 [] = []
minOddV2 xs = take 2 (quickSort (filter odd xs))


-- (5)
-- Scrivere una funzione che costruisce, a partire da una lista di numeri interi, una lista di coppie in cui 
--  (a) il primo elemento di ogni coppia è uguale all’elemento di corrispondente posizione nella lista originale e 
--  (b) il secondo elemento di ogni coppia è uguale alla somma di tutti gli elementi conseguenti della lista originale
creaListaCoppieV1 :: [Int] -> [(Int,Int)]
creaListaCoppieV1 [] = []
creaListaCoppieV1 xs = creaListaCoppieAux xs (sum xs)
  where
    creaListaCoppieAux :: [Int] -> Int -> [(Int,Int)]
    creaListaCoppieAux [] inc = []
    creaListaCoppieAux (x:xs) inc = (x, inc - x) : creaListaCoppieAux xs (inc - x)

creaListaCoppieV2 :: [Int] -> [(Int,Int)]
creaListaCoppieV2 [] = []
creaListaCoppieV2 (x:xs) = (x, foldr (+) 0 xs) : creaListaCoppieV2 xs

creaListaCoppieV3 :: [Int] -> [(Int,Int)]
creaListaCoppieV3 [] = []
creaListaCoppieV3 (x:xs) = (x, sum xs) : creaListaCoppieV3 xs

-- (6)
-- Scrivere una funzione che costruisce, a partire da una lista di numeri interi (provate poi a generalizzare), una lista di coppie in cui 
--  (a) il primo elemento di ogni coppia è uguale all’elemento di corrispondente posizione nella lista originale e 
--  (b) il secondo elemento di ogni coppia è uguale alla somma di tutti gli elementi antecedenti della lista originale
-- [farlo con foldr o foldl è difficile]
-- TODO: è possibile farlo con foldl?
creaListaCoppieAntV1 :: [Int] -> [(Int,Int)]
creaListaCoppieAntV1 [] = []
creaListaCoppieAntV1 xs = creaListaCoppieAntAux xs 0
  where
    creaListaCoppieAntAux [] inc     = []
    creaListaCoppieAntAux (x:xs) inc = (x, inc) : creaListaCoppieAntAux xs (inc + x)

-- (7)
-- Si scriva una funzione Haskell shiftToZero che data una lista costruisce un nuova lista che contiene gli elementi diminuiti del valore minimo. 
-- A titolo di esempio, shiftToZero [5,4,2,6] => [3,2,0,4]. La funzione non deve visitare gli elementi della lista più di una volta (si sfrutti la laziness).
-- [farlo con foldr o foldl è difficile]
shiftToZero [] = []
shiftToZero (x:xs) = map (\y -> y - foldr min x xs) (x:xs)

---------------------------------------------------------------------------------
-- argomento 3: MATRICI
-- Le matrici si implementano come liste di liste, per righe o per colonne a seconda delle preferenze.
--
-- difficoltà: 1    base
--             2..  fold for beginners

-- (1)
-- Si scriva una funzione matrix_dim che data una matrice ne calcola le dimensioni, se la matrice è ben formata, altrimenti restituisce (-1,-1).
matrixDim :: [[a]] -> [Int]
matrixDim []  = [0,0]
matrixDim mat = matrixDimAux (length mat) (length (mat !! 0)) mat
  where
    matrixDimAux :: Int -> Int -> [[a]] -> [Int]
    matrixDimAux n m [] = [n,m]
    matrixDimAux n m (line:rest)
      | length line == m         = matrixDimAux n m rest
      | otherwise                = [-1,-1]

-- (2)
-- Si scriva una funzione colsums che data una matrice calcola il vettore delle somme delle colonne
matColSumV1 :: (Num a) => [[a]] -> [a]
matColSumV1 []          = []
matColSumV1 (line:rest) = colSumAux rest line
  where
    colSumAux :: (Num a) => [[a]] -> [a] -> [a]
    colSumAux [] result = result
    colSumAux (line:rest) result
      | null line = result
      | otherwise = colSumAux rest (sumVect result line)
        where
          sumVect :: (Num a) => [a] -> [a] -> [a]
          sumVect = zipWith (+)

-- matColSumV2 [] = []
-- matColSumV2 ([]:_) = []
-- matColSumV2 mat = sum (map head mat) : matColSumV2 (map tail mat)

pippo = [[1,2,3,4],
         [1,2,3,4],
         [1,2,3,4],
         [1,2,3,4]]

pluto = [[5,7,9,8],
         [1,3,4,2],
         [9,3,10,7],
         [11,12,0,3]]

-- (3)
-- Si scriva una funzione colaltsums che, data una matrice implementata come liste di liste per righe, calcola il vettore delle somme a segni alternati delle colonne della matrice.
-- Sia sⱼ la somma della colonan j-esima: sⱼ = ∑ᵢ₌₁ⁿ (-1)ⁱ⁺¹ aᵢⱼ, colaltsums(matrice n x m) = (s₁ ... sₘ) 
colAltSumsV1 :: (Num a) => [[a]] -> [a]
colAltSumsV1 []          = []
colAltSumsV1 (line:rest) = colSumAux rest line True
  where
    colSumAux :: (Num a) => [[a]] -> [a] -> Bool -> [a]
    colSumAux [] result _ = result
    colSumAux (line:rest) result invert
      | null line = result
      | invert = colSumAux rest (subVect result line) (not invert)
      | otherwise = colSumAux rest (sumVect result line) (not invert)
        where
          sumVect :: (Num a) => [a] -> [a] -> [a]
          sumVect = zipWith (+)
          subVect :: (Num a) => [a] -> [a] -> [a]
          subVect = zipWith (-)

-- (4)
-- Si scriva una funzione colMinMax che, data una matrice implementata come liste di liste per righe, calcola il vettore delle coppie (minimo,massimo) delle colonne della matrice
colMinMax :: (Ord a) => [[a]] -> [(a,a)]
colMinMax []     = []
colMinMax (l:ls) = foldl (zipWith updatePair) (vectToPairs l) ls
  where
    updatePair (min,max) value
      | value < min = (value, max)
      | value > max = (min, value)
      | otherwise   = (min,max)
    vectToPairs :: [a] -> [(a,a)]
    vectToPairs [] = []
    vectToPairs xs = map (\y -> (y,y)) xs

-- (5)
-- Si scriva un predicato lowertriangular che determina se una matrice (quadrata) è triangolare inferiore. A titolo di esempio, lowertriangular([[1,0,0],[2,-3,0],[4,5,6]]) restituisce True, mentre lowertriangular([[0,0,1],[2,-3,0],[4,5,6]]) restituisce False
lowerTriangular :: (Eq a, Num a) => [[a]] -> Bool
lowerTriangular mat = fst (foldl lowerTriangularAux (True, 1) mat)
  where
    lowerTriangularAux :: (Eq a, Num a) => (Bool, Int) -> [a] -> (Bool, Int)
    lowerTriangularAux (isTriangular, rowNr) line
      | all (== 0) (drop rowNr line) = (isTriangular && True, rowNr + 1)
      | otherwise                    = (False, rowNr + 1)

-- (6)
-- Si scriva un predicato uppertriangular che determina se una matrice (quadrata) è triangolare superiore
upperTriangular :: (Eq a, Num a) => [[a]] -> Bool
upperTriangular mat = fst (foldl upperTriangularAux (True, 0) mat)
  where
    upperTriangularAux :: (Eq a, Num a) => (Bool, Int) -> [a] -> (Bool, Int)
    upperTriangularAux (isTriangular, rowNr) line
      | all (== 0) (take rowNr line) = (isTriangular && True, rowNr + 1)
      | otherwise                    = (False, rowNr + 1)

-- (7)
-- Si scriva un predicato diagonal che determina se una matrice (quadrata) è diagonale.
diagonalV1 :: (Eq a, Num a) => [[a]] -> Bool
diagonalV1 mat = lowerTriangular mat && upperTriangular mat

diagonalV2 :: (Eq a, Num a) => [[a]] -> Bool
diagonalV2 mat = fst (foldl diagonalAux (True, 0) mat)
  where
    diagonalAux :: (Eq a, Num a) => (Bool, Int) -> [a] -> (Bool, Int)
    diagonalAux (isDiagonal, rowNr) line
      | all (== 0) (take rowNr line ++ drop (rowNr+1) line) = (isDiagonal && True, rowNr + 1)
      | otherwise                                           = (False, rowNr + 1)

-- (8)
-- Una matrice quadrata M di ordine n si dice convergente con raggio r se il modulo della somma degli elementi di ogni riga, escluso quello sulla diagonale, è inferiore a r. Si scriva un predicato convergent m r che determina se una matrice (quadrata) m è convergente con raggio r.

-- (9)
-- Si scriva una funzione che data una matrice di dimensioni m × n restituisce la corrispondente matrice trasposta (di dimensioni n × m).

-- (10)
-- Si scriva un predicato isSymmetric che, data una matrice quadrata, determina se è simmetrica

-- (11)
-- Si scriva una funzione che data una matrice di dimensioni n × k ed una k × m restituisca la matrice prodotto corrispondente (di dimensioni n × m). Si assuma di moltiplicare matrici con dimensioni compatibili e (se facesse comodo) matrici non degeneri


---------------------------------------------------------------------------------
-- argomento 4: ALBERI BINARI DI RICERCA
-- Si definiscano gli Alberi Binari di Ricerca col seguente tipo di dato astratto (polimorfo) 
-- data ( Ord a , Show a , Read a ) = > BST a = Void | Node { 
--   val :: a , 
--   left , right :: BST a 
--   } 
--   deriving ( Eq , Ord , Read , Show ) 
--
-- e si usi (per comodità) lo stesso tipo di dato anche per Alberi Binari normali.
--
-- difficoltà: 1..12  base
--             14..20 fold for beginners
--             21..   fold & co

data BST a = Void | Node {
  val :: a ,
  left , right :: BST a
  }
  deriving ( Eq , Ord , Read , Show )


gioia = Node 10 (Node 5 Void Void) (Node 3 Void Void)
yei = Node 30 (Node 5 (Node 40 Void Void) Void) (Node 3 Void Void)

yai = Node 30 (Node 3 (Node 40 Void Void) Void) (Node 5 Void Void)


-- (1)
-- Scrivere una funzione che calcola la somma dei valori di un albero a valori sommabili.
sommaAlbero Void = 0
sommaAlbero t    = val t + sommaAlbero (left t) + sommaAlbero (right t)

-- (2)
-- Scrivere una funzione che calcola la somma dei valori dispari di un albero a valori sommabili su cui sia utilizzabile la funzione odd.
sommaDispariAlbero Void = 0
sommaDispariAlbero t
  | odd (val t) = val t + sommaDispariAlbero (left t) + sommaDispariAlbero (right t)
  | otherwise   = sommaDispariAlbero (left t) + sommaDispariAlbero (right t)

-- (3)
-- Si scriva un predicato samesums che presa una lista di alberi [t₁,...,tₙ] determina se le somme
-- s₁,...,sₙ dei valori degli elementi di ogni ti sono tutte uguali fra loro.
sameSumsV1 [] = True
sameSumsV1 ts = fst (foldr step (True, Nothing) ts)
  where
    step t (_, Nothing)    = (True, Just (sommaAlbero t))
    step t (False, sum)    = (False, sum)
    step t (True, sum)
      | Just (sommaAlbero t) == sum  = (True, sum)
      | otherwise                    = (False, sum)

sameSumsV2 [] = True
sameSumsV2 (t:ts) =
  all ((== sommaAlbero t) . sommaAlbero) ts

-- (4)
-- Scrivere un predicato bstElem (infisso magari) per determinare se un valore è presente in un BST.
bstElem Void _  = False
bstElem t n
  | val t == n  = True
  | otherwise   = bstElem (left t) n || bstElem (right t) n

-- (5)
-- Si scriva una funzione per eseguire l’inserimento di un dato x in un albero t.
insertInTree :: (Ord a) => BST a -> a -> BST a
insertInTree Void x = Node x Void Void
insertInTree t x 
  | x > val t = Node (val t) (left t) (insertInTree (right t) x)
  | otherwise = Node (val t) (insertInTree (left t) x) (right t)

bst = Node 10 (Node 5 (Node 3 Void Void) (Node 7 Void Void)) (Node 20 (Node 14 Void Void) (Node 30 Void Void))

-- (6)
-- Si scriva una funzione bst2List che calcola la lista ordinata degli elementi di un BST. Ci si assicuri di scrivere una funzione lineare.
bst2List :: BST a -> [a]
bst2List Void = []
bst2List t = bst2List (left t) ++ [val t] ++ bst2List (right t)

-- (7)
-- Si scriva una (semplice) funzione di ordinamento di liste come combinazione di funzioni fatte nei
-- precedenti esercizi

-- (8)
-- Si scriva una funzione filtertree p t che costruisce una lista (ordinata) di tutti gli elementi
-- dell’albero t che soddisfano il predicato p.

-- (9)
-- Si scriva una funzione annotate che costruisca un nuovo BST che in ogni nodo contenga, al posto
-- del valore originale, una coppia composta dal medesimo valore e dall’altezza del nodo stesso (la
-- lunghezza del massimo cammino, cio`e 1 + max(height(sx),height(dx)). Si scelga di attribuire
-- all’albero vuoto 0 o -1 a seconda delle preferenze.
-- [Con una opportuna scelta dell’ordine di ricorsione si pu`o fare in tempo lineare]

-- (10)
-- Si scriva un predicato (funzione a valori booleani) almostBalanced per determinare se un albero
-- binario ha la seguente propriet`a: per ogni nodo le altezze dei figli destro e sinistro diﬀeriscono al
-- massimo di 1.

-- (11)
-- Data la seguente definizione del tipo di dato astratto (polimorfo) Weighted Binary Search Tree che
-- consiste in un BST in cui in ogni nodo viene mantenuta l’altezza del nodo stesso.
-- data WBST a = Void | Node a Int ( WBST a ) ( WBST a )
-- Si scriva una funzione insert che inserisce un nuovo valore in un WBST.

-- (12)
-- Si scriva una funzione diff2next che, dato un albero binario di ricerca, costruisce un albero
-- binario di ricerca (annotato) di coppie dove il primo elemento di ogni coppia è l’elemento dell’albero
-- originale mentre il secondo elemento è Just(la diﬀerenza rispetto al valore successivo), secondo
-- l’ordinamento dei valori contenuti, oppure Nothing per il nodo di valore massimo. A titolo di
-- esempio,
-- Node 4 Void (Node 7 (Node 5 Void Void) Void)
-- restituisce la soluzione
-- Node (4,Just 1) Void (Node (7,Nothing) (Node (5,Just 2) Void Void) Void).

-- (13)
-- Si scriva una funzione che dato un BST ne restituisce la lista degli elementi ottenuti visitando
-- l’albero a livelli.
-- Si consideri d’ora in poi la seguente generalizzazione a BST della funzione foldr su liste:
-- fold :: ( Ord a ) = > ( a -> b -> b -> b ) -> b -> BST a -> b
-- fold _ z Void = z
-- fold f z ( Node x l r ) = f x ( fold f z l ) ( fold f z r )
-- Ci si assicuri di scrivere funzioni lineari (non ha senso scrivere soluzioni che usino “forzosamente” una
-- fold).

-- (14)
-- Si scriva una funzione treeheight per calcolare l’altezza di un albero usando opportunamente
-- fold.

-- (15)
-- Si riscriva la funzione annotate dell’Esercizio 9 usando opportunamente fold.

-- (16)
-- Si riscriva la funzione almostBalanced dell’Esercizio 10 usando opportunamente fold.

-- (17)
-- Si scriva una funzione maxDiameter che data una lista l di BST determina il massimo dei dia-
-- metri dei BST di l. Il diametro di un BST è la lunghezza del massimo cammino fra due nodi,
-- indipendentemente dall’orientamento degli archi.

-- (18)
-- Si scriva un predicato isBST, usando opportunamente fold, che dato un albero verifica se i valori
-- in esso contenuti soddisfano la propriet`a strutturale dei Binary Search Trees.

-- (19)
-- Si scriva un predicato isAVL che dato un albero secondo la seguente definizione di tipo
-- data ( Ord a ) = > ABST a = Void | Node Bal a ( ABST a ) ( ABST a )
-- deriving ( Eq , Ord , Read , Show )
-- data Bal = Left | Bal | Right deriving ( Eq , Ord , Read , Show )
-- determina se è ben formato, cio`e se
-- •la diﬀerenza fra le profondit`a dei sottoalberi destro e sinistro di un qualunque nodo è al
-- massimo 1;
-- •le etichette Bal dei nodi sono consistenti con lo (s)bilanciamento

-- (20)
-- Si scriva un predicato isRBT che dato un albero secondo la seguente definizione di tipo
-- data ( Ord a ) = > RBT a = Void | Node a Color ( RBT a ) ( RBT a )
-- deriving ( Eq , Ord , Read , Show )
-- data Color = Red | Black deriving ( Eq , Ord , Read , Show )
-- determina se è ben formato, cio`e se
-- • ogni nodo contiene un valore non minore dei valori del suo sottoalbero sinistro e minore dei
-- valori del sottoalbero destro;
-- • tutti i cammini dalla radice a una foglia hanno lo stesso numero di nodi Black;
-- • i nodi Red devono avere genitore Black;
-- • la radice è Black.

-- (21)
-- Si riscriva la funzione bst2List dell’Esercizio 6 usando opportunamente fold.
-- Diﬃcile quanto l’Esercizio 6 delle liste

-- (22)
-- Si riscriva la funzione filtertree dell’Esercizio 8 usando opportunamente fold.

-- (23)
-- Si riscriva la funzione diff2next dell’Esercizio 12 usando opportunamente fold.

-- (24)
-- Si scriva una funzione limitedVisit che dato un BST e due valori x,ycostruisce la lista (ordinata)
-- degli elementi dell’albero compresi nell’intervallo di valori da x a y.
--
-- NB: Garantire che let d=d in limitedVisit 3 7 (Node 7 (Node 2 (Node d Void Void)
-- Void) (Node d Void Void)) termini non è immediato.

-- (25)
-- Si scriva una funzione shiftToZero che dato un BST t costruisce un nuovo BST isomorfo che
-- contiene gli elementi t diminuiti del valore minimo di t.
-- La funzione non deve visitare un nodo dell’albero t pi`u di una volta (si sfrutti laziness e scoping
-- mutuamente ricorsivo).
--
-- NB: Diﬃcile quanto l’Esercizio 7 delle liste


---------------------------------------------------------------------------------
-- argomento 5: ALBERI GENERICI
-- Si definiscano Alberi “generici” col seguente tipo di dato astratto (polimorfo) 
--
-- data Tree a = Void | Node a [Tree a] 
--   deriving ( Eq , Show ) 
--
-- Con questo tipo di dato ci sono vari possibili modi per rappresentare una foglia: 
-- + (Node x []), 
-- + (Node x [Void]), 
-- + (Node x [Void, Void]), 
-- + ...,
-- + (Node x [Void, . . . Void]), 
-- + ...
--
-- Rinunciando all’albero vuoto si avrebbe una formulazione unica come
-- data (Eq a , Show a) => NonEmptyTree a = Node a [NonEmptyTree a]
--   deriving (Eq , Show)
-- 
-- ma nel seguito abbiamo bisogno dell’albero vuoto e andremo a convivere con la rappresentazione non univoca.
--
-- difficoltà: 1..14 fold for beginners
--             15..  fold & co
data Tree a = VoidTree | NodeTree a [Tree a] 
  deriving (Eq , Show)

-- (1)
-- Si scriva una generalizzazione della funzione foldr delle liste per Alberi Generici che abbia il
-- seguente tipo:
treefold :: (Eq a, Show a) => (a -> [b] -> b) -> b -> Tree a -> b
treefold _ accumulatore VoidTree = accumulatore
treefold f accumulatore (NodeTree a ts) =
  f a (map (treefold f accumulatore) ts)

x = NodeTree 10 [NodeTree 4 [], NodeTree 3 []] 
risultato = treefold (\a b -> a + sum b) 0 x -- somma tutti i nodi dell'albero

-- (2)
-- Si scriva una funzione height per calcolare l’altezza di un albero usando opportunamente la
-- treefold dell’Esercizio 1. Si attribuisca altezza -1 all’albero vuoto.
-- Si colga l’occasione per verificare che treefold sia stata definita correttamente e quindi
-- height ( Node ’a ’ $ replicate n Void )
-- restituisca sempre 0 al variare di n.
treeHeight :: (Eq a, Show a) => Tree a -> Int
treeHeight VoidTree = -1
treeHeight t = treefold increaseHeight (-1) t
  where
    increaseHeight :: (Eq a, Show a) => a -> [Int] -> Int
    increaseHeight x [] = 0
    increaseHeight x tx = 1 + maximum tx

y = NodeTree 150 (replicate 10 VoidTree)


-- (3)
-- Si scriva una funzione simplify per eliminare i figli Void ridondanti usando opportunamente la
-- treefold dell’Esercizio 1.
treeSimplify VoidTree = VoidTree
treeSimplify t = treefold simiplifyChildrenList VoidTree t
  where 
    simiplifyChildrenList :: (Eq a, Show a) => a -> [Tree a] -> Tree a
    simiplifyChildrenList x ts = NodeTree x (removeVoidInList ts) 
    removeVoidInList [] = []
    removeVoidInList (VoidTree:ts) = removeVoidInList ts
    removeVoidInList (x:ts) = x : removeVoidInList ts

-- (4)
-- Si scrivano le generalizzazioni delle funzioni foldr e foldl delle liste per Alberi Generici aventi i
-- seguenti tipi (abbiamo bisogno di due “zeri” corrispondenti all’albero vuoto e alla lista di alberi
-- vuota):
-- treefoldr treefoldl :: :: ( Eq ( Eq a , Show a , Show a ) a ) = > = > (a - >b - > c ) - >c - >( c - >b - > b ) - >b - > Tree (b - >a - > c ) - >c - >( c - >b - > b ) - >b - > Tree a - > c
-- a - > c
-- Con queste fold non c’e bisogno di costruire la lista intermedia a cui applicare la funzione di
-- “aggregazione” ma si esegue il lavoro man mano.

-- (5)
-- Si riscriva la funzione height per calcolare l’altezza di un albero usando opportunamente la
-- treefoldr dell’Esercizio 4.

-- (6)
-- Si riscriva la funzione simplify per eliminare i figli Void ridondanti usando opportunamente la
-- treefoldr dell’Esercizio 4.

-- (7)
-- Si scriva una funzione degree che restituisce il grado di un albero (il massimo del numero di figli
-- per ogni nodo).

-- (8)
-- Si scriva una funzione transpose che restituisce il trasposto di un albero (per ogni nodo i trasposti
-- dei figli in ordine inverso).

-- (9)
-- Si scriva un predicato issymm che stabilisce se un albero ha una forma simmetrica (cio`e `e uguale,
-- non considerando il contenuto, al suo trasposto)

-- (10)
-- Si scriva una funzione normalize che dato un albero con valori nella classe Integral costruisca un
-- nuovo albero che in ogni nodo contenga, al posto del valore originale, tale valore moltiplicato per
-- l’inverso dell’altezza. (Si presti attenzione nell’espressione della moltiplicazione in modo da avere
-- tipi compatibili).

-- (11)
-- Si scriva una funzione annotate che costruisca un nuovo albero che in ogni nodo contenga, al posto
-- del valore originale, una coppia composta dal medesimo valore e dall’altezza del nodo stesso.

-- (12)
-- Si scriva un predicato iscorrect che determina se un albero `e un albero di parsing secondo le
-- regole di una grammatica codificata mediante una funzione che, dato un simbolo, restituisce la
-- lista delle possibili espansioni (stringhe di simboli) secondo le produzioni.

-- (13)
-- Si scriva una funzione diameter che determina il diametro di un albero. Il diametro di un albero `e
-- la lunghezza del massimo cammino fra due nodi, indipendentemente dall’orientamento degli archi.

-- (14)
-- Si scriva una funzione maxPathWeight che, dato un albero di valori numerici positivi, determina il
-- massimo peso di tutti i cammini, indipendentemente dall’orientamento degli archi. Il peso di un
-- cammino, `e la somma dei valori dei nodi del cammino.

-- (15)
--

-- (16)
--

-- (17)
--

-- (18)
--

-- (19)
--

-- (20)
--

-- (21)
--

-- (22)
--

-- (23)
--

-- (24)
--

-- (25)
--

---------------------------------------------------------------------------------
-- argomento 6: QUAD TREES
-- Molte tecniche sviluppate per la compressione di immagini si basano su una codifica ad albero chiamata “Quad Tree”. Si codificano in questo modo immagini quadrate il cui lato sia una potenza di 2. Se l’immagine è omogenea (stesso colore) si codifica, indipendentemente dalle sue dimensioni, con una foglia contenente il colore. Se l’immagine è eterogenea allora si utilizza un nodo i cui figli contengono le codifiche dei quadranti superiore-sinistro, superiore-destro, inferiore-sinistro, inferiore-destro, rispettivamente. 
-- Si definiscano i QuadTrees col seguente tipo di dato astratto (polimorfo) 
--
-- data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) 
--   deriving (Eq , Show) 
--
-- Con questa struttura si possono costruire termini che non corrispondono propriamente ad un QuadTree. Ad esempio 
-- let u = C 1 in Q u u u u 
-- non è la codifica di un’immagine, visto che dovrebbe essere semplicemente C 1. Chiamerò "termini di tipo QT" questi casi patologici, mentre QuadTrees quelli che corrispondono correttamente alla codifica di un'immagine. Possiamo subito notare dall'esempio di prima che partendo da 4 QuadTrees non si garantisce di costruire con il costruttore Q un QuadTree, ma solo un termine di tipo QT.
--
-- difficoltà: 1..8 base
--             9..  fold for beginners

data QT a = C a | Q (QT a) (QT a) (QT a) (QT a) 
  deriving (Eq , Show)

myQuadTree = Q (C 1) (C 2) (C 3) (Q (C 4) (C 1) (C 2) (C 3))
quad1 = Q (C 1) (C 1) (C 1) (C 1)
quad2 = Q (C 7) (C 3) (C 2) (C 9)
quad3 = Q (C 8) (C 9) (C 1) (C 8)
quad4 = Q (C 9) (C 1) (C 0) (Q (C 1) (C 1) (C 1) (C 1))
quad5 = Q (C 18) (C 19) (C 11) (C 18)
quad6 = Q (C 9) (C 1) (C 0) (Q (C 1) (C 1) (C 1) (C 2))

-- (1)
-- Si scriva una funzione `buildNSimplify` che dati 4 QuadTree costruisca un QuadTree la cui im-
-- magine codificata sia quella ottenuta dalle 4 immagini corrispondenti ai 4 QuadTree messe nei
-- quadranti superiore-sinistro, superiore-destro, inferiore-sinistro, inferiore-destro, rispettivamente.
-- (Attenzione che tutti sono e devono essere QuadTrees, non solo termini di tipo QT)
buildNSimplify :: (Eq a) => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify q1 q2 q3 q4 = fixPatologic (Q q1 q2 q3 q4)
  where
    fixPatologic :: (Eq a) => QT a -> QT a
    fixPatologic (C a) = C a
    fixPatologic (Q (C a) (C b) (C c) (C d))
      | a == b && b == c && c == d  = C a
      | otherwise                   = Q (C a) (C b) (C c) (C d)
    fixPatologic (Q a b c d) = fixPatologic (Q (fixPatologic a) (fixPatologic b) (fixPatologic c) (fixPatologic d))


-- (2)
-- Si scriva una funzione `simplify` che dato un termine di tipo QT genera il QuadTree corrispondente.
simplify :: (Eq a) => QT a -> QT a
simplify (C a) = C a
simplify (Q (C a) (C b) (C c) (C d))
  | a == b && b == c && c == d  = C a
  | otherwise                   = Q (C a) (C b) (C c) (C d)
simplify (Q a b c d) = simplify (Q (simplify a) (simplify b) (simplify c) (simplify d))

-- (3)
-- Si scriva una funzione map che data una funzione f e un QuadTree q determina il QuadTree che
-- codifica l’immagine risultante dall’applicazione di f a tutti i pixel dell’immagine codificata da q.
transform f q = simplify (compute_transform f q)
  where
    compute_transform f (C a)       = C (f a)
    compute_transform f (Q a b c d) = Q (compute_transform f a) (compute_transform f b) (compute_transform f c) (compute_transform f d)
-- esempio di funzione da utilizzare per f
add10 n = n + 10
square n = n^2
decina n = 10 * (n `div` 10) -- !! interessante usare questa f su quad5 -> si ottiene un immagine maggiormente compressa dopo aver applicato transforma
                             -- transform decina quad5

-- (4)
-- Si scriva una funzione howManyPixels che dato un QuadTree determina il numero (minimo) di
-- pixel di quell’immagine. Ad esempio
--
-- let z = C 0; u = C 1; q = Q z u u u in howManyPixels (Q q (C 0) (C 2) q)
-- restituisce 16.
howManyPixelsV1 :: QT a -> Int
howManyPixelsV1 q = pixelPerSide q ^ 2
  where
    pixelPerSide :: QT a -> Int
    pixelPerSide (C a) = 1
    pixelPerSide (Q a b c d) = 
      2 * foldr max (pixelPerSide a) [pixelPerSide a,pixelPerSide b,pixelPerSide c,pixelPerSide d]

howManyPixelsV2 :: QT a -> Int
howManyPixelsV2 q = pixelPerSide q ^ 2
  where
    pixelPerSide :: QT a -> Int
    pixelPerSide (C a) = 1
    pixelPerSide (Q a b c d) = 
      2 * maximum [pixelPerSide a,pixelPerSide b,pixelPerSide c,pixelPerSide d]

z = C 0
u = C 1
q = Q z u u u 
ris = howManyPixelsV1 (Q q (C 0) (C 2) q) -- ris = 16

-- (5)
-- Si scriva una funzione limitAll che dato un colore c e una lista di QuadTrees costruisca la lista
-- dei QuadTrees che codificano le immagini i cui pixels sono limitati al colore c (pixel originale se il
-- colore è <c, c altrimenti)
limitAllV1 :: (Ord a) => [QT a] -> a -> [QT a]
limitAllV1 qs lim = map (limit lim) qs
  where
    limit :: (Ord b) => b -> QT b -> QT b
    limit lim (C a)
      | a <= lim  = C a
      | otherwise = C lim
    limit lim (Q a b c d) = Q (limit lim a) (limit lim b) (limit lim c) (limit lim d)


-- (6)
-- Si scriva una funzione occurrencies che dato un QuadTree ed un colore determina il numero
-- (minimo) di pixel di quel colore. Ad esempio
--
-- let z = C 0; u = C 1; q = Q z u u u in occurrencies (Q q (C 0) (C 2) q ) 0
-- restituisce 6 (visto che il QuadTree codifica almeno 16 pixel).
-- occurrencies :: QT a -> a -> Int
occurrenciesV1 q color = occurrenciesAux q color (howManyPixelsV1 q)
  where
    occurrenciesAux :: (Eq a) => QT a -> a -> Int -> Int
    occurrenciesAux (C a) color size
      | a == color   = size
      | otherwise    = 0
    occurrenciesAux (Q a b c d) color size
      = sum [occurrenciesAux a color (size `div` 4),
             occurrenciesAux b color (size `div` 4),
             occurrenciesAux c color (size `div` 4),
             occurrenciesAux d color (size `div` 4)]

occurrenciesV2 q color = occurrenciesAux q color (howManyPixelsV1 q)
  where
    occurrenciesAux :: (Eq a) => QT a -> a -> Int -> Int
    occurrenciesAux (C a) color size
      | a == color   = size
      | otherwise    = 0
    occurrenciesAux (Q a b c d) color size
      = sum (map (\y -> occurrenciesAux y color (size `div` 4)) [a,b,c,d])

-- (7)
-- Si scriva una funzione Haskell difference che dato un colore c ed un QuadTree q determina la
-- differenza fra il numero di pixel dell’immagine codificata da q che hanno un colore maggiore di c e
-- quelli minori (O UGUALI???) di c. Ad esempio
--
-- let d = C 2; u = C 1; q = Q d u u u
-- in difference 1 (Q q (C 0) (C 3) q )
-- restituisce -4 (visto che il QuadTree codifica almeno 16 pixel).

-- WARNING: questa implementazione considera la anche gli elementi uguali al colore di filtro e li aggiunge alla conta dei numeri "minori di c"
-- questa implementazione sembra giusta siccome rispetta la logica dell'esempio fornito
differenceV1 color q = differenceAux q color (howManyPixelsV1 q)
  where
    differenceAux (C a) color size
      | a > color = size
      | otherwise = -size
    differenceAux (Q a b c d) color size
      = sum (map (\y -> differenceAux y color (size `div` 4)) [a,b,c,d])

-- WARNING: questa implementazione non considera la anche gli elementi uguali al colore di filtro come da richiesta originale -> "[...] che hanno un colore MAGGIORE di c e quelli MINORI di c."
-- questa implementazione sembra giusta siccome rispetta la consegna fornita
differenceV2 color q = differenceAux q color (howManyPixelsV1 q)
  where
    differenceAux (C a) color size
      | a > color = size
      | a < color = -size
      | otherwise = 0
    differenceAux (Q a b c d) color size
      = sum (map (\y -> differenceAux y color (size `div` 4)) [a,b,c,d])

quad7 = Q (Q (C 2) (C 1) (C 1) (C 1)) (C 0) (C 3) (Q (C 2) (C 1) (C 1) (C 1))

-- (8)
-- Si scriva una funzione Haskell overColor che dato un colore c ed un QuadTree q determina il
-- numero (minimo) di pixel dell’immagine codificata da q che hanno un colore maggiore di c. Ad
-- esempio
-- 
-- let d = C 2; u = C 1; q = Q d u u u
-- in overColor 1 (Q q (C 0) (C 3) q)
--
-- restituisce 6 (visto che il QuadTree codifica almeno 16 pixel)

-- (9)
-- Si scriva una generalizzazione della funzione foldr delle liste per i termini di tipo QT che abbia il
-- seguente tipo:
-- fold :: ( Eq a , Show a ) = > (b - >b - >b - >b - > b ) -> (a - > b ) -> QT a -> b

-- (10)
-- Si scriva una funzione height che dato un QuadTree ne determina l’altezza usando opportunamente
-- fold

-- (11)
-- Si scriva una funzione length che dato un QuadTree ne determina il numero di nodi usando
-- opportunamente fold.

-- (12)
-- Si riscriva la funzione simplify dell’Esercizio 2 usando opportunamente fold.

-- (13)
-- Si riscriva la funzione map dell’Esercizio 3 usando opportunamente fold.

-- (14)
-- Si scrivano due funzioni flipHorizontal/flipVertical che costruiscono il QuadTree dell’immagine
-- simmetrica rispetto all’asse orizzontale/verticale.

-- (15)
-- Si scrivano tre funzioni rotate90Right, rotate90Left e rotate180 che costruiscono il QuadTree
-- dell’immagine ruotata di−π/2, +π/2 e π.

-- (16)
-- Si scrivano tre predicati isHorizontalSymmetric, isVerticalSymmetric e isCenterSymmetric
-- che determinano se un QuadTree codifica un’immagine simmetrica rispetto all’asse orizzontale,
-- all’asse verticale o al centro

-- (17)
-- Si scriva un predicato elem_or_mele che dati un QuadTree te una lista di QuadTrees tsdetermina
-- se t, o il QuadTree che codifica l’immagine di tribaltata rispetto all’asse orizzontale, sono elementi
-- della lista ts.

-- (18)
-- Si scriva un predicato isRotatedIn che dati un QuadTree te una lista di QuadTrees tsdetermina
-- se uno dei QuadTrees che codificano l’immagine di truotata di 0, 90, 180 o 270 gradi `e un elemento
-- della lista ts

-- (19)
-- Si riscriva la funzione howManyPixels dell’Esercizio 4 usando opportunamente fold.

-- (20)
-- Si riscriva la funzione occurrencies dell’Esercizio 6 usando opportunamente fold.

-- (21)
-- Si scriva una funzione zipWith per QuadTrees che, analogamente alla zipWith per le liste, data
-- un’operazione binaria ⊕ e due QuadTrees q1 e q2 costruisce il QuadTree che codifica l’immagine
-- risultante dall’applicazione di ⊕ a tutti i pixel della stessa posizione nelle immagini codificate da
-- q1 e q2.

-- (22)
-- Si scriva una funzione Haskell insertPict che dati i QuadTrees di due immagini qₜ, q_f ed un Quad-
-- Tree “maschera” a valori booleani, costruisce il QuadTree dell’immagine risultante mantenendo i
-- pixel di qₜ in corrispondenza del valore True (della maschera) oppure di q_f in corrispondenza del
-- valore False.

-- (23)
-- Si scriva una funzione Haskell insertLogo che dati i QuadTrees di due immagini qₗ, qₚ ed un
-- QuadTree “maschera” a valori booleani, costruisce il QuadTree dell’immagine risultante inserendo
-- la figura qₗ all’interno del quadrante marcato * di qₚ scegliendo i pixel di qₗ o qₚ in corrispondenza
-- del valore True o False della maschera
--
-- |---------------|---------------|
-- |               |               |
-- |               |               |
-- |               |               |
-- |               |               |
-- |               |               |
-- |               |               |
-- |               |               |
-- |---------------|-------|-------|
-- |               |       |       |
-- |               |       |       |
-- |               |       |       |
-- |               |-------|---|---|
-- |               |       | * |   |
-- |               |       |---|---|
-- |               |       |   |   |
-- |---------------|-------|---|---|

-- (24)
-- Si scriva una funzione Haskell commonPoints che data una lista non-vuota di QuadTrees lcostruisce
-- il QuadTree “maschera”, a valori booleani, che ha “un pixel” a True se nella medesima posizione
-- tutte le immagini di l hanno pixels uguali, False altrimenti.

-- (25)
-- Si scriva un predicato framed che dato un predicato sui colori p ed un QuadTree determina se il
-- bordo esterno dell’immagine codificata è tutto composto da pixels che soddisfano p.

-- (26)
-- Si scriva una funzione frame che dato un QuadTree restituisca Just c se il bordo esterno dell’im-
-- magine codificata è tutto composto da pixels di colore c (Nothing altrimenti).

---------------------------------------------------------------------------------
-- argomento 7: MATRICI MEDIANTE QUAD TREES
-- Grazie ai Quad Trees introdotti nella sezione precedente si possono implementare certe operazioni matriciali, nel caso dei linguaggi funzionali puri ovviamente, in modo molto più efficiente. 
-- Si implementino matrici 2ⁿ × 2ⁿ utilizzando il seguente tipo di dato astratto (polimorfo) 
--
-- data Mat a = Mat { 
--   nexp :: Int ,
--   mat :: QT a
-- } deriving ( Eq , Show )
--
-- dove nel campo mat non metteremo mai solo “termini di tipo QT” ma QuadTrees “propri”.
--
-- difficoltà: 1..12  base
--             12..14 fold for beginners
data Mat a = Mat { 
  nexp :: Int ,
  mat :: QT a
} deriving (Eq , Show)


allZero :: (Eq a, Num a) => QT a -> Bool
allZero (C a)
  | a == 0    = True
  | otherwise = False
allZero (Q q1 q2 q3 q4)
  = all allZero [q1,q2,q3,q4]

-- (1)
-- Si scriva un predicato lowertriangular che determina se una matrice è triangolare inferiore.
-- Attenti a cosa devono restituire
-- lowertriangular $ Mat 0 (C 2) e lowertriangular $ Mat 1 (C 2).
-- lowerTriangular :: (Eq a) => Mat a -> Bool
lowerTriangularQT :: (Eq m, Num m) => Mat m -> Bool
lowerTriangularQT (Mat nexp (C a))
  | nexp == 0      = True
  | a == 0         = True
  | otherwise      = False
lowerTriangularQT (Mat nexp (Q a b c d))
  = lowerTriangularQT (Mat (nexp-1) a) && allZero b && lowerTriangularQT (Mat (nexp-1) d)

-- (2)
-- Si scriva un predicato uppertriangular che determina se una matrice è triangolare superiore.
upperTriangularQT :: (Eq m, Num m) => Mat m -> Bool
upperTriangularQT (Mat nexp (C a))
  | nexp == 0      = True
  | a == 0         = True
  | otherwise      = False
upperTriangularQT (Mat nexp (Q a b c d))
  = upperTriangularQT (Mat (nexp-1) a) && allZero c && upperTriangularQT (Mat (nexp-1) d)

-- (3)
-- Si scriva un predicato diagonal che determina se una matrice è diagonale.
diagonalQTV1 m = lowerTriangularQT m && upperTriangularQT m

diagonalQTV2 :: (Eq m, Num m) => Mat m -> Bool
diagonalQTV2 (Mat nexp (C a))
  | nexp == 0      = True
  | a == 0         = True
  | otherwise      = False
diagonalQTV2 (Mat nexp (Q a b c d))
  = and [diagonalQTV2 (Mat (nexp-1) a), allZero b, allZero c, diagonalQTV2 (Mat (nexp-1) d)]

-- (4)
-- Si scriva una funzione matSum che date 2 matrici calcoli la matrice somma.
matSum :: (Num a) => Mat a -> Mat a -> Mat a
matSum (Mat nexp1 (C a)) (Mat nexp2 (C b))                     = Mat nexp1 (C (a + b))
matSum (Mat nexp1 (C a)) (Mat nexp2 (Q b1 b2 b3 b4))           = matSum (Mat nexp1 (Q (C a) (C a) (C a) (C a))) (Mat nexp2 (Q b1 b2 b3 b4))
matSum (Mat nexp1 (Q a1 a2 a3 a4)) (Mat nexp2 (C b))           = matSum (Mat nexp1 (Q a1 a2 a3 a4)) (Mat nexp2 (Q (C b) (C b) (C b) (C b)))
matSum (Mat nexp1 (Q a1 a2 a3 a4)) (Mat nexp2 (Q b1 b2 b3 b4)) 
  = Mat nexp1 (Q 
  (mat (matSum (Mat (nexp1-1) a1) (Mat (nexp2-1) b1)))
  (mat (matSum (Mat (nexp1-1) a2) (Mat (nexp2-1) b2))) 
  (mat (matSum (Mat (nexp1-1) a3) (Mat (nexp2-1) b3))) 
  (mat (matSum (Mat (nexp1-1) a4) (Mat (nexp2-1) b4)))
  )

d = C 2
t = C 3
qu = C 4
c = C 5
s = C 6
e = C 7
o = C 8
n = C 9

mat1 = Mat 2 (Q d (Q z u d qu) u t)
mat2 = Mat 2 (Q (Q s t d u) qu z d) -- mat1 + mat2 funziona come mi sarei aspettato ✔
mat3 = Mat 2 (Q (Q u u d d) qu d u)
mat4 = Mat 2 (Q (Q s d u z) d o z) -- mat3 + mat4 funziona come mi sarei aspettato ✔
mat5 = Mat 1 (C 1)
mat6 = Mat 1 (Q t u s n) -- mat3 + mat4 funziona come mi sarei aspettato ✔
mat7 = Mat 2 (C 1)
mat8 = Mat 2 (Q (Q s d u z) s u t) -- tutto ok anche per mat7 + mat8 ✔


-- (5)
-- Si scriva una funzione matMul che date 2 matrici calcoli la matrice prodotto.

-- (6)
-- Si scriva una funzione zong che, dati due valori x,y e una matrice A, calcola la matrice xA−yI
-- (dove I è la matrice unitaria della giusta dimensione).

-- (7)
-- Si scriva una funzione f che, dati un vettore v e una matrice A, calcola lo scalare vAvT . Si scelga
-- la struttura dati per i vettori nel modo che si ritiene pi`u opportuno.

-- (8)
-- Si scriva una funzione colSums che data una matrice calcola il vettore delle somme delle colonne
-- della matrice.
-- Ad esempio
-- let z = C 0; u = C 1; d = C 2 in colsums $ Mat 3 $ Q ( Q u d d u ) z u d
-- deve produrre [10,10,10,10,8,8,8,8].

-- (9)
-- Si scriva una funzione rowSums che data una matrice calcola il vettore delle somme delle righe della
-- matrice.

-- (10)
-- Si scriva una funzione colMinMax che data una matrice calcola il vettore delle coppie (minimo,massimo)
-- delle colonne della matrice.

-- (11)
-- Si scriva una funzione colVar che, data una matrice, calcola il vettore delle variazioni (= massimo
-- - minimo) delle colonne della matrice.

-- (12)
-- Si scriva una funzione colAltSums che calcola il vettore delle somme a segni alternati delle colonne
-- della matrice. Detto 
--                                  | a₁₁  ...  a₁ₘ |
-- sⱼ = ∑ᵢ₌₁ⁿ(−1)ⁱ⁺¹aᵢⱼ, colaltsums(| ...       ... |) = (s₁ ... sₘ)
--                                  | aₙ₁       aₙₘ |


-- (13)
-- Si scriva una funzione transpose che calcola la matrice trasposta

-- (14)
-- Si scriva un predicato isSymmetric che determina se una matrice è simmetrica.

-- (15)
-- Si scriva una funzione foldMat con tipo
-- foldMat :: ( Num a ) = >
--   ( Int -> b -> b -> b -> b -> b ) ->
--   ( Int -> a -> b ) -> Mat a -> b
