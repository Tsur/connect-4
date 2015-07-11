
import AP.GUI
import Control.Monad (forM,when,zipWithM_)
import Array
import Maybe

-- Add new Player type
data Player = X | O  deriving (Eq,Ord)

instance Show Player where
  show X     = "X"
  show O     = "O"

next :: Player -> Player
next X = O
next O = X

jugador1 = X
jugador2 = O

type Celda = Maybe Player
type Columna = [ Celda ]
type Tablero = [ Columna ]

type Tiempo = (Int,Int)

type Estadistica = (Int,Int,Int,Int)

tamTablero :: Int
tamTablero = 42

cero :: IO Int
cero = return 0;

columna :: Columna
columna = [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]

toCeldas :: Tablero -> [Celda]  -- Comprobado funcionamiento
toCeldas = concat


estaLleno :: Tablero -> Bool --  Comprobado funcionamiento
estaLleno = all (all isJust)

cuentaJust :: [Celda] -> Int -- Comprobado funcionamiento
cuentaJust [] = 0
cuentaJust (x:xs)
            | x /= Nothing = 1 + cuentaJust xs
            | otherwise = cuentaJust xs

data State = ST { tablero :: Tablero
                , turn :: Player
                , tiempo :: Tiempo
                , estadistica :: Estadistica
                } deriving Show

data GUI = G { mainWindow :: Window
              ,cuadrados :: [Label]
              ,botonesLanzar:: [Button]
              ,seleccionTiempo :: [RadioButton]
              ,menuNP :: MenuItem
              ,contadorTiempo1,contadorTiempo2 :: Timer
              ,xImage, oImage, noImage :: Image
              ,tiempo1, tiempo2, ganadas1, perdidas1, ganadas2, perdidas2,ritmoTiempo :: Entry Int
              
             }

main :: IO ()
main = gui prog  

-- titulo aplicacion
appTitle = "Conecta 4"

--tiempo por defecto
defaultTime :: Tiempo
defaultTime = (300,300)

--estadisticas iniciales
defaultEstad = (0,0,0,0)

fuenteMediana = arial 14
fuenteGrande = arial 16

estadoInicial :: State -- Comprobado funcionamiento
estadoInicial = ST { tablero = [columna,columna,columna,columna,columna,columna,columna]
                      , turn = jugador1, tiempo = defaultTime,estadistica = defaultEstad}

insertaEn :: Columna -> Player -> Columna -- Comprobado funcionamiento
insertaEn [] _ = []
insertaEn (x:xs) j = if x == Nothing then (Just j):xs else x:insertaEn xs j

inserta ::  State -> Int -> State -- Comprobado funcionamiento
inserta est col = est{tablero= (take col (tablero est) ++ [insertaEn ((tablero est)!!col) (turn est)] ++ drop (col+1) (tablero est))
                      , turn = next (turn est)}

prog :: IO ()
prog = do { w  <- window [ title =: "Conecta 4" 
                         , on windowClose =: quit
                         , resizable =: False
                         ]
          ; [ xIm, oIm, noIm ] <- forM ["Cross","Nought","None"] $ \nm ->
                               loadImageFrom ("./"++nm++".gif") 
          ;bs <- forM [0..tamTablero-1] $ \i -> 
                  label [ relief =: Groove
                 , size =: sz 90 90
                 , bgColor =: white
                 , photo =: noIm
                 ] w 
          ; v <- var []
          ; [wins, loses, res, selTime] <- forM [0..3] $ \i -> 
                     label [ font =: fuenteGrande] w
          ; mapM_ (\(lab,txt) -> lab !: text =: txt) (zip [wins, loses, res, selTime] ["Ganadas","Perdidas", " Resultado:", "   Tiempo"])
          ; [e11,e12,e21,e22] <- forM [0..3] $ \i-> 
                     entry  [ value =: (0::Int) 
                       , width =: 7 
                       , justify =: JustifyRight
                       , font =: fuenteMediana
                       , enabled =: False
                       ] w --contador j1 ganadas
          ; [j1, j2, t1,t2] <- forM [0..3] $ \i ->
                     label [  font =: fuenteGrande ] w
          ; mapM_ (\(lab,(txt,c)) -> do 
                                                 lab !: text =: txt
                                                 lab !: color =: c
                                             ) (zip [j1,j2,t1,t2] [("Jugador 1: ",blue),("Jugador 2: ",red),("Tiempo J1: ",blue),("Tiempo J2: ",red)])	
          ; [te1,te2] <- forM [0,1] $ \i -> 
                   entry [ width =: 3 
                        , justify =: JustifyRight
                        , font =: fuenteGrande
                        , value =: (300::Int)  -- tiempo por defecto
                        , enabled =: False
                        ] w      
          ; bl <- forM [0..6] $ \i -> 
                    button [ text =: "Tira ficha"
                         , size =: sz 10 1
                         , font =: arial 11
                         , enabled =: False
                         ] w
          ; ritmo <- entry [ value =: (300::Int)] w
          ; rbs <- forM [0..4] $ \i ->
                    radioButton[ ] w
          ; mapM_ (\(rb,(tex,t)) -> do 
                                              rb !: text =: tex ++ " segundos" 
                                              rb !: on action =: estableceTiempo t te1 te2 
                                              
                                             )
                                     (zip rbs [(" 60",60),("120",120),("180",180),("300",300),("900",900)])
          ; [tm1,tm2] <- forM [0,1] $ \i -> 
                         timer [ interval =: 1000
                        , enabled =: False
                        ]
          ; mnb <- menu [ ]
          ; menuItem [ text =: "&Acerca de..." 
                     , on action =: ms w "Acerca de " "by José Carlos Moreno García \n     Zurishaddai Pavón Villalobos \n"
                     ] mnb
          ; mng <- submenu [ text =: "&Juego" ] mnb
          ; nuevaP <- menuItem [ text =: "&Nueva partida" 
                                ,enabled =: True
                               ] mng
          ; let xImage = xIm
          ; let oImage = oIm
          ; let noImage = noIm
          ; let gui = G{ mainWindow=w, cuadrados=bs, botonesLanzar=bl, seleccionTiempo = rbs, menuNP=nuevaP
             , contadorTiempo1=tm1, contadorTiempo2=tm2,xImage=xIm, oImage=oIm, noImage=noIm, tiempo1 = te1, tiempo2 = te2
             , ganadas1 = e11, perdidas1 = e12, ganadas2 = e21, perdidas2 = e22, ritmoTiempo=ritmo}
          ; nuevaP !: on action =: (reiniciar gui v bl)
                                      
          ; menuSep [] mng
          ; menuItem [ text =: "&Ayuda"
                     , on action=: ms w "Ayuda" "Para tirar una ficha en una columne pulse el botón sobre la columna deseasa. El jugador que consiga una linea, tanto vertical como horizontal o diagonal, de 4 fichas de su color ganará la partida. También si a algún jugador se le agota el tiempo de movimiento perderá la partida"
                     ] mng
          ; menuItem [ text =: "&Salir"
                     , on action=: quit
                     ] mng
          ; f <- frame [ layout =: matrix 7 bs
                   , relief =: Groove 
                   ] w	
          ; f1 <- frame [ layout =: matrix 1 rbs
                           , relief =: Groove
                           ] w
          ;let showMsg str = do 
                    okMessageBox (mainWindow gui) appTitle str 
                    accionVacia

          ;mapM_ (\(time,(t,str)) -> time !: on action =:do t !: enabled =:True 
                                                            t !: value <: (+(-1))
                                                            t !: enabled =:False
                                                            temp <- t ?: value
                                                            if (temp == 0) then (showMsg (("Gana Jugador " ++ str)++ " por tiempo")) else accionVacia
                                                            if (temp == 0) then (ventanaLleno gui v) else accionVacia
                                  ) (zip [tm1,tm2] [(te1,"2 (O)"),(te2,"1 (X)")])
          ;mapM_ (\(bt,n)-> bt !: on action =: pintar v n (reverse [(bs !!n),(bs !!(n+7)),(bs !!(n+14)),(bs !!(n+21)),(bs !!(n+28)),(bs !!(n+35))]) gui )
                  (zip bl [0,1,2,3,4,5,6])
          ; let botonesTirarFichas = attachLeft (horizontal (map (\b ->  b) bl))
          ; let contadores = horizontal [res,wins,loses] ^.^ (j1 <.< e11 <.< e12) ^.^ (j2 <.< e21 <.< e22)
          ; let tiempos = t1 <.< te1 ^.^ t2 <.< te2
          ;w!:menuBar =: mnb
          ;w!:layout =: ( botonesTirarFichas ^.^ f {- ^.^ matrix 7 (map flexible bs)-})  <.< ((west selTime ^.^ f1 <.< tiempos) ^.^ contadores)
          
          }

--entryInt :: (Container w) => [Prop (Entry Int)] -> w -> IO (Entry Int)
--entryInt = entry

ms :: Window -> String -> String -> IO () -- Comprobado funcionamiento
ms w str1 str2= do
  okMessageBox w (str1 ++appTitle) 
         $ unlines [ appTitle++"\n"
                   , str2]
  return ()    


--f :: Int -> Int -> [[(Int,Int)]]
--f i j = [(zip  [i..5] [j..6])] ++ [(zip  [i,i-1..0] [j..])]

--g :: [[(Int,Int)]] -> Tablero ->Tablero
--g (xs:xss) tab= [(map (\(x,y)-> ((tab !!y)!!x) ) xs)] ++ g xss tab
--g [] _ = []

--formarDiag2 :: Tablero -> Tablero
--formarDiag2 tab = concat ([xs | i <- [0..5], j <-[0..6], let xs = g (f i j) tab, x <- xs, length x >4])



reiniciar :: GUI -> Var State -> [Button] -> IO () 
reiniciar gui v bt = do
                  v !: value =: estadoInicial
                  reseteaPuntuaciones gui
                  t <- tiempo1 gui?: value
                  (ritmoTiempo gui) !: value =: t
                  mapM_ (\b -> b !: enabled=: True) bt
                  mapM_ (\b -> b !: enabled=: False) (seleccionTiempo gui)
                  (contadorTiempo1 gui) !: enabled=: True
                  mapM_ (\b -> b !: photo=: (imageFor Nothing gui)) (cuadrados gui)
                  (menuNP gui) !: enabled =: False 
                  
                  


otraPartida :: GUI -> Var State  -> Int -> IO () -- Comprobado funcionamiento
otraPartida gui v t = do
                  [wins1, loses1, wins2, loses2] <- forM [ganadas1, perdidas1, ganadas2, perdidas2] $ \c->  c gui ?: value
                  v !: value =: ST{tablero = [columna,columna,columna,columna,columna,columna,columna]
                      , turn = jugador1, tiempo = defaultTime, estadistica = (wins1,loses1,wins2,loses2)}
                  estableceTiempo t  (tiempo1 gui) (tiempo2 gui)
                  (contadorTiempo1 gui) !: enabled=: True
                  mapM_ (\b -> b !: photo =: imageFor Nothing gui) (cuadrados gui)
                  mapM_ (\b -> b !: enabled=: True) (botonesLanzar gui)

reseteaPuntuaciones :: GUI -> IO ()
reseteaPuntuaciones gui = do
                  let lista = [(ganadas1 gui),(perdidas1 gui),(ganadas2 gui),(perdidas2 gui)]
                  mapM_(\ent -> ent !: enabled =: True) lista
                  mapM_(\ent -> ent !: value =:0) lista
                  mapM_(\ent -> ent !: enabled =: False) lista

cmp4X :: Columna -> Int -> Bool -- FUNCIONA
cmp4X xs 4 = True
cmp4X [] _ = False
cmp4X (x:xs) y
       |x == (Just X) = cmp4X xs (y+1)
       |otherwise = cmp4X xs 0


formarFilas :: Tablero -> Tablero -- Comprobado funcionamiento
formarFilas xs = map (\i->(map (!! i) xs)) [0..5]
--Esta funcion toma un tablero como entrada y devuelve todas las filas del tablero

listaDiag :: Int -> Tablero -> Tablero -- FUNCIONA
listaDiag y xs
                |y<3 = (map (\j-> map (\i-> ((xs !! i)!!(i+j-y))) [y..y+3]) [0..2]) ++ (map (\j-> map (\i-> ((xs !! i)!!(j-i+y))) [y..y+3]) [3..5])
                |y==3 =[]
                |otherwise = (map (\j-> map (\i-> ((xs !! i)!!(j-i+y))) [y,y-1,y-2,y-3]) [0..2]) ++ (map (\j-> map (\i-> ((xs !! i)!!(i-j+(6-y)))) [y,y-1,y-2,y-3]) [3,2,1])

formarDiag :: Tablero -> Tablero --FUNCIONA
formarDiag xs = (concat) (map (\i->listaDiag i xs) [0..6])
--Esta funcion se engarga de devolvernos todas las posibles diagonales que pueden formarse en nuestro tablero, comenzando en la columna cero en adelante.
--Por ejemplo, para i=0, listaDiag nos devolveria todas las posibles diagonales que pueden formarse para cada una de las casillas de la columna cero(i=0), en tanto
--que formarDiag nos devuelves en una lista cada una de las salidas de ListaDiag desde i=0(primera columna) hasta i=6(última columna)
-- La función listaDiag discrimina tres casos según la columna en la que estemos. Si la columna es menor que la tercera entonces para cada una de las casillas de la
--columna en la que estemos(0,1 o 2) utilizamos dos variables (i y j), incrementando o decrementandolas segun las casillas sean las tres primeras de la columna
-- o las tres últimas respectivamente y aplicando la correspondiente expresion para obtener en cada caso las casillas adecuadas mediante un map anidado, ocupandose el más externo
-- de situarse en la casilla dentro de la columna  y el más interno de obtener las tres siguientes casillas que forman diagonal con aquella primera. Si nos hallamos en la columna
-- cuarta entonces no obtenemos ninguna diagonal porque estas ya son obtenidas por los otros casos , luego sería una solución redundante volverlas a calcular. Para las
-- tres ultimas columnas el procedimiento es simétrico intercambiando el comportamiento de las tres primeras casillas de la columna correspondiente(5,6 o 7) por el de las tres últimas 
-- y viceversa.


ganaX :: Tablero -> Bool --FUNCIONA
ganaX xs = or [or (map (\a -> cmp4X a 0) xs),or (map (\a -> cmp4X a 0) (formarFilas xs)),
               or (map (\a -> cmp4X a 0) (formarDiag xs))]


--Se trata de comprobar todas las posibles combinaciones ganadoras del jugador X. Esto incluye soluciones en las filas, en las columnas o en diagonales del tablero
--así, esta funcion comprueba si hubo alguna combinacion ganadora para el jugador X en las filas, columnas o diagonales del tablero. Para ello usa la 
--funcion cmp4X que verifica si una posible combinación ganadora(4 casillas marcadas con X o con O) efectivamente lo es, es decir, si los cuatro los elementos de la
--combinación que recibe como entrada son iguales(todos X). El funcionamiento de ganaO es análogo junto con el de cmp4O. Finalmente estas funciones son usadas en la
--función 'chequear' cada vez que efectuamos un nuevo cambio en el tablero. Si el cambio implica la victoria de alguno de los jugadores entonces ganaX o ganaO devuelve 
--True en cuyo caso actualizamos los marcadores de los jugadores incrementando en uno el marcador de ganadas del jugador ganador y el marcador de perdidas en el del
--jugador derrotado. Asimismo detenemos el tiempo y mostramos el correspondiente mensaje de aviso acerca del fin de la partida y el ganador de la misma. En caso
--de que no ganen ninguno, detenemos el tiempo y mostramos el mensaje final de Empate. La comprobación del tiempo no es realizada por esta funcion. De ello toma
--control el on action del timer que al comprobar el valor del entry que registra el tiempo transcurrido de cada jugador alcanza el valor cero muestra por pantalla el jugador ganador
--por tiempo y nos informa sobre jugar una nueva partida.

cmp4O :: Columna -> Int -> Bool -- FUNCIONA
cmp4O xs 4 = True
cmp4O [] _ = False
cmp4O (x:xs) y
       |x == (Just O) = cmp4O xs (y+1)
       |otherwise = cmp4O xs 0



ganaO :: Tablero -> Bool  --FUNCIONA
ganaO xs= or [or (map (\a -> cmp4O a 0) xs),or (map (\a -> cmp4O a 0) (formarFilas xs)),
              or (map (\a -> cmp4O a 0) (formarDiag xs))]


chequear :: GUI -> Var State -> [Button] -> IO () 
chequear gui v  bt= do
  st <- v ?: value 
  check' (tablero st)
  where
   check' b
    | ganaX b    = do  
                       let lista = [(ganadas1 gui),(perdidas2 gui)]
                       mapM_(\ent -> ent !: enabled =: True) lista
                       mapM_(\ent -> ent !: value <: (+1)) lista
                       mapM_(\ent -> ent !: enabled =: False) lista
                       (contadorTiempo1 gui) !: enabled=: False 
                       (contadorTiempo2 gui) !: enabled=: False 
                       showMsg "Gana Jugador 1 (X)"
                       ventanaLleno gui v

    | ganaO b    = do  
                       let lista = [(ganadas2 gui),(perdidas1 gui)]
                       mapM_(\ent -> ent !: enabled =: True) lista
                       mapM_(\ent -> ent !: value <: (+1)) lista
                       mapM_(\ent -> ent !: enabled =: False) lista
                       (contadorTiempo1 gui) !: enabled=: False 
                       (contadorTiempo2 gui) !: enabled=: False 
                       showMsg "Gana Jugador 2 (O)"
                       ventanaLleno gui v
 
    | estaLleno b      = do (contadorTiempo1 gui) !: enabled=: False 
                            (contadorTiempo2 gui) !: enabled=: False 
                            showMsg "Empate"
                            ventanaLleno gui v 
    | otherwise          = return ()
       where showMsg str = do 
              okMessageBox (mainWindow gui) appTitle str 
              accionVacia
                  

pintar :: (Display w) => Var State -> Int -> [w] -> GUI -> IO()
pintar v col bts gui = do {st <- v ?: value
                          ; let st' = inserta st col
                          ; asigEstado gui v st' bts col
                       }

controlaTiempo :: Timer -> Timer -> Player -> IO()
controlaTiempo t1 t2 O = do
                       t1 !: enabled=: False
                       t2 !: enabled=: True
                      

controlaTiempo t1 t2 X = do
                         t1 !: enabled=: True
                         t2 !: enabled=: False
                         
                 

asigEstado ::(Display w) => GUI -> Var State -> State -> [w] -> Int -> IO ()
asigEstado gui v st bts col = do
  v !: value =: st 
  controlaTiempo (contadorTiempo1 gui) (contadorTiempo2 gui) (turn st)
  (bts !! ((cuentaJust ((tablero st) !! col))-1)) !: photo =: imageFor (Just (next (turn st))) gui
  chequear gui v (botonesLanzar gui)
  if (estaLleno [(tablero st) !! col])  then (((botonesLanzar gui) !! col)!: enabled =: False ) else 
          (bts !! ((cuentaJust ((tablero st) !! col))-1)) !: photo =: imageFor (Just  (next (turn st))) gui
 
  
accionVacia :: IO () -- Comprobado funcionamiento
accionVacia = putStr ""

estableceTiempo :: Int -> Entry Int -> Entry Int-> IO() -- Comprobado funcionamiento
estableceTiempo x t1 t2 = do { 
                                           ; let lista = [t1,t2]
                                           ; mapM_ (\ent -> ent !: enabled =: True) lista
                                           ; mapM_ (\ent -> ent !: value =: x) lista
                                           ; mapM_ (\ent -> ent !: enabled =: False) lista
}

ventanaLleno :: GUI -> Var State -> IO ()
ventanaLleno gui v = do { w  <- window [ title =: "Conecta 4" -- Comprobado funcionamiento
                         , resizable =: False
                         ]
                         ;mapM_ (\b -> b !: enabled=: False) (botonesLanzar gui)
                         ; [b1,b2] <- forM [0..1] $ \i -> 
                                          button [ size =: sz 10 1
                                                   , font =: arial 11
                                                 ] w
                         ;mapM_ (\(b,tex) -> do 
                                              b !: text =: tex 
                                             )
                                     (zip [b1,b2] ["Si","No"])
                         
                        ; b1 !: on action =: do
                                                close w 
                                                (menuNP gui) !: enabled =: False
                                                t <- ritmoTiempo gui ?: value
                                                otraPartida gui v t
                        ; b2 !: on action =: do
                                                close w
                                                (menuNP gui) !: enabled =: False
                                                mapM_ (\b -> b !: enabled=: False) (botonesLanzar gui)
                                                mapM_ (\rb -> rb !: enabled=: True) (seleccionTiempo gui)
                                                t <- ((ritmoTiempo gui) ?: value)
                                                estableceTiempo t (tiempo1 gui) (tiempo2 gui)

                       
                        ; lab <- label [ text =: "¿Desean comenzar otra partida? "
                                            , font =: fuenteMediana
                                            ] w
                        ; w !: layout =: lab ^.^ (b1 <.< b2)
  }


imageFor :: Celda -> GUI -> Image
imageFor (Just X) gui = xImage gui
imageFor (Just O) gui = oImage gui
imageFor Nothing  gui = noImage gui

