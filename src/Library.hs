module Library where
import PdePreludat

--Una agencia de remises contrata los más eficientes choferes de los que conoce:
--el nombre
--el kilometraje de su auto
--los viajes que tomó
--qué condición impone para tomar un viaje

--Cada viaje se hace en una fecha particular, lo toma un cliente 
--(queremos saber su nombre y dónde vive) y tiene un costo.

-- Se pide
-- (2 puntos) Modelar los TAD cliente, chofer y viaje.
type Dia = Number
type Mes = Number
type Anio = Number
data Cliente = Cliente {
    nombreCliente :: String,
    direccion :: String
} deriving (Eq, Show)
data Viaje = Viaje{
    fecha :: (Dia,Mes,Anio),
    cliente :: Cliente,
    costo :: Number
} deriving (Eq, Show)
data Chofer = Chofer {
    nombre :: String,
    kilometraje :: Number,
    viajes :: [Viaje],
    condiciones :: Condicion
} deriving (Eq, Show)

--En cuanto a la condición para tomar un viaje
--algunos choferes toman cualquier viaje
--otros solo toman los viajes que salgan más de $ 200
--otros toman aquellos en los que el nombre del cliente tenga más de n letras
--y por último algunos requieren que el cliente no viva en una zona determinada

--(2 puntos) Implementar con las abstracciones que crea conveniente las condiciones que 
--cada chofer tiene para tomar un viaje. Debe utilizar en este punto composición 
--y aplicación parcial.

type Condicion = Viaje -> Bool

tomarCualquierViaje :: Condicion
tomarCualquierViaje _ = True

tomarViajeMayorA :: Condicion
tomarViajeMayorA = (> 200).costo

tomarViajeClienteNombreMayorA :: Number -> Condicion
tomarViajeClienteNombreMayorA cantidadLetras = (> cantidadLetras).cantidadLetrasCliente.cliente

tomarViajePorUbicacion :: String -> Condicion
tomarViajePorUbicacion direccion = (/=direccion).direccionCliente.cliente

cantidadLetrasCliente :: Cliente -> Number
cantidadLetrasCliente = length.nombreCliente

direccionCliente :: Cliente -> String
direccionCliente = direccion

--(1 punto) Definir las siguientes expresiones: 
--el cliente “Lucas” que vive en Victoria
--el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente Lucas el
--20/04/2017 cuyo costo fue $ 150, y toma los viajes donde el cliente no viva en “Olivos”.
--la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma cualquier viaje.
lucas = Cliente {
    nombreCliente = "Lucas",
    direccion = "Victoria"
}

viajeLucas = Viaje{
    fecha = (20,04,2017),
    cliente = lucas,
    costo = 150
}

daniel = Chofer {
    nombre = "Daniel",
    kilometraje = 23500,
    viajes = [viajeLucas],
    condiciones = tomarViajePorUbicacion "Olivos"
}

alejandra = Chofer {
    nombre = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condiciones = tomarCualquierViaje
}

--(1 punto) Saber si un chofer puede tomar un viaje.
tomarViaje :: Chofer -> Condicion
tomarViaje = condiciones

-- (2 puntos) Saber la liquidación de un chofer, que consiste en sumar los costos de cada uno de los viajes.
--  Por ejemplo, Alejandra tiene $ 0 y Daniel tiene $ 150.
liquidacionChofer :: Chofer -> Number
liquidacionChofer chofer = foldr ((+).costo) 0 $ viajes chofer

-- (4 puntos) Realizar un viaje: dado un viaje y una lista de choferes, se pide que
-- a) filtre los choferes que toman ese viaje. Si ningún chofer está interesado, no se preocupen: el viaje no se puede realizar.
choferesPuedanTomarViaje :: Viaje -> [Chofer] -> [Chofer]
choferesPuedanTomarViaje viaje choferes = filter (\chofer -> tomarViaje chofer viaje) choferes
-- b) considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.
-- choferPuedaTomarViaje :: [Chofer] -> Chofer
--choferPuedaTomarViaje (chofer:choferes) = foldr (elQueMenosViajesHizo chofer chofer2) chofer $ choferes
-- -- choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes ((elQueMenosViajesHizo chofer1 chofer2):choferes)
-- otra opcion es hacerlo con fold

elQueMenosViajesHizo :: Chofer -> Chofer -> Chofer
elQueMenosViajesHizo chofer1 chofer2 
   | cuantosViajes chofer1 > cuantosViajes chofer2 = chofer2 
   | otherwise                                     = chofer1

cuantosViajes :: Chofer -> Number
cuantosViajes = length . viajes

-- c) efectuar el viaje: esto debe incorporar el viaje a la lista de viajes del chofer. ¿Cómo logra representar este cambio de estado?
realizarViaje :: Viaje -> Chofer -> Chofer
realizarViaje viaje chofer = chofer {
    viajes = viaje : viajes chofer
}

-- (1 punto) Al infinito y más allá
-- Modelar al chofer “Nito Infy”, su auto tiene 70.000 kms., que el 11/03/2017 hizo infinitos viajes de $ 50 con Lucas y 
-- toma cualquier viaje donde el cliente tenga al menos 3 letras. Puede ayudarse con esta función:
nitoInfy = Chofer {
    nombre = "Nito Infy",
    kilometraje = 70000,
    viajes = [viajeLucas],
    condiciones = tomarViajeClienteNombreMayorA 3
}


repetirViaje viaje = viaje : repetirViaje viaje

-- ¿Puede calcular la liquidación de Nito? Justifique.
-- No se puede porque como es una lista infinita no va a frenar nunca y tirar error
-- ¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? Justifique. 
-- Si puede tomar ya que la unica condicion de Nito es que el nombre tenga mas de 3 letras y lucas tiene 5 letras.
-- (1 punto) Inferir el tipo de la función gōngnéng

gongNeng :: Ord b => b -> (b -> Bool) -> (a -> b) -> [a] -> b
gongNeng arg1 arg2 arg3 = 
     max arg1 . head . filter arg2 . map arg3
