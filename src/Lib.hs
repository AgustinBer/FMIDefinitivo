module Lib where
import Text.Show.Functions
type Recurso=String
data Pais = UnPais {
    ingPerCap::Float,
    pobActivaPub :: Float,
    pobActivaPriv ::Float,
    rrnn::[Recurso],
    deuda::Float

} deriving (Show,Eq)

namibia = UnPais {ingPerCap=4000,pobActivaPriv=650000,pobActivaPub=400000,rrnn=["Mineria","Ecoturismo"],deuda=50}


--prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta (por los intereses)
type Estrategia = Pais -> Pais
prestarDinero:: Float-> Estrategia

prestarDinero n pais = pais {deuda = deuda pais + cobrarIntereses n}

cobrarIntereses:: Float -> Float
cobrarIntereses n = n*1.5

--reducir x cantidad de puestos de trabajo del sector público, lo que provoca que se reduzca la cantidad de activos en el sector público 
--y además que el ingreso per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en caso contrario

reducirPuestos :: Float-> Estrategia
reducirPuestos cantidadPuestos pais = pais {pobActivaPub = pobActivaPub pais - cantidadPuestos, ingPerCap = ingPerCap pais * coeficiente pais cantidadPuestos}

coeficiente:: Pais -> Float->Float
coeficiente pais cantidadPuestos | cantidadPuestos>100 = 0.80
                                 | otherwise = 0.85


explotar :: Recurso -> Estrategia
explotar recurso pais = pais {deuda = deuda pais - 20, rrnn = quitarRecurso recurso pais}

quitarRecurso :: Recurso -> Pais -> [Recurso]
quitarRecurso recurso pais = filter (/=recurso) (rrnn pais)

blindajePais :: Estrategia

blindajePais pais = prestarDinero ((calcularPbi pais) *0.5).reducirPuestos 500 $ pais

calcularPbi :: Pais -> Float

calcularPbi pais = ingPerCap pais * (pobActiva pais)

pobActiva :: Pais -> Float
pobActiva pais = pobActivaPub pais + pobActivaPriv pais

-- Punto 3 - 2 puntos
-- a) Modelar una receta que consista en prestar 200 millones, y darle a una empresa X
-- la explotación de la Minería de un país.

receta :: [Estrategia]
receta = [explotar "Mineria",prestarDinero 200]

aplicarReceta ::[Estrategia] -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta

puedenZafar :: [Pais] -> [Pais]

puedenZafar paises = filter (elem "Petroleo".rrnn) paises

deudaTotal :: [Pais] -> Float
deudaTotal paises = sum.map (deuda) $ paises

estaOrdenado :: Pais -> [Estrategia] -> Bool

estaOrdenado pais [receta] = True
estaOrdenado pais (receta1:receta2:recetas) = revisarPBI [receta1] pais <= revisarPBI [receta2] pais && estaOrdenado pais (receta2:recetas)  
     
     
     where revisarPBI receta pais = calcularPbi . aplicarReceta receta $ pais