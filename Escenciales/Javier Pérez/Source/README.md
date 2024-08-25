# Documentación

## Resumen

Repositorio para el trabajo de titulo de pregrado y postgrado

## Hospitales públicos de alta complejidad

[Este](raw_data/minsal_data/establishments_2023.xlsx) archivo obtenido desde el MINSAL fue usado para determinar que hospitales son considerados finalmente en este estudio.

## Diccionario de hospitales

Aca hay un [shortcut](data/hospitals_dictionary.csv) a un archivo csv que contiene el diccionario de hospitales.

Sus columnas son:

- **hospital_id** -> Código único para representar un hospital
- **hospital_fns_id** -> Código único para representar un hospital en los archivos de data financiera
- **hospital_name** -> Nombre del hospital
- **hospital_alternative_name** -> Un nombre más comun para el hospital
- **slug** -> Slug auxiliar para siguientes querys con respecto al hospital

En la siguiente tabla hay un ejemplo de cómo está estrucutrado el archivo:

|hospital_id|hospital_fns_id|hospital_name                         |hospital_alternative_name|slug                                 |
|-----------|---------------|--------------------------------------|-------------------------|-------------------------------------|
|101100     |103            |Hospital Regional Dr. Juan Noé Crevani|Hospital Doctor Juan Noé |hospital-regional-dr-juan-noe-crevani|
|102100     |203            |Hospital Dr. Ernesto Torres Galdames  |Hospital de Iquique      |hospital-dr-ernesto-torres-galdames  |
|103100     |303            |Hospital Dr. Leonardo Guzmán          |Hospital de Antofagasta  |hospital-dr-leonardo-guzman          |
|103101     |304            |Hospital Dr. Carlos Cisternas         |Hospital de Calama       |hospital-dr-carlos-cisternas         |
|104100     |403            |Hospital San José del Carmen          |Hospital de Copiapó      |hospital-san-jose-del-carmen         |

## Información financiera de los hospitales

Para calcular la eficiencia técnica, primero es necesario tener en cuenta el capital de ingresos que tienen cada una de las instituciones que se van a estudiar.

Esta información se puede encontrar en esta [ruta](raw_data/financial_data)

El capital se divide en dos partes:

- **Gastos de personal**
- **Gastos en bienes y servicios**

Los gastos de personal se pueden dividir en:

- *Gastos de personal 1 remuneración del personal médico 1 Titulares*
- *Gastos de personal 1 remuneración del personal médico 2 Contrato*
- *Gastos de personal 1 remuneración del personal médico 4 Incentivos*
- *Gastos de personal 1 remuneración del personal médico 3 Internos*
- *Gastos de personal 1 remuneración del personal médico 5 Contribución del empleador*
- *Gastos de personal 2 remuneración del personal no médico 1 Titulares*
- *Gastos de personal 2 remuneración del personal no médico 2 Contrato*
- *Gastos de personal 2 remuneración del personal no médico 3 Contribución del empleador*
- *Gastos de personal 3 remuneraciones variables 1 Tarjetas de tarifa*
- *Gastos de personal 3 remuneraciones variables 2 Trabajos extraordinarios*
- *Gastos de personal 3 remuneraciones variables 3 Sustituciones y reemplazos*
- *Gastos de personal 5 remuneraciones variables Bonificaciones*
- *Gastos de personal 5 remuneraciones variables Incentivos*
- *Gastos de personal 4 remuneración por Viajes, traslados y otros*

Los gastos en bienes y servicios se pueden dividir en:

- *Bienes y servicios de consumo 2201 Fijos 0 Comida*
- *Bienes y servicios de consumo 2201 Fijos 1 Ropa y calzado*
- *Bienes y servicios de consumo 2201 Fijos 2 Combustibles y lubricantes*
- *Bienes y servicios de consumo 2201 Fijos 3 Servicios básicos y generales*
- *Bienes y servicios de consumo 2201 Fijos 4 Material de oficina*
- *Bienes y servicios de consumo 2201 Fijos 5 Lavandería*
- *Bienes y servicios de consumo 2201 Fijos 6 Mantenimiento y reparaciones*
- *Bienes y servicios de consumo 2201 Fijos 7 Alquileres y seguros*
- *Bienes y servicios de consumo 2201 Fijos 8 Consultorías*
- *Bienes y servicios de consumo 2201 Fijos 10 Otros gastos fijos*
- *Bienes y servicios de consumo 2202 Variables 1 Farmacéuticos*
- *Bienes y servicios de consumo 2202 Variables 4 Servicios adquiridos*
- *Bienes y servicios de consumo 2202 Variables 2 Materiales y suministros quirúrgicos*
- *Bienes y servicios de consumo 2202 Variables 3 Productos químicos*
- *Bienes y servicios de consumo 2202 Variables 6 DFL 36*
- *Bienes y servicios de consumo 2202 Variables 5 Revisiones*

## Obteniendo la información financiera de los hospitales

De los archivos guardados [aca](raw_data/financial_data), podemos obtener los datos financieros de cada uno de los hospitales públicos de alta complejidad,
considerando *Gastos de personal* y *Bienes y Servicios*.

La data resultante es guardada en este [path](data/financial_data) como un archivo .csv files por cada año comprendido en el estudio.

Las columnas en los archivos .csv son:

- **hospital_id** -> Código único para representar un hospital
- **hospital_fns_id** -> Código único para representar un hospital en los archivos de data financiera
- **{year}_row_index** -> El indice de la fila del hospital en el archivo .xlsm respectivo (Considerando año).
- **{year}_21_value** -> El valor acumulado de Gastos personales del hospital (Considerando año)
- **{year}_22_value** -> El valor acumulado de Bienes y Servicios del hospital (Considerando año)

Aquí hay un ejemplo de cómo está estrucutado (Año 2014):

|hospital_id|hospital_fns_id|2014_row_index|2014_21_value|2014_22_value|
|-----------|---------------|--------------|-------------|-------------|
|101100     |103            |12145         |18.300.952   |10.429.020   |
|102100     |203            |12148         |24.681.604   |11.111.050   |
|103100     |303            |12152         |26.119.740   |13.203.894   |
|103101     |304            |12153         |10.207.581   |3.294.598    |
|104100     |403            |12160         |15.598.491   |7.780.655    |
|105100     |503            |12167         |16.024.288   |10.723.572   |
|105101     |504            |12168         |17.314.811   |11.314.705   |
|105102     |505            |12169         |10.148.641   |4.523.144    |

## Egresos hospitalarios

Luego de tener los ingresos de cada uno de los hospitales del estudio, es necesario definir cuáles seran los egresos sobre los que calcularemos la eficiencia técnica.

Para los egresos hospitalarios consideraremos 4 campos:

### Consultas médicas totales

Las consultas médicas totales están compuestas por la sumatoria de las siguientes variables:

- *03020101 -> Ira Alta*
- *03020201 -> Sindrome Bronquial Obstructivo*
- *03020301 -> Neumonía*
- *03020402 -> Asma*
- *03020403 -> Enfermedad Pulmonar Obstructiva Crónica*
- *03020401 -> Otras Respiratorias*
- *03040210 -> Obstetrica*
- *03040220 -> Ginecologica*
- *04040100 -> Ginecologica Por Infertilidad*
- *04025010 -> Infección Transmisión Sexual*
- *04025020 -> VIH-SIDA*
- *03020501 -> Otras Morbilidades*

### Intervenciones Quirúrgicas

Las intervenciones quirúrgicas están compuestas por la sumatoria de las siguientes variables:

- *17050100 -> Neurocirugia*
- *17050200 -> Cirugia Oftalmologica*
- *17050300 -> Cirugia Otorrinolaringologica*
- *17050400 -> Cirugia de Cabeza y Cuello*
- *17050500 -> Cirugia Plastica y Reparadora*
- *17050600 -> Tegumentos*
- *17050700 -> Cirugia Cardiovascular*
- *17050800 -> Cirugia Toraxica*
- *17050900 -> Cirugia Abdominal*
- *17051000 -> Cirugia Proctologica*
- *17051100 -> Cirugia Urologica y Suprarrenal*
- *17051200 -> Cirugia de la Mama*
- *17051300 -> Cirugia Ginecologica*
- *17051400 -> Cirugia Obstetrica*
- *17051500 -> Traumatologia y Ortopedia*
- *17051600 -> Odontologia (Cod 27-03+Cod 27-02-001) Aranc.Fonasa*
- *17051700 -> XVI Retiro Elementos Osteosintesis*

### Examenes de diagnóstico

Los examenes de diagnóstico estan compuestos por la sumatoria de las siguientes variables:

- *17010100 -> Hematologicos*
- *17010200 -> Bioquimicos*
- *17010300 -> Hormonales*
- *17010400 -> Genetica*
- *17010500 -> Inmunologicos*
- *17010601 -> Bacterias y Hongos*
- *17010602 -> Parasitos*
- *17010603 -> Virus*
- *17010700 -> Procedimiento o Determinacion Directa C/Paciente*
- *17010800 -> Ex. de Deposiciones Exudados. Secrec. y Otros Liq.*
- *17010900 -> Orina*
- *17011001 -> Ex. Radiologicos Simples*
- *17011002 -> Ex. Radiologicos Complejos*
- *17011003 -> Tomografia Axial Comp.*
- *17011004 -> Ecotomografias ( Sin Ecografia Obstetr. y Abdominal )*
- *17011005 -> Ecografias Obstetricas*
- *17181000 -> Ecotomografias Abdominal*
- *17019999 -> Resonancia Magnetica*
- *99999991 -> Total Examenes Anatomia Patologica*

### Número de egresos

El número de egresos es el especificado en el siguiente [archivo](raw_data/minsal_data/consolidated_establishments_data.xlsx), donde el dato es representado por la glosa con el mismo nombre

## Obteniendo los egresos de los hospitales

De los archivos guardados [aca](raw_data/discharge_data), podemos obtener los datos de los egresos de los hospitales.

La data resultante es guardada en este [path](data/discharge_data) como un archivo .csv files por cada año comprendido en el estudio.

Las columnas en los archivos .csv son:

- **hospital_id** -> Código único para representar un hospital
- **...** -> Todas las columnas correspondientes los egresos hospitalarios especificadas anteriormente
- **total_discharge** -> Total de egresos hospitalarios (Considerando año)
- **GRD{year}** -> Valor peso medio GRD para el hospital (Considerando año)

|hospital_id|17051100|17051200|03040210|........|Datos Establecimiento Numero de Egresos|discharge_total|GRD2014|
|-----------|--------|--------|--------|--------|---------------------------------------|---------------|-------|
|101100     |108.0   |634.0   |0.0     |........|341                                    |732869.0       |7313.0 |
|102100     |62.0    |741.0   |0.0     |........|481                                    |646377.0       |762.0  |
|103100     |154.0   |497.0   |0.0     |........|550                                    |766166.0       |8336.0 |
|103101     |15.0    |201.0   |0.0     |........|125                                    |329963.0       |6249.0 |
|104100     |61.0    |281.0   |0.0     |........|291                                    |450402.0       |7375.0 |

## Data consolidada

En [este](data/consolidated_data) directorio se encuentra la data consolidada para el estudio en formato .csv por cada año comprendido en el estudio.

Las columnas en los archivos .csv son:

- **hospital_id** -> Código único para representar un hospital
- **hospital_fns_id** -> Código único para representar un hospital en los archivos de data financiera
- **financial_total** -> Valor consolidado de Gastos personales y Bienes y servicios del hospital (Considerando año)
- **total_discharge** -> Total de egresos hospitalarios (Considerando año)
- **GRD{year}** -> Valor peso medio GRD para el hospital (Considerando año)

## Peso medio GRD

Para poder cuantificar el impacto que posee GRD al momento de evaluar la eficiencia técnica de los hospitales, necesitamos los pesos medios que están especificados en el siguiente [archivo](data/grd_weight.csv) .csv