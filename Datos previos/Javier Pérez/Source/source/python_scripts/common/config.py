""" File with constants values for read/write data """

# Paths
HOSPITAL_DICTIONARY_PATH = '../../data/hospitals_dictionary.csv'

# Years range
YEARS_RANGE = [str(year) for year in range(2014, 2024)]
#YEARS_RANGE = [str(year) for year in range(2014, 2021)]

# Sheets name
FINANCIAL_DATA_SHEET = 'Gasto 21-22'

# Hospital variables codes/names
HOSPITAL_ID = ['hospital_id']
MEDICAL_CONSULTATIONS_IDS = ['03020101', '03020201', '03020301',
                             '03020402', '03020403', '03020401',
                             '03040210', '03040220', '04040100',
                             '04025010', '04025020', '03020501']
SURGICAL_INTERVENTIONS_IDS = ['17050100', '17050200', '17050300',
                              '17050400', '17050500', '17050600',
                              '17050700', '17050800', '17050900',
                              '17051000', '17051100', '17051200',
                              '17051300', '17051400', '17051500',
                              '17051600', '17051700']
DIAGNOSTIC_TEST_IDS = ['17010100', '17010200', '17010300',
                       '17010400', '17010500', '17010601',
                       '17010602', '17010603', '17010700',
                       '17010800', '17010900', '17011001',
                       '17011002', '17011003', '17011004',
                       '17011005', '17181000', '17019999',
                       '99999991']
DISCHARGE_NUMBER = ['Datos Establecimiento Numero de Egresos']

# To normalize columns
COLUMNS_TO_NORMALIZE = ['21_value', '22_value', 'hospital_discharge', 'diagnostic_tests',
                        'surgical_interventions', 'medical_consultations']
