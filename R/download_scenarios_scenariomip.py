import pyam
import os

df = pyam.read_iiasa(
  'ssp-submission',
  model="MESSAGE*",
  scenario='SSP2 - Low*',
  region="World"
  )

# output_file = 'justmip_data.csv'

df.to_csv(
    os.path.join('..', 'data','scenariomip','some_data.csv')
)
