import pyam
import os

# Establish connection to the legacy pyam platform 'ssp-submission'
pyam.iiasa.Connection('ssp_submission')
conn_ssp = pyam.iiasa.Connection('ssp_submission')
props = conn_ssp.properties().reset_index()

# Download marker scenarios
df1 = pyam.read_iiasa(
    'ssp_submission',
    model='REMIND*',
    scenario='SSP1 - Very Low Emissions',
)
df1.to_csv(
      os.path.join('data','marker_set','vllo.csv')
  )
df2 = pyam.read_iiasa(
    'ssp_submission',
    model='AIM*',
    scenario='SSP2 - Low Overshoot_b',
)
df2.to_csv(
      os.path.join('data','marker_set','vlho.csv')
  )
df3 = pyam.read_iiasa(
    'ssp_submission',
    model='MESSAGE*',
    scenario='SSP2 - Low Emissions',
)
df3.to_csv(
      os.path.join('data','marker_set','l.csv')
  )
df4 = pyam.read_iiasa(
    'ssp_submission',
    model='COFFEE*',
    scenario='SSP2 - Medium-Low Emissions',
)
df4.to_csv(
      os.path.join('data','marker_set','ml.csv')
  )
df5 = pyam.read_iiasa(
    'ssp_submission',
    model='IMAGE*',
    scenario='SSP2 - Medium Emissions',
)
df5.to_csv(
      os.path.join('data','marker_set','m.csv')
  )
df6 = pyam.read_iiasa(
    'ssp_submission',
    model='GCAM*',
    scenario='SSP3 - High Emissions',
)
df6.to_csv(
      os.path.join('data','marker_set','h.csv')
  )
df7 = pyam.read_iiasa(
    'ssp_submission',
    model='WITCH*',
    scenario='SSP5 - Medium-Low Emissions_a',
)
df7.to_csv(
      os.path.join('data','marker_set','hl.csv')
  )
