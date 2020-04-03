HOST = '127.0.0.1'

SERVER_IDS = ['Goloman', 'Hands', 'Holiday', 'Welsh', 'Wilkes']

SERVERS_TO_PORTS = {
  'Goloman' : 11460,
  'Hands' : 11461,
  'Holiday' : 11462,
  'Welsh' : 11463,
  'Wilkes' : 11464
}

SERVER_NEIGHBORS = {
  'Goloman' : ['Hands', 'Holiday', 'Wilkes'],
  'Hands' : ['Goloman', 'Wilkes'],
  'Holiday' : ['Goloman', 'Welsh', 'Wilkes'],
  'Welsh' : ['Holiday'],
  'Wilkes' : ['Goloman', 'Hands', 'Holiday']
}

API_URL_BASE = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'

API_KEY = 'AIzaSyB-yDFvkbZaE46BqRgQaOmc52n5req7kiM'