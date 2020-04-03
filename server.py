import aiohttp
import asyncio
import config
import json
import re
import sys
import time
import logging

cache = {}

class EchoServerClientProtocol(asyncio.Protocol):

  def __init__(self, server_id, evt_loop):
    self.evt_loop = evt_loop
    self.server_id = server_id
    self.peername = None
    self.message = ""

  def connection_made(self, transport):
    self.peername = transport.get_extra_info('peername')
    self.log_message('Connection made: {}', self.peername)
    self.transport = transport

  def data_received(self, data):
    decoded_data = data.decode()
    self.log_message('Data received: {!r}', decoded_data)
  
    self.message = decoded_data
    self.process_data(self.message)

  def process_data(self, message):
    command = self.message.split()
    opcode = command[0]
    data = command[1:]

    if opcode == 'IAMAT' and self.valid_iamat(data):
      self.process_iamat(data)
    elif opcode == 'WHATSAT' and self.valid_whatsat(data):
      self.process_whatsat(data)
    elif opcode == 'AT' and self.valid_at(data):
      self.process_at(data)
    else:
      response = '? {}\n'.format(command)
      self.transport.write(response.encode())
      self.log_message('Data sent: {!r}', response)

  def log_message(self, message, param):
    print(message.format(param))
    logging.info(message.format(param))

  def update_cache(self, client_id, client_info):
    cached_client_info = cache.get(client_id)

    if cached_client_info is not None:
      if client_info == cached_client_info:
        self.log_message('New client info for {} is up-to-date', client_id)
        return False
      cached_time = cached_client_info[3]
      if float(cached_time) >= float(client_info[3]):
        self.log_message('New client info for {} is stale', client_id)
        return False

    cache[client_id] = client_info
    self.log_message('New client info for {} is updated', client_id)
    return True

  def valid_location(self, location):
    if not re.fullmatch('[+-]\d*\.?\d+[+-]\d*\.?\d+', location):
      return False

    coords = re.split('[+-]', location[1:])
    latitude, longitude = float(coords[0]), float(coords[1])
    if latitude < -90 or latitude > 90 or longitude < -180 or longitude > 180:
      return False
    return True

  def valid_time(self, time):
    try:
      float(time)
    except ValueError:
      return False

    return True

  def valid_iamat(self, data):
    if len(data) != 3:
      print("Error: Invalid number of args for IAMAT")
      return False

    location = data[1]
    time = data[2]

    if not self.valid_location(location):
      print("Error: Invalid location format")
      return False

    if not self.valid_time(time):
      print("Error: Invalid time format")
      return False

    return True

  def process_iamat(self, data):
    client_id = data[0]
    client_location = data[1]
    client_time = data[2]

    time_diff = time.time() - float(client_time)
    if time_diff > 0:
      time_diff_str = '+' + str(time_diff)
    else:
      time_diff_str = str(time_diff)

    response = 'AT {} {} {} {} {}\n'.format(
      self.server_id, time_diff_str, client_id, client_location, client_time
    )
    self.transport.write(response.encode())
    self.log_message('Data sent: {!r}', response)

    new_client_info = [self.server_id, time_diff_str, client_location, client_time]
    updated_cache = self.update_cache(client_id, new_client_info)

    if updated_cache:
      message = 'AT {} {} {} {} {} {}\n'.format(
        self.server_id, time_diff_str, client_id, client_location, client_time, self.server_id
      )
      self.log_message('Attempting to flood client info: {}', message)
      self.evt_loop.create_task(self.flood_to_neighbors(message, []))

  def valid_whatsat(self, data):
    if len(data) != 3:
      print("Error: Invalid number of args for WHATSAT")
      return False

    client_id = data[0]
    radius = data[1]
    num_results = data[2]

    if client_id not in cache.keys():
      print("Error: Invalid client ID")
      return False

    try:
      radius = int(radius)
      num_results = int(num_results)
    except ValueError:
      print("Error: Invalid radius/num_results type")
      return False

    if radius < 0 or radius > 50 or num_results < 0 or num_results > 20:
      print("Error: Radius/num_results out of bounds")
      return False

    return True

  async def fetch(self, session, url):
    async with session.get(url, ssl=False) as response:
      return await response.text()

  async def query(self, location, radius, num_results):
    url = '{}location={}&radius={}&key={}'.format(config.API_URL_BASE, location, radius, config.API_KEY)
    async with aiohttp.ClientSession() as session:
      response = await self.fetch(session, url)
      deserialized_response = json.loads(response)
      deserialized_response['results'] = deserialized_response['results'][:num_results]
      serialized_response = '{}\n\n'.format(json.dumps(deserialized_response, indent=2))
      self.transport.write(serialized_response.encode())
      self.log_message('Data sent: {!r}', serialized_response)

  def process_whatsat(self, data):
    client_id = data[0]
    radius = int(data[1]) * 1000
    num_results = int(data[2])
    client_server = cache[client_id][0]
    client_time_diff = cache[client_id][1]
    client_location = cache[client_id][2]
    client_time = cache[client_id][3]


    at_response = 'AT {} {} {} {} {}\n'.format(
      client_server, client_time_diff, client_id, client_location, client_time
    )
    self.transport.write(at_response.encode())

    coords = re.split('[+-]', client_location[1:])
    latitude_sign = (1 if client_location[0] == '+' else -1)
    longitude_sign = (1 if ('+' in client_location[1:]) else -1)
    latitude, longitude = float(coords[0]) * latitude_sign, float(coords[1]) * longitude_sign
    latitude_and_longitude = '{},{}'.format(latitude, longitude)
    

    self.evt_loop.create_task(self.query(latitude_and_longitude, radius, num_results))


  def valid_at(self, data):
    if len(data) != 6:
      print("Error: Invalid number of args for AT")
      return False

    server_id = data[0]
    client_location = data[3]
    client_time = data[4]

    if server_id not in config.SERVER_IDS:
      print("Error: Invalid server ID")
      return False

    if not self.valid_location(client_location) or not self.valid_time(client_time):
      print("Error: Invalid location/time format")
      return False

    return True

  async def flood_to_neighbors(self, message, senders):
    for neighbor in config.SERVER_NEIGHBORS[self.server_id]:
      if neighbor not in senders:
        try:
          #  Flood to neighbor
          port = config.SERVER_PORT[neighbor]
          reader, writer = await asyncio.open_connection(host=config.HOST, port=port, evt_loop=self.evt_loop)
          writer.write(message.encode())
          await writer.drain()
          writer.close()
          self.log_message('Success: Flooded data to {}', neighbor)
        except ConnectionRefusedError:
          self.log_message('Failure: Unable to connect to {}', neighbor)

def process_at(self, data):
    source_server = data[0]
    time_diff = data[1]
    client_id = data[2]
    client_location = data[3]
    client_time = data[4]
    client_server = data[5]

    new_client_info = [source_server, time_diff, client_location, client_time]
    updated_cache = self.update_cache(client_id, new_client_info)

    if updated_cache:
      message = 'AT {} {}\n'.format(' '.join(data[:-1]), self.server_id)
      self.log_message('Attempting to flood client info: {}', message)
      self.evt_loop.create_task(self.flood_to_neighbors(message, [source_server, client_server]))

def connection_lost(self, exc):
    self.log_message('Connection lost: {}', self.peername)

def main():
  #CLA correct?
  if len(sys.argv) != 2:
    print("Error: Incorrect number of arguments\nCorrect Usage: python3 server.py <server_id>")
    sys.exit(1)

  if sys.argv[1] not in config.SERVER_IDS:
    print("Error: Incorrect server ID\nUsage: [Golomon Hands Holiday Welsh Wilkes]")
    sys.exit(1)

  logging.basicConfig(filename='server.{}.log'.format(server_id), filemode='w', format='%(asctime)s %(levelname)s %(message)s', level=logging.INFO)

  server_id = sys.argv[1]
  port = config.SERVER_PORT[server_id]

  evt_loop = asyncio.get_event_loop()

  coro = evt_loop.create_server(lambda: EchoServerClientProtocol(server_id, evt_loop),'127.0.0.1', port)
  server = evt_loop.run_until_complete(coro)

  print('Serving server {} on {}'.format(server_id, server.sockets[0].getsockname()))
  logging.info('Serving server {} on {}'.format(server_id, server.sockets[0].getsockname()))
  try:
    evt_loop.run_forever()
  except KeyboardInterrupt:
    pass

  server.close()
  evt_loop.run_until_complete(server.wait_closed())
  evt_loop.close()

if __name__ == "__main__":
  main()