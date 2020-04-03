import asyncio
import config
import logging

class EchoClientProtocol(asyncio.Protocol):
    def __init__(self, msg, evt_loop):
      self.msg = msg
      self.evt_loop = evt_loop

    def connection_made(self, transport):
      msg1 = 'IAMAT'
      msg2 = ' kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997'

      transport.write(msg1.encode())
      print('Data sent: {!r}'.format(msg1))

      transport.write(msg2.encode())
      print('Data sent: {!r}'.format(msg2))
      
      if transport.can_write_eof():
        transport.write_eof()

    def data_received(self, data):
      logging.info('Data received: {!r}'.format(data.decode()))
      print('Data received: {!r}'.format(data.decode()))

    def connection_lost(self, exc):
      print('The server closed the connection')
      print('Stop the event loop')
      self.evt_loop.stop()

def main():
  logging.basicConfig(filename='client.log',
                      filemode='w',
                      format='%(asctime)s %(levelname)s %(msg)s',
                      level=logging.INFO)
  evt_loop = asyncio.get_event_loop()
  msg1 = 'IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997'
  msg2 = 'WHATSAT kiwi.cs.ucla.edu 10 5'
  coro = evt_loop.create_connection(lambda: EchoClientProtocol(msg2, evt_loop),
                                '127.0.0.1', 11760)
  evt_loop.run_until_complete(coro)
  evt_loop.run_forever()
  evt_loop.close()

if __name__ == '__main__':
  main()