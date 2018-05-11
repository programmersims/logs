#iqfeed.py


import sys
import socket
#from subprocess import call
import datetime
import os
import argparse

CLI=argparse.ArgumentParser()
CLI.add_argument(
  "--list",  # name on the CLI - drop the `--` for positional/required parameters
  nargs="*",  # 0 or more values expected => creates a list
  type=str,
  default=[]
)

CLI.add_argument(
  "--minutes",  # name on the CLI - drop the `--` for positional/required parameters
  nargs="*",  # 0 or more values expected => creates a list
  type=int,
  default=180,
)

args = CLI.parse_args()
time = args.minutes[0] * 60


def get_day():
  now = datetime.datetime.now()
  if now.month < 10:
    if now.day < 10: 
      day = str(now.year)+ "0" + str(now.month) + str("0") + str(now.day)
    else:
      day = str(now.year)+ "0" + str(now.month) + str(now.day)
  else:
    if now.day < 10: 
      day = str(now.year) + str(now.month) + str("0") + str(now.day)
    else:
      day = str(now.year) + str(now.month) + str(now.day)
  return day
  
def read_historical_data_socket(sock, recv_buffer=4096):
    """
    Read the information from the socket, in a buffered
    fashion, receiving only 4096 bytes at a time.

    Parameters:
    sock - The socket object
    recv_buffer - Amount in bytes to receive per read
    """
    buffer = ""
    data = ""
    while True:
        data = sock.recv(recv_buffer)
        buffer += data

        # Check if the end message string arrives
        if "!ENDMSG!" in buffer:
            break

    # Remove the end message string
    buffer = buffer[:-12]
    return buffer


if __name__ == "__main__":
    # Define server host, port and symbols to download
    host = "127.0.0.1"  # Localhost
    port = 9100  # Historical data socket port
    syms = args.list
    # Download each symbol to disk
    for sym in syms:
        print "Downloading symbol: %s..." % sym

        # Construct the message needed by IQFeed to retrieve data
        # message = "HIT,%s,23400,20000101 0930000,20200101 160000,,093000,160000,\n" % sym
        # message = "HDX,%s,23400,20000101 0930000,20200101 160000,,093000,160000,\n" % sym
        # message = "HDT,%s,20000101,20200101\n" % sym
	day = get_day()
	message = "HIT,{0}, {1}, " + day +" 0930000,,,0930000,1000000,1\n".format(sym, time)

        # Open a streaming socket to the IQFeed server locally
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect((host, port))

        # Send the historical data request
        # message and buffer the data
        sock.sendall(message)
        data = read_historical_data_socket(sock)
        sock.close

        # Remove all the endlines and line-ending
        # comma delimiter from each record
#        data = "".join(data.split("\r"))
#        data = data.replace(",\n","\n")[:-1]

        # Write the data stream to disk
        fname = "stocks/%s.csv" % sym
        f = open( fname, "w")
        f.write(data)
        f.close()
        os.system( "sed -i -e \"s@\\r@@g\" " + fname)
        os.system( "echo >> " + fname)
        os.system( "tac " + fname + " > tmp")
        os.system( "mv tmp " + fname)
#        call( ["tac", fname, ">", fname] )


