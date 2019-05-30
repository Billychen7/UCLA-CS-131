# references: 
# TA Kimmo Karkkainen's slides
# https://asyncio.readthedocs.io/en/latest/tcp_echo.html#tcp-echo-client

import sys # for command line arguments
import asyncio
import time # for timing when messages are received/sent between the client and server

IAMAT = 1
WHATSAT = 2

# for bidirectional communication between servers 
server_connections = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'],
    'Hands': ['Goloman', 'Wilkes'],
    'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
    'Welsh': ['Holiday'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday']
}

server_ports = {
    'Goloman': 12372,
    'Hands': 12373,
    'Holiday': 12374,
    'Welsh': 12375,
    'Wilkes': 12376
}

# dictionary that maps a client ID to a list containing
# [Server name, Coordinates, Time client sent message, Time server received message]
clients = {}

# input: coordinate string: +34.068930-118.445127
# output: (latitude, longitude) tuple: ('+34.068930', '-118.445127')
# return None if input is invalid
def separate_lat_and_long(coordinates):
    positions_of_signs = []
    for i in range(len(coordinates)):
        # if the current character is a sign
        if coordinates[i] == '+' or coordinates[i] == '-':
            positions_of_signs.append(i)

    # there should only be 2 signs
    if len(positions_of_signs) != 2:
        return None

    # one of the signs should be at the beginning, and no signs should be at the end
    if 0 not in positions_of_signs or (len(coordinates) - 1) in positions_of_signs:
        return None

    latitude = coordinates[positions_of_signs[0]:positions_of_signs[1]]
    longitude = coordinates[positions_of_signs[1]:]

    return (latitude,longitude)



# note: the message has been split at this point and is represented as an array
# return value:
# -1 for invalid message
# 1 for IAMAT
# 2 for WHATSAT
def check_valid_message(message):
    if len(message) < 1:
        return -1

    command_name = message[0]

    # check for valid command name
    if command_name != 'IAMAT' and command_name != 'WHATSAT':
        return -1

    # check for correct amount of tokens
    if len(message) != 4:
        return -1

    if command_name == 'IAMAT':
        return IAMAT

    return WHATSAT


# format for IAMAT message:
# IAMAT [Client ID] [Coordinates] [Timestamp (POSIX)]
# Ex: IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
# format for server response:
# AT [Server name] [Timestamp diff] [Copy of client data]
async def handle_IAMAT_message(message, time_received):
    client_ID = message[1]
    coordinates = message[2]
    time_sent = message[3]

    coordinate_pair = separate_lat_and_long(coordinates)

    if coordinate_pair is None:
        # DO ERROR STUFF
        return

    # update clients dictionary
    clients[client_ID] = [server_name, coordinates, time_sent, time_received]

    time_difference = str(time_received - float(time_sent))

    # prepend a '+' if the difference is positive
    if time_difference[0] != '-':
        time_difference = '+' + time_difference

    copy_of_client_data = ' '.join(message)
    server_response = "AT " + server_name + " " + time_difference + " " + copy_of_client_data + "\n"




# format for WHATSAT message:
# WHATSAT [Name of another client] [Radius (km)] [Max # of results]
# Ex: WHATSAT kiwi.cs.ucla.edu 10 5
async def handle_WHATSAT_message(message, time_received):


# server implementation
async def handle_connection(reader, writer):
    data = await reader.readline()
    received_message = data.decode()
    time_received = time.time()

    log_file.write("RECEIVED: " + received_message)

    received_message = received_message.strip() # remove leading and trailing spaces
    received_message = received_message.split() # convert message into a list of strings

    message_type = check_valid_message(received_message)

    if message_type == -1:
        #do error stuff

    elif message_type == IAMAT:
        await handle_IAMAT_message(received_message, time_received)

    elif message_type == WHATSAT:
        await handle_WHATSAT_message(received_message, time_received)




def main():
    # command line argument checking
    if len(sys.argv) != 2:
        print("Error: incorrect number of arguments.\nUsage: python3 server.py [server_name]")
        exit()
    elif sys.argv[1] not in server_connections:
        print("Error: invalid server name.\nUsage: python3 server.py [server_name]")
        exit()

    global server_name
    server_name = sys.argv[1]
    
    log_file_name = server_name + "-log.txt"
    print(log_file_name)

    global log_file
    log_file = open(log_file_name, 'a+') # open for reading and appending

    # check about doing .close or something like that

    # set up event loop
    global event_loop
    event_loop = asyncio.get_event_loop()
    coroutine = asyncio.start_server(handle_connection, host='127.0.0.1', port=server_ports[server_name], loop=event_loop)
    server = event_loop.run_until_complete(coroutine)

    # serve requests until Ctrl+C is pressed
    print('Serving on {}'.format(server.sockets[0].getsockname()))
    try:
        event_loop.run_forever()
    except KeyBoardInterrupt:
        pass

    # close the server
    server.close()
    event_loop.run_until_complete(server.wait_closed())
    event_loop.close()
    log_file.close()

if __name__=='__main__':
    main()
