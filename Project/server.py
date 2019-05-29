import sys # for command line arguments
import asyncio

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

def handle_connection(reader, writer):
    print("test")


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
    # reference: TA Kimmo Karkkainen's slides
    # also: https://asyncio.readthedocs.io/en/latest/tcp_echo.html#tcp-echo-client
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
