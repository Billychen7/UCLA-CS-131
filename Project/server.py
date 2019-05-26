import sys # for command line arguments
server_names = ['Goloman', 'Hands', 'Holiday', 'Welsh', 'Wilkes']

def main():
    # command line argument checking
    if len(sys.argv) != 2:
        print("Error: incorrect number of arguments.\nUsage: python3 server.py [server_name]")
        exit()
    elif sys.argv[1] not in server_names:
        print("Error: invalid server name.\nUsage: python3 server.py [server_name]")
        exit()
    
    print(sys.argv[1])

if __name__=='__main__':
    main()
