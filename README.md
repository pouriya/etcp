![etcp travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/etcp.png?branch=master)

# Status
Completing documentation ...

# Synopsis
This is an Erlang/Elixir framework for implementing servers, clients and connection pools over TCP protocols which provides fast, flexible, useful and easy-to-use API.


# Features
* In server, client and connection pool every TCP connnection has its own Erlang process which that process handles following actions in separate callbacks:  
    
    |Action                          |Callback             |Server|Client|Connection pool|
    |:-------------------------------|:--------------------|:----:|:----:|:-------------:|
    |TCP inputs                      |`handle_packet/3`    |●     |●     |●              |
    |Initialization                  |`connector_init/2`   |●     |●     |●              |
    |TCP disconnection               |`handle_disconnect/2`|●     |●     |●              |
    |Erlang generic calls            |`handle_call/4`      |●     |●     |●              |
    |Erlang generic casts            |`handle_cast/3`      |●     |●     |●              |
    |Termination                     |`terminate/3`        |●     |●     |●              |
    |Changing code                   |`code_change/3`      |●     |●     |●              |
    |TCP listen initialize           |`listen_init/2`      |●     |○     |○              |
    |Erlang generic events           |`handle_event/3`     |●     |●     |●              |
    |Erlang other messages           |`handle_info/3`      |●     |●     |●              |
    |Erlang message receiving timeout|`timeout/2`          |●     |●     |●              |
    |TCP packet receiving timeout    |`srtimeout2`         |●     |●     |●              |  

    Callbacks have their own possible return values for sending packet, replying for Erlang message, hibernation, changing state, setting timeout, etc.  
* Erlang hibernation is supported in every connection handler process.  
* Active and Passive sockets are supported.  
* **ETCP** has two transporter modules `'etcp_transporter_tcp'` and `'etcp_transporter_ssl'`. You can have your own by implementing `'etcp_transporter'` behavior.  
* Acceptor and connection handler codes have been written as [Erlang special process](http://erlang.org/doc/design_principles/spec_proc.html#id79834) and they are blazingly fast.  
* Server API:
    * start_link_server/3-4-5
    * stop_server/1-2
    * fetch_server_connections/2
    * fetch_acceptors/1
    * sleep_acceptors/1
    * wakeup_acceptors/1
    * ...
* Client connection pool API:
    * start_link_connection_pool/3-4-5
    * stop_connection_pool/1-2
    * add_connection/3
    * add_connections/4
    * fetch_pool_connections/2
* Client connection (After looking up pid of a connection in server or client pool, you can use following API for that pid too (except `start_link_connection/4-5-6`)):
    * start_link_connection/4-5-6
    * send_sync/2-3 (Sends a packet synchronously through connection handler process.)
    * send_async/2 (Sends a packet asynchronously through connection handler process.)
    * stop_connection/1-2-3
* **ETCP** can use ETS or Mnesia for keeping connection pids. By using this feature acceptors have fast accepting and you can have fast access to connection handler pids.  

All features not listed here. For more info see wiki and examples.  


# License
**`BSD 3-Clause`**

# Author
**`pouriya.jahanbakhsh@gmail.com`**
