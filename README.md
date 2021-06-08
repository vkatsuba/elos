# elos

**elos** is **Erlang Log Server**

[![Build Status](https://github.com/vkatsuba/elos/workflows/CI/badge.svg)](https://github.com/vkatsuba/elos/actions)

# Build & Run

```sh
$ clone https://github.com/vkatsuba/elos.git
$ cd elos
$ rebar3 compile
$ rebar3 as prod tar
$ rebar3 as prod release
$ _build/prod/rel/elos/bin/elos console
```

# Xref

```sh
$ rebar3 xref
```

# Dialyzer

```sh
$ rebar3 dialyzer
```

# Notes

For case if application provide error:
```erlang
===> Failed to boot elos for reason {{shutdown,
                                                 {failed_to_start_child,elos,
                                                  eaddrinuse}},
                                                {elos_app,start,[normal,[]]}}
```

See IPs by List Of Opened Files:

```sh
$ lsof -i -n -P
```

Kill signal:
```sh
kill -9 Pid
```

Pid of `elos`:
```erlang
1> whereis('127.0.0.1:8181').
```

# API

## Create new log listener

```erlang
Config1 = #{
    name => '127.0.0.1:8282',
    ip => {127, 0, 0, 1},
    port => 8282,
    file => "log/127.0.0.1:8282.log",
    level => error,
    recbuf => 8192
},
{ok, Pid} = elos:start_instance(Config1).

whereis('127.0.0.1:8282').
```

## Create new log listener

```erlang
whereis('127.0.0.1:8282').

elos:stop_instance('127.0.0.1:8282').

whereis('127.0.0.1:8282').
```

# Clients

## Python client Example

Create `send_log.py`:

```
$ touch send_log.py
```

Copy & Paste:

```python
#!/usr/bin/env python3

import socket

UDP_IP = "127.0.0.1"
UDP_PORT = 8181
LOG_LEVEL = chr(1)
MESSAGE = (LOG_LEVEL + 'This message was sended from Python client').encode('ascii')

print("UDP target IP: %s" % UDP_IP)
print("UDP target port: %s" % UDP_PORT)
print("message: %s" % MESSAGE)

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.sendto(MESSAGE, (UDP_IP, UDP_PORT))


data, addr = sock.recvfrom(1024) # buffer size is 1024 bytes
print("received message: %s" % data)
```

Run:
```sh
$ chmod 777 send_log.py
$ ./send_log.py
UDP target IP: 127.0.0.1
UDP target port: 8181
message: b'\x01This message was sended from Python client'
received message: b'\x01'
```

## Erlang Client Example
```
1> {ok, Socket} = gen_udp:open(8081, [{ip, {127,0,0,1}}, {active, true}, binary, {reuseaddr, true}]).
{ok,#Port<0.6>}
2> gen_udp:send(Socket, "127.0.0.1", 8181, <<1:8/integer, "This message was sended from Erlang client">>).
ok
3> {udp, Socket, {127, 0, 0, 1}, 8181, Res} = receive R -> R end.
{udp,#Port<0.6>,{127,0,0,1},8181,<<1>>}
4> Res.
<<1>>
```
