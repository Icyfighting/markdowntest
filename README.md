---
layout: post
title: "Erlang learning (6) - Concurrent Programming (2)"
subtitle: "Register Process & Distributed Programming"
author: "Bing Yan"
header-img: "img/erlang-6/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Concurrent Programming
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp;Last time I have learned process and how to communicate between process - passing messages in Erlang concurrent programming.And practise a simple ping-pong example to achieve the sending and receiving of messages between two processes.<br/>
&ensp;&ensp;&ensp;&ensp;The purpose of this study is to see what other knowledge points in Erlang concurrent programming can achieve more abundant functions.

## Text

### Register Process

**Why register process name:**

In the last blog [Erlang learning (5) - Concurrent Programming (1)](https://icyfighting.github.io/2018/12/24/erlang-concurrent-programming-1/), in Ping-Pong example, we know processes which need to know each other's identities are started independently of each other. But as communication with a person, you would like use his name instead of his ID. Erlang thus provides a mechanism for processes to be given names so that these names can be used as identities instead of pids. This is done by using the register BIF:<br/>
```
register(some_atom, Pid)
```

**Modify Ping-Pong Example:**

Using process name register, the code is more fresh and cool.<br/>
ping/2 now becomes ping/1 as the argument Pong_PID is not needed.
```
-module(tut11).
-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong ! finished,
    io:format("ping finished~n", []);

ping(N) ->
    pong ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start() ->
    register(pong, spawn(tut11, pong, [])),
    spawn(tut11, ping, [3]).
```
Test: <br/>

![](/img/erlang-6/example-1.png)


### Distributed Programming

I want to experiment with distributed Erlang, but I only have one computer to work on, so I start two separate Erlang systems on the same computer but give them different names. Each Erlang system running on a computer is called an Erlang node.<br/>

```
$ erl -sname my_name
```

**Modify Ping-Pong Example:**

```
-module(tut13).

-export([start_ping/1, start_pong/0,  ping/2, pong/0]).

ping(0, Pong_Node) ->
    {pong, Pong_Node} ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_Node) ->
	io:format("Pong_Node: ~w~n", [Pong_Node]),
	io:format("ping_self(): ~w~n", [self()]),
    {pong, Pong_Node} ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N - 1, Pong_Node).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start_pong() ->
    register(pong, spawn(tut13, pong, [])).

start_ping(Pong_Node) ->
    spawn(tut13, ping, [3, Pong_Node]).

```

Test: <br/>

![](/img/erlang-6/pong-2.png)

![](/img/erlang-6/ping-2.png)

<br/>

**Learning notes: From result, I understand there are 2 Erlang system:"ping" and "pong" on same Node "Icy". Process ping on Erlang system ping@Icy can find process pong on Erlang system pong@Icy by argument Pong_Node. <br/>
But how process pong find process ping on different Erlang system just by Ping_PID <0.83.0>? <br/>
There is one explanation as this: Erlang pids contain information about where the process executes. So if you know the pid of a process, the "!" operator can be used to send it a message disregarding if the process is on the same node or on a different node. <br/>
So that means, the reason function start_ping/1 need argument Pong_Node, because process pong register name as pong, name pong does not include all information as pid itself?**

<br/>


## Summary

&ensp;&ensp;&ensp;&ensp; This study is mainly to learn the basics of distributed programming in Erlang. There are some questions in the study, which are solved by testing and finding information. But I still need to continue practicing to understand the mechanism of concurrency and distribution.


## Reference
http://www.erlang.org <br/>
http://erlang.org/doc/getting_started/conc_prog.html <br/>
