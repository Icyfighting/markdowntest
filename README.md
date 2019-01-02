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



## Summary

&ensp;&ensp;&ensp;&ensp; These methods used in concurrent programming have been touched in my previous blog [First meet with Erlang](https://icyfighting.github.io/2018/12/15/erlang-basic/), and this time I learn the processing logic and precautions in more detail.<br/>
&ensp;&ensp;&ensp;&ensp; For some of the questions in the study, I haven't gotten the answer yet. I will continue to learn the rest of the knowledge of concurrent programming, hoping to find or think about the answer.

## Reference
http://www.erlang.org <br/>
http://erlang.org/doc/getting_started/conc_prog.html <br/>
