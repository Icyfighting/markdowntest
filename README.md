---
layout: post
title: "Erlang learning (5) - Concurrent Programming"
subtitle: "Process & Message Passing & Distributed Programming"
author: "Bing Yan"
header-img: "img/erlang-5/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Concurrent Programming
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp;One of the main reasons for using Erlang instead of other functional languages is Erlang's ability to handle concurrency and distributed programming. By concurrency is meant programs that can handle several threads of execution at the same time.For example, modern operating systems allow you to use a word processor, a spreadsheet, a mail client, and a print job all running at the same time.<br/>
&ensp;&ensp;&ensp;&ensp;Let's see how Erlang realize concurrency and distributed programming.

## Text

### Processes

**What's process**
Each processor (CPU) in the system is probably only handling one thread (or job) at a time, but it swaps between the jobs at such a rate that it gives the illusion of running them all at the same time. It is easy to create parallel threads of execution in an Erlang program and to allow these threads to communicate with each other. In Erlang, each thread of execution is called a process.<br/>
The term "process" is usually used when the threads of execution share no data with each other and the term "thread" when they share data in some way. Threads of execution in Erlang share no data, that is why they are called processes.<br/>
**Learning notes: the concept of thread and process are consistent with those in Java.<br/>
In Java, each process has its own heap space, but threads of same process share the same heap spaces, but each thread has own stack space.**
<br/>

**How to create process**

The Erlang BIF spawn is used to create a new process: spawn(Module, Exported_Function, List of Arguments).

```
-module(tut9).
-export([start/0, say_something/2]).

say_something(What, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(tut9, say_something, [hello, 3]),
    spawn(tut9, say_something, [goodbye, 3]).
```
Test: <br/>

![](/img/erlang-5/example-1.png)

**Knowledge Points:**

*   -export:
>A function used in this way by spawn, to start a process, must be exported from the module. That is, in the -export at the start of the module. This should be noticed, because according to earlier study, it seems only start/0 should be exported, and say_something/2 seems like local function. 

*   output order:
>Notice that it did not write "hello" three times and then "goodbye" three times. Instead, the first process wrote a "hello", the second a "goodbye", the first another "hello" and so forth. 

**Learning notes: I want to know, if the output alway has this order? How Erlang handle the process? Polling? Resource competition? 
To see if I can find answer later.**

*   process identifier:
>where did the <0.1153.0> come from? The return value of a function is the return value of the last "thing" in the function. The last thing in the function start is spawn(tut14, say_something, [goodbye, 3]). <br/>
spawn returns a process identifier, or pid, which uniquely identifies the process. So <0.1153.0> is the pid of the spawn function call above.

### Message Passing





## Summary

&ensp;&ensp;&ensp;&ensp; From these simple examples, I practise some data type and modules of Erlang.<br/>
&ensp;&ensp;&ensp;&ensp; Next time, I will start to learn Concurrent Programming chapter following the Guides.<br/>
&ensp;&ensp;&ensp;&ensp; One of the main reasons for using Erlang instead of other functional languages is Erlang's ability to handle concurrency and distributed programming. So next chapter will be very interesting.
## Reference
http://www.erlang.org <br/>
http://erlang.org/doc/getting_started/seq_prog.html <br/>
