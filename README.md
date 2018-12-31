---
layout: post
title: " First meet with Erlang"
subtitle: "Erlang learning (1) - basic"
author: "Bing Yan"
header-img: "img/erlang/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Golang
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp;This is the first time I met Erlang. I heard about Erlang because Erlang is a programming language developed by Ericsson, the company I work for 6 years. <br/>
&ensp;&ensp;&ensp;&ensp;In order to adapt to the potential needs of the new position, I would like to have a preliminary understanding of Erlang through self-study, so that I can use Erlang for project development more quickly when needed later.

## Text

### What's Erlang

Let us first understand the basic situation of Erlang from Baidu Encyclopedia. Although it is not very beautiful to read...

>&ensp;&ensp;&ensp;&ensp;Erlang is a general-purpose, concurrent programming language developed by CS-Lab, a Swedish telecom equipment manufacturer based in Ericsson, to create a programming language that can handle large-scale concurrent activities and the operating environment. <br/>
&ensp;&ensp;&ensp;&ensp;Erlang was born in 1987. After ten years of development, it released an open source version in 1998. Erlang is an interpreted language that runs on virtual machines, but now also includes native code compilers developed by Uppsala University's High Performance Erlang Project (HiPE). Since R11B-4, Erlang has also supported scripted interpreters. <br/>
&ensp;&ensp;&ensp;&ensp;In the programming paradigm, Erlang is a multi-paradigm programming language, covering functional, concurrent and distributed. The sequential execution of Erlang is a functional programming language for early evaluation, single assignment and dynamic typing. <br/>
&ensp;&ensp;&ensp;&ensp;Erlang is a structured, dynamic type programming language with built-in parallel computing support. Originally designed for communication applications by Ericsson, such as control switches or conversion protocols, it is ideally suited for building distributed, real-time, soft-parallel computing systems. <br/>
&ensp;&ensp;&ensp;&ensp;Application runtimes written in Erlang typically consist of thousands of lightweight processes and communicate with each other via messaging. Inter-process context switching is only one or two links for Erlang, much more efficient than thread switching for C programs.<br/>
&ensp;&ensp;&ensp;&ensp;Using Erlang to write a distributed application is much simpler because its distributed mechanism is transparent: it is not known to the program that it is running distributed. The Erlang runtime environment is a virtual machine, a bit like a Java virtual machine, so that once the code is compiled, it can be run anywhere. <br/>
&ensp;&ensp;&ensp;&ensp;Its runtime system even allows code to be updated without interruption. In addition, if you need more efficient, the byte code can also be compiled to run the code.<br/>

**So in a nutshell, Erlang is a concurrent, suitable for building distributed, structured, dynamic type programming languages.** <br/>

### Erlang Features

Erlang is a programming language with multiple paradigms. It has many features. The main features are as follows:<br/>

*   Functional formula:<br/>
>Function is a programming model that treats operations in a computer as function calculations in mathematics, avoiding the concept of states and variables.Functions are the basic unit of the Erlang programming language. In Erlang, functions are the first type, and functions are used almost exclusively, including the simplest calculations. All concepts are expressed by functions, and all operations are also operated by functions.

*   Concurrency:<br/>
>The Erlang programming language can support a very large number of levels of concurrency without relying on the operating system and third-party external libraries. Erlang's concurrency relies heavily on Erlang virtual machines and lightweight Erlang processes.

*   Distributed:<br/>
>Erlang's distributed features are supported by Erlang at the language level. You can use the language built-in API functions to create Erlang processes on remote nodes and then execute the specified module functions. Similarly, you can use the RPC module of Erlang to call the module function of the remote node.<br/>
The mutual call between Erlang nodes and the execution of cross-node remote module functions are very convenient. The communication between Erlang nodes is completely supported by the Erlang programming language at the language level. The Erlang language has its own node protocol.

*   Robustness:<br/>
>Robustness is a very important feature of the Erlang programming language. The robustness of the Erlang programming language depends on the following points:<br/>
>*   Process isolation
>*   Perfect error exception handling
>*   Error handling philosophy
>*   Monitor process
 
*   Soft real time:<br/>
>The characteristics of Erlang soft real-time mainly depend on:<br/>
>*   Erlang virtual machine scheduling mechanism
>*   Memory garbage collection strategy
>*   Process resource isolation <br/>
The Erlang system garbage collection strategy is divided and recycled. The incremental garbage collection method is based on the characteristics of process resource isolation. Erlang memory garbage collection is based on a single Erlang process. In the process of garbage collection, the world will not stop. That is, it will not affect the entire system. Combine the Erlang virtual machine preemptive scheduling mechanism to ensure high availability and soft real-time performance of the Erlang system.

*   Hot update:<br/>
>The Erlang system allows program code to be modified during runtime, and old code logic can be phased out and replaced by new code logic. In this process, the old and new code logic coexist in the system, Erlang "hot update" features, can guarantee the operation of the Erlang system to the greatest extent, and will not suspend the system due to business updates.

*   Incremental code loading:<br/>
>Erlang's libraries, including the existing libraries in Erlang and the libraries created by Code Farm, are run on the outer layer of the Erlang virtual machine (there is a picture above). It can be loaded, started, stopped, and uninstalled while the Erlang system is running. These are all programmers can control.

*   Dynamic type:<br/>
>Erlang is both a dynamic language and a dynamic type.<br/>
Dynamic language means that the structure of the code can be changed during system operation. Existing functions can be deleted or modified. The runtime code can change its structure according to certain conditions. This is also a basis for Erlang to be hot updated.<br/>The dynamic type is worth checking that the data type is checked during the prototype, and the binding of the data type is not in the compile phase, but is delayed to the run phase.

*   Explanatory:<br/>
>Erlang is an interpreted language that runs on a virtual machine and has good platform compatibility.

### Erlang Functions

From our birth to the present, we have been maintaining this memory in the brain, and we are constantly updating this memory with constant interaction with the outside world. We learned a lot about how to deal with people, we talk, write letters, send text messages, and make phone calls. This is asynchronous messaging.<br/>
<br/>
The world of Erlang is very similar to our real world. Each Erlang process maintains its own unique memory, and other processes cannot access its internal state unless they exchange messages for each other. All messaging is asynchronous, just like our real world.<br/>
<br/>
It is said that the following six functions make up the world view of Erlang. Let me know one by one:<br/>

**spawn**
 ```
1>spawn(foo,hello,[]).

<0.70.0>
 ```
 
This will create a new process, calling foo:hello(). Once the process completes the function, it will die and return all allocated memory to the BEAM.<br/>

If you want to create two processes to do things at the same time, just spawn twice:<br/>

```
1>spawn(foo,hello,[]).

<0.70.0>

2>spawn(foo,hello,[]).

<0.71.0>
```

This will create two processes concurrently calling foo:hello(). This is the concurrency model of Erlang - also called the actor model.<br/>
If you want to work on 100 processes at the same time, call spawn 100 times.<br/>

**send**
We know how to create a process, the next step is to send a message to it:<br/>

```
1>Pid=spawn(foo,loop,[]).

<0.80.0>

2>Pid!hello.

hello
```

Here we start a process call foo:loop(). We assume that this loop function will call itself recursively, so that our process will die without a click. Spawn will return a process ID <0.80.0>, we bind it to the Pid variable and send a message hello to it. Exclamation mark in Erlang! Send a message. The above is asynchronous messaging. This is also the only means of communication between the two processes in Erlang.

**receive**
When a message is sent to a process, how does the process receive the message? <br/>

```
1>Pid=spawn(fun()->

1>receive

1>hello->io:format("Got hello message~n")

1>end

1>end).<0.86.0>

2>Pid!hello.

Gothellomessage

hello
```

Use receive to receive the message. You can use pattern matching here to match the information you want to receive, ignoring other messages. In this example, we only receive the message hello.

**register**
We usually can't remember the phone number of a friend, so we use the address book to add a name to the phone number. There is no need to remember the Pid of each process in Erlang, register it with a name and access it later by name.<br/>

```
1>Pid=spawn(fun()->receivehello->io:format("Got hello message~n")endend).

<0.93.0>

2>register(foo,Pid).

true

3>foo!hello.

Gothellomessage

hello
```

Just call register(Name,Pid) to register the name for any process. Then we can use the name to send a message to the process.


**whereis**

When we register a name for a process, we can also find its Pid by looking up the address book.

```
1>register(foo,spawn(fun()->receivehello->helloendend)).

true

2>whereis(foo).

<0.102.0>
```

**self**

A process can find its own Pid by calling self().<br/>

```
1>self().

<0.90.0>
```

## Summary

Erlang is a functional programming language whose core is the Erlang virtual machine. Erlang concurrent process is different from operating system process, it is very lightweight, Erlang built-in distributed features, very convenient, Erlang programming language soft real-time features can be more robust under the protection of its error exception handling mechanism, its hot update Can bring us a lot of convenience to the code farmers.

It is said that this is the founder of the Erlang language, our colleague: Joe Armstrong
<br/>
![](/img/erlang/Joe.jpeg)


## Reference
https://www.cnblogs.com/--00/p/erlang_into_style.html <br/>
https://www.sohu.com/a/244980510_473282 <br/>
https://www.cnblogs.com/dasea/archive/2012/08/18/2644927.html <br/>
http://www.erlang.org/docs

