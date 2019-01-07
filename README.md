---
layout: post
title: "Erlang learning (10) - Data Types (2)"
subtitle: "Types & Type Conversions"
author: "Bing Yan"
header-img: "img/erlang-10/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Data Type
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp;After learning the basic knowledge and examples of Erlang according to Getting Started in the manual in blog [Erlang learning (3) - Data Types (1)](https://icyfighting.github.io/2018/12/20/erlang-data-type/), let's learn other knowledge points in various aspects.<br/>
&ensp;&ensp;&ensp;&ensp;This study mainly supplements the data structure and the conversion between the data types.<br/>

## Text


### Number

There are 4 kinds of Number type in Erlang: <br/>

*   integer:
*   float:
*   $char (Erlang-specific):
>ASCII value or unicode code-point of the character char.
*   base#value (Erlang-specific):
>Integer with the base base, that must be an integer in the range 2..36.

**Example**

```
1> 30.
30
2> 3.2e2.
320.0
3> $a.
97
4> $\r.
13
5> 2#110.
6
6> 16#ff.
255
```

### Atom

Atom is learned in last study about data type of Erlang, but it needs some additional information. <br/>
An atom is a literal, a constant with name. <br/>
An atom is to be enclosed in single quotes (') if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore (_), or @.<br/>
**Learning notes: I already know atom is a literal which begin with a lower-case letter. But this time I know, it can contains _ or @.<br/>
And if atom contains other conditions, need enclosed in single quotes.<br/>
Besides, double quotes are used to enclose String, which is not a data type of Erlang but can be used.**

**Example**

```
7> office_addr.
office_addr
8> 'office addr'.
'office addr'
9> 'Monday'.
'Monday'
```

### Pid

A process identifier, pid, identifies a process. <br/>
The following BIFs, which are used to create processes, return values of this data type:<br/>
*   spawn/1,2,3,4
*   spawn_link/1,2,3,4
*   spawn_opt/4
**Learning notes: I used BIF spawn in earlier study blogs, but I did not realize pid is a data type of Erlang, I thought the return of these BIFs are kind of String.**


```
-module(tut18).
-export([loop/0]).

loop() ->
    receive
        who_are_you ->
            io:format("I am ~p~n", [self()]),
            loop()
    end.
```
**Test**

![](/img/erlang-10/pid-1.png)

<br/>

### Tuple

A tuple is a compound data type with a fixed number of terms.<br/>
Each term Term in the tuple is called an element. The number of elements is said to be the size of the tuple.<br/>
**Learning notes: I learn Tuple already, but this time I found there are difference of Tuple between Erlang and Python. <br/>
~~Tuple elements in Erlang can be modified, but the number of elements is fixed.~~(Test proves this understanding is wrong.)<br/>
Tuple elements in Python cannot be modified, of course the number of elements is fixed too.** 

```
tup1 = (12, 34.56)
tup1[0] = 100  # Illegal operation in Python.
```

```
1> P = { icy, 18, {Mar, 28}}.
* 1: variable 'Mar' is unbound
2> P = { icy, 18, {mar, 28}}.
{icy,18,{mar,28}}
3> setelement(2,P,19).
{icy,19,{mar,28}}
4> element(2,P).
18
5> P2 = setelement(2,P,19).
{icy,19,{mar,28}}
6> element(2,P2).
19
```

### Map
 
Maps are a set of key to value associations. These associations are encapsulated with "#{" and "}". <br/>
Only the => operator is allowed when creating a new map. <br/>
The syntax for updating an existing key with a new value is with the := operator. <br/>

**Example-1**

```
-module(tut5).
-export([score/3]).
-define(is_score(V), (is_integer(V) andalso V >= 0 andalso V =< 100)).

score(Java, Python, Erlang) when ?is_score(Java), ?is_score(Python), ?is_score(Erlang) ->
	#{java => Java, python => Python, erlang => Erlang}.
```

**Test**

![](/img/erlang-3/map-1.png)

**Example-2**

Guides provides an example shows how to calculate alpha blending using maps to reference color and alpha channels.<br/>
I follow the steps, code and test it, to practise the => operator, := operator and function definition.<br/>

```
-module(color).

-export([new/4,blend/2]).
-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
				  ?is_channel(B), ?is_channel(A) ->
	#{red => R, green => G, blue => B, alpha => A}.

blend(Src, Dst) ->
	blend(Src, Dst, alpha(Src,Dst)).

blend(Src, Dst, Alpha) when Alpha > 0.0 ->
	Dst#{
		 red	:=  red(Src, Dst) / Alpha,
		 green	:=	green(Src, Dst) / Alpha,
		 blue	:=	blue(Src, Dst) / Alpha,
		 alpha	:=	Alpha
		};

blend(_,Dst,_) ->
	Dst#{
		 red	:= 0.0,
		 green 	:= 0.0,
		 blue	:= 0.0,
		 alpha	:= 0.0
		}.

alpha(#{alpha := SA}, #{alpha := DA}) ->
    SA + DA*(1.0 - SA).

red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
```

**Test**

![](/img/erlang-3/map-2.png)

### Data Type Decide Method

According to list of data type decide method, we can also basically know all types in Erlang.<br/>

```
is_atom/1           
is_binary/1        
is_bitstring/1      
is_boolean/1        
is_builtin/3       
is_float/1          
is_function/1       is_function/2      
is_integer/1        
is_list/1           
is_number/1        
is_pid/1            
is_port/1           
is_record/2         is_record/3         
is_reference/1      
is_tuple/1
```

## Summary

&ensp;&ensp;&ensp;&ensp; From these simple examples, we practise some data type of Erlang.<br/>
&ensp;&ensp;&ensp;&ensp; Besides these types, we also learn the simple function definition.<br/>
&ensp;&ensp;&ensp;&ensp; Next time, I will learn more data types and other knowledge following the Guides.<br/>


## Reference
http://www.erlang.org <br/>
http://erlang.org/doc/getting_started/seq_prog.html#atoms <br/>
https://www.cnblogs.com/studynote/p/3218958.html 
