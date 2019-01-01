---
layout: post
title: "Erlang learning (3) - Data Types (1)"
subtitle: "Atom & Tuple & List & Map"
author: "Bing Yan"
header-img: "img/erlang-3/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Data Type
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp;Erlang can be said to be very different from the language I have been in contact with before. This can be seen from its type definition. It is not familiar.<br/>
&ensp;&ensp;&ensp;&ensp;However, existence is reasonable. I think Erlang has great advantages in concurrent, lightweight processes, convenient data processing and fault tolerance, and may also be related to these data types.<br/>

## Text

### Erlang Data Types Features

*   Erlang does not define the type of a variable, it can be assigned to any type of value, and all types of values in Erlang are collectively referred to as a Term. <br/>
This makes the compiler not find a mismatch of numeric types at compile time, only errors are found at runtime, the advantage is that unlike C++, you can blind the system by forcing a type conversion, causing memory leaks. <br/>
However, Erlang now has a set of types, function definition mechanism, can use the dialyzer to find the mismatch in the code.<br/>

*   Variables in Erlang can only be assigned once, and the second assignment will be interpreted by the compiler as a comparison. If the value is the same, the value will be returned. If it is different, an exception will be thrown. <br/>

In the examples of **[Getting started with Erlang User's Guide](http://erlang.org/doc/getting_started/users_guide.html)**, new variable names are used, instead of reusing the old ones: First, TheRest, E1, E2, R, A, B, and C. The reason for this is that a variable can only be given a value once in its context (scope). 

*   Variables must start with a capital letter. Examples of variables are Number, ShoeSize, and Age, otherwise they will not be interpreted by the compiler as variables, and the variables starting with uppercase letters and underscores will behave differently.<br/>
More details of variable are included in manual **[Variables](http://erlang.org/doc/reference_manual/expressions.html#variables)**.

### Atom

Atom is data type in Erlang. Atoms start with a small letter, for example, charles, centimeter, and inch. <br/>
Atoms are simply names, nothing else. <br/>
They are not like variables, which can have a value.

**Example**

```
-module(tut2).
-export([convert/2]).

convert(M, inch) ->
	M / 2.54;

convert(N, centimeter) ->
	N * 2.54.
```

**Test**

<br/>

![](/img/erlang-3/atom-1.png)

<br/>
Let us see what happens if something other than centimeter or inch is entered in the convert function:<br/>

<br/>

![](/img/erlang-3/atom-2.png)

<br/>
The two parts of the convert function are called its **clauses** . <br/>
As shown, meter is not part of either of the clauses. The Erlang system cannot match either of the clauses so an error message function_clause is returned. 

### Tuple

**Example**

```
-module(tut3).
-export([convert_length/1]).

convert_length({centimeter, X}) ->
	{inch, X / 2.54};
convert_length({inch, Y}) ->
	{centimeter, Y * 2.54}.
```

**Test**

<br/>

![](/img/erlang-3/tuple-1.png)

<br/>

Tuples can have more than two parts, in fact as many parts as you want, and contain any valid Erlang term. <br/>
Tuples have a fixed number of items in them. Each item in a tuple is called an element. 

### List

Lists in Erlang are surrounded by square brackets, "[" and "]". For example, a list of the scores can be: <br/>

```
[{java,90},{python,85},{erlang,95}]
```
A useful way of looking at parts of lists, is by using "|". This is best explained by an example using the shell: <br/>

```
1> [E1,E2 | Rest] = [97,98,99,100,101].
"abcde"
2> E1.
97
3> E2.
98
4> Rest.
"cde"
```
<br/>
Erlang does not have a string data type. Instead, strings can be represented by lists of Unicode characters. The Erlang shell is "clever" and guesses what list you mean and outputs it in what it thinks is the most appropriate form. See above example.<br/>

**Example**

Using "|", we can realize how to find the length of a list. <br/>

```
-module(tut4).
-export([list_length/1]).

list_length([]) ->
	0;
list_length([First | Rest]) ->
	1 + list_length(Rest).
```

**Test**

<br/>

![](/img/erlang-3/list-1.png)

<br/>

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

<br/>

![](/img/erlang-3/map-1.png)

<br/>

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
