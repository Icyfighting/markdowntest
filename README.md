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
Tuple elements in Python cannot be modified, of course the number of elements is fixed too. <br/>
The declaration of erlang:setelement/3: <br/>
setelement(Index, Tuple1, Value) -> Tuple2 <br/>
It return the copy of Tuple1, not Tuple1 itself. So I think it's more like use Tuple1 to generate Tuple2, but Tuple1 can not be modified.**

```
tup1 = (12, 34.56)
tup1[0] = 100  # Illegal operation in Python.
```

```
2> P = { icy, 18, {mar, 28}}.
{icy,18,{mar,28}}
3> setelement(2,P,19).     
{icy,19,{mar,28}}
4> element(2,P).    % element is not modified by setelement/3.
18
5> P2 = setelement(2,P,19).    % usage of setelement/3
{icy,19,{mar,28}}
6> element(2,P2).
19
```


### String
 
String is normal data type in Java and Python. But string is not a data type in Erlang.<br/>
In Erlang, string is a shorthand for list.<br/>
Two adjacent string literals are concatenated into one. This is done in the compilation, thus, does not incur any runtime overhead.<br/>

```
8> list_to_binary("hello").
<<"hello">>
9> "hello""world".
"helloworld"
```

### Boolean

There is no Boolean data type in Erlang. Instead the atoms true and false are used to denote Boolean values.

```
10> true or false.
true
11> true and false.
false
12> 1>2.
false
```


### Type Conversion

There are a number of BIFs for type conversions: <br/>
I consider list as string in these BIFs.<br/>

```
13> atom_to_list(erlang).
"erlang"
14> list_to_atom("python").
python
15> binary_to_list(<<"Java">>).
"Java"
16> binary_to_list(<<104,101,108,108,111>>).
"hello"
17> list_to_binary("hello").
<<"hello">>
18> float_to_list(1.0).
"1.00000000000000000000e+00"
19> list_to_float("1.0e+00").
1.0
20> integer_to_list(100).
"100"
21> list_to_integer("99").
99
22> tuple_to_list({a,b,c}).
[a,b,c]
23> list_to_tuple([1,2,3]).
{1,2,3}
24> integer_to_binary(88).
<<"88">>
26> binary_to_integer(<<"104">>).
104
```


## Summary

&ensp;&ensp;&ensp;&ensp; From these simple examples, we practise some data types and type conversion of Erlang.<br/>
&ensp;&ensp;&ensp;&ensp; During study, I found some misunderstanding of description in manual, the better way is coding and running examples.<br/>


## Reference
http://www.erlang.org <br/>
http://erlang.org/doc/reference_manual/data_types.html<br/>
