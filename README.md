---
layout: post
title: "Erlang learning (11) - Functions"
subtitle: "Functions & Tail recursion"
author: "Bing Yan"
header-img: "img/erlang-11/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp;Although we have used functions, modules, etc. in the previous start chapter learning, but for the specific details, let's further clear in this study.<br/>
&ensp;&ensp;&ensp;&ensp;And in this study, get more information and understanding of tail recursion in Erlang.<br/>

## Text


### Function Declaration

*   A function declaration is a sequence of function clauses separated by semicolons, and terminated by period (.).
*   A function clause consists of a clause head and a clause body, separated by ->.
*   A clause head consists of the function name, an argument list, and an optional guard sequence beginning with the keyword when:
*   The function name is an atom. Each argument is a pattern.
*   The number of arguments N is the arity of the function. A function is uniquely defined by the module name, function name, and arity. That is, two functions with the same name and in the same module, but with different arities are two different functions.A function named f in the module m and with arity N is often denoted as m:f/N.
*   A clause body consists of a sequence of expressions separated by comma (,):

```
fact(N) when N>0 -> 
    N * fact(N-1);   

fact(0) -> 
    1.             
```

### Function Evaluation

When a function m:f/N is called, first the code for the function is located. If the function cannot be found, an undef runtime error occurs. Notice that the function must be exported to be visible outside the module it is defined in.<br/>
If the function is found, the function clauses are scanned sequentially until a clause is found that fulfills both of the following two conditions:<br/>
*   The patterns in the clause head can be successfully matched against the given arguments.
*   The guard sequence, if any, is true.

If such a clause cannot be found, a function_clause runtime error occurs.<br/>
If such a clause is found, the corresponding clause body is evaluated. That is, the expressions in the body are evaluated sequentially and the value of the last expression is returned.<br/>

To better understand the function evaluation, we need firstly review the pattern matching.<br/>
>Variables are bound to values through the pattern matching mechanism. Pattern matching occurs when evaluating a function call, case- receive- try- expressions and match operator (=) expressions.<br/>
In a pattern matching, a left-hand side pattern is matched against a right-hand side term. If the matching succeeds, any unbound variables in the pattern become bound. If the matching fails, a run-time error occurs.

**Learning notes: pattern matching is the most interesting point when I met in start chapter.<br/>
It use one expression to realize conditional matching and variables values bounding.<br/>
Pattern matching makes Erlang function declaration more simple.
**

**Example**

```
-module(m).
-export([fact/1]).

fact(N) when N>0 ->
    N * fact(N-1);
fact(0) ->
    1.
```
1> m:fact(1).

Function evaluation steps:<br/>
*   Evaluation starts at the first clause. The pattern N is matched against argument 1. The matching succeeds and the guard (N>0) is true, thus N is bound to 1, and the corresponding body is evaluated:<br/>
N * fact(N-1) => (N is bound to 1)<br/>
1 * fact(0)<br/>
*   Now, fact(0) is called, and the function clauses are scanned sequentially again. First, the pattern N is matched against 0. The matching succeeds, but the guard (N>0) is false. Second, the pattern 0 is matched against 0. The matching succeeds and the body is evaluated:<br/>
1 * fact(0) =><br/>
1 * 1 =><br/>
1<br/>
*   If m:fact/1 is called with a negative number as argument, no clause head matches. A function_clause runtime error occurs.


### Built-In Functions (BIFs)

BIFs are implemented in C code in the runtime system. BIFs do things that are difficult or impossible to implement in Erlang. Most of the BIFs belong to the module erlang but there are also BIFs belonging to a few other modules, for example lists and ets.<br/>
The most commonly used BIFs belonging to erlang(3) are auto-imported. They do not need to be prefixed with the module name. Which BIFs that are auto-imported is specified in the erlang(3) module in ERTS. For example, standard-type conversion BIFs like atom_to_list and BIFs allowed in guards can be called without specifying the module name.<br/>



### Tail recursion

If the last expression of a function body is a function call, a tail recursive call is done. This is to ensure that no system resources, for example, call stack, are consumed. This means that an infinite loop can be done if it uses tail-recursive calls.<br/>

**Example**
Use tail recursion to realize factorial.<br/>
Compare the difference between tut19:fact/2 and examples in Function Evaluation.<br/>

```
-module(tut19).
-export([fact/2]).

fact(N,_) when N<0 ->
    0;
fact(0,_) ->
	1;
fact(1,A) ->
	A;
fact(N,A) ->
    fact(N-1,N*A).
```

![](/img/erlang-11/tail.png)

Erlang is functional language, and also the stack language, if we want to maintain the high concurrency of the language, we must replace the traditional recursion with tail recursion.

**Learning notes: there is another chapter Efficiency Guide -Myth: Tail-Recursive Functions are Much Faster Than Recursive Functions.<br/>
The reason why tail recursion is better recommended than ordinary recursion is that tail recursion does not need to save a lot of intermediate function stacks. Every time recursion, the corresponding result is calculated. The function call appears at the end of the caller function. Because it is the tail, there is no such thing. It is necessary to save any local variables.<br/>
In Java learning, we know that the function that can realized by recursion almost can realzied by loop. Choosing recursion or loop is based on pressure on the stack. <br/>
Just when I think that tail recursion can totally replace body recursion, a different discussion is given in article [Erlang's Tail Recursion is Not a Silver Bullet](https://ferd.ca/erlang-s-tail-recursion-is-not-a-silver-bullet.html). <br/>
By now I still cannot tell which opinion is better. **


## Summary

&ensp;&ensp;&ensp;&ensp; This study focus on the function declaration and evaluation process of Erlang.Also include thinking of difference between tail recursion and body recursion.<br/>

## Reference
http://www.erlang.org <br/>
http://erlang.org/doc/reference_manual/functions.html<br/>
http://erlang.org/doc/efficiency_guide/introduction.html <br/>
