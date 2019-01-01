---
layout: post
title: "Erlang learning (4) - Sequential Programming"
subtitle: "Module & Guard & BIFs & High-Order Func "
author: "Bing Yan"
header-img: "img/erlang-4/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp;This study is based on the chapter of the Guide - Sequential Programming. This chapter is dedicated to the reader's basic knowledge of Erlang programming by writing and modifying simple examples.
I quickly read and practice these examples, which gives me an intuitive understanding of how Erlang handles iterable data, such as List. And compares with Java, Python that I have learned before.

## Text

### Example-v1

**Knowledge Points:**

*   io:format function: 
> Module io: io - Standard I/O Server Interface Functions.<br/>
The function format/2 (that is, format with two arguments) takes two lists. The first one is nearly always a list written between " ". This list is printed out as it is, except that each ~w is replaced by a term taken in order from the second list. Each ~n is replaced by a new line. The io:format/2 function itself returns the atom ok if everything goes as planned. Like other functions in Erlang, it crashes if an error occurs. 

*   comment: 
>A comment starts with a %-character and goes on to the end of the line.

*   export: 
>The -export([format_temps/1]). line only includes the function format_temps/1. The other functions are local functions, that is, they are not visible from outside the module tut6.

*   Nested:
>Here is a function call as convert_to_celsius({moscow,{c,-10}}) as the argument to the function print_temp. When function calls are nested like this, they execute (evaluate) from the inside out. That is, first convert_to_celsius({moscow,{c,-10}}) is evaluated, which gives the value {moscow,{c,-10}} as the temperature is already in Celsius. Then print_temp({moscow,{c,-10}}) is evaluated. 

*   Recursion:
>format_temps(Rest) is called with the rest of the list as an argument. This way of doing things is similar to the loop constructs in other languages.So the same format_temps function is called again, this time City gets the value {cape_town,{f,70}} and the same procedure is repeated as before. This is done until the list becomes empty, that is [], which causes the first clause format_temps([]) to match.<br/>
As my understanding by now, handling iterable data as list, Erlang uses a recursive-like approach that implements a loop traversal of the list. Later, foreach and map functions are introduced for iterable data handling.


```
-module(tut6).
-export([format_temps/1]).

%% Only this function is exported
format_temps([])->                        % No output for an empty list
    ok;
format_temps([City | Rest]) ->			  % By now, traversing list use |, and recursive call
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) ->  % Atom is used as matching. Variables get corresponding values in tuple.
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->  % Do the conversion
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]). % Module io
```

**Test**

![](/img/erlang-4/example-1.png)

### Example-v2

**Knowledge Points:**

*   if:
```
if
    Condition 1 ->
        Action 1;
    Condition 2 ->
        Action 2;
    Condition 3 ->
        Action 3;
    Condition 4 ->
        Action 4
end
```
>Notice that there is no ";" before end. Conditions do the same as guards, that is, tests that succeed or fail. Erlang starts at the top and tests until it finds a condition that succeeds. Then it evaluates (performs) the action following the condition and ignores all other conditions and actions before the end. If no condition matches, a run-time failure occurs. A condition that always succeeds is the atom true. This is often used last in an if, meaning, do the action following the true if all other conditions have failed.

*   lists:
> The module lists contains many functions for manipulating lists, for example, for reversing them. So before writing a list-manipulating function it is a good idea to check if one not already is written for you.
In below example, to find the max and min value list, can use lists:max/1, lists:min/1, lists:sort/2 and so on.

```
-module(tut7).
-export([format_temps/1]).

format_temps(List_of_cities) ->
    Converted_List = convert_list_to_c(List_of_cities),    
    print_temp(Converted_List),
    {Max_city, Min_city} = find_max_and_min(Converted_List),
    print_max_and_min(Max_city, Min_city).

convert_list_to_c([{Name, {f, Temp}} | Rest]) ->        % Convert all temperature in list to Celsius
    Converted_City = {Name, {c, (Temp -32)* 5 / 9}},
    [Converted_City | convert_list_to_c(Rest)];

convert_list_to_c([City | Rest]) ->
    [City | convert_list_to_c(Rest)];

convert_list_to_c([]) ->                                % Boundary conditions of recursive-like function
    [].

print_temp([{Name, {c, Temp}} | Rest]) ->		% print list of city with recursive-like function
    io:format("~-15w ~w c~n", [Name, Temp]),
    print_temp(Rest);
print_temp([]) ->					% Boundary conditions
    ok.

find_max_and_min([City | Rest]) ->
    find_max_and_min(Rest, City, City).

find_max_and_min([{Name, {c, Temp}} | Rest], 
         {Max_Name, {c, Max_Temp}}, 
         {Min_Name, {c, Min_Temp}}) ->
    if 
        Temp > Max_Temp ->
            Max_City = {Name, {c, Temp}};           % Change
        true -> 					% similar as 'else' in Java, make sure all conditions have matching actions.
            Max_City = {Max_Name, {c, Max_Temp}} 	% Unchanged
    end,
    if
         Temp < Min_Temp ->
            Min_City = {Name, {c, Temp}};           % Change
        true -> 
            Min_City = {Min_Name, {c, Min_Temp}} 	% Unchanged
    end,
    find_max_and_min(Rest, Max_City, Min_City);		% Recursive call

find_max_and_min([], Max_City, Min_City) ->
    {Max_City, Min_City}.

print_max_and_min({Max_name, {c, Max_temp}}, {Min_name, {c, Min_temp}}) ->
    io:format("Max temperature was ~w c in ~w~n", [Max_temp, Max_name]),
    io:format("Min temperature was ~w c in ~w~n", [Min_temp, Min_name]).
```

**Test**

![](/img/erlang-4/example-2.png)

### Example-v3

**Knowledge Points:**

*   anonymous variable:
> In sort the fun is used: fun({_, {c, Temp1}}, {_, {c, Temp2}}) -> Temp1 < Temp2 end .
This is simply shorthand for a variable that gets a value, but the value is ignored. This can be used anywhere suitable,

*   lists:
>The standard module lists also contains a function sort(Fun, List) where Fun is a fun with two arguments.  <br/>
This fun returns true if the first argument is less than the second argument, or else false. <br/>
Sorting is added to the convert_list_to_c .

*   foreach and map:
>These two functions are provided in the standard module lists. foreach takes a list and applies a fun to every element in the list. map creates a new list by applying a fun to every element in a list.

```
-module(tut8).

-export([convert_list_to_c/1]).

convert_to_c({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_c({Name, {c, Temp}}) ->
    {Name, {c, Temp}}.

convert_list_to_c(List) ->
    New_list = lists:map(fun convert_to_c/1, List),
    lists:sort(fun({_, {c, Temp1}}, {_, {c, Temp2}}) ->
                       Temp1 < Temp2 end, New_list).
```

**Test**

![](/img/erlang-4/example-3.png)


## Summary

&ensp;&ensp;&ensp;&ensp; From these simple examples, I practise some data type and modules of Erlang.<br/>
&ensp;&ensp;&ensp;&ensp; Next time, I will start to learn Concurrent Programming chapter following the Guides.<br/>
&ensp;&ensp;&ensp;&ensp; One of the main reasons for using Erlang instead of other functional languages is Erlang's ability to handle concurrency and distributed programming. So next chapter will be very interesting.
## Reference
http://www.erlang.org <br/>
http://erlang.org/doc/getting_started/seq_prog.html <br/>
