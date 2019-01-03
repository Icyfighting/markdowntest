---
layout: post
title: "Erlang learning (9) - Larger Example Devided into Files"
subtitle: "Records and Marcros"
author: "Bing Yan"
header-img: "img/erlang-9/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Basic Knowledge
  - Learning
---


## Preface

&ensp;&ensp;&ensp;&ensp;Let's continue to learn the next part of Getting Started with Erlang - Records and Macros. <br/>
&ensp;&ensp;&ensp;&ensp;In this chapter, I deeply feel the benefits of contrast learning. It will be easier to understand and remember the new concepts and uses encountered in Erlang, compared to Java, Python and other languages that have been learned.

## Text

### Example Divided into Files

Larger programs are usually written as a collection of files with a well-defined interface between the various parts. <br/>
The messenger example in [Erlang learning (7) - Concurrent Programming (3)](https://icyfighting.github.io/2018/12/28/erlang-concurrent-programming-3/) can be devided into files as below: <br/>

**These example files in User's Guide is example with good coding comments. This should be learned in my daily work.**

*   mess_config.hrl:  Header file for configuration data.<br/>
**Though in Java there is no header file, but the function of this header file is same as config.xml / .yml, realize soft coding.**
<br/>

```
%%%----FILE mess_config.hrl----

%%% Configure the location of the server node,
-define(server_node, messenger@super).

%%%----END FILE----
```

*   mess_interface.hrl: Interface definitions between the client and the messenger.<br/>
**Records are included in this .hrl file. After learning the record, I think function of this header file like Java entity definition.
According to coding principle, when there are many arguments passing between functions, it should be packaged in a structure. In Java, we use entity class, in Erlang it seems can use record. Of course, there are other data types can hold information, such as tuple. The comparison between them is in next part.**
<br/>

```
%%----FILE mess_interface.hrl----

%%% Message interface between client and server and client shell for
%%% messenger program 

%%%Messages from Client to server received in server/1 function.
-record(logon,{client_pid, username}).
-record(message,{client_pid, to_name, message}).
%%% {'EXIT', ClientPid, Reason}  (client terminated or unreachable.

%%% Messages from Server to Client, received in await_result/0 function 
-record(abort_client,{message}).
%%% Messages are: user_exists_at_other_node, 
%%%               you_are_not_logged_on
-record(server_reply,{message}).
%%% Messages are: logged_on
%%%               receiver_not_found
%%%               sent  (Message has been sent (no guarantee)
%%% Messages from Server to Client received in client/1 function
-record(message_from,{from_name, message}).

%%% Messages from shell to Client received in client/1 function
%%% spawn(mess_client, client, [server_node(), Name])
-record(message_to,{to_name, message}).
%%% logoff

%%%----END FILE----
```
*   user_interface.erl: Functions for the user interface <br/>
**This files includes function provided to clients. In earlier Blog [How to write high quality code](https://icyfighting.github.io/2018/12/30/high-quality-code/), there is one principle "Think more for your users" can be used in this file. The function provided to user should be as simple as possible.**
<br/>

```
%%%----FILE user_interface.erl----

%%% User interface to the messenger program
%%% login(Name)
%%%     One user at a time can log in from each Erlang node in the
%%%     system messenger: and choose a suitable Name. If the Name
%%%     is already logged in at another node or if someone else is
%%%     already logged in at the same node, login will be rejected
%%%     with a suitable error message.

%%% logoff()
%%%     Logs off anybody at that node

%%% message(ToName, Message)
%%%     sends Message to ToName. Error messages if the user of this 
%%%     function is not logged on or if ToName is not logged on at
%%%     any node.

-module(user_interface).
-export([logon/1, logoff/0, message/2]).
-include("mess_interface.hrl").
-include("mess_config.hrl").

logon(Name) ->
    case whereis(mess_client) of 
        undefined ->
            register(mess_client, 
                     spawn(mess_client, client, [?server_node, Name]));
        _ -> already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of % Test if the client is running
        undefined ->
            not_logged_on;
        _ -> mess_client ! #message_to{to_name=ToName, message=Message},
             ok
end.

%%%----END FILE----
```

*   mess_client.erl: Functions for the client side of the messenger


```
%%%----FILE mess_client.erl----

%%% The client process which runs on each user node

-module(mess_client).
-export([client/2]).
-include("mess_interface.hrl").

client(Server_Node, Name) ->
    {messenger, Server_Node} ! #logon{client_pid=self(), username=Name},
    await_result(),
    client(Server_Node).

client(Server_Node) ->
    receive
        logoff ->
            exit(normal);
        #message_to{to_name=ToName, message=Message} ->
            {messenger, Server_Node} ! 
                #message{client_pid=self(), to_name=ToName, message=Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_Node).

%%% wait for a response from the server
await_result() ->
    receive
        #abort_client{message=Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        #server_reply{message=What} ->
            io:format("~p~n", [What])
    after 5000 ->
            io:format("No response from server~n", []),
            exit(timeout)
    end.

%%%----END FILE---
```

*   mess_server.erl: Functions for the server side of the messenger

```
%%%----FILE mess_server.erl----

%%% This is the server process of the messenger service

-module(mess_server).
-export([start_server/0, server/0]).
-include("mess_interface.hrl").

server() ->
    process_flag(trap_exit, true),
    server([]).

%%% the user list has the format [{ClientPid1, Name1},{ClientPid22, Name2},...]
server(User_List) ->
    io:format("User list = ~p~n", [User_List]),
    receive
        #logon{client_pid=From, username=Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {'EXIT', From, _} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        #message{client_pid=From, to_name=To, message=Message} ->
            server_transfer(From, To, Message, User_List),
            server(User_List)
    end.

%%% Start the server
start_server() ->
    register(messenger, spawn(?MODULE, server, [])).

%%% Server adds a new user to the user list
server_logon(From, Name, User_List) ->
    %% check if logged on anywhere else
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! #abort_client{message=user_exists_at_other_node},
            User_List;
        false ->
            From ! #server_reply{message=logged_on},
            link(From),
            [{From, Name} | User_List]        %add user to the list
    end.

%%% Server deletes a user from the user list
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

%%% Server transfers a message between user
server_transfer(From, To, Message, User_List) ->
    %% check that the user is logged on and who he is
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! #abort_client{message=you_are_not_logged_on};
        {value, {_, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.
%%% If the user exists, send the message
server_transfer(From, Name, To, Message, User_List) ->
    %% Find the receiver and send the message
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! #server_reply{message=receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! #message_from{from_name=Name, message=Message}, 
            From !  #server_reply{message=sent} 
    end.

%%%----END FILE---
```

### Knowledge Point

#### Header Files

Files have extension .hrl. These are header files that are included in the .erl files by:  <br/>

```
-include("File_Name").
```
In the case above the file is fetched from the same directory as all the other files in the messenger example.<br/>
.hrl files can contain any valid Erlang code but are most often used for record and macro definitions.<br/>

#### Records

**Records vs Tuples**

The main advantage of using records rather than tuples is that fields in a record are accessed by name, whereas fields in a tuple are accessed by position.
For example, to extract data from a variable P that contains such a tuple, you can write the following code and then use pattern matching to extract the relevant fields:

```
Name = element(1, P),
Address = element(2, P),
...
```
Such code is difficult to read and understand, and errors occur if the numbering of the elements in the tuple is wrong. If the data representation of the fields is changed, by re-ordering, adding, or removing fields, all references to the person tuple must be checked and possibly modified.
Records allow references to the fields by name, instead of by position. In the following example, a record instead of a tuple is used to store the data:

```
-record(person, {name, phone, address}).
```
This enables references to the fields of the record by name. For example, if P is a variable whose value is a person record, the following code access the name and address fields of the records:

```
Name = P#person.name,
Address = P#person.address,
...
```

**Have to say, all programming language have some things in common. For the reason of record instead of tuple here, Python use keyword arguments and Java Mybatis prefers arguments name than position. They all realize easy read and understand, avoid error caused by wrong arguments passing.**


**Define a Record**

Define a person record. Three fields are included, name, phone, and address. The default values for name and phone is "" and [], respectively. The default value for address is the atom undefined, since no default value is supplied for this field: <br/>

```
-record(person, {name = "", phone = [], address}).
```

**To easier understand, I consider this record definition as Java bean, though by now I did not find similar get/set method. But I can access a record field and update a record, that means this record must provide some method like get/set.**

**Create a Record**

A new person record is created as follows:<br/>

```
> #person{phone=[0,8,2,3,4,3,1,2], name="Robert"}.
#person{name = "Robert",phone = [0,8,2,3,4,3,1,2],address = undefined}
```
As the address field was omitted, its default value is used.

**If consider define a record as define a Java class, create a record is more like new a instance of class. Speaking of instance, it seems I need learn memory management of Erlang. <br/>
Since by learning Java and Python, I find same memory management principle, but they still have differnt points. And Erlang must have special memory management mechanism to support high concurrency and strict time requirements in telecommunication system.<br/>
When I search information about Erlang memory management, I found resource blog with [speech of Lukas Larsson](https://www.cnblogs.com/zhengsyao/p/erts_allocators_speech_by_lukas_larsson.html), one of Erlang core developers.**

**Access a Record Field**

The following example shows how to access a record field: <br/>

```
> P = #person{name = "Joe", phone = [0,8,2,3,4,3,1,2]}.
#person{name = "Joe",phone = [0,8,2,3,4,3,1,2],address = undefined}
> P#person.name.
"Joe"
```

**Update a Record**

The following example shows how to update a record: <br/>

```
> P1 = #person{name="Joe", phone=[1,2,3], address="A street"}.
#person{name = "Joe",phone = [1,2,3],address = "A street"}
> P2 = P1#person{name="Robert"}.
#person{name = "Robert",phone = [1,2,3],address = "A street"}
```

**Nested Records**

The value of a field in a record can be an instance of a record. Retrieval of nested data can be done stepwise, or in a single step, as shown in the following example:<br/>
demo() evaluates to "Robert".<br/>

```
-record(name, {first = "Robert", last = "Ericsson"}).
-record(person, {name = #name{}, phone}).

demo() ->
  P = #person{name= #name{first="Robert",last="Virding"}, phone=123},
  First = (P#person.name)#name.first.
```


#### Macros

As I said above, I think Erlang Macros is configuration file in Java. This can realize soft coding. But not only this.<br/>
Searching information said there are different kinds of Macros and ways to use:<br/>
*   Define a macor:<br/>
```
-define(Const, Replacement).
-define(Func(Var1,...,VarN), Replacement).
```
A macro definition can be placed anywhere among the attributes and function declarations of a module, but the definition must come before any usage of the macro.<br/>

*   Use a macor:<br/>

```
?Const
?Func(Arg1,...,ArgN)
```

**Standard Macross**

```
spawn(?MODULE, server, [])
```
This is a standard macro (that is, defined by the system, not by the user). ?MODULE is always replaced by the name of the current module (that is, the -module definition near the start of the file). There are more advanced ways of using macros with, for example, parameters.

**Customize Macros**

The file mess_config.hrl contains the definition: <br/>

```
-define(server_node, messenger@super).
```
This file is included in mess_server.erl:<br/>

```
-include("mess_config.hrl").
```
Every occurrence of ?server_node in mess_server.erl is now replaced by messenger@super.


## Summary

&ensp;&ensp;&ensp;&ensp; This study is mainly to follow Chapter Getting Started with Erlang in User's Guide.<br/>
&ensp;&ensp;&ensp;&ensp; Each simple example involve several knowledge points, but for details, we have to search in manual or web.<br/>
&ensp;&ensp;&ensp;&ensp; During learning Erlang, I always try to think about the same and different points with Java and Python. And this way make it earier to understand and remember.


## Reference
http://www.erlang.org <br/>
http://erlang.org/doc/getting_started/record_macros.html <br/>
