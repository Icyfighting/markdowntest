---
layout: post
title: "Golang says Hello World"
subtitle: "Install & Example "
author: "Bing Yan"
header-img: "img/golang-1/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Golang
  - Learning
---


## Preface

&ensp;&ensp;&ensp;&ensp; After trying Erlang for a while, I want to see what Golang looks like, because both languages are usually used to write system-based applications.<br/>
&ensp;&ensp;&ensp;&ensp; I learn and use Java and Python. But we know, they are both solving the problem of the business layer, belonging to the application language, to solve the business logic. <br/>
&ensp;&ensp;&ensp;&ensp; But there is another area that is the system domain, the partial network layer and the underlying operations.Consider language for system domain, C's development efficiency is too low, Java is more appropriate, it is too bloated, and lacks the gene for system programming. So Erlang and Golang are used.


## Text

### What's Golang

Go is an open source programming language that makes it easy to build simple, reliable, and efficient software.<br/>
And I like this logo. How can I say no to this little cute...<br/>

![](/img/golang-1/logo.png)

The Go language is designed to be a system programming language for large central servers with web servers, storage clusters or similar applications.<br/>

For the field of high-performance distributed systems, the Go language is undoubtedly more efficient than most other languages. It provides massive parallel support, which is great for game server development.<br/>

### Golang Feature

*   Simple, Fast and Safe
*   Parallel, Interesting, Open source
*   Memory management, Array security, Fast compilation

### Golang Installation

So easy, download a binary release suitable for your system in [Golang official](https://golang.org/dl/), install it follow [Getting Started](https://golang.org/doc/install).

After installation, write a 'Hello world' and execute it.

![](/img/golang-1/hello.png)


### Golang Program Structure

In Hello world example, we can find parts of Golang Structure:<br/>

```
package main

import "fmt"

func main() {
   /* This is my first example of Golang */
   fmt.Println("Hello, World!")
}
```

*   Package statement
>The first line of code 'package main' defines the package name. You must indicate in the first line of the source file that the file belongs to the first line, such as: 'package main'. Package main represents a program that can be executed independently, and each Go application contains a package called main.

*   Import package
>The next line 'import "fmt" ' tells the Go compiler that this program needs to use the fmt package (function, or other elements), and the fmt package implements a function that formats IO (input/output).

*   Function
>The next line 'func main()' is the function that the program starts executing. The main function is required for every executable. It is generally the first function to be executed after startup (if there is an init() function, it will be executed first).

*   Comments
>The next line /*...*/ is a comment and will be ignored during program execution. Single-line comments are the most common form of comments, and you can use single-line comments starting with // anywhere. Multi-line comments, also called block comments, have started with /* and end with */ and cannot be nested. Multi-line comments are generally used for package descriptions or coded blocks of code.

*   Statement & expression
>The next line fmt.Println(...) can output the string to the console and automatically add the newline character \n at the end.
Use fmt.Print("hello, world\n") to get the same result.
The Print and Println functions also support the use of variables such as: fmt.Println(arr). If not specified, they will output the variable arr to the console in the default print format.

*   Variable
>When identifiers (including constants, variables, types, function names, structure fields, etc.) start with an uppercase letter, such as: Group1, objects that use this form of identifier can be used by the code of the external package (client The terminal needs to import the package first. This is called export (like public in an object-oriented language); if the identifier starts with a lowercase letter, it is invisible to the outside of the package, but they are visible inside the package. And available (like protected in an object-oriented language).


## Summary

&ensp;&ensp;&ensp;&ensp; According to information I collected by now, Golang is easier to learn than Erlang for Python/C/Ruby programmers. Although Golang has enough depth, but engineers can reuse many of the existing knowledge. <br/>
&ensp;&ensp;&ensp;&ensp; Next step, I will try to learn Golang by practising examples, then I can have deeper feeling about Erlang and Golang.

## Reference
https://golang.org/ <br/>
http://www.runoob.com/go/go-tutorial.html <br/>
