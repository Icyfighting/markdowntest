---
layout: post
title: "How to write high quality code"
subtitle: "Software development learning (1)"
author: "Bing Yan"
header-img: "img/high/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Software development
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp; As one who wants to be a good programmer, besides learning a rich and solid programming skills, I need to think about how to write high quality and elegant code.<br/>

&ensp;&ensp;&ensp;&ensp; To be frank, I haven't thought about this question before, because my understanding is that software development is a very programmed work, and there are many processes and management methods to ensure the quality of the code.<br/>

&ensp;&ensp;&ensp;&ensp; But in addition to relying on software development project management, it is also necessary to understand for the programmers ourselves, to strengthen the need to write high-quality code, and to master certain correct methods.<br/>

&ensp;&ensp;&ensp;&ensp; The purpose of this study is to sum up the answer to this question based on the experience of other programmers.


## Text

### What is Code Quality

Before we answer how, we have to know what does code quality include. <br/>
There are 5 aspects: <br/>

*   Coding specification
*   Code duplication
*   Code coverage
*   Dependency analysis
*   Complexity analysis

Let's look at these aspects one by one: <br/>

*   Coding specification
>In general, companies have a coding specification that stipulates class naming, package naming, code style and so on. The quality of this specification, and level that code compliance with specification, are both included.

*   Code duplication
>If there is a large amount of duplicate code in the code, consider whether to extract the duplicate code and package it into a common method or component.

*   Code coverage
> The ratio of test code that can run to code. <br/>
The code has been unit tested? Is every method tested? <br/>
What is the code coverage ratio? <br/>
This is related to the functionality and stability of the code.

*   Dependency analysis
>How about code dependencies? <br/>
What is the coupling relationship? <br/>
Is there a circular dependency?<br/>
Does it meet the principle of high cohesion and low coupling?

*   Complexity analysis
> If there are many layers of if else nested in the code, it will be hard to read. The more excellent the code, the easier it is to read. <br/>
As my understanding, time complexity and space complexity will also have a big impact on the efficiency of the program.<br/>
Optimize code and reduce time and space complexity is important for high quality code.

### How to write high quality code

Experienced programmers gave us a lot of suggestions, summarized as follow:

*   Thinking before action
In the actual software development cycle, the design time is usually not shorter than the encoding time. We should not rush to write the code first, but carefully analyze and design at the beginning. Rather than writing a loophole of code, it's better to analyze it carefully and write robust code.<br/>

*   Develop good coding habits
Developing a good habit is very important to our work. To develop a good habit requires us to cultivate from the very beginning, and persevere.<br/>

*   Coding follow specification
The code follows a uniform format specification, first of all to facilitate future maintenance, and secondly to facilitate the transfer of others. A good coding specification can reduce the maintenance cost of a software as much as possible. <br/>
A good coding specification can also improve the readability of the software. Not only will it be clear to the reader, but others will be easier to understand the new code, which will maximize the efficiency of team development cooperation. This is very important for a project team. <br/>

*   Write code comments
Software development is a coordination effort, and team members' communication becomes very important, so the code written by one person needs to be understood by other members of the entire team. <br/>
Moreover, with the rapid development of hardware devices, the readability of the program instead of the execution efficiency has become the first consideration. Program comments are an important part of the source code. For a standard program source code, comments should account for more than 30% of the source code.<br/>

*   Methods with limited arguments
When method arguments exceed 5, you should consider whether method design is reasonable. Do you really need so many arguments ? Can it be streamlined? Not only the difficulty of understanding is added, also increase risk of problem caused by arguments position. <br/>
If it is necessary, then when you have to change, you need to encapsulate the object to pass it. This not only reduces the number of parameters, but also provides the possibility of infinite expansion. At the same time, the user does not have to remember the order of the arguments.<br/>

*   Do not write duplicate code
Duplicate code is definitely the first feature of junk code and is the biggest feature. Copying and pasting is easy, but once it goes wrong, it means double the workload and continuous uncontrollable. For repeated functions, be sure to extract the method.<br/>

*   Properly placed code
Besides implementing functionality, it's important to place the code correctly. <br/>
Check methods to see if any logic should be placed in the method;  <br/>
Check the class to see if methods are placed in the correct class;  <br/>
Check the project to see if the classes are placed in correct project.<br/>

*   Think more for your users
If there is no user, there is no meaning of job, as does coding.<br/>
If you develop framework, then users are software developers; <br/>
if you develop projects, then your users are clients.<br/>
No matter what object you are facing, a good starting point is very important: think more for your users. In other words, customers come first.<br/>
Whether method overload in java or adapter in design pattern, it is all about this concept, so that your users are as simple as possible and do less.<br/>

*   Disassemble your code
One of the most important indicators for evaluating a code is: Whether it is easy to disassemble,means  its coupling.<br/>

*   Use of inspection tools
When code is finished, use some simple static checking tools, such as checkstyle to check the format of your code and some hidden vulnerabilities. In addition, you can do unit tests to make your code more robust.<br/>

*   Refactor your code
It can improve software design, makes the software easier to understand,helps to find bugs, increase programming speed.<br/>

*   Use open source
The quality of the open source projects with high degree of concern, high number of users and good reputation is guaranteed.<br/>
Even if there are some problems, you can continue to improve by submitting feedback. Most importantly, the time you build your own wheels requires a lot of energy to maintain, and taking advantage of open source projects can help you save a lot of time and focus on what you care most about.<br/>
On the other hand, some well-known projects in open source projects are often written by the leaders in the field. Learning the code of these masters will let you know what good code should be and cultivate a more sensitive sense of smell.<br/>

## Summary

&ensp;&ensp;&ensp;&ensp;After learning the theoretical knowledge of writing high quality code, it is important to cultivate and follow it in the daily coding work.<br/>
&ensp;&ensp;&ensp;&ensp;In the process of collecting data, I also find some more advanced and sophisticated ways. But I can't get away from reality, what I need to do now is to write the code well.<br/>
&ensp;&ensp;&ensp;&ensp; When I have more programming experience, I will continue to learn this knowledge.


