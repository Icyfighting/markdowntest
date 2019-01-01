---
layout: post
title: " Erlang says Hello World!"
subtitle: "Erlang learning (2) - installation and erlide"
author: "Bing Yan"
header-img: "img/erlang-2/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Erlang
  - Installation
  - Erlide
  - Learning
---

## Preface

&ensp;&ensp;&ensp;&ensp;In the last study, I had a preliminary understanding of erlang. At the beginning of this study, let us install the erlang environment, install the erlang development plugin, and run the first erlang program - Hello World!<br/>
&ensp;&ensp;&ensp;&ensp;For a new language, it is important to master the correct learning method and do more with less. This learning method includes: check the official Guide document and learn by doing.

## Text

### Erlang Installation

1. Find the official **[website](http://www.erlang.org/downloads)** of Erlang and download the latest installation package.
<br/>

![](/img/erlang-2/download-1.png)

2. Install Erlang by running .exe file -> select components -> Next -> Installation Complete.<br/>
During installation, there is prompt about Microsoft Visual C++，Redistributable package, I selected 'Repair'.<br/>
<br/>

![](/img/erlang-2/env-1.png)

3. Configure environment variables. Similar as other env configuration - add Erlang bin path in env 'Path'.<br/>
Run 'erl' in cmd, confirm installation and env configuration are correct.<br/>
<br/>

![](/img/erlang-2/erl-version.png)


### Erlide Installation

Erlide is an IDE for Erlang, powered by Eclipse.<br/>
Erlide official **[website](https://erlide.org/)** <br/>

Find **[link](https://erlide.org/articles/eclipse/120_Installing-and-updating.html)** of Erlide Installing and updating.<br/>
My Eclipse is Release 4.7.0 (Oxygen), later than required version 4.6. The Erlide installation is OK.<br/>
After Erlide installation, Eclipse can new Erlang project.
<br/>

![](/img/erlang-2/erlide.png)


### First Erlang Project 

According to programming conventions, the first project of Erlang also outputs 'Hello World'. <br/>

1. New project: File -> New -> Project -> Erlang -> Erlang Project -> Name: first_erlang_project
<br/>
2. New module: Right click on src of first_erlang_project -> new module -> Module name: tut
<br/>
3. Edit tut.erl file as below:
<br/>

![](/img/erlang-2/tut.png)

<br/>
4. Right click on first_erlang_project -> run as -> run configurations <br/>
Right click on Erlang application on the left -> new <br/>
Select first_erlang_project on main page on the right. <br/>
<br/>

![](/img/erlang-2/run-as-2.png)

<br/>
Select runtimes page -> run
<br/>

![](/img/erlang-2/run-as-1.png)

<br/>

5. In Console, input 'test:say()'
<br/>

![](/img/erlang-2/result.png)

<br/>


## Summary

&ensp;&ensp;&ensp;&ensp;Erlang's runtime and development environment is installed, and I can start learning Erlang while doing it. <br/>
&ensp;&ensp;&ensp;&ensp;Hello World will write, I feel that I have succeeded a big step.


## Reference
http://www.erlang.org <br/>
https://erlide.org/articles/eclipse/index.html <br/>
https://erlide.org/articles/eclipse/120_Installing-and-updating.html <br/>
https://weiqingfei.iteye.com/blog/264684 <br/>
https://www.cnblogs.com/minily/p/7398445.html <br/>
