---
layout: post
title: "Java memory management learning (1)"
subtitle: "Stack & Heap & Method Area"
author: "Bing Yan"
header-img: "img/post-bg-java.jpg"
header-mask: 0.2
tags:
  - Java
  - Learning
---

为什么学习java内存管理？

首先我们要了解我们为什么要学习java虚拟机的内存管理，不是java的gc垃圾回收机制都帮我们释放了内存了吗？但是在写程序的过程中却也往往因为不懂内存管理而造成了一些不容易察觉到的内存问题，
并且在内存问题出现的时候，也不能很快的定位并解决。因此，了解并掌握Java的内存管理是我们必须要做的是事，也只有这样才能写出更好的程序，更好地优化程序的性能。

Java虚拟机在执行Java程序的过程中会把它所管理的内存划分为若干不同的数据区域，这些区域都有各自的用途以及创建和销毁的时间。Java虚拟机所管理的内存将会包括以下几个运行时数据区域，如下图所示：

![](/img/java-mem-1.jpg)

最重要的是了解栈内存（Stack）和堆内存（Heap）和方法区（Method Area）这三部分：
**Java栈(Stack):**
>在栈内存中保存的是堆内存空间的访问地址。Java栈是Java方法执行的内存模型每个方法在执行的同时都会创建一个栈帧的用于存储局部变量表、操作数栈、动态链接、方法出口等信息。
每个方法从调用直至执行完成的过程就对应着一个栈帧在虚拟机中入栈和出栈的过程。

**Java堆(Heap):**
>堆内存用来存放由new创建的对象实例和数组。Java堆是所有线程共享的一块内存区域，在虚拟机启动时创建，此内存区域的唯一目的就是存放对象实例 。Java堆是垃圾收集器管理的主要区域。Java堆可以处于物理上不连续的内存空间，只要逻辑上连续的即可。
在实现上，既可以实现固定大小的，也可以是扩展的。如果堆中没有内存完成实例分配，并且堆也无法完成扩展时，将会抛出OutOfMemoryError异常。

**方法区(Method Area):**
>方法区是各个线程共享的内存区域，它用于存储已被虚拟机加载的类信息、常量、静态变量、即时编译器编译后的代码等数据。相对而言，垃圾收集行为在这个区域比较少出现，但并非数据进了方法区就永久的存在了，这个区域的内存回收目标主要是针对常量池的回收和对类型的卸载，当方法区无法满足内存分配需要时，将抛出OutOfMemoryError异常。
运行时常量池：是方法区的一部分，它用于存放编译期生成的各种字面量和符号引用。**






当我们（企业、用户）需要 web 平台承载包括视频、游戏在内的各种富交互内容而 web 平台本身还不具备这样的能力时，我们通过给予这个平台一种新的格式，以满足大家的需求，这就是 Flash Player，作为一种私有平台与浏览器插件，却能一度成为 web 事实标准的客观原因。

而时至今日，这些 web 平台所欠缺的能力，在得到市场与社区的认可之后，逐渐被从 Flash 中吸收与扬弃，成为了诸如 HTML5 Video/Audio/Canvas、WebGL 这些真正的 Open Web 标准。这时候，这些在诞生之初颇为创新的，作为了一种「过渡手段」、「Shim」的私有平台，便自然而然的，慢慢的不再被需要了。

**这并不应该理解为一种失败，而应该说，它们「功成身退」了。**

<br/>

ActionScript 3.0，Flash 中的御用编程语言，作为 ES4 的唯一实现，[推动了 ECMAScript 标准的发展，深远得影响着现代 JavaScript](https://www.zhihu.com/question/49170215/answer/114640341)；

Adobe Flex，Flash 平台的企业开发框架，在今年和 [@徐飞](https://www.zhihu.com/people/sharpmaster) 老师聊到时，还一起怀念并认可其相比现代 web 前端/客户端开发在工具链、协作、兼容性、UI 组件等方面的先进与成熟；
Adobe AIR，作为最早借鉴 JRT 将 web 相关技术的 Runtime 植入操作系统或捆绑在可执行文件内的跨平台开发方案，或许可以视作 Cordova、Electron、NodeWebkit、ReactNative 这些方案的一个前身与成功先例；

Microsoft IE 私有技术 ActiveX 中的 XMLHTTP，作为 XMLHTTPRequest 的前身，促进了 Ajax 的诞生与 Web 2.0 时代的来临；

Google Gears 作为 2008 年时为了增强 web 应用的浏览器插件，其私有 API 分别是 App Cache、Web Worker、WebSQL 等标准或标准未遂的前身；

Cordova/Phonegap 作为第一个面向移动端的 Hybrid 方案，成为了 web 开发与移动设备的 polyfill 与桥梁，加速了 Web 平台 Device APIs 的发展，并与 WebOS、FirefoxOS、Chrome Apps、Windows Runtime Apps 等一同影响了 Progressive Web App 的出现；

Google Extension 中 Background Page 与 Event Page 多年对 web 平台后台持续计算的尝试，直接帮助了 Service Worker 的 API 设计；

Google 的 NativeClient、Mozilla 的 asm.js 对于 web 追逐 native 性能的极致追求，则奠定了 Web Assembly 的诞生……

你看，在这条道路上，Flash 与它的朋友们，其实并不孤单。

**「看到你长大了，我也就可以心满意足的离开了。」**

**就像是， web 技术发展的必然规律一样，**

**而 Open Web 则因此不朽。**

<br/>

我很高兴，Google Chrome、Mozilla Firefox、Microsoft Edge 都能这么写到：

> Flash helped make the web a rich, dynamic experience, and **shaped the modern set of web standards.**  
>   
> --- "[So long, and thanks for all the Flash](https://blog.chromium.org/2017/07/so-long-and-thanks-for-all-flash.html)" Chromium Blog

  

> Over the years, Flash has helped bring the Web to greatness with innovations in media and animation, **which ultimately have been added to the core web platform.**  
>   
> --- "[Firefox Roadmap for Flash End-of-Life](https://blog.mozilla.org/futurereleases/2017/07/25/firefox-roadmap-flash-end-life/)" Mozilla Blog

  

> Flash led the way on the web for rich content, gaming, animations, and media of all kinds, and **inspired many of the current web standards powering HTML5.**  
>   
> --- "[The End of an Era – Next Steps for Adobe Flash](https://blogs.windows.com/msedgedev/2017/07/25/flash-on-windows-timeline/)" Windows Blog

  

感谢你，Flash。

感谢你们，那些「功成身退」的你们。
