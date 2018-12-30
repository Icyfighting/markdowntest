---
layout: post
title: "Java memory management learning (2)"
subtitle: "Garbage collection & GC algorithm "
author: "Bing Yan"
header-img: "img/java-mem-2/post-bg-java.jpg"
header-mask: 0.2
tags:
  - Java
  - GC
  - Learning
---
## 前言
通过上一次学习，已经了解了JVM中对于内存的分配。这一次我要学习关于Java中重要的垃圾回收（GC）机制和回收机制中涉及到的分代收集原则，新生代(Young generation)、老年代(Tenured / Old Generation)、永久代(Perm Area)的划分，GC常用的算法(Algorithm)，Java中垃圾回收器(Garbage collector)的类型等内容。

## 正文

堆内存是如何划分的？
>Java中对象都在堆上创建。为了GC，堆内存分为三个部分，也可以说三代，分别称为新生代，老年代和永久代。<br/>其中新生代又进一步分为Eden区，Survivor 1区和Survivor 2区。新创建的对象会分配在Eden区,在经历一次Minor GC后会被移到Survivor 1区，再经历一次Minor GC后会被移到Survivor 2区，直到升至老年代,需要注意的是，一些大对象(长字符串或数组)可能会直接存放到老年代。<br/>永久代有一些特殊，它用来存储类的元信息。

下面分别详细了解下新生代，老年代和永久代的特点：

**新生代(Young generation):**
>所有新生成的对象首先都是放在年轻代的。年轻代的目标就是尽可能快速的收集掉那些生命周期短的对象。<br/>年轻代分三个区。一个Eden区，两个 Survivor区(一般而言)。<br/>大部分对象在Eden区中生成。当Eden区满时，还存活的对象将被复制到Survivor区（两个中的一个），当这个 Survivor区满时，此区的存活对象将被复制到另外一个Survivor区，当这个Survivor去也满了的时候，从第一个Survivor区复制过来的并且此时还存活的对象，将被复制“年老区(Tenured)”。<br/>需要注意，Survivor的两个区是对称的，没先后关系，所以同一个区中可能同时存在从Eden复制过来对象，和从前一个Survivor复制过来的对象，而复制到年老区的只有从第一个Survivor去过来的对象。<br/>而且，Survivor区总有一个是空的。<br/>同时，根据程序需要，Survivor区是可以配置为多个的（多于两个），这样可以增加对象在年轻代中的存在时间，减少被放到年老代的可能。

**老年代(Tenured / Old Generation):**
>在年轻代中经历了N次垃圾回收后仍然存活的对象，就会被放到年老代中。因此，可以认为年老代中存放的都是一些生命周期较长的对象。

**永久代(Perm Area):**
>用于存放静态文件，如今Java类、方法等。持久代对垃圾回收没有显著影响，但是有些应用可能动态生成或者调用一些class，例如Hibernate 等，在这种时候需要设置一个比较大的持久代空间来存放这些运行过程中新增的类。持久代大小通过-XX:MaxPermSize=<N>进行设置。


![](/img/java-mem-2.jpg)


## 总结
### 1. 为了分代垃圾回收，Java堆内存分为3代：新生代，老年代和永久代。
### 2. 新的对象实例会优先分配在新生代，在经历几次Minor GC后(默认15次)，还存活的会被移至老年代(某些大对象会直接在老年代分配)。
### 3. 永久代是否执行GC，取决于采用的JVM。
### 4. Minor GC发生在新生代，当Eden区没有足够空间时，会发起一次Minor GC，将Eden区中的存活对象移至Survivor区。Major GC发生在老年代，当升到老年代的对象大于老年代剩余空间时会发生Major GC。
### 5. 发生Major GC时用户线程会暂停，会降低系统性能和吞吐量。
### 6. JVM的参数-Xmx和-Xms用来设置Java堆内存的初始大小和最大值。依据个人经验这个值的比例最好是1:1或者1:1.5。比如，你可以将-Xmx和-Xms都设为1GB，或者-Xmx和-Xms设为1.2GB和1.8GB。
### 7. Java中不能手动触发GC，但可以用不同的引用类来辅助垃圾回收器工作(比如：弱引用或软引用)。
