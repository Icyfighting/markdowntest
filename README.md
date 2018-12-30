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
### GC 内存划分
堆内存是如何划分的？
>Java中对象都在堆上创建。为了GC，堆内存分为三个部分，也可以说三代，分别称为新生代，老年代和永久代。<br/>其中新生代又进一步分为Eden区，Survivor 1区和Survivor 2区。新创建的对象会分配在Eden区,在经历一次Minor GC后会被移到Survivor 1区，再经历一次Minor GC后会被移到Survivor 2区，直到升至老年代,需要注意的是，一些大对象(长字符串或数组)可能会直接存放到老年代。<br/>永久代有一些特殊，它用来存储类的元信息。

下面分别详细了解下新生代，老年代和永久代的特点：

**新生代(Young generation):**
>所有新生成的对象首先都是放在年轻代的。年轻代的目标就是尽可能快速的收集掉那些生命周期短的对象。<br/>年轻代分三个区。一个Eden区，两个 Survivor区(一般而言)。<br/>大部分对象在Eden区中生成。当Eden区满时，还存活的对象将被复制到Survivor区（两个中的一个），当这个 Survivor区满时，此区的存活对象将被复制到另外一个Survivor区，当这个Survivor去也满了的时候，从第一个Survivor区复制过来的并且此时还存活的对象，将被复制“年老区(Tenured)”。<br/>需要注意，Survivor的两个区是对称的，没先后关系，所以同一个区中可能同时存在从Eden复制过来对象，和从前一个Survivor复制过来的对象，而复制到年老区的只有从第一个Survivor去过来的对象。<br/>而且，Survivor区总有一个是空的。<br/>同时，根据程序需要，Survivor区是可以配置为多个的（多于两个），这样可以增加对象在年轻代中的存在时间，减少被放到年老代的可能。

**老年代(Tenured / Old Generation):**
>在年轻代中经历了N次垃圾回收后仍然存活的对象，就会被放到年老代中。因此，可以认为年老代中存放的都是一些生命周期较长的对象。

**永久代(Perm Area):**
>用于存放静态文件，如今Java类、方法等。持久代对垃圾回收没有显著影响，但是有些应用可能动态生成或者调用一些class，例如Hibernate 等，在这种时候需要设置一个比较大的持久代空间来存放这些运行过程中新增的类。持久代大小通过-XX:MaxPermSize=<N>进行设置。
  
 ![](/img/java-mem-2/generation.png) 

### GC 算法

**标记清除算法:**
>“标记-清除”（Mark-Sweep）算法，如它的名字一样，算法分为“标记”和“清除”两个阶段：首先标记出所有需要回收的对象，在标记完成后统一回收掉所有被标记的对象。之所以说它是最基础的收集算法，是因为后续的收集算法都是基于这种思路并对其缺点进行改进而得到的。<br/>
它的主要缺点有两个：一个是效率问题，标记和清除过程的效率都不高；另外一个是空间问题，标记清除之后会产生大量不连续的内存碎片，空间碎片太多可能会导致，当程序在以后的运行过程中需要分配较大对象时无法找到足够的连续内存而不得不提前触发另一次垃圾收集动作。

![](/img/java-mem-2/mark-sweep.jpg)


**复制算法:**
>把内存空间划为两个区域，每次只使用其中一个区域。垃圾回收时，遍历当前使用区域，把正在使用中的对象复制到另外一个区域中。算法每次只处理正在使用中的对象，因此复制成本比较小，同时复制过去以后还能进行相应的内存整理，不会出现“碎片”问题。<br/>优点：实现简单，运行高效。<br/>缺点：会浪费一定的内存。一般新生代采用这种算法。

![](/img/java-mem-2/copy.jpg)


**标记压缩算法:**
>标记阶段与标记清除算法一样。但后续并不是直接对可回收的对象进行清理，而是让所有存活对象都想一端移动，然后压缩。<br/>优点：不会造成内存碎片。

![](/img/java-mem-2/Mark-Compact.jpg)

### Java中垃圾回收器的类型

Java提供多种类型的垃圾回收器。JVM中的垃圾收集一般都采用“分代收集”，不同的堆内存区域采用不同的收集算法，主要目的就是为了增加吞吐量或降低停顿时间。
*   Serial收集器：新生代收集器，使用复制算法，使用一个线程进行GC，串行，其它工作线程暂停。
![](/img/java-mem-2/Serial-collector.jpg)
*   ParNew收集器：新生代收集器，使用复制算法，Serial收集器的多线程版，用多个线程进行GC，并行，其它工作线程暂停。使用-XX:+UseParNewGC开关来控制使用ParNew+Serial Old收集器组合收集内存；使用-XX:ParallelGCThreads来设置执行内存回收的线程数。
![](/img/java-mem-2/parnew-collector.jpg)
*   Parallel Scavenge 收集器：吞吐量优先的垃圾回收器，作用在新生代，使用复制算法，关注CPU吞吐量，即运行用户代码的时间/总时间。使用-XX:+UseParallelGC开关控制使用Parallel Scavenge+Serial Old收集器组合回收垃圾。
*   Serial Old收集器：老年代收集器，单线程收集器，串行，使用标记整理算法，使用单线程进行GC，其它工作线程暂停。
*   Parallel Old收集器：吞吐量优先的垃圾回收器，作用在老年代，多线程，并行，多线程机制与Parallel Scavenge差不错，使用标记整理算法，在Parallel Old执行时，仍然需要暂停其它线程。
*   CMS（Concurrent Mark Sweep）收集器：老年代收集器，致力于获取最短回收停顿时间（即缩短垃圾回收的时间），使用标记清除算法，多线程，优点是并发收集（用户线程可以和GC线程同时工作），停顿小。使用-XX:+UseConcMarkSweepGC进行ParNew+CMS+Serial Old进行内存回收，优先使用ParNew+CMS，当用户线程内存不足时，采用备用方案Serial Old收集。
![](/img/java-mem-2/CMS-collector.jpg)

## 总结
 1. 为了分代垃圾回收，Java堆内存分为3代：新生代，老年代和永久代。
 2. 新的对象实例会优先分配在新生代，在经历几次Minor GC后(默认15次)，还存活的会被移至老年代(某些大对象会直接在老年代分配)。
 3. 永久代是否执行GC，取决于采用的JVM。
 4. Minor GC发生在新生代，当Eden区没有足够空间时，会发起一次Minor GC，将Eden区中的存活对象移至Survivor区。Major GC发生在老年代，当升到老年代的对象大于老年代剩余空间时会发生Major GC。
 5. 发生Major GC时用户线程会暂停，会降低系统性能和吞吐量。
 6. JVM的参数-Xmx和-Xms用来设置Java堆内存的初始大小和最大值。依据个人经验这个值的比例最好是1:1或者1:1.5。比如，你可以将-Xmx和-Xms都设为1GB，或者-Xmx和-Xms设为1.2GB和1.8GB。
 7. Java中不能手动触发GC，但可以用不同的引用类来辅助垃圾回收器工作(比如：弱引用或软引用)。
