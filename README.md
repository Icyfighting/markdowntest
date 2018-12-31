---
layout: post
title: "Design Patterns (2)"
subtitle: "Design patterns -- Singleton "
author: "Bing Yan"
header-img: "img/deadlock/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Java
  - Design Patterns
  - Learning
---
## 前言

经过上一次的设计模式基础的学习，已经了解了设计模式的意义和基本原则。<br/>
从这次学习开始，主要针对20多种具体的设计模式的意义，场景，和具体实现来深入学习。<br/>
这些学习的内容，都是根据网络上对于设计模式的书籍、博客等内容进行整理。

## 正文
### 什么是单例(Singleton)

一个类只有一个实例，且该类能自行创建这个实例的一种模式。例如，Windows 中只能打开一个任务管理器，这样可以避免因打开多个任务管理器窗口而造成内存资源的浪费，或出现各个窗口显示内容的不一致等错误。<br/>
<br/>
在计算机系统中，还有 Windows 的回收站、操作系统中的文件系统、多线程中的线程池、显卡的驱动程序对象、打印机的后台处理服务、应用程序的日志对象、数据库的连接池、网站的计数器、Web 应用的配置对象、应用程序中的对话框、系统中的缓存等常常被设计成单例。

### 单例模式特点
<br/>

*   单例类只有一个实例对象
*   该单例对象必须由单例类自行创建
*   单例类对外提供一个访问该单例的全局访问点
<br/>

### 单例模式的结构

<br/>
单例模式是设计模式中最简单的模式之一。<br/>
通常，普通类的构造函数是公有的，外部类可以通过“new 构造函数()”来生成多个实例。但是，如果将类的构造函数设为私有的，外部类就无法调用该构造函数，也就无法生成多个实例。这时该类自身必须定义一个静态私有实例，并向外提供一个静态的公有函数用于创建或获取该静态私有实例。
<br/>
单例模式的主要角色如下：<br/>

*   单例类：包含一个实例且能自行创建这个实例的类。
*   访问类：使用单例的类。

<br/>
其结构如下图所示: <br/>

![](/img/dp_singleton/singleton-1.png)

<br/>

### 单例模式的实现
<br/>
Singleton 模式通常有两种实现形式：<br/>

*   懒汉式单例：
<br/>
该模式的特点是类加载时没有生成单例，只有当第一次调用 getlnstance 方法时才去创建这个单例。<br/>

```
public class LazySingleton
{
    private static volatile LazySingleton instance=null;    //保证 instance 在所有线程中同步
    private LazySingleton(){}    //private 避免类在外部被实例化
    public static synchronized LazySingleton getInstance()
    {
        //getInstance 方法前加同步
        if(instance==null)
        {
            instance=new LazySingleton();
        }
        return instance;
    }
}
```

<br/>
注意：如果编写的是多线程程序，则不要删除上例代码中的关键字 volatile 和 synchronized，否则将存在线程非安全的问题。如果不删除这两个关键字就能保证线程安全，但是每次访问时都要同步，会影响性能，且消耗更多的资源，这是懒汉式单例的缺点。<br/>
<br/>

**学习笔记:** 

<br/>
这种方式的懒汉式单例实现，在多线程的时候效率比较低，为了可以提高在多线程场景下的效率，有一种“双重检测”(double check)方法的实现，在保证线程安全的前提下，更高效的实现了单例模式。<br/>

```
public class Singleton{
 
    private static Singleton singleton;   
 
    private Singleton(){
 
    }
 
    public static Singleton getInstance(){
 
       if(singleton == null){
 
           synchronized(Singleton.class){
 
              if(singleton == null){
 
                  singleton = new Singleton();
 
              }
           }
       }
 
    return singleton;
    }
}

```

<br/>


*   饿汉式单例:
<br/>
该模式的特点是类一旦加载就创建一个单例，保证在调用 getInstance 方法之前单例已经存在了。<br/>

```
public class HungrySingleton
{
    private static final HungrySingleton instance=new HungrySingleton();
    private HungrySingleton(){}
    public static HungrySingleton getInstance()
    {
        return instance;
    }
}
```

<br/>
饿汉式单例在类创建的同时就已经创建好一个静态的对象供系统使用，以后不再改变，所以是线程安全的，可以直接用于多线程而不会出现问题。
<br/>

### 单例模式的应用场景

前面分析了单例模式的结构与特点，以下是它通常适用的场景的特点:<br/>
*   在应用场景中，某类只要求生成一个对象的时候，如一个班中的班长、每个人的身份证号等。
*   当对象需要被共享的场合。由于单例模式只允许创建一个对象，共享该对象可以节省内存，并加快对象访问速度。如 Web 中的配置对象、数据库的连接池等。
*   当某类需要频繁实例化，而创建的对象又频繁被销毁的时候，如多线程的线程池、网络连接池等。

### 单例模式的扩展
单例模式可扩展为有限的多例（Multitcm）模式，这种模式可生成有限个实例并保存在 ArmyList 中，客户需要时可随机获取，其结构图如下图所示：<br/>
![](/img/dp_singleton/singleton-2.png)
<br/>

## 总结

单例模式确保某个类只有一个实例，而且自行实例化并向整个系统提供这个实例。<br/>
在计算机系统中，线程池、缓存、日志对象、对话框、打印机、显卡的驱动程序对象常被设计成单例。这些应用都或多或少具有资源管理器的功能。<br/>
如果我们能够保证系统中自始至终只有唯一一个数据库连接对象，显然我们会节省很多内存开销和cpu利用率。这就是单件模式的用途。当然单件模式不仅仅只用于这样的情况。在《设计模式：可复用面向对象软件的基础》一书中对单件模式的适用性有如下描述：<br/>
*   当类只能有一个实例而且客户可以从一个众所周知的访问点访问它时。<br/>
*   当这个唯一实例应该是通过子类化可扩展的，并且客户应该无需更改代码就能使用一个扩展的实例时。<br/>

## Reference-1
此次学习主要依赖于下面技术网站:<br/> 
http://c.biancheng.net/view/1338.html <br/>
https://blog.csdn.net/qq_42145871/article/details/82014532 <br/>

