---
layout: post
title: "Design Patterns (4)"
subtitle: "Design patterns -- Adapter "
author: "Bing Yan"
header-img: "img/dp-adapter/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Java
  - Design Patterns
  - Learning
---
## 前言

经过上几次的设计模式基础的学习，已经了解了设计模式的意义和基本原则。<br/>
从这次学习开始，主要针对20多种具体的设计模式的意义，场景，和具体实现来深入学习。<br/>
这些学习的内容，都是根据网络上对于设计模式的书籍、博客等内容进行整理。<br/>
本次学习适配器(Adapter)模式。

## 正文
### 什么是适配器(Adapter)模式

在现实生活中，经常出现两个对象因接口不兼容而不能在一起工作的实例，这时需要第三者进行适配。例如，讲中文的人同讲英文的人对话时需要一个翻译，用直流电的笔记本电脑接交流电源时需要一个电源适配器，用计算机访问照相机的 SD 内存卡时需要一个读卡器等。<br/>
在软件设计中也可能出现：需要开发的具有某种业务功能的组件在现有的组件库中已经存在，但它们与当前系统的接口规范不兼容，如果重新开发这些组件成本又很高，这时用适配器模式能很好地解决这些问题。<br/>
将一个类的接口转换成客户希望的另外一个接口，使得原本由于接口不兼容而不能一起工作的那些类能一起工作。适配器模式分为类结构型模式和对象结构型模式两种，前者类之间的耦合度比后者高，且要求程序员了解现有组件库中的相关组件的内部结构，所以应用相对较少些。<br/>

### 适配器模式特点
<br/>
优点： <br/>

*   客户端通过适配器可以透明地调用目标接口。
*   复用了现存的类，程序员不需要修改原有代码而重用现有的适配者类。
*   将目标类和适配者类解耦，解决了目标类和适配者类接口不一致的问题。
<br/>
缺点： <br/>

*   对类适配器来说，更换适配器的实现过程比较复杂。

<br/>

### 适配器模式的结构

<br/>
工厂方法模式构成要素：<br/>

*   目标（Target）接口: 当前系统业务所期待的接口，它可以是抽象类或接口。
*   适配者（Adaptee）类: 它是被访问和适配的现存组件库中的组件接口。
*   适配器（Adapter）类: 它是一个转换器，通过继承或引用适配者的对象，把适配者接口转换成目标接口，让客户按目标接口的格式访问适配者。

<br/>
类适配器模式的结构图如下图所示: <br/>

![](/img/dp-adapter/adapter-1.png)

<br/>
对象适配器模式的结构图如下图所示: <br/>

![](/img/dp-adapter/adapter-2.png)

### 工厂方法模式的实现
<br/>
其实现方式主要有两种：<br/>
1. 类的适配器(采用继承实现) <br/>
2. 对象适配器(采用对象组合方式实现) <br/>

*   类适配器模式的代码如下：<br/>

```
package adapter;
//目标接口
interface Target
{
    public void request();
}
//适配者接口
class Adaptee
{
    public void specificRequest()
    {       
        System.out.println("适配者中的业务代码被调用！");
    }
}
//类适配器类
class ClassAdapter extends Adaptee implements Target
{
    public void request()
    {
        specificRequest();
    }
}
//客户端代码
public class ClassAdapterTest
{
    public static void main(String[] args)
    {
        System.out.println("类适配器模式测试：");
        Target target = new ClassAdapter();
        target.request();
    }
}
```
程序的运行结果如下：<br/>
>类适配器模式测试：<br/>
适配者中的业务代码被调用！

*   对象适配器模式的代码如下:<br/>

```
package adapter;
//对象适配器类
class ObjectAdapter implements Target
{
    private Adaptee adaptee;
    public ObjectAdapter(Adaptee adaptee)
    {
        this.adaptee=adaptee;
    }
    public void request()
    {
        adaptee.specificRequest();
    }
}
//客户端代码
public class ObjectAdapterTest
{
    public static void main(String[] args)
    {
        System.out.println("对象适配器模式测试：");
        Adaptee adaptee = new Adaptee();
        Target target = new ObjectAdapter(adaptee);
        target.request();
    }
}
```

说明：对象适配器模式中的“目标接口”和“适配者类”的代码同类适配器模式一样，只要修改适配器类和客户端的代码即可。
<br/>
程序的运行结果如下：<br/>
>对象适配器模式测试：<br/>
适配者中的业务代码被调用！<br/>

### 模式的应用场景

适配器模式（Adapter）通常适用于以下场景:<br/>

*   以前开发的系统存在满足新系统功能需求的类，但其接口同新系统的接口不一致。
*   使用第三方提供的组件，但组件接口定义和自己要求的接口定义不同。

### 模式的扩展

适配器模式（Adapter）可扩展为双向适配器模式，双向适配器类既可以把适配者接口转换成目标接口，也可以把目标接口转换成适配者接口，其结构图如图所示：<br/> 
![](/img/dp-adapter/adapter-3.png)
<br/>
```
package adapter;
//目标接口
interface TwoWayTarget
{
    public void request();
}
//适配者接口
interface TwoWayAdaptee
{
    public void specificRequest();
}
//目标实现
class TargetRealize implements TwoWayTarget
{
    public void request()
    {       
        System.out.println("目标代码被调用！");
    }
}
//适配者实现
class AdapteeRealize implements TwoWayAdaptee
{
    public void specificRequest()
    {       
        System.out.println("适配者代码被调用！");
    }
}
//双向适配器
class TwoWayAdapter  implements TwoWayTarget,TwoWayAdaptee
{
    private TwoWayTarget target;
    private TwoWayAdaptee adaptee;
    public TwoWayAdapter(TwoWayTarget target)
    {
        this.target=target;
    }
    public TwoWayAdapter(TwoWayAdaptee adaptee)
    {
        this.adaptee=adaptee;
    }
    public void request()
    {
        adaptee.specificRequest();
    }
    public void specificRequest()
    {       
        target.request();
    }
}
//客户端代码
public class TwoWayAdapterTest
{
    public static void main(String[] args)
    {
        System.out.println("目标通过双向适配器访问适配者：");
        TwoWayAdaptee adaptee=new AdapteeRealize();
        TwoWayTarget target=new TwoWayAdapter(adaptee);
        target.request();
        System.out.println("-------------------");
        System.out.println("适配者通过双向适配器访问目标：");
        target=new TargetRealize();
        adaptee=new TwoWayAdapter(target);
        adaptee.specificRequest();
    }
}
```
程序的运行结果如下：<br/>
>目标通过双向适配器访问适配者：<br/>
适配者代码被调用！<br/>
-------------------<br/>
适配者通过双向适配器访问目标：<br/>
目标代码被调用！

## 总结

&ensp;&ensp;&ensp;&ensp;适配器模式也是一种包装模式，与之前的 Decorator 装饰模式同样具有包装的功能；此外，对象适配器模式还具有显式委托的意思在里面（其实类适配器也有这种意思，只不过比较隐含而已），那么我在认为它与 Proxy 代理模式也有点类似。<br/>

&ensp;&ensp;&ensp;&ensp;从上面一点对比来看， Decorator 、 Proxy、 Adapter 在实现了自身的最主要目的（这个得看各个模式的最初动机、描述）之外，都可以在包装的前后进行额外的、特殊的功能上的增减，因为我认为它们都有委托的实现意思在里面。<br/>

&ensp;&ensp;&ensp;&ensp;也有书中说适配器模式不适合在详细设计阶段使用它，它是一种补偿模式，专用来在系统后期扩展、修改时所用。

## 参考资料
此次学习主要依赖于下面技术网站:<br/> 
http://c.biancheng.net/view/1361.html <br/>
https://www.cnblogs.com/V1haoge/p/6479118.html <br/>

