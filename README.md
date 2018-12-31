---
layout: post
title: "Design Patterns (3)"
subtitle: "Design patterns -- Factory Method "
author: "Bing Yan"
header-img: "img/dp_factory/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Java
  - Design Patterns
  - Learning
---
## 前言

经过上两次的设计模式基础的学习，已经了解了设计模式的意义和基本原则。<br/>
从这次学习开始，主要针对20多种具体的设计模式的意义，场景，和具体实现来深入学习。<br/>
这些学习的内容，都是根据网络上对于设计模式的书籍、博客等内容进行整理。<br/>
本次学习工厂方法(Factory Method)模式。

## 正文
### 什么是工厂方法(Factory Method)

定义一个创建产品对象的工厂接口，将产品对象的实际创建工作推迟到具体子工厂类当中。这满足创建型模式中所要求的“创建与使用相分离”的特点。<br/>
我们把被创建的对象称为“产品”，把创建产品的对象称为“工厂”。如果要创建的产品不多，只要一个工厂类就可以完成，这种模式叫“简单工厂模式”，它不属于 GoF 的 23 种经典设计模式，它的缺点是增加新产品时会违背“开闭原则”。<br/>
本次学习的“工厂方法模式”是对简单工厂模式的进一步抽象化，其好处是可以使系统在不修改原来代码的情况下引进新的产品，即满足开闭原则。<br/>

### 工厂方法模式特点
<br/>
优点： <br/>

*   用户只需要知道具体工厂的名称就可得到所要的产品，无须知道产品的具体创建过程
*   在系统增加新的产品时只需要添加具体产品类和对应的具体工厂类，无须对原工厂进行任何修改，满足开闭原则

缺点： <br/>
*   每增加一个产品就要增加一个具体产品类和一个对应的具体工厂类，这增加了系统的复杂度。

<br/>

### 单例模式的结构

<br/>
工厂方法模式构成要素：<br/>

*   抽象工厂 (Abstract Factory): 提供了创建产品的接口，调用者通过它访问具体工厂的工厂方法 newProduct() 来创建产品。
*   具体工厂 (Concrete Factory): 主要是实现抽象工厂中的抽象方法，完成具体产品的创建。
*   抽象产品 (Product): 定义了产品的规范，描述了产品的主要特性和功能。
*   具体产品 (Concrete Product): 实现了抽象产品角色所定义的接口，由具体工厂来创建，它同具体工厂之间一一对应。

<br/>
其结构如下图所示: <br/>

![](/img/dp_factory/factory-1.png)

<br/>

### 工厂方法模式的实现
<br/>
根据上述结构图，写出该模式的代码如下：<br/>

```
package FactoryMethod;
public class AbstractFactoryTest
{
    public static void main(String[] args)
    {
        try
        {
            Product a;
            AbstractFactory af;
            af=(AbstractFactory) ReadXML1.getObject();
            a=af.newProduct();
            a.show();
        }
        catch(Exception e)
        {
            System.out.println(e.getMessage());
        }
    }
}
//抽象产品：提供了产品的接口
interface Product
{
    public void show();
}
//具体产品1：实现抽象产品中的抽象方法
class ConcreteProduct1 implements Product
{
    public void show()
    {
        System.out.println("具体产品1显示...");
    }
}
//具体产品2：实现抽象产品中的抽象方法
class ConcreteProduct2 implements Product
{
    public void show()
    {
        System.out.println("具体产品2显示...");
    }
}
//抽象工厂：提供了厂品的生成方法
interface AbstractFactory
{
    public Product newProduct();
}
//具体工厂1：实现了厂品的生成方法
class ConcreteFactory1 implements AbstractFactory
{
    public Product newProduct()
    {
        System.out.println("具体工厂1生成-->具体产品1...");
        return new ConcreteProduct1();
    }
}
//具体工厂2：实现了厂品的生成方法
class ConcreteFactory2 implements AbstractFactory
{
    public Product newProduct()
    {
        System.out.println("具体工厂2生成-->具体产品2...");
        return new ConcreteProduct2();
    }
}
```

```
package FactoryMethod;
import javax.xml.parsers.*;
import org.w3c.dom.*;
import java.io.*;
class ReadXML1
{
    //该方法用于从XML配置文件中提取具体类类名，并返回一个实例对象
    public static Object getObject()
    {
        try
        {
            //创建文档对象
            DocumentBuilderFactory dFactory=DocumentBuilderFactory.newInstance();
            DocumentBuilder builder=dFactory.newDocumentBuilder();
            Document doc;                           
            doc=builder.parse(new File("src/FactoryMethod/config1.xml"));        
            //获取包含类名的文本节点
            NodeList nl=doc.getElementsByTagName("className");
            Node classNode=nl.item(0).getFirstChild();
            String cName="FactoryMethod."+classNode.getNodeValue();
            //System.out.println("新类名："+cName);
            //通过类名生成实例对象并将其返回
            Class<?> c=Class.forName(cName);
              Object obj=c.newInstance();
            return obj;
         }  
         catch(Exception e)
         {
                   e.printStackTrace();
                   return null;
         }
    }
}

```

可以根据XML配置文件指定需要使用的具体工厂<br/>

```
<?xml version="1.0" encoding="UTF-8"?>
<config>
  <className>ConcreteFactory1</className>
</config>
```
运行结果如下：<br/>
>具体工厂1生成-->具体产品1... <br/>
具体产品1显示...

将XML配置文件中的ConcreteFactory1 改为 ConcreteFactory2，则程序运行结果如下：<br/>
>具体工厂2生成-->具体产品2... <br/>
具体产品2显示...

### 模式的应用场景

工厂方法模式通常适用于以下场景:<br/>
*   客户只知道创建产品的工厂名，而不知道具体的产品名。如 TCL 电视工厂、海信电视工厂等。
*   创建对象的任务由多个具体子工厂中的某一个完成，而抽象工厂只提供创建产品的接口。
*   客户不关心创建产品的细节，只关心产品的品牌。

### 模式的扩展

当需要生成的产品不多且不会增加，一个具体工厂类就可以完成任务时，可删除抽象工厂类。这时工厂方法模式将退化到简单工厂模式，其结构图下图所示: <br/>

![](/img/dp_factory/factory-2.png)
<br/>

## 总结

    在现实生活中社会分工越来越细，越来越专业化。各种产品有专门的工厂生产，彻底告别了自给自足的小农经济时代，这大大缩短了产品的生产周期，提高了生产效率。<br/>   在程序设计中，工厂方法模式也提供了一个这样的思路，实现了软件对象的生产和使用相分离。<br/>   
无论是简单工厂模式，工厂方法模式，还是抽象工厂模式，他们都属于工厂模式，在形式和特点上也是极为相似的，他们的最终目的都是为了解耦。在使用时，我们不必去在意这个模式到底工厂方法模式还是抽象工厂模式，因为他们之间的演变常常是令人琢磨不透的。<br/>   经常你会发现，明明使用的工厂方法模式，当新需求来临，稍加修改，加入了一个新方法后，由于类中的产品构成了不同等级结构中的产品族，它就变成抽象工厂模式了；而对于抽象工厂模式，当减少一个方法使的提供的产品不再构成产品族之后，它就演变成了工厂方法模式。<br/>   
所以，在使用工厂模式时，只需要关心降低耦合度的目的是否达到了。

## Reference-1
此次学习主要依赖于下面技术网站:<br/> 
http://c.biancheng.net/view/1348.html <br/>
https://www.cnblogs.com/yumo1627129/p/7197524.html <br/>

