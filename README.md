---
layout: post
title: " Magic Method "
subtitle: "Python learning (3)"
author: "Bing Yan"
header-img: "img/magic/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Python
  - Learning
---
## 前言

&ensp;&ensp;&ensp;&ensp;魔法方法就如同它的名字一样神奇，总能在你需要的时候为你提供某种方法来让你的想法实现。魔法方法是指Python内部已经包含的，被双下划线所包围的方法，这些方法在进行特定的操作时会自动被调用，它们是Python面向对象下智慧的结晶。作为初学者的我，掌握Python的魔法方法也就变得尤为重要，今天就学习一下吧。

## 正文
### 什么是魔法方法(Magic Method)

&ensp;&ensp;&ensp;&ensp;简单的讲，python中以双下划线开始和结束的函数（不可自己定义）为魔法函数。<br/>
调用类实例化的对象的方法时自动调用魔法函数（感觉不需要显示调用的函数都叫）。

### 为什么要用魔法方法

使用Python的魔法方法可以使Python的自由度变得更高，当不需要重写时魔法方法也可以在规定的默认情况下生效，在需要重写时也可以让使用者根据自己的需求来重写部分方法来达到自己的期待。而且众所周知Python是支持面向对象的语言Python的基本魔法方法就使得Python在面对对象方面做得更好。


### 魔法方法分类

来来来，扔个魔法方法分类表格镇镇宅
<br/>
![](/img/magic/mm-1.png)
<br/>
![](/img/magic/mm-2.png)
<br/>
![](/img/magic/mm-3.png)
<br/>
![](/img/magic/mm-4.png)
<br/>
![](/img/magic/mm-5.png)
<br/>
![](/img/magic/mm-6.png)
<br/>


### 常用魔法方法

虽然上面表格给出了各种魔法方法，但是今天我们先学习几个常用的魔法方法。
<br/>

**__new__**

__new__ 方法主要是当你继承一些不可变的class时(比如int, str, tuple)， 提供给你一个自定义这些类的实例化过程的途径。还有就是实现自定义的metaclass。
假如我们需要一个永远都是正数的整数类型，通过集成int，我们可能会写出这样的代码。

```
class PositiveInteger(int):

  def __init__(self, value):

    super(PositiveInteger, self).__init__(self, abs(value))

i = PositiveInteger(-3)

print i
```
<br/>
输出结果：<br/>

```
-3
```
<br/>

运行后会发现，结果根本不是我们想的那样，我们仍然得到了-3。这是因为对于int这种不可变的对象，我们只有重载它的__new__方法才能起到自定义的作用。
<br/>
修改代码如下：
<br/>

```
class PositiveInteger(int):

  def __new__(cls, value):

    return super(PositiveInteger, cls).__new__(cls, abs(value))

i = PositiveInteger(-3)

print i
```
<br/>
输出结果：<br/>

```
3
```
还可以用来实现单例(singleton)。因为类每一次实例化后产生的过程都是通过__new__来控制的，所以通过重载__new__方法，我们 可以很简单的实现单例模式。
<br/>
```
class Singleton(object):

  def __new__(cls):

    # 关键在于这，每一次实例化的时候，我们都只会返回这同一个instance对象

    if not hasattr(cls, 'instance'):

      cls.instance = super(Singleton, cls).__new__(cls)

    return cls.instance

obj1 = Singleton()

obj2 = Singleton()

obj1.attr1 = 'value1'

print obj1.attr1, obj2.attr1

print obj1 is obj2
```

<br/>
输出结果：<br/>

```
value1 value1
True
```

**__init__**

我们知道__init__方法负责对象的初始化，系统执行该方法前，其实该对象已经存在了，要不然初始化什么东西呢？
如果把创造对象比喻成建造一栋房子，__new__方法可以创造出房子的骨架，那么__init__更像是给房子装修。<br/>
其实__init__方法意义重大的原因有两个。第一个原因是在对象生命周期中初始化是最重要的一步；每个对象必须正确初始化后才能正常工作。第二个原因是__init__()参数值可以有多种形式。<br/>

对于__init__()和__new__()的顺序问题可以通过以下例子来验证：

```
class A:
 def __init__(self):
  print("__init__ ")
  print(self)
  super(A, self).__init__()
 
 def __new__(cls):
  print("__new__ ")
  print(self)
  return super(A, cls).__new__(cls)
 
 def __call__(self): # 可以定义任意参数
  print('__call__ ')
 
A()
```
输出结果为：<br/>
```
__new__ 
<__main__.A object at 0x1007a95f8>
__init__ 
<__main__.A object at 0x1007a95f8>
```
<br/>
从输出结果来看，__new__方法先被调用，返回一个实例对象，接着__init__ 被调用。<br/>
并且可以知道__new__ 方法的返回值就是类的实例对象，这个实例对象会传递给__init__ 方法中定义的self参数，以便实例对象可以被正确地初始化。<br/>

**__call__**

关于__call__ 方法，不得不先提到一个概念，就是可调用对象（callable），我们平时自定义的函数、内置函数和类都属于可调用对象，但凡是可以把一对括号()应用到某个对象身上都可称之为可调用对象，判断对象是否为可调用对象可以用函数 callable。<br/>
如果在类中实现了__call__ 方法，那么实例对象也将成为一个可调用对象，我们回到最开始的那个例子：

```
class Foo(object): 
  def __call__(self): 
    pass
  
f = Foo()#类Foo可call 
f()#对象f可call 
```

## 总结
&ensp;&ensp;&ensp;&ensp;今天只学习最常用的三种魔法方法，而__new__、__init__、__call__等方法不是必须写的，会默认调用，如果自己定义了，就是override,可以custom。既然override了，通常也会显式调用进行补偿以达到extend的目的。<br/> 
之后也会随着代码的积累和项目的需要对其他魔法方法进行进一步的学习。<br/> 

## 参考资料
此次学习主要依赖于下面技术网站:<br/> 
https://www.jb51.net/article/118917.htm <br/>
