---
layout: post
title: " Meta Class "
subtitle: "Python learning (2)"
author: "Bing Yan"
header-img: "img/metaclass/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Python
  - Learning
---
## 前言

&ensp;&ensp;&ensp;&ensp;对于元类有一个通俗易懂又颇具禅意的比喻：<br/>
道生一，一生二，二生三，三生万物<br/>

![](/img/metaclass/dao.jpg)
<br/>

*   道 即是 type
*   一 即是 metaclass(元类，或者叫类生成器)
*   二 即是 class(类，或者叫实例生成器)
*   三 即是 instance(实例)
*   万物 即是 实例的各种属性与方法，我们平常使用python时，调用的就是它们。

“一”就是今天要学习的重点--元类(Meta Class)。

## 正文
### 什么是元类(Meta Class)

&ensp;&ensp;&ensp;&ensp;简单的讲，元类创建了Python中所有的对象。<br/>
&ensp;&ensp;&ensp;&ensp;我们说Python是一种动态语言，而动态语言和静态语言最大的不同，就是函数和类不是编译时定义的，而是运行时动态创建的。<br/>
&ensp;&ensp;&ensp;&ensp;class的定义是运行时动态创建的，而创建class的方法就是使用type()函数。<br/>
&ensp;&ensp;&ensp;&ensp;通过type()函数创建的类和直接写class是完全一样的，因为Python解释器遇到class定义时，仅仅是扫描一下class定义的语法，然后调用type()函数创建出class。<br/>
&ensp;&ensp;&ensp;&ensp;除了使用type()动态创建类以外，要控制类的创建行为，还可以使用metaclass，直译为元类。<br/>
&ensp;&ensp;&ensp;&ensp;当我们定义了类以后，就可以根据这个类创建出实例，所以：先定义类，然后创建实例。但是如果我们想创建出类呢？那就必须根据metaclass创建出类，所以：先定义metaclass，然后创建类。所以，metaclass允许你创建类或者修改类。换句话说，你可以把类看成是metaclass创建出来的“实例”。
<br/>

### 动态创建类

下面来举例集几种类的创建方式
<br/>

*   函数创建
*   type()创建

**函数创建:**

```
def create_animal(type):
    if type == 'Dog':
        # 创建类
        class Dog(object):
            pass
 
        return Dog
    elif type == 'Cat':
        class Cat(object):
            pass
 
        return Cat
 
 
Dog = create_animal("Dog")
print(type(Dog))
dog = Dog()
print(type(dog))

```
<br/>
输出结果：<br/>

```
<class 'type'>#类类型 <br/>
<class '__main__.create_animal.<locals>.Dog'>#对象类型
```
<br/>

这里通过调用函数传图不同的参数，来创建不同的类，创建出返回的是类的引用，并不是对象，我们可以通过返回的类来创建对象。
<br/>

**type()创建:**

```
class Test1(object):
    pass
 
 
Test2 = type("Test2", (object,), {})
 
print(type(Test1))
print(type(Test2))
```
输出结果：
<br/>
```
<class 'type'>
<class 'type'>
```

可以看出两种创建方式是相同的效果，说到这里我们大家应该都明白了，原来type就是创建类的一个方法，python用它来创建类，也就是说他是所有类的元类，例如在pyhton中是不是还有int，str...等等类型，int就是用来创建整数的类，而str就是用来创建字符串的类，这里的type也是，他就是python用来创建类的类（元类）。

<br/>


### 自定义元类

自定义类的的目的，就是拦截类的创建，然后修改一些特性，然后返回该类。感觉是装饰器干的事情，只是装饰器是修饰一个函数，同样是一个东西进去，然后被额外加了一些东西，最后被返回。

```
def upper_attr(class_name, class_parents, class_attr):
    """
    返回一个对象,将属性都改为大写的形式
    :param class_name:  类的名称
    :param class_parents: 类的父类tuple
    :param class_attr: 类的参数
    :return: 返回类
    """
    # 生成了一个generator
    attrs = ((name, value) for name, value in class_attr.items() if not name.startswith('__'))
    uppercase_attrs = dict((name.upper(), value) for name, value in attrs)
    return type(class_name, class_parents, uppercase_attrs)

__metaclass__ = upper_attr

pw = upper_attr('Trick', (), {'bar': 0})
print hasattr(pw, 'bar')
print hasattr(pw, 'BAR')
print pw.BAR
```

输出结果：<br/>
>False<br/>
True

可以从上面看到，实现了一个元类(metaclass)， 然后指定了模块使用这个元类来创建类，所以当下面使用type进行类创建的时候，可以发现小写的bar参数被替换成了大写的BAR参数，并且在最后调用了这个类属性并，打印了它。<br/>

## 总结
&ensp;&ensp;&ensp;&ensp;类其实是能够创建出类实例的对象。好吧，事实上，类本身也是实例，当然，它们是元类的实例。<br/>
&ensp;&ensp;&ensp;&ensp;Python中的一切都是对象，它们要么是类的实例，要么是元类的实例，除了type。type实际上是它自己的元类，在纯Python环境中这可不是你能够做到的，这是通过在实现层面耍一些小手段做到的。其次，元类是很复杂的。对于非常简单的类，你可能不希望通过使用元类来对类做修改。

## 参考资料
此次学习主要依赖于下面技术网站:<br/> 
https://stackoverflow.com/questions/100003/what-are-metaclasses-in-python <br/>
http://blog.jobbole.com/21351/ <br/>
