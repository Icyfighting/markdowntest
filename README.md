---
layout: post
title: " Python Data Structure learning"
subtitle: "List & Tuple & Set & Dictionary"
author: "Bing Yan"
header-img: "img/magic/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Python
  - Data Structure
  - Learning
---
## 前言

&ensp;&ensp;&ensp;&ensp;这次学习主要针对Python中常用的4中数据结构的创建、访问、修改、删除等基本操作，及各个数据结构的特点进行学习。<br/>

## 正文
### 列表(List)

&ensp;&ensp;&ensp;&ensp;序列是Python中最基本的数据结构。序列中的每个元素都分配一个数字 - 它的位置，或索引，第一个索引是0，第二个索引是1，依此类推。<br/>
&ensp;&ensp;&ensp;&ensp;Python有6个序列的内置类型，但最常见的是列表和元组。<br/>
&ensp;&ensp;&ensp;&ensp;序列都可以进行的操作包括索引，切片，加，乘，检查成员。<br/>
&ensp;&ensp;&ensp;&ensp;此外，Python已经内置确定序列的长度以及确定最大和最小的元素的方法。<br/>
&ensp;&ensp;&ensp;&ensp;列表是最常用的Python数据类型，它可以作为一个方括号内的逗号分隔值出现。<br/>
&ensp;&ensp;&ensp;&ensp;列表的数据项不需要具有相同的类型。<br/>


**List创建**
创建一个列表，只要把逗号分隔的不同的数据项使用方括号括起来即可。<br/>

```
list1 = ['physics', 'chemistry', 1997, 2000]
list2 = [1, 2, 3, 4, 5 ]
list3 = ["a", "b", "c", "d"]
```
**List中值的访问**
使用下标索引来访问列表中的值，同样你也可以使用方括号的形式截取字符：<br/>

```
list1 = ['physics', 'chemistry', 1997, 2000]
list2 = [1, 2, 3, 4, 5, 6, 7 ]
print("list1[0]: ", list1[0])
print(list2[1:5]: ", list2[1:5])
```
<br/>
输出结果：<br/>

```
list1[0]:  physics
list2[1:5]:  [2, 3, 4, 5]
```

**更新List**
```
list = []          ## 空列表
list.append('Google')   ## 使用 append() 添加元素
list.append('Runoob')
print(list)
```
<br/>
输出结果：<br/>

```
['Google', 'Runoob']
```

**删除List中元素**

```
list1 = ['physics', 'chemistry', 1997, 2000]
 
print(list1)
del(list1[2])
print("After deleting value at index 2 : ")
print(list1)
```
<br/>
输出结果：<br/>

```
['physics', 'chemistry', 1997, 2000]
After deleting value at index 2 :
['physics', 'chemistry', 2000]
```

### 元组(Tuple)

Python的元组与列表类似，不同之处在于元组的元素不能修改。<br/>
元组使用小括号，列表使用方括号。

**Tuple创建**

元组创建很简单，只需要在括号中添加元素，并使用逗号隔开即可：<br/>
注意：元组中只包含一个元素时，需要在元素后面添加逗号。<br/>

```
tup1 = ('physics', 'chemistry', 1997, 2000)
tup2 = (1, 2, 3, 4, 5 )
tup3 = "a", "b", "c", "d"
tup4 = (50,)
```
<br/>

**Tuple访问**

```
tup1 = ('physics', 'chemistry', 1997, 2000)
tup2 = (1, 2, 3, 4, 5, 6, 7 )
 
print("tup1[0]: ", tup1[0])
print("tup2[1:5]: ", tup2[1:5])
```
<br/>
输出结果：<br/>

```
tup1[0]:  physics
tup2[1:5]:  (2, 3, 4, 5)
```

**Tuple修改**

元组中的元素值是不允许修改的，但我们可以对元组进行连接组合：<br/>

```
tup1 = (12, 34.56)
tup2 = ('abc', 'xyz')
 
# tup1[0] = 100   #修改元组元素操作是非法的
 
# 创建一个新的元组
tup3 = tup1 + tup2
print(tup3)
```
<br/>
输出结果：<br/>

```
(12, 34.56, 'abc', 'xyz')
```

**Tuple删除**

元组中的元素值是不允许删除的，但我们可以使用del语句来删除整个元组:
```
tup = ('physics', 'chemistry', 1997, 2000)
 
print(tup)
del(tup)
print("After deleting tup : ")
print(tup)
```
<br/>
输出结果：<br/>

```
('physics', 'chemistry', 1997, 2000)
After deleting tup :
Traceback (most recent call last):
  File "test.py", line 9, in <module>
    print tup
NameError: name 'tup' is not defined
```

### 集合(Set)

集合（set）是一个无序的不重复元素序列。

**Set创建**
可以使用大括号 { } 或者 set() 函数创建集合，注意：创建一个空集合必须用 set() 而不是 { }，因为 { } 是用来创建一个空字典。:
>parame = {value01,value02,...} <br/>
或者set(value)<br/>

```
>>>basket = {'apple', 'orange', 'apple', 'pear', 'orange', 'banana'}
>>> print(basket)                      # 这里演示的是去重功能
{'orange', 'banana', 'pear', 'apple'}
>>> 'orange' in basket                 # 快速判断元素是否在集合内
True
>>> 'crabgrass' in basket
False
>>>a = {x for x in 'abracadabra' if x not in 'abc'}  #集合推导式(Set comprehension)
>>> a
{'r', 'd'}
```

**Set元素移除**

```
>>>thisset = set(("Google", "Runoob", "Taobao"))
>>> thisset.remove("Taobao")
>>> print(thisset)
{'Google', 'Runoob'}
>>> thisset.remove("Facebook")   # 使用remove()不存在会发生错误
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'Facebook'
```

推荐使用discard()，元素不存在时，不会发生错误。<br/>

```
>>>thisset = set(("Google", "Runoob", "Taobao"))
>>> thisset.discard("Facebook")  # 不存在不会发生错误
>>> print(thisset)
{'Taobao', 'Google', 'Runoob'}
```

随机删除集合中的一个元素<br/>
然而在交互模式，pop 是删除集合的第一个元素（排序后的集合的第一个元素）<br/>

```
thisset = set(("Google", "Runoob", "Taobao", "Facebook"))
x = thisset.pop()
print(x)
```
<br/>
输出结果：<br/>

```
Runoob
```

也可以清空集合<br/>

```
>>>thisset = set(("Google", "Runoob", "Taobao"))
>>> thisset.clear()
>>> print(thisset)
set()
```

**Set元素个数计算**

```
>>>thisset = set(("Google", "Runoob", "Taobao"))
>>> len(thisset)
3
```


**Set元素是否存在**

判断元素 x 是否在集合 s 中，存在返回 True，不存在返回 False：<br/>

```
>>>thisset = set(("Google", "Runoob", "Taobao"))
>>> "Runoob" in thisset
True
>>> "Facebook" in thisset
False
```

### 字典(Dictionary)

字典是另一种可变容器模型，且可存储任意类型对象。<br/>

**Dictionary创建**

字典的每个键值 key=>value 对用冒号 : 分割，每个键值对之间用逗号 , 分割，整个字典包括在花括号 {} 中：<br/>
d = {key1 : value1, key2 : value2 } <br/>
键一般是唯一的，如果重复最后的一个键值对会替换前面的，值不需要唯一。<br/>
值可以取任何数据类型，但键必须是不可变的，如字符串，数字或元组。<br/>
*学习笔记：<br/>

针对字典键的要求，还有更准确的说法是：<br/>
>python中什么对象不能作为字典的key：有__hash__方法可以做字典的key，没有则不能作为字典的key;<br/>
除了list、dict、set和内部至少带有上述三种类型之一的tuple之外，其余对象均可作为字典的key。

```
>>>dict = {'a': 1, 'b': 2, 'b': '3'}
>>> dict['b']
'3'
>>> dict
{'a': 1, 'b': '3'}
```

**Dictionary访问**

把相应的键放入方括弧。
```
dict = {'Name': 'Zara', 'Age': 7, 'Class': 'First'}
print("dict['Name']: ", dict['Name'])
print("dict['Age']: ", dict['Age'])
```
<br/>
输出结果：<br/>

```
dict['Name']:  Zara
dict['Age']:  7
```

如果用字典里没有的键访问数据，会输出错误如下：<br/>

```
dict = {'Name': 'Zara', 'Age': 7, 'Class': 'First'}
print("dict['Alice']: ", dict['Alice'])
```
<br/>
输出结果：<br/>
```
dict['Alice']: 
Traceback (most recent call last):
  File "test.py", line 5, in <module>
    print "dict['Alice']: ", dict['Alice']
KeyError: 'Alice'
```

为了避免这种异常，可以使用collections.defaultdict()方法创建带默认值的dictionary。<br/>
```
from collections import defaultdict
d2 = defaultdict(lambda :'default value')
d2['one'] = 1
d2['two'] = 2
print(d2['two'])
print(d2['three'])
```
<br/>
输出结果：<br/>
```
2
default value
```

**Dictionary修改**

向字典添加新内容的方法是增加新的键/值对，修改或删除已有键/值对:<br/>
```
dict = {'Name': 'Zara', 'Age': 7, 'Class': 'First'}
dict['Age'] = 8 # 更新
dict['School'] = "RUNOOB" # 添加 
print("dict['Age']: ", dict['Age'])
print("dict['School']: ", dict['School'])
```
<br/>
输出结果：<br/>

```
dict['Age']:  8
dict['School']:  RUNOOB
```

**Dictionary删除**

*   删除字典元素
*   删除字典

```
dict = {'Name': 'Zara', 'Age': 7, 'Class': 'First'}
 
del dict['Name']  # 删除键是'Name'的条目
dict.clear()      # 清空词典所有条目
del dict          # 删除词典
```


## 总结
&ensp;&ensp;&ensp;&ensp;今天学习了Python中常用的4种数据结构，它们有不同的存储结构和特点。除了记录的几种常用的创建、访问、修改、删除等基本操作，每一种数据结构的类还提供了丰富的方法来方便操作。<br/>
&ensp;&ensp;&ensp;&ensp;在编写代码的过程中，也已经使用到了一部分，最重要的是需要使用什么操作时候，使用help来查看具体的方法说明，可以边用边学。

## 参考资料
此次学习主要依赖于下面技术网站:<br/> 
http://www.runoob.com/python3/python3-tutorial.html <br/>
