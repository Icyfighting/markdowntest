---
layout: post
title: "Producer-Consumer model"
subtitle: "KFC vs McDonald's "
author: "Bing Yan"
header-img: "img/deadlock/post-bg-java.jpg"
header-mask: 0.2
catalog: true
tags:
  - Java
  - Learning
---
## 前言
相信多数人都进过肯德基和麦当劳消费，笔者进店消费的时候发现他们的点单流程和并发模型十分接近。虽然每家店的流程有所差异，但是大概就只有两种模型。在肯德基里，你点单之后点单员会把所点的食物完成封装之后拿来你面前，然后让你结账，有时候有些耗时操作没完成就会留下一个餐台号稍后送来。而在麦当劳的点餐模型大致是，你点完快餐之后要求你立即付款，付完款之后下一位点餐，而取餐的是在旁边等待，另一个服务员专责负责配餐。

KFC：<br/>
![](/img/producer/KFC.png)

McDonald's：<br/>
![](/img/producer/McDonald.png)

在并发模型中，肯德基比较倾向于一个线程把所有的服务都做完，而麦当劳倾向于服务解耦，让他们更专注于自己的业务。而肯德基的模型与BIO服务器的模型设计类似，麦当劳的模型则与生产者消费者模型十分相似。<br/>
<br/>
而在大型电商网站中，他们的服务或者应用解耦之后，是通过消息队列在彼此间通信的。消息队列和应用之间的架构关系就是生产者消费者模型。

## 正文
### 生产者消费者模型(Producer-Consumer model)

生产者消费者模型具体来讲，就是在一个系统中，存在生产者和消费者两种角色，他们通过内存缓冲区进行通信，生产者生产消费者需要的资料，消费者把资料做成产品。生产消费者模式如下图:

![](/img/producer/model.png)

在日益发展的服务类型中，譬如注册用户这种服务，它可能解耦成好几种独立的服务（账号验证，邮箱验证码，手机短信码等）。它们作为消费者，等待用户输入数据，在前台数据提交之后会经过分解并发送到各个服务所在的url，分发的那个角色就相当于生产者。消费者在获取数据时候有可能一次不能处理完，那么它们各自有一个请求队列，那就是内存缓冲区了。做这项工作的框架叫做消息队列。

### 生产者消费者模型的实现
**5种实现方法**

*   用synchronized对存储加锁，然后用object原生的wait() 和 notify()做同步
*   用concurrent.locks.Lock，然后用condition的await() 和signal()做同步
*   直接使用concurrent.BlockingQueue
*   使用PipedInputStream/PipedOutputStream
*   使用信号量semaphore



**synchronized实现方法代码**

作为学习之初，这个实现方法比较容易理解。<br/>
对于其他几种方法，也通过查找资料进行了一定的了解。<br/>
这里重在通过模型实现理解生产者消费者模型的原理。

```
import java.util.LinkedList;
import java.util.Queue;

public class ProducerAndConsumer {
    private final int MAX_LEN = 10;
    private Queue<Integer> queue = new LinkedList<Integer>();
    class Producer extends Thread {
        @Override
        public void run() {
            producer();
        }
        private void producer() {
            while(true) {
                synchronized (queue) {
                    while (queue.size() == MAX_LEN) {
                        queue.notify();
                        System.out.println("当前队列满");
                        try {
                            queue.wait();
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                    queue.add(1);
                    queue.notify();
                    System.out.println("生产者生产一条任务，当前队列长度为" + queue.size());
                    try {
                        Thread.sleep(500);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }
    class Consumer extends Thread {
        @Override
        public void run() {
            consumer();
        }
        private void consumer() {
            while (true) {
                synchronized (queue) {
                    while (queue.size() == 0) {
                        queue.notify();
                        System.out.println("当前队列为空");
                        try {
                            queue.wait();
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                    queue.poll();
                    queue.notify();
                    System.out.println("消费者消费一条任务，当前队列长度为" + queue.size());
                    try {
                        Thread.sleep(500);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }
    public static void main(String[] args) {
        ProducerAndConsumer pc = new ProducerAndConsumer();
        Producer producer = pc.new Producer();
        Consumer consumer = pc.new Consumer();
        producer.start();
        consumer.start();
    }
}
```

## 总结
生产者消费者模式 为信息传输开辟了一个崭新的概念，因为它的优先级最高，所以即使网络发生堵塞时它也会最先通过，最大程度的保证了设备的安全。<br/>
也有缺点，就是在网络中的个数是有限制的。<br/>
生产者消费者模式在设置时比较简单，使用方便安全，在将来的自动化行业必定会大大被人们所认同。<br/>
生产者消费者模式，其实只要保证在存储端同一时刻只有一个线程读或写就不会有问题，然后再去考虑线程同步。<br/>
方法1、2、5都比较类似，都是加锁来限制同一时刻只能有一个读或写。<br/>
而方法3、4其实是在存储内部去保证读和写的唯一的，最低层肯定还是通过锁机制来实现的，java底层代码都封装好了而已。

对于此模型的其他几种实现方法，可以参考如下内容：
https://juejin.im/entry/596343686fb9a06bbd6f888c
