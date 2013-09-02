chat_server
===========

Build:

~~~bash
$ make
~~~

Setup 3 node cluster:

Node 1:
~~~bash
$ make run NODE=chat1
~~~

Node 2:
~~~bash
$ make run NODE=chat2
~~~

Node 3:
~~~bash
$ make run NODE=chat3
~~~

Start service:

Node1:
~~~bash
$ (chat1@localhost)1> chat_server:start([node(), 'chat2@localhost', 'chat3@localhost']).
~~~

Enjoy!!!

