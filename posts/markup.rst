:title: Markup
:date: 2015-02-25 12:35:37
:published: no
:category: test

.. role:: strike
   :class: strike

Normal text.

    Blockquote.

        More blockquoting.

*Italic*, **Bold**, ``Literal``, :strike:`Crossed out` [1]_. Some more
normal text following below.  Nothing fancy to see here.  Another line
of text without any weird elements in it.

1. First things first.
2. Other things second.

* Apple
* Banana

.. code:: c

    #include <stdio.h>

    int main(void) {
        printf("Hello World!\n"); // prints "Hello World!"
        return 0;
    }

`Inline URL <http://www.google.com/>`_.  Google_. `Search Engine`_.

.. image:: http://rib.aibor.de/images/dealwithit.gif

==== ======
 ID   Value
==== ======
 1    foo
---- ------
 2    bar
==== ======

.. [1] Actually, a ``<span>`` tag with a CSS style that puts a line
       through the enclosed text.

.. _Google: http://www.google.com/
.. _Search Engine: http://www.google.com/
