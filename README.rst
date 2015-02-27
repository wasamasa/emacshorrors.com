This started out as an experiment after growing frustrated with buggy
static blog generators that felt painful for requiring you to
constantly rebuild locally to see changes taking effect.  However I
didn't want to have a database-backed dynamic blog where I'd need to
edit posts in a web interface either.

In the end I've settled for taking `ReStructured Text`_ documents and
parsing their contents and metadata on every view.  Let's just hope
Hackernews_ won't discover this blog.

.. _ReStructured Text: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html
.. _Hackernews: https://news.ycombinator.com/
