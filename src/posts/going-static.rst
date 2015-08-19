((title . "Going Static")
 (date . "2015-08-18 10:27:54"))

I'm not sure whether you did notice, but as the amount of whimsy posts
grew, the page load times became increasingly intolerable.  This is
because this blog was based on a Python script dynamically generating
its responses from plain text files, an approach I've chosen because I
grew frustrated with the static blog generator solutions I've
evaluated at that time.

Hyde_ is different.  It doesn't try to be particularly smart about
`cache invalidation techniques`_ or to impose `yet another irregular
templating language`_ on you.  Adding support for translating my
existing posts was as simple as writing a few lines of Scheme_.
That's why I went the full route and rebuilt my blog in nearly all of
its entirety with it.  Full sources are `available on Github`_.

Given the low amount of hits for category-specific views, feeds and
pagination links, I've decided to kill off these features.
Unfortunately all old links are broken, so please update to the new
`Atom feed`_ URL in your news feed reader.

edit: Nginx_ is pretty amazing:

.. code:: nginx

    location / {
        rewrite ^/feed/?(elpa|emacs|other)?$ /feed.atom permanent;
        rewrite ^/post/(.*)$ /posts/$1.html permanent;
    }

In other words, nearly all links will keep working.  I could perhaps
add some manual exceptions for the cases where the different
slugification_ algorithm did make a difference in URL...

.. _Hyde: http://wiki.call-cc.org/eggref/4/hyde
.. _cache invalidation techniques: http://martinfowler.com/bliki/TwoHardThings.html
.. _yet another irregular templating language: http://www.more-magic.net/posts/structurally-fixing-injection-bugs.html
.. _Scheme: http://call-cc.org/
.. _available on Github: https://github.com/wasamasa/emacshorrors.com
.. _Atom feed: http://emacshorrors.com/feed.atom
.. _Nginx: http://nginx.org/en/docs/http/ngx_http_rewrite_module.html
.. _slugification: https://kerihena.re/notebook/2009/jul/23/fancy-urls-and-slugs
