#!/usr/bin/env python3

from datetime import datetime
from math import ceil
from pathlib import Path

import docutils.core
import docutils.io
import docutils.nodes
from docutils.writers import html4css1
import flask
from pyphen import Pyphen


# TODO investigate in other deployment options compatible with
# uberspace like FCGI (or [U]WSGI for other hosters)
# TODO add route for atom feed made with flask contrib module

# deployment blurb
app = flask.Flask(__name__)


class HTML5Writer(html4css1.Writer):
    """Writer class, will emit HTML5 soon."""
    def __init__(self):
        html4css1.Writer.__init__(self)
        self.translator_class = HTML5Translator


class HTML5Translator(html4css1.HTMLTranslator):
    """Translator class, will translate HTML4 to HTML5 soon.
    It only hyphenates text at the moment."""
    pyphen = Pyphen(lang='en')

    def visit_Text(self, node):
        text = node.astext()
        text = ' '.join([self.pyphen.inserted(word, hyphen='­')
                         for word in text.split(' ')])
        encoded = self.encode(text)
        self.body.append(encoded)


def ensure_metadata(metadata, published=True):
    """If the metadata is well-formed, return True."""
    if 'title' in metadata and 'date' in metadata and 'published' in metadata:
        timedelta = (datetime.now() - datetime.strptime(
            metadata['date'], '%Y-%m-%d %H:%M:%S')).total_seconds() > 0
        if metadata['published'] == 'yes' and timedelta:
            if published:
                return True
            else:
                return False
        elif metadata['published'] == 'no' or not timedelta:
            if published:
                return False
            else:
                return True
    return False


@app.template_filter()
def approximate_datetime(timestamp):
    """Jinja2 filter that turns a timestamp into the approximate datetime."""
    past = datetime.strptime(timestamp, "%Y-%m-%d %H:%M:%S")
    now = datetime.now()
    delta = (now - past).total_seconds()

    times = [
        {'delta': 60, 'format_string': "Just now", 'factor': None},
        {'delta': 120, 'format_string': "A minute ago", 'factor': None},
        {'delta': 3600, 'format_string': "{} minutes ago", 'factor': 60},
        {'delta': 7200, 'format_string': "An hour ago", 'factor': None},
        {'delta': 86400, 'format_string': "{} hours ago", 'factor': 3600},
        {'delta': 172800, 'format_string': "A day ago", 'factor': None},
        {'delta': 2592000, 'format_string': "{} days ago", 'factor': 86400},
        {'delta': 5184000, 'format_string': "A month ago", 'factor': None},
        {'delta': 31104000, 'format_string': "{} months ago", 'factor': 2592000},
        {'delta': 62208000, 'format_string': "A year ago", 'factor': None}
    ]

    for time in times:
        if delta < time['delta']:
            if time['factor']:
                return time['format_string'].format(int(delta / time['factor']))
            else:
                return time['format_string']
    return "{} years ago".format(int(delta / 31104000))


def parse_post(path):
    """Parse a ReST post, return metadata and content."""
    metadata = {}
    doctree = docutils.core.publish_doctree(
        None, source_class=docutils.io.FileInput, source_path=path)
    docinfos = doctree.traverse(docutils.nodes.docinfo)
    for docinfo in docinfos:
        for child in docinfo.children:
            if child.tagname == 'field':
                tag, content = child.children
                metadata[tag.astext()] = content.astext()
            else:
                metadata[child.tagname] = child.astext()

    content = docutils.core.publish_parts(
        None, source_class=docutils.io.FileInput,
        source_path=path, writer=HTML5Writer())['body']
    return metadata, content


def parse_posts():
    """Parse all valid ReST posts."""
    post_filenames = Path('posts').glob('*.rst')
    unsorted_posts = []
    for post_filename in post_filenames:
        slug = Path(post_filename).stem
        metadata, content = parse_post(post_filename)
        if ensure_metadata(metadata):
            post = metadata
            metadata['content'] = content
            metadata['slug'] = slug
            unsorted_posts.append(post)

    posts = []
    if post_filenames:
        posts = sorted(unsorted_posts, key=lambda post: post['date'])
    return posts


def reverse_chunks(items, pagination):
    """Pagination helper function.

    >>> reverse_chunks(list(range(1, 11)), 4)
    [[10, 9, 8, 7], [6, 5, 4, 3], [2, 1]]"""
    chunks = []
    pages = ceil(len(items) / pagination)
    if pages <= 1:
        chunks.append(items)
    else:
        start = len(items) - pagination
        end = len(items)
        for page in range(1, pages+1):
            chunks.append(list(reversed(items[start:end])))
            start = max(0, start - pagination)
            end -= pagination
    return chunks


@app.route('/')
@app.route('/posts')
@app.route('/posts/<int:page>')
def show_index(page=None):
    """Display the appropriate paginated page.
    If the page is None, display the first page."""
    posts = parse_posts()
    if posts:
        pagination = 5
        if not page:
            page = 1
        pages = reverse_chunks(posts, pagination)
        if page in range(1, len(pages)+1):
            posts = pages[page-1]
            old, new = False, False
            if len(pages) > 1:
                if page != len(pages):
                    old = True
                if page != 1:
                    new = True
            return flask.render_template('posts.tmpl', posts=posts,
                                         page=page, old=old, new=new)
        else:
            return flask.render_template('error.tmpl', error="Invalid index")
    else:
        return flask.render_template('error.tmpl', error="No posts yet")


# add route for unpublished posts


@app.route('/about')
def show_about():
    """Display an about page."""
    return flask.render_template('about.tmpl')


@app.route('/archive')
def show_archive():
    """Display an archive of all posts."""
    posts = parse_posts()
    if posts:
        return flask.render_template('archive.tmpl', posts=posts)
    else:
        return flask.render_template('error.tmpl', error="No posts yet")


@app.route('/post/<post_slug>')
def show_post(post_slug):
    """Display a single post."""
    slug_path = Path('posts') / Path('{}.rst'.format(post_slug))
    if slug_path.exists():
        metadata, content = parse_post(str(slug_path))
        if ensure_metadata(metadata):
            title = metadata['title']
            date = metadata['date']
        return flask.render_template(
            'post.tmpl', title=title, date=date, content=content)
    else:
        return flask.render_template('error.tmpl', error="No such post")


@app.errorhandler(404)
def page_not_found(error):
    """404 error handler."""
    return flask.render_template('error.tmpl', error="404 Page not found"), 404


# more boilerplate code
if __name__ == '__main__':
    app.run(debug=True)
