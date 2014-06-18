#!/usr/bin/env python3

from datetime import datetime
from math import ceil
from pathlib import Path
from urllib.parse import urljoin

import docutils.core
import docutils.io
import docutils.nodes
from docutils.writers import html4css1
import flask
from pyphen import Pyphen
from werkzeug.contrib.atom import AtomFeed


# TODO investigate in other deployment options compatible with
# uberspace like FCGI (or [U]WSGI for other hosters)
# TODO tags, categories, both + template support
# TODO fix imprint design
# TODO improve privacy statement
# TODO turn feed into feeds and implement tag-specific feeds
# TODO abstract these: about, imprint, privacy statement
# TODO add license hint
# TODO latex/mathjax? svg?

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


def ensure_metadata(metadata):
    """If the metadata is well-formed, return True."""
    if 'title' in metadata and 'date' in metadata:
        return True
    return False


@app.template_filter()
def displayed_datetime(timestamp):
    return datetime.strptime(timestamp, EXACT_FORMAT).strftime(DISPLAY_FORMAT)


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
    post_filenames = [str(p) for p in Path('posts').glob('*.rst')]
    posts = []
    for post_filename in post_filenames:
        slug = Path(post_filename).stem
        metadata, content = parse_post(post_filename)
        if ensure_metadata(metadata):
            post = metadata
            post['content'] = content
            post['slug'] = slug
            if 'tags' in post:
                post['tags'] = post['tags'].split(', ')
            posts.append(post)
    return posts


def processed_posts(posts, **criteria):
    """Sort and filter posts by the given criteria."""
    filtered_posts = [post for post in posts if fits_criteria(post, criteria)]
    sorted_posts = sorted(filtered_posts, key=lambda post: post['date'])
    if 'reverse' in criteria:
        return list(reversed(sorted_posts))
    return sorted_posts


def fits_criteria(post, criteria):
    """Check whether a posts fits all criteria."""
    return all([fits_criterium(post, key, value) for key, value in criteria.items()])


def fits_criterium(post, key, value):
    """Check whether a posts fits a criterium defined by key and value."""
    if key == 'published' and 'published' in post and 'date' in post:
        timedelta = (datetime.now() - datetime.strptime(
            post['date'], EXACT_FORMAT)).total_seconds() > 0
        condition = post['published'] and timedelta
        if value:
            return condition
        else:
            return not condition
    elif key == 'tags' and 'tags' in post:
        return any(tag in post['tags'] for tag in value)
    else:
        # ignore non-existant keys
        return True


def reverse_chunks(items, pagination):
    """Pagination helper function.

    >>> reverse_chunks(list(range(1, 11)), 4)
    [[10, 9, 8, 7], [6, 5, 4, 3], [2, 1]]"""
    chunks = []
    pages = ceil(len(items) / pagination)
    if pages <= 1:
        chunks.append(reversed(items))
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
    posts = processed_posts(parse_posts(), published=True)
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


@app.route('/unpublished')
def show_unpublished():
    """Display unpaginated view of unpublished posts."""
    posts = processed_posts(parse_posts(), published=False, reverse=True)
    if posts:
        return flask.render_template('unpublished.tmpl', posts=posts)
    else:
        return flask.render_template('error.tmpl', error="No unpublished posts")


@app.route('/archive')
def show_archive():
    """Display an archive of all posts."""
    posts = processed_posts(parse_posts(), published=True, reverse=True)
    if posts:
        return flask.render_template('archive.tmpl', posts=posts)
    else:
        return flask.render_template('error.tmpl', error="No posts yet")


@app.route('/feed')
def atom_feed():
    """Display an atom feed of all published posts."""
    posts = processed_posts(parse_posts(), published=True, reverse=True)
    atom_feed = AtomFeed(
        title='My Blog', title_type='text', author='Vasilij Schneidermann',
        subtitle='Technical Writings', url=flask.request.url,
        feed_url=flask.request.url_root)
    for post in posts[:10]:
        title = post['title']
        content = post['content'].replace('­', '')
        url = urljoin(flask.request.url_root, '/posts/{}'.format(post['slug']))
        updated = datetime.strptime(post['date'], '%Y-%m-%d %H:%M:%S')
        published = datetime.strptime(post['date'], '%Y-%m-%d %H:%M:%S')
        atom_feed.add(
            title=title, title_type='text', content=content, content_type='html',
            url=url, updated=updated, published=published)
    if posts:
        return atom_feed.to_string()
    else:
        return flask.render_template('error.tmpl', error="No posts yet")


@app.route('/about')
def show_about():
    """Display an about page."""
    return flask.render_template('about.tmpl')


@app.route('/imprint')
def show_imprint():
    """Display an imprint page."""
    return flask.render_template('imprint.tmpl')


@app.route('/privacy')
def show_privacy():
    """Display a privacy statement page."""
    return flask.render_template('privacy.tmpl')


@app.errorhandler(404)
def page_not_found(error):
    """404 error handler."""
    return flask.render_template('error.tmpl', error="404 Page not found"), 404


if __name__ == '__main__':
    app.run(debug=True)
