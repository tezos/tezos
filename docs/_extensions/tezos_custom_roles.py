from docutils import nodes
import os
import os.path

def setup(app):
    app.add_role('package', package_role)

def package_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    rel_lvl = inliner.document.current_source.replace(os.getcwd(),'').count('/')
    if not os.path.exists('_build/api/odoc/'+text):
        raise ValueError('opam package ' + text + ' does not exist in the odoc')
    url = "api/api-inline.html#" + text + '/index.html'
    for i in range(1,rel_lvl):
        url = '../' + url
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], []
