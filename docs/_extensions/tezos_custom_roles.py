from docutils import nodes
import os
import os.path
import re

def setup(app):
    app.add_role('package', package_role)
    app.add_role('package-name', package_role)
    app.add_role('package-src', package_role)
    app.add_role('opam', opam_role)
    app.add_role('src', src_role)

def find_dot_opam(name):
    for path, dirs, files in os.walk('..'):
        for file in files:
            if file == name + '.opam':
                return path.lstrip('../')
    raise ValueError('opam file ' + name + '.opam does not exist in the odoc')

def package_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    rel_lvl = inliner.document.current_source.replace(os.getcwd(),'').count('/')
    parts = re.match("^([^<>]*)<([^<>]*)>$", text)
    if parts:
      text = parts.group(2)
      lib = parts.group(1)
    else:
      lib = text
    src = find_dot_opam(lib)
    branch = os.environ.get('CI_COMMIT_REF_NAME', 'master')
    project_url = os.environ.get('CI_PROJECT_URL', 'https://gitlab.com/tezos/tezos')
    src_url = project_url + "/tree/" + branch + "/" + src
    if os.path.isdir('_build/api/odoc/_html/'+lib):
      if os.path.isdir(os.path.join('_build','api','odoc','_html',lib,lib.replace('-','_').capitalize())):
        lib = lib + '/' + lib.replace('-','_').capitalize()
      url = "api/api-inline.html#" + lib + '/index.html'
      for i in range(1,rel_lvl):
        url = '../' + url
    else:
      url = src_url
    if name == 'package':
      node = nodes.reference(rawtext, text, refuri=url, **options)
    elif name == 'package-name':
      node = nodes.literal(text, text)
    elif name == 'package-src':
      node = nodes.reference(rawtext, src, refuri=src_url, **options)
    return [node], []

def opam_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    rel_lvl = inliner.document.current_source.replace(os.getcwd(),'').count('/')
    parts = re.match("^([^<>]*)<([^<>]*)>$", text)
    if parts:
      text = parts.group(2)
      lib = parts.group(1)
    else:
      lib = text
    tagged = re.match('([^.]+)[.].*', lib)
    if tagged:
      url = "https://opam.ocaml.org/packages/" + tagged.group(1) + "/" + lib
    else:
      url = "https://opam.ocaml.org/packages/" + lib
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], []

def src_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    rel_lvl = inliner.document.current_source.replace(os.getcwd(),'').count('/')
    parts = re.match("^([^<>]*)<([^<>]*)>$", text)
    if parts:
      text = parts.group(2)
      src = parts.group(1)
    else:
      src = text
      text = text
    branch = os.environ.get('CI_COMMIT_REF_NAME', 'master')
    project_url = os.environ.get('CI_PROJECT_URL', 'https://gitlab.com/tezos/tezos')
    url = project_url + "/tree/" + branch + "/" + src
    node = nodes.reference(rawtext, text, refuri=url, **options)
    return [node], []
