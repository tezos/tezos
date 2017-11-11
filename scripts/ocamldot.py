#!/usr/bin/python

import re
import sys
import os
import argparse
from sets import Set

alldeps={}
allmodules={}

def sanitize(s):
    s=re.sub('.*/','',s)
    s=re.sub('[^0-9a-zA-Z]+', '_', s)
    return s

def cleanName(s):
    ml = os.path.basename(s)
    (mod,_) = os.path.splitext(ml)
    return mod.capitalize()

def mangle(f,modulename):

    dictionary = {}
    for line in open(f):
        s=line.split();
        for x in range(2, len(s)):
            mod = cleanName(s[0])
            dep = sanitize(s[x])
            if mod in dictionary :
                dictionary[mod].append(dep)
            else :
                dictionary[mod] = [dep]
            allmodules.update({mod : modulename})

    alldeps[modulename] = dictionary

def cleanup(alldeps):
    # remove references to external libraries
    for (name,dictionary) in alldeps.iteritems() :
        for (mod,deps) in dictionary.iteritems() :
            dictionary[mod] = [x for x in deps if x in allmodules]
        alldeps[name] = dictionary
    return alldeps

def print_graph(alldeps):
    print("strict digraph G {")
    print('graph [fontsize=10 fontname="Verdana"];')
    print('node [shape=record fontsize=10 fontname="Verdana" compound=true];')
    counter = 0
    l = { x: i for i,x in enumerate(alldeps.keys())}
    for (name,dictionary) in alldeps.iteritems() :
        names = ['"%s"' % mod for mod in dictionary.keys()]
        if len(names) > 0 :
            print ('subgraph cluster_%i { label = "%s"; color=blue; node [style=filled];' % (l[name],name))
            counter += 1
            for (mod,deps) in dictionary.iteritems() :
                for dep in deps :
                    if dep in dictionary :
                        print ('"%s" -> "%s";' % (mod,dep))
            print "}"
            # for (mod,deps) in dictionary.iteritems() :
                # for dep in deps :
                    # if dep not in dictionary :
                        # print ('"%s" -> "%s" [ltail=cluster_%i lhead=cluster_%i];' % (mod,dep,l[name],l[allmodules[dep]]))
    print "}"

def scan(directories):
    ext = ".depends.ocamldep-output"
    for directory in directories:
        for root, dirs, files in os.walk(directory):
            for f in files:
                if f.endswith(ext):
                     mangle(os.path.join(root, f),f[:-len(ext)])
    print_graph(cleanup(alldeps))

def main():
    parser = argparse.ArgumentParser(description='OcamlDep Dependency Tree')
    parser.add_argument('inputdirs', type=str, nargs='*', help="directories to scan")
    args = parser.parse_args()

    scan(args.inputdirs)

if __name__ == '__main__':
    main()
