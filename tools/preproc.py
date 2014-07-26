#!/usr/bin/python

"""A pre-processor for GHC/GCC.

Usage: preproc.py < foo.xghc > foo.ghc
"""


import re
import sys


RE_DEFINE_CONST = re.compile(r'^#define ([A-Za-z0-9_]+)\s+(.*)$')
RE_DEFINE_LABEL = re.compile(r'^:([A-Za-z0-9_]+)$')

RE_REFERENCE = re.compile(r'@([A-Za-z0-9_]+)')


def main():
    defines = {}
    program = []
    for line in sys.stdin:
        # Strip the command and leading/trailing space.
        line = line.split(';', 1)[0].strip()
        if not line:
            continue
        m_const = RE_DEFINE_CONST.match(line)
        if m_const:
           defines[m_const.group(1)] = m_const.group(2)
           continue
        m_label = RE_DEFINE_LABEL.match(line)
        if m_label: 
            defines[m_label.group(1)] = str(len(program))
            continue
        program.append(line)

    for line in program:
        m = RE_REFERENCE.search(line)
        if m:
            line = ''.join([line[:m.start()], defines[m.group(1)], line[m.end():]])
        sys.stdout.write(line + '\n')


if __name__ == '__main__':
    main()
