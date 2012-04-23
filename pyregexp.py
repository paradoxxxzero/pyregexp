# Copyright (C) 2012 Marko Bencun
#
# Author : benma <mbencun@gmail.com>
# URL : https://github.com/benma/pyregexp/
# Version : 0.1
# Keywords : regexp, replace, python
#
# This file is part of pyregexp.
#
# pyregexp is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# pyregexp is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with pyregexp.  If not, see <http://www.gnu.org/licenses/>.

import sys, re
argv = sys.argv

# True if we are running on Python 3.
PY3 = sys.version_info[0] == 3

def escape(s):
    return s.replace('\\','\\\\').replace('\n', '\\n')

def message(msg):
    sys.stdout.write(escape(msg))
    sys.stdout.write('\n')

if argv[1] == 'matches':
    # output positions of matches

    regex = argv[2]
    region = sys.stdin.read()

    try:
        c = 0
        for match in re.finditer(regex, region):
            c += 1
            # show only if match length is nonzero
            if match.start() != match.end(): 
                sys.stdout.write(' '.join("%s %s" % span for span in match.regs))
                sys.stdout.write('\n')
        if c:
            message("%d occurences" % c)
        else:
            message("no match")
    except re.error as e:
        message("Invalid: %s" % e)

elif argv[1] == "replace":
    regex = argv[-2]
    replace = argv[-1]
    if not PY3:
        replace = replace.decode('utf-8')
    do_eval = '--eval' in argv
    feedback = '--feedback' in argv
    if do_eval:
        # use \1, \2 instead of m.group(0), m.group(1), ...
        replace = re.sub(r'\\(\d+)', r'm.group(\1)', replace)
    region = sys.stdin.read()
    i = [0]

    def eval_replace(match):
        _globals = {}
        # those variables can be used in the replacement expression
        _locals = {
            'm': match,
            'i': i[0],
            }
        if PY3:
            replacement = str(eval(replace, _globals, _locals))
        else:
            replacement = unicode(eval(replace, _globals, _locals)).encode('utf-8')
        i[0] += 1
        return replacement

    try:
        # output one replacement per line 
        c = 0
        for match in re.finditer(regex, region):
            c += 1
            # output replacement in feedback-mode only when the match length is nonzero
            if not feedback or match.start() != match.end():
                sys.stdout.write("%s %s %%s" % match.span() % escape(re.sub(regex, eval_replace if do_eval else replace, match.group(0), count=1)))
                sys.stdout.write('\n')
        if feedback:
            if c:
                message("%d occurences" % c)
            else:
                message("no match")
        else:
            message("replaced %d occurences" % c)
    except Exception as e:
        message("Invalid: %s" % e)
