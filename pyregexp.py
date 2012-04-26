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


# True if we are running on Python 3.
PY3 = sys.version_info[0] == 3

if not PY3:
    import codecs
    sys.stdout = codecs.getwriter('utf-8')(sys.stdout)
    sys.stdin = codecs.getreader('utf-8')(sys.stdin)

argv = sys.argv

# not using argparse because it is not in the stdlib of python2.7/3.1.
BOOL_ARGS = ('--eval', '--feedback', )
STR_ARGS = ('--regexp', '--replace', )
INT_ARGS = ('--feedback-limit', )
def parse_arg(arg, required=True):
    if not required and arg not in sys.argv:
        return None
    if arg in BOOL_ARGS:
        return arg in sys.argv
    def lookahead():
        try:
            return sys.argv[sys.argv.index(arg)+1]
        except ValueError:
            raise Exception("Argument missing: %s" % arg)
    if arg in STR_ARGS:
        return lookahead()
    if arg in INT_ARGS:
        return int(lookahead())
    raise Exception("Unrecognized argument: %s" % arg)

def escape(s):
    return s.replace('\\','\\\\').replace('\n', '\\n')

def message(msg):
    sys.stdout.write(escape(msg))
    sys.stdout.write('\n')

if argv[1] == 'matches':
    # output positions of matches

    regexp = parse_arg('--regexp')
    region = sys.stdin.read()

    if not PY3:
        regexp = regexp.decode('utf-8')

    feedback_limit = parse_arg('--feedback-limit', required=False)
    try:
        matches = list(re.finditer(regexp, region))
        for i, match in enumerate(matches):
            if feedback_limit is not None and i >= feedback_limit:
                break
            # show only if match length is nonzero
            if match.start() != match.end(): 
                sys.stdout.write(' '.join("%s %s" % span for span in match.regs))
                sys.stdout.write('\n')
        if matches:
            message("%d occurences" % len(matches))
        else:
            message("no match")
    except re.error as e:
        message("Invalid: %s" % e)

elif argv[1] == "replace":
    regexp = parse_arg('--regexp')
    replace = parse_arg('--replace')
    do_eval = parse_arg('--eval')
    feedback = parse_arg('--feedback')
    feedback_limit = parse_arg('--feedback-limit', required=False)
    region = sys.stdin.read()

    if not PY3:
        regexp = regexp.decode('utf-8')
        replace = replace.decode('utf-8')

    if do_eval:
        # use \1, \2 instead of m.group(0), m.group(1), ...
        replace = re.sub(r'\\(\d+)', r'm.group(\1)', replace)
    match_counter = [0]

    def eval_replace(match):
        _globals = {}
        # those variables can be used in the replacement expression
        _locals = {
            'm': match,
            'i': match_counter[0],
            }
        
        replacement = eval(replace, _globals, _locals)
                
        match_counter[0] += 1
        return replacement

    try:
        # output one replacement per line 
        matches = list(re.finditer(regexp, region))
        for i, match in enumerate(matches):
            # output replacement in feedback-mode only when the match length is nonzero
            if feedback and feedback_limit is not None and i >= feedback_limit:
                break
            if not feedback or match.start() != match.end():
                sys.stdout.write("%s %s " % match.span())
                sys.stdout.write(re.sub(regexp, eval_replace if do_eval else replace, match.group(0), count=1,flags=re.UNICODE))
                sys.stdout.write('\n')
        if feedback:
            if matches:
                message("%d occurences" % len(matches))
            else:
                message("no match")
        else:
            message("replaced %d occurences" % len(matches))
    except Exception as e:
        message("Invalid: %s" % e)
