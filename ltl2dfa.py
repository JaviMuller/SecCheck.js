import argparse
import json
import re
from ltlf2dfa.parser.ltlf import LTLfParser
from ltlf2dfa.parser.pltlf import PLTLfParser


def dfa_to_json(file_content):
    lines = file_content.strip().split('\n')

    # Extract variables
    variables = list(map(lambda x : x.lower(), 
                  re.findall(r'DFA for formula with free variables: ([A-Z ]+)', lines[0])[0].split()))

    # Extract initial state
    initial_state = int(re.findall(r'Initial state: (\d+)', lines[1])[0])
    
    # Extract accepting states
    accepting_states = list(map(int, re.findall(r'Accepting states: ([\d+ ]+)', lines[2])[0].split()))
    
    # Extract number of states
    states = int(re.findall(r'Automaton has (\d+) states', lines[5])[0])
    
    # Extract transitions
    transitions = []

    for line in lines[7:]:
      tr = re.match(r'State (\d+): ([01X]+) -> state (\d+)', line)
      if tr:
        src, label, dest = int(tr.group(1)), tr.group(2), int(tr.group(3))
        label_true = re.finditer(r'1', label)
        matches = 0
        label = "*"
        for match in label_true:
          label = variables[match.start()]
          matches += 1
        # We remove the transitions that require more than one action, as our model does not allow it
        if matches > 1: 
          continue
        transitions += [{
          "src"  : src,
          "dest" : dest,
          "lbl"  : label
        }]
    
    return {
      "vars"        : variables,
      "states"      : states,
      "init_state"  : initial_state,
      "acc_states"  : accepting_states,
      "transitions" : transitions
    }

if __name__ == '__main__':
  cmdline = argparse.ArgumentParser(description='Usage: ltlf2dfa -l {ltlf | ppltl} -f <path/to/formula>')

  cmdline.add_argument('--lang',
                      '-l', 
                      choices=['ltlf', 'ppltl'],
                      required=True,
                      help='LTL mode')

  group = cmdline.add_mutually_exclusive_group(required=True)
  group.add_argument('--file',
                    '-f',
                    type=str,
                    help='Input formula filename')
  group.add_argument('--string',
                    '-s',
                    type=str,
                    help='Input formula string')
  cmdline.add_argument('--output',
                      '-o',
                      choices=['mona', 'dot', 'json'],
                      default='json',
                      help='Output format')

  args = cmdline.parse_args()

  # Choose formula parser (LTLf | PPLTL)
  if args.lang == 'ltlf':
    parser = LTLfParser()
  else:
    parser = PLTLfParser()

  # Choose input method (file | string)
  if args.file:
    file = open(args.file)
    formula = parser(file.read())
  else:
    formula = parser(args.string)

  # Choose output method (mona | dot)
  if args.output == 'json':
    out = formula.to_dfa(mona_dfa_out=True)
    out = json.dumps(dfa_to_json(out), indent=2)
  elif args.output == 'mona':
    out = formula.to_dfa(mona_dfa_out=True)
  else:
    out = formula.to_dfa()

  # Print output to stdout
  print(out)