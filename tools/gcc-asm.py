#!/usr/bin/python
# coding: UTF-8
import sys;
import re;

inst_arity = {
	"LDC": 1,
	"LD": 2,
	"ADD": 0,
	"SUB": 0,
	"MUL": 0,
	"DIV": 0,
	"CEQ": 0,
	"CGT": 0,
	"CGTE": 0,
	"ATOM": 0,
	"CONS": 0,
	"CAR": 0,
	"CDR": 0,
	"SEL": 2,
	"JOIN": 1,
	"LDF": 1,
	"AP": 1,
	"RTN": 0,
	"DUM": 1,
	"RAP": 1,
	"STOP": 0,
	"TAP": 0,
	"TRAP": 0,
	"ST": 2,
	"DEBUG": 0,
	"BRK": 0,
}

re_separator = re.compile("\s+")
re_empty = re.compile("^\s+$")
re_label = re.compile("(\w+)\s*:")

asm = None
for arg in sys.argv:
	if arg.endswith(".gcc"):
		asm = open(arg)
if asm == None:
	asm = sys.stdin

line = asm.readline()
line_no = 0
addr = 0
labels = {}
results = []

while line:
	# remove comments
	comm = line.find(";")
	if comm >= 0:
		line = line[0:comm]
	# empty line
	if re_empty.match(line):
		line = asm.readline()
		line_no += 1
		continue
	# label
	elif re_label.match(line):
		matched = re_label.match(line)
		label = matched.group(1)
		if label in labels:
			raise Exception("Label '%s' conflicts." % label)
		else:
			labels[label] = addr
	# instructions
	else:
		tokens = re_separator.split(line.strip())
		tokens[0] = tokens[0].upper();
		if (tokens[0] in inst_arity and
			len(tokens) == inst_arity[tokens[0]] + 1):
			results.append(tokens)
		else:
			raise Exception("Invalid '%s' at line %d." % (tokens[0], line_no))
		addr += 1

	line = asm.readline()
	line_no += 1

for result in results:
	print result[0],
	for i in range(1, len(result)):
		token = result[i]
		print " ",
		if token.isdigit():
			print token,
		elif token in labels:
			print labels[token],
		else:
			raise Exception("Label '%s' not found." % token)
	print
