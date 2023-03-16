import os
import sys
import re
import math

function_def = '''
#include <stdlib.h>
#include <stdio.h>
#include <time.h> 
void data_gen(int* data, int N){
	srand(time(NULL));

'''

#######################################
def read_content_from_file(path):
	f=open(path,"r");
	content=f.read();
	f.close()
	return content;

#######################################
def write_content_to_file(content, path):
	f=open(path,"w")
	f.write(content);
	f.close()

#######################################
def get_variables(line):
	line = line.strip("[ ]");
	line = line.split(",");
	return line;

def construct_for_loop(index,var):
	output = "";
	for i in range(0,len(var)):
		index[i] = index[i].strip().split(":");
		if index[i][0]=="":
			index[i][0] = "0";
		if index[i][1] == "":
			index[i][1] = "N";
		output += "\tfor(int " + var[i] + " = " + index[i][0] + "; " + var[i] + " < " + index[i][1] + "; "+ var[i] + " ++ ){\n"
#	print(output);
	return output;

def construct_const(const,var):
	output = "\t\tdata";
	const = const.strip("[ ]");
	for i in range(0,len(var)):
		output += "[" + var[i] + "]";
	output += " = " + const + ";\n";
	return output;

def construct_range(rang,var):
	rang = rang[0].replace("range(","");
	rang = rang.replace(")","");
	rang = rang.split(",");

	output = "\t\tdata";
	for i in range(0,len(var)):
		output += "[" + var[i] + "]";
	output += " = " + "(rand() %( "+ rang[1] + " - " + rang[0] + "+ 1)) + " + rang[0] + ";\n";
	return output;

def construct_expr(expr,var):
	expr = expr.strip("[ ]");
	output = "\t\tdata";
	for i in range(0,len(var)):
		output += "[" + var[i] + "]";
	output += " = " + expr + ";\n";
	return output;
def end_loop():
	output = "\t}\n\t}\n\t}\n";
	return output;

def get_constraints(line,var):
	output = "";
	index = re.findall(r"(\[(\d*:\d*,)*\d*:\d*\])", line);
	line = line.replace(str(index[0][0]),"");	
	print(line)
	index = index[0][0].strip("[ ]").split(",");
	output += construct_for_loop(index,var);

	rang = re.findall(r"range\(.*\)",line)
	if rang:
		output += construct_range(rang,var);
		output += end_loop();
	#	print(output);
		return output;


	const = re.findall(r"(\[\d+.?\d*\])",line)
	if const:
		output += construct_const(const[0],var);
		output += end_loop();
		#print(output);
		return output;

	

	expr = re.findall(r"(\[.*\])",line)
	if expr:
		output += construct_expr(expr[0],var);
		output += end_loop();
		#print(output);
		return output;


def main():
	argc = len(sys.argv);
	src_path = "";
	result_path = "";
	output = "";
	output += function_def;
	
	if argc == 2:
		src_path = sys.argv[1]	
	else:
		print("args: [1]:data_gen_path")
	content = read_content_from_file(src_path);
	result_path = src_path.replace(".conf",".c");
	content = content.split("\n");
	var = get_variables(content.pop(0));
	print(var);
	for line in content:
		output += get_constraints(line,var);
	output += "\n}\n";
	write_content_to_file(output, result_path);
main();