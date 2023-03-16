
import os
import sys
import re
import math

main_dir="../results"
MAX_NORM=100;
MIN_NORM=1;
valid_cases=[1,2]
#######################################
#emulation_times={"1,1024":"1", "2,512":"1", "4,256":"2", "8,128":"3", \
		#		"16,64":"4","32,32":"5", "64,16":"6", "128,8":"7", "256,4":"8", \
		#		"512,2":"9"}
#
########################################
#cuda_times={"1,1024":"10", "2,512":"11", "4,256":"12", "8,128":"13", \
		#		"16,64":"14","32,32":"15", "64,16":"16", "128,8":"17", "256,4":"18", \
		#		"512,2":"19"}
#
#
########################################
#data=[\
#		["1,1024","1","10"],\
#		["2,512","1","11"],\
#		["4,256","2","12"], 
#		["8,128","3","13"],\
#		["16,64","4","14"],\
#		["32,32","5","15"],\
#		["64,16","6","16"],\
#		["128,8","7","17"],\
#		["256,4","8","18"]]	
#
#######################################
report_title_template='''
\\begin{center}
Running-time diagrams for EXAMPLE_NAME

Collected on DEVICE_NAME
\\end{center}
'''

#######################################
diagram_begin='''
\\begin{figure}[H]
\\begin{tikzpicture}
\\Large{
\\begin{axis}[
	width=0.99\\textwidth,
	height=0.35\\textwidth,
	xlabel=Points,
	ylabel=Time,
	axis x line*=bottom,
	axis y line*=left,
	ytick style={draw=none},
	ymin = Y_MIN,
	ymax = Y_MAX,
	xticklabels={LABELS},
	x tick label style={rotate=90,anchor=east},
	xlabel style={at={(1.0,0)},anchor=west, rotate=0},
	legend style={at={(0.85,1.1)},anchor=west},
	xtick={1,...,N_LABELS},
	]
'''

#######################################
diagram_end='''\legend{LEGEND}
\end{axis}
}
\end{tikzpicture}
\caption{CAPTION} \label{fig:FIGURE_LABEL}
\end{figure}
'''

#######################################
color_list=["navy","contrast"]
mark_list=["triangle*", "pentagon*"];

plot_begin='''\\addplot[nodes near coords, sharp plot, color=COLOR, mark=MARK] coordinates {
'''
plot_end='''};\n'''

#######################################
dash_plot_begin='''\\addplot [mark=none, dashed]coordinates{
'''
dash_plot_end='''};
'''

#######################################
def find_min(data, target_idx):
	n=len(data);

	for i in range(n):
		data[i][target_idx]=float(data[i][target_idx]);

	min_val=(data[0][target_idx]);
	min_val_idx=0;	
	for i in range(1,n):
		if (data[i][target_idx]<min_val):
			min_val=(data[i][target_idx]);
			min_val_idx=i;

	return [min_val, min_val_idx];
#######################################
def find_max(data, target_idx):
	n=len(data);

	for i in range(n):
		data[i][target_idx]=float(data[i][target_idx]);	
	max_val=data[0][target_idx]
	max_val_idx=0	

	for i in range(1,n):
		if (data[i][target_idx]>max_val):
			max_val=(data[i][target_idx]);
			max_val_idx=i;
	
	return [max_val, max_val_idx];

#######################################
def normalize_data(data, target_idx):
	n=len(data);

	for i in range(n):
		data[i][target_idx]=float(data[i][target_idx]);
#
#	min_val=find_min(data, target_idx)[0]
#	max_val=find_max(data, target_idx)[0]
#
#	for i in range(n):
#		data[i][target_idx]-=min_val
#

	min_val=find_min(data, target_idx)[0]
	max_val=find_max(data, target_idx)[0]

	denom=float(max_val-min_val);
#	if denom==0:
#		denom+=0.001;
#		print
	a=float(MAX_NORM-MIN_NORM)/denom
	b=MIN_NORM-a*min_val

#	print("a",a);
#	print("b",b);

	for i in range(n):
		data[i][target_idx]=a*data[i][target_idx]+b;

	return data;


#######################################
def normalize_data2(data,target_idx0, target_idx1):
	n=len(data);

	for i in range(n):
		data[i][target_idx0]=float(data[i][target_idx0])*1000.;
		data[i][target_idx1]=float(data[i][target_idx1])*100.;
#		print(data[i])
	

	
#	[min_val0, min_val0_idx]=find_min(data, target_idx0)
#	[min_val1, min_val1_idx]=find_min(data, target_idx1)
#
#	[max_val0, max_val0_idx]=find_max(data, target_idx0)
#	[max_val1, max_val1_idx]=find_max(data, target_idx1)
#
#	d=float(min_val1)/float(min_val0)	
##	a=pow(10,a)
##	a*=100.
#	a=d
#	b=0;
#
#	for i in range(n):
#		data[i][target_idx0]=a*data[i][target_idx0]+b;
#	

#	[min_val0, min_val0_idx]=find_min(data, target_idx0)
#	[min_val1, min_val1_idx]=find_min(data, target_idx1)
##	d=float(min_val1)/float(min_val0)	
#	d0=math.floor(math.log(min_val0,10))
#	d1=math.floor(math.log(min_val1,10))
#	d_factor0=float(pow(10,d0-1))
#	d_factor1=float(pow(10,d1-1))
#
#	for i in range(n):
#		data[i][target_idx0]/=d_factor0
#		data[i][target_idx1]/=d_factor1


#	
#	[min_val0, min_val0_idx]=find_min(data, target_idx0)
#	[min_val1, min_val1_idx]=find_min(data, target_idx1)
#
	[max_val0, max_val0_idx]=find_max(data, target_idx0)
	[max_val1, max_val1_idx]=find_max(data, target_idx1)
	
	h=100.
	for i in range(n):
		data[i][target_idx0]*=h
		data[i][target_idx0]/=max_val0
	
		data[i][target_idx1]*=h
		data[i][target_idx1]/=max_val1


	
#	[min_val0, min_val0_idx]=find_min(data, target_idx0)
#	[min_val1, min_val1_idx]=find_min(data, target_idx1)
#
#	[max_val0, max_val0_idx]=find_max(data, target_idx0)
#	[max_val1, max_val1_idx]=find_max(data, target_idx1)
	
#	min_val0/=2.0
#	min_val1/=2.0;
#	for i in range(n):
#		data[i][target_idx0]-=min_val0
#		data[i][target_idx1]-=min_val1

	
	min_val0=find_min(data, target_idx0)[0]
	min_val1=find_min(data, target_idx1)[0]
	min_y=min(min_val0, min_val1);
	
	max_val0=find_max(data, target_idx0)[0]
	max_val1=find_max(data, target_idx1)[0]
	max_y=max(max_val0, max_val1);
		
#	for i in range(n):
#			data[i][target_idx0]-=min_val0
#			data[i][target_idx1]-=min_val0
#
	return [data, min_y, max_y];




#######################################
def draw(input_size, data, kernel_name,example, output="diagrams.tex", ):
	
#	input_size=1024
	diagram=""
	legend=""
	
	font_size="\\normalsize "
	color_idx=0;
	mark_idx=0;
	
	cuda_idx=3;
	em_idx=4;
	

	[data, min_y, max_y]=normalize_data2(data, cuda_idx, em_idx);
#	data=normalize_data(data, cuda_idx);
#	data=normalize_data(data, em_idx);
	
	emulation_mag_factor=1;
	cuda_mag_factor=1;

	############################
	############################
	diagram+=diagram_begin;	
	labels=""
	
#	label_list=emulation_times.keys();
	label_list=[]
	for x in data:
		current_label=str(x[1])+","+str(x[2])
		label_list.append(current_label);

	n=len(label_list)
	diagram=diagram.replace("N_LABELS",str(n))
	for i in range(n):
		x=label_list[i]
		labels+=font_size+"{("+str(x)+")}"
		if i!=n-1:
			labels+=","
	diagram=diagram.replace("LABELS",labels);


	############################
	## label, cuda, emulation 
	############################
	diagram+=plot_begin.replace("COLOR",color_list[color_idx]).replace("MARK",mark_list[mark_idx])	
	for i in range(n):
#		x=label_list[i]
#		diagram+="("+str(i+1)+","+str(emulation_times[x])+")\n"
		em_time=emulation_mag_factor*float(data[i][em_idx])
		diagram+="("+str(i+1)+","+str(em_time)+")\n"##
	diagram+=plot_end

	legend+=font_size+"{Emulation (\\tt{X} "+str(emulation_mag_factor)+", n="+str(input_size)+")},"

	############################
	############################
	color_idx=(color_idx+1)%len(color_list)
	mark_idx=(mark_idx+1)%len(mark_list)

	diagram+=plot_begin.replace("COLOR",color_list[color_idx]).replace("MARK",mark_list[mark_idx])	
	for i in range(n):
#		x=label_list[i]
#		diagram+="("+str(i+1)+","+str(cuda_times[x])+")\n"
		
		cuda_time=cuda_mag_factor*float(data[i][cuda_idx])
		diagram+="("+str(i+1)+","+str(cuda_time)+")\n"##

	diagram+=plot_end
	legend+=font_size+"{CUDA (\\tt{X} "+str(cuda_mag_factor)+", n="+str(input_size)+")}"

	diagram+=dash_plot_begin
	for i in range(n):
		x=label_list[i]
		diagram+="("+str(i+1)+","+str(1)+")\n"
	diagram+=dash_plot_end

	############################
	############################	
	diagram+=diagram_end
	diagram=diagram.replace("LEGEND",legend);

	caption="Example:"+example+", Kernel:" +kernel_name
	fig_label=kernel_name+"-"+str(input_size);
		
	diagram=diagram.replace("CAPTION",normalize_string(caption));
	diagram=diagram.replace("FIGURE_LABEL",fig_label);
	diagram=diagram.replace("Y_MIN", str(0.6*min_y));
	diagram=diagram.replace("Y_MAX", str(1.1*max_y));

	f=open(output, "a");
	f.write(diagram);
	f.close();
#	print(diagram)		

#######################################
def parse_raw_cuda_data(file_path, pattern, major_split=",", minor_split="="):

	print("process cuda data for ["+file_path+"]")
	f=open(file_path, "r"); content=f.read(); f.close();
	
#	print(pattern.pattern)
#	exit
#	print(content)
	## find all matched lines.
	matches=re.findall(pattern, content);

#	print("cuda_matches", matches)
#	return;
	
	## parse the pattern itself.
	p=pattern.pattern;
	p=p.replace("\[trace: ","");
	p=re.sub(r'\\\].*','',p);
	p=p.split(major_split)

	
	elapsed_tokens_idx={}
	n_elapsed_tokens=0;
	for t in p:
		if "elapsed" in t:
#			print(t)
			elapsed_tokens_idx[t]=n_elapsed_tokens
			n_elapsed_tokens+=1

	n=len(matches);
	n_list=n*[0];	bx_list=n*["1"];	by_list=n*["1"];

	elapsed_list=n_elapsed_tokens*[None]
	for i in range(n_elapsed_tokens):
		elapsed_list[i]=n*[0]

	cnt=0;
	## look for n, bx, by in all matches.
	for m in matches:
#		print("mcuda",m)
#		continue;
		for x in p:
			c=re.findall(re.compile(x),m);
			c=c[0]
#			print(c)
			c=re.sub('[ ]*','',c)
			c=c.split(minor_split)
#			print(c)

			## remove all non-digits (except .) from the value
			c[1]=re.sub(r'[^0-9|^\.]',"",c[1])
#			print("cuda-c0c1",c[0],c[1])
			if c[0]=="n":
				n_list[cnt]=c[1]
#				print(n_list[cnt])
			if c[0]=="bx":
				bx_list[cnt]=c[1]
			if c[0]=="by": 
				by_list[cnt]=c[1]
			if "elapsed" in c[0]:
				elapsed_idx=elapsed_tokens_idx[x]
				elapsed_list[elapsed_idx][cnt]=c[1]
		cnt+=1;
#		print("==================");
	
#	for i in range(n):	
#		for t in range(n_elapsed_tokens):	
#			print ("cuda",n_list[i],bx_list[i],by_list[i],elapsed_list[t][i])
	
	called_kernels=elapsed_tokens_idx.keys();

	for i in range(len(called_kernels)):
		k=called_kernels[i];
		k=re.sub(r'.*elapsed_','',k);
		k=re.sub(r'=.*','',k)
		called_kernels[i]=k
	
	return [n_list, bx_list, by_list, elapsed_list, elapsed_tokens_idx, called_kernels];
#######################################
def parse_raw_emulation_data(file_path, pattern, major_split=",", minor_split=":"):

	print("process emulation data for ["+file_path+"]")
		
	emulation_result=[]
	f=open(file_path, "r"); content=f.read(); f.close();
#	print(content)
	## find all matched lines.
	matches=re.findall(pattern, content);

#	print("match", matches)
	## parse the pattern itself.
	p=pattern.pattern;
	p=p.replace("\[","");
	p=re.sub(r'\\\].*','',p);
	p=p.split(major_split)
		
#	n=len(matches);
#	n_list=n*[0];	bx_list=n*["1"];	by_list=n*["1"]; elapsed_list=n*[0]
#	
#	cnt=0;
	## look for n, bx, by in all matches.
	for m in matches:	
		is_valid_entry=0;
		res=""
		for x in p:
			c=re.findall(re.compile(x),m);
			c=c[0]
			c=re.sub('[\s]*','',c)
			c=c.split(minor_split)
			## remove all non-digits (except .) from the value

			res+="["+str(c[0])+": "+str(c[1])+"]\n"	
		
		emulation_result.append(res);

	for x in emulation_result:
		print(x)
		print("----------------------");
	
	return emulation_result;
#######################################
def read_regex_from_file(file_path):
	f=open(file_path, "r");
	content=f.read();
	f.close();
	content=re.sub('\\n',"",content);
#	print(content)
	return re.compile(content,re.MULTILINE);


#######################################
def get_kernel_name_list(path):
	f=open(path+"/kernel_name_list.tmp", "r")
	kernels=f.read().split("\n")[:-1];
	f.close()
	return kernels

#######################################
def unify_lists(cuda_data, emulation_data, kernel_name):
	[cuda_n_list, cuda_bx_list, cuda_by_list, cuda_elapsed_list, cuda_elapsed_tokens_idx]=cuda_data[:]


	[em_n_list, em_bx_list, em_by_list, em_clocks]=emulation_data[:]

	## find which cuda elapsed time token is associated with this kernel.
	for t in cuda_elapsed_tokens_idx.keys():
		tk=t;
		tk=re.sub(r'.*elapsed_','',tk);
		tk=re.sub(r'=.*','',tk);
#		print("tk",tk)
#		print("kernel_name",kernel_name)
		if (re.search(tk,".*"+kernel_name+".*")):
			elapsed_idx=cuda_elapsed_tokens_idx[t]
			print("idx for elapsed cuda time for kernel ["+kernel_name+"]="+str(elapsed_idx))
	
	
#	data=[]
#	for i in range(len(em_n_list)):
#		case=-1
#		if cuda_n_list[i]==em_n_list[i]:
#
##			if cuda_n_list[i] not in em_result.keys():
##				em_result[cuda_n_list[i]]={};
##				cuda_result[cuda_n_list[i]]={};
#			
#			print("cuda_bx",cuda_bx_list[i],"cuda_by",cuda_by_list[i])	
#			print("em_bx",em_bx_list[i],"em_by",em_by_list[i])		
#			if cuda_bx_list[i]==em_bx_list[i] and cuda_by_list[i]==em_by_list[i]:
#				case=1;
#			if cuda_bx_list[i]==em_by_list[i] and cuda_by_list[i]==em_bx_list[i]:	
#				case=2;	
#
#			print("[n, bx, by]:",cuda_n_list[i], cuda_bx_list[i], cuda_by_list[i]);
#			print("case", case, "cuda", cuda_elapsed_list[elapsed_idx][i],"emul:", em_clocks[i]);
#			print("============================")
#			data.append([cuda_n_list[i], cuda_bx_list[i], cuda_by_list[i], \
#					cuda_elapsed_list[elapsed_idx][i], em_clocks[i]]);
	
#	data=[]
#	l=min(len(cuda_n_list),len(em_n_list))
#	for i in range(l):
#		case=-1
#		if cuda_n_list[i]==em_n_list[i]:
#
##			if cuda_n_list[i] not in em_result.keys():
##				em_result[cuda_n_list[i]]={};
##				cuda_result[cuda_n_list[i]]={};
#			
#			print("cuda_bx",cuda_bx_list[i],"cuda_by",cuda_by_list[i])	
#			print("em_bx",em_bx_list[i],"em_by",em_by_list[i])		
#			if cuda_bx_list[i]==em_bx_list[i] and cuda_by_list[i]==em_by_list[i]:
#				case=1;
#			if cuda_bx_list[i]==em_by_list[i] and cuda_by_list[i]==em_bx_list[i]:	
#				case=2;
#
#			if case==-1:
#				print("MISMATCH")
#				exit(-1)
#
#			print("[n, bx, by]:",cuda_n_list[i], cuda_bx_list[i], cuda_by_list[i]);
#			print("case", case, "cuda", cuda_elapsed_list[elapsed_idx][i],"emul:", em_clocks[i]);
#			print("============================")
#			data.append([cuda_n_list[i], cuda_bx_list[i], cuda_by_list[i], \
#					cuda_elapsed_list[elapsed_idx][i], em_clocks[i]]);


	data=[]
	l=min(len(cuda_n_list),len(em_n_list))
	
	case=-1;
	em_list_idx=0;
	for i in range(len(cuda_n_list)):
		[n,bx,by]=[cuda_n_list[i], cuda_bx_list[i], cuda_by_list[i]]
		for j in range(em_list_idx, len(em_n_list)):
			[em_n,em_bx,em_by]=[em_n_list[j], em_bx_list[j], em_by_list[j]]
			if n==em_n and bx==em_bx and by==em_by:
#			if n==em_n and by==em_bx and bx==em_by:##
				print("match[n,bx,by,time, clock]", \
						n, bx, by, cuda_elapsed_list[elapsed_idx][i],em_clocks[j]);
			
				data.append([n, bx, by, cuda_elapsed_list[elapsed_idx][i],em_clocks[j]]);
				em_list_idx=j+1;
				break;

	return data;
	exit(-1)

#	for i in range(l):
#		case=-1
#		if cuda_n_list[i]==em_n_list[i]:
#			print("cuda_bx",cuda_bx_list[i],"cuda_by",cuda_by_list[i])	
#			print("em_bx",em_bx_list[i],"em_by",em_by_list[i])		
#			if cuda_bx_list[i]==em_bx_list[i] and cuda_by_list[i]==em_by_list[i]:
#				case=1;
#			if cuda_bx_list[i]==em_by_list[i] and cuda_by_list[i]==em_bx_list[i]:	
#				case=2;
#
#			if case==-1:
#				print("MISMATCH")
#				exit(-1)
#
#			print("[n, bx, by]:",cuda_n_list[i], cuda_bx_list[i], cuda_by_list[i]);
#			print("case", case, "cuda", cuda_elapsed_list[elapsed_idx][i],"emul:", em_clocks[i]);
#			print("============================")
##			data.append([cuda_n_list[i], cuda_bx_list[i], cuda_by_list[i], \
#					cuda_elapsed_list[elapsed_idx][i], em_clocks[i]]);
#	return data;


#######################################
def normalize_string(s):

	t=""
	for c in s: 
		n=ord(c)
		if (n>=ord('a') and n<=ord('z')) \
				or (n>=ord('A') and n<=ord('Z'))\
				or (n>=ord('0') and n<=ord('9')):
					t+=c;	
		else:	
			t+="\\"+c
	return t;
########################################

def write_report_title(report_title_template, example, device, output):
	
	device=normalize_string(device);
	example=normalize_string(example);
	
	t=report_title_template;
	t=t.replace("EXAMPLE_NAME",example);
	t=t.replace("DEVICE_NAME",device.upper());
	report_title=t;
	f=open(output,"w");
	f.write(report_title);
	f.close();

#######################################
def write_list_to_file(content_list, path):
	out=open(path,"w");
	for x in content_list:
		out.write(x+"\n")
	out.close()
	
#######################################
def generate_diagrams(example, device, output):

#	write_report_title(report_title_template, example, device, output)

	cuda_data_pattern=read_regex_from_file(main_dir+"/"+example+"/cuda_trace.regex");
	emulation_data_pattern=read_regex_from_file(main_dir+"/"+example+"/emulation_trace.regex");

	cuda_result_file="cuda_results_"+device+".txt"

	cuda_data=parse_raw_cuda_data(main_dir+"/"+example+"/"+cuda_result_file,\
			cuda_data_pattern);
	called_kernels=cuda_data[5]
	print("called_kernels",called_kernels)
	cuda_data=cuda_data[:5]
#	return;
	
	kernel_name_list=get_kernel_name_list(main_dir+"/"+example)

	for k in kernel_name_list:
		matched=0;
		for c in called_kernels:
			c=re.compile(c);
#			print("checking match against "+k+" , "+c)
			if re.match(c,k):
				matched=1;
				break;
		if matched==0:
			continue;
		print("============================")
		print("processing kernel "+k)
		emulation_result_file="kernel_"+k+"_results_"+device+".txt"
		
		emulation_result_file_path=main_dir+"/"+example+"/"+emulation_result_file
		if os.path.exists(emulation_result_file_path)==False:
			continue;
		emulation_data=parse_raw_emulation_data(emulation_result_file_path, emulation_data_pattern);
		out=main_dir+"/"+example+"/"+"kernel_"+k+"_table_"+device+".txt";
		write_list_to_file(emulation_data, out);

#		unified_list=unify_lists(cuda_data, emulation_data, k);
#		print("\n")
#		
#		label_list=[]
#		cuda_times=[]
#		emulation_times=[]
#		
#		if len(unified_list)==0:
#			print("ERROR: unified_list size==0");
#			exit(-1)
#		
##		print("unified list")
#		print(unified_list)
#		continue;
#		for i in range(len(unified_list)):
#			e=unified_list[i]
##			print(e)
#			[n,bx,by,ct, clocks]=e[:]
#
#			if i==0:	
#				prev_n=n;
#				prev_idx=0;
#
#			if n!=prev_n:
#
##				print(prev_idx, i)
#				draw(prev_n,unified_list[prev_idx:i],k, normalize_string(example), output);
#
#				prev_idx=i;
#				prev_n=n;
##		print(prev_idx, len(unified_list))
#		draw(prev_n,unified_list[prev_idx:],k, normalize_string(example), output	);
#

#######################################
def generate_pdf_for_diagram(diagram_path, plot_path, result_dir):
	
	plots_tex='''
\documentclass[12pt,a2paper]{article}
\usepackage[utf8x]{inputenc}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}

\input{macros_plots}

\\begin{document}
\input{DIAGRAM_PATH}
\end{document}
'''
	
	plots_tex=plots_tex.replace("DIAGRAM_PATH", diagram_path);

	output=open(plot_path, "w");
	output.write(plots_tex);
	output.close();

	os.system("pdflatex "+plot_path + "> /dev/null");
#	os.system("pdflatex "+plot_path);
#	
	rm_cmd="rm -rf "
	for x in ["aux", "bbl", "blg", "dvi", "log", "nav", "out", "ps", "snm"]:
		rm_cmd+=plot_path.replace(".tex","."+x)+ " "
	os.system(rm_cmd)
	
	os.system("mv "+diagram_path+" "+result_dir)
	os.system("mv "+plot_path+" "+result_dir)
	os.system("mv "+plot_path.replace(".tex",".pdf")+ " "+result_dir)


#######################################
def main():
	argc=len(sys.argv);
	if argc!=3:
		print("args: [1]:example_dir [2]:device_name");
	else:

#		[example, device]=["VectorAddition","gtx1080ti"]
#		example="1D_Jacobi"
#		device="M2050"
#		output="diagrams.tex"
		[example, device]=[sys.argv[1], sys.argv[2]]
		diagram_path=example+"_"+device+"_diagrams.tex"
		plot_path=example+"_"+device+"_plot.tex"
		result_dir="result/"+device
		os.system("mkdir -p "+result_dir)

		f=open(diagram_path,"w");
		f.close();
		generate_diagrams(example, device, diagram_path)
#		generate_pdf_for_diagram(diagram_path, plot_path, result_dir);


########################################

main();
