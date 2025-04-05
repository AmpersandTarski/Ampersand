from graphviz import Digraph
import sys
import re
import os

# This function extracts all hyperlinks from the .md files in the current directory and recursively in all its subdirectories.
# It yields pairs. Each pair is (filename,nodeName) in which nodeName is the hyperlink to another file.
def extract_strings_in_brackets(filename, text):
    strings = re.findall(r'\((.*?)\)', text)   # isolate strings between parentheses. All hyperlinks are among them.
    result = []
    for str in strings:
        if str.endswith("/"):                  # normalize for README.MD files
            s = str+"README.MD"
        else: s = str
        # filter out the unwanted links, normalize and collect them.
        if (s.startswith("/") or s.startswith("../")) and not (s.endswith(".png") or s.endswith(".jpg")):
            nodeName = normPath(filename, s)
            result.append((filename,nodeName))
    return list(set(result))

# This function normalizes the hyperlink to ensure that every node in the graph represents one file uniquely.
def normPath(filename, s):
    (path,ref) = os.path.split(s)       # splits s in a pathname and the filename that is referred to
    ls = filename.split("/")            # The name of the calling file is transformed to a list, which contains directories and a file name.
    rs = path.split("/")                # The path of the referred file is split up in parts
    while rs[0] == '..' :               # getting rid of back references
        ls.pop(-2); rs.pop(0)
        if rs==[] : ls.pop(-1); return (ls+rs+[ref])
    if rs[0] == '' : return (['.']+rs[2:]+[ref])   # normalizing absolute references
    ls.pop(-1); return ("/".join(ls+rs+[ref]))     # producing the normalized result as a readable path.

# Specify the directory containing the files
directory1 = './Ampersand/docs'
directory2 = './Prototype/docs'
directory3 = './TheToolsWeUse'

# Iterate over all files in the directory
def extract_strings():
    result = []
    for dirpath, dirnames, filenames in os.walk(directory1)+os.walk(directory2)+os.walk(directory3):
        for filename in filenames:
            if filename.endswith(".md"):
                filepath = os.path.join(dirpath, filename)
                with open(filepath, 'r') as file:
                    text = file.read()
                    for pair in extract_strings_in_brackets(filepath,text):
                        result.append(pair)
    return result

g = Digraph(format='png', graph_attr={'rankdir':'LR'})

# Add edges from the list of pairs
for edge in extract_strings():
    g.edge(str(edge[0]), str(edge[1]))

g.render('example.gv', view=True)

