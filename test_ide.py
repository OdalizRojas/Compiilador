import tkinter
from tkinter import *
import esperanto
from tkinter.filedialog import asksaveasfilename, askopenfilename
import subprocess

compiler = Tk()
compiler.title('Esperanto UPB IDE')
file_path = ''

def set_file_path(path):
    global file_path
    file_path = path

def open_file():
    path = askopenfilename(filetypes=[('Python Files', '*.myopl')])
    with open(path, 'r') as file:
        code = file.read()
        editor.delete('1.0', END)
        editor.insert('1.0', code)
        set_file_path(path)

def save_as():
    if file_path == '':
        pass
    else:
        path = asksaveasfilename(filetypes=[('Python Files', '*.myopl')])
        path = file_path
    with open(path, 'w') as file:
        code = editor.get('1.0', END)
        file.write(code)
        set_file_path(path)


def run(event=None):
    global code, file_path

    code = editor.get(1.0, END)

    if True:
        result, error = esperanto.run('<stdin>', code)
        if error:
            # delete the previous text from output_windows
            editor.delete(1.0, END)
            code_output.insert(1.0, error.as_string())
            print(error.as_string())
        elif result:
            if len(result.elements) == 1:
                print(repr(result.elements[0]))
                # delete the previous text from output_windows
                code_output.delete(1.0, END)
                #print tokens
                #print_tokens(cat.tokens)
                # insert the new output text in output_windows
                code_output.insert(1.0, repr(result.elements[0]))
                code_output.insert(1.0, "\n")
                code_output.insert(1.0, esperanto.output)

            else:
                print(result)
                # delete the previous text from output_windows
                code_output.delete(1.0, END)
                #print tokens
                #print_tokens(cat.tokens)
                # insert the new output text in output_windows
                code_output.insert(1.0, repr(result.elements))
                code_output.insert(1.0, "\n")
                code_output.insert(1.0, esperanto.output)


menu_bar = Menu(compiler)

file_menu = Menu(menu_bar, tearoff=0)
file_menu.add_command(label='Open', command=open_file)
file_menu.add_command(label='Save', command=save_as)
file_menu.add_command(label='Save As', command=save_as)
file_menu.add_command(label='Exit', command=exit)
menu_bar.add_cascade(label='File', menu=file_menu)

run_bar = Menu(menu_bar, tearoff=0)
run_bar.add_command(label='Run', command=run)
menu_bar.add_cascade(label='Run', menu=run_bar)

compiler.config(menu=menu_bar)


editor = tkinter.Text(compiler, bg="#acdeaa")
editor.pack(fill=tkinter.BOTH, expand=True)
scroll_bar = Scrollbar(master=editor)

division_bar = Menu()
division = Menu(division_bar, tearoff=0)

code_output = Text(height=15, bg="#8fbbaf")
code_output.pack(fill=tkinter.BOTH, expand=True)
division.add_command(label='Resultado')
division.add_command(label='Errores')
compiler.mainloop()
