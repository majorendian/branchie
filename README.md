
# Branchie
Branchie is a branching-based game engine.
The UI relies heavily on Tcl/Tk as it's back-bone for graphics "rendering" because it already provides
all the widgets one could possibly want. The only problem is multithreading so if you want to use the UI provided you will need to get my fork of LTK which enables multiple threads to send commands to `wish`.
You can find the **fork** here: [majorendian/ltk](https://github.com/majorendian/ltk)  
If you wish to take a look at the original source, you can find it here: [herth/ltk](https://github.com/herth/ltk)

To best understand what is meant by this, have a look at the examples in the examples folder.

## Terminal-based Hello World example
A trivial example of a terminal-based branchie game would be as following:
```common-lisp
(require :branchie)
(defpackage :helloworld
  (:use :cl :branchie-core))

(in-package :helloworld)

(br "Press enter to exit." :name 'quit-branch :code #'quit)

(term-loop (br "Hello world!"
               :name 'start-branch
               :options (list
                          (list "Hello program!" (br "You said hello to the program."
                                                     :code (lambda (current_branch userinput)
                                                             (declare (ignore current_branch) (ignore userinput))
                                                             nil)))
                          (list "Exit" 'quit-branch))))

```
This example is also included in the `examples` folder.

### Explanation

You define branches and give them reference names, such as 'quit-branch or 'start-branch
These you can then reference deeper in the structure.

`(term-loop ...)` is a predefined function in the `branchie-core` package that simply handles the main loop
of the program in the terminal. The `branchie-core` module is designed to be more or less "plugable" into other larger programs. Look at the function in `branchie-core.lisp` if you want to have a better idea of how you can use this if you don't want to use the console.

`(br ...)` defines a simple function that creates a branch and registers it in a global hash-table.

The function has the following usage
```common-lisp
(br "Text to display"
    :options (list
                (list "Option1 text" (br ...))
                (list "Option2 text" (br ...))
    :code (lambda (current_branch userinput) ...)
    :name 'branch-name-for-jump-reference
```
It should be self-explanatory. The `:options` key takes a list of pairs which are also lists. The first element is the text of the option and the second is either a branch or a branch name such as 'start-branch or 'quit-branch or as in the above example 'branch-name-for-jump-reference.

You can define branches outside of the `:options` key and then just use the branch name as it is demonstrated in the *hello world* example.

The `:code` key defines a function to be run after the player enters the branch. This can be used to set variables and even change the text of the branch. Look at `bc-example-1.lisp` in the examples folder to see how that can be done.

The `:name` key defines the name of the branch to be used for refrencing when jumping. In the *hello world* example a simple 'quit-branch is defined for program exit so that we don't have to write it again.


## The UI
The UI allows for more interaction then just reading a line from the terminal such as processing key inputs. Look at `uiexample.lisp` in the examples folder for an example of key input processing.

The UI is currently under construction and this README will be updated as soon as something tangible is done.