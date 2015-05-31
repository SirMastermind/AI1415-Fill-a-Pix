## AI1415-Fill-a-Pix

### Notes

* The folder _public_tests_ contains the tests that were given by the teachers in order to locally test our project.

***

### Instructions

1 - Open your console window;  
2 - Initiate your Lisp Execution Environment;  
3 - Do:  
> (compile-file "examples.lisp"

4 - Do:  
> (load (compile-file "project.lisp"))  

Note: The file _project.lisp_ loads the _examples.fas_.  
  
5 - Replace FUNC_NAME with a function in the following set {resolve-simples, resolve-best} and X with a number in the following set {0, 1, 1_1, 2, 3, 4, 5, 6, 7} and do:  
> (desenha-fill-a-pix (FUNC_NAME eX))  

6 - If you want to run the program with the other algorithms implemented, you have to edit the file _project.lisp_, specifically the function *resolve-best*. Replace FUNC_NAME with a function in the following set {procura-retrocesso-grau, procura-retrocesso-fc-mrv, procura-retrocesso-mac-mrv, best-procura-retrocesso-fc-mrv}.  

>(defun resolve-best (array)  
  (let (  (psr)
          (array-final))  
  (setf psr (best-fill-a-pix->psr array))  
  (setf psr (FUNC_NAME psr))  
  (cond ( (null psr)  
            nil)  
          (T  
            (setf array-final (psr->fill-a-pix-best psr (array-dimension array 0) (array-dimension array 1)))))))  
  
Then, save the file and repeat point 4.  

***

Tomás Alves @ 31/05/2015
