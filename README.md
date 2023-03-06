# WordSearch
CPSC312 Haskell Project

=== What is the problem? ===
Our goal is to make an application in which the user can provide a set of words to the program and it will return a generated word search including the provided words. 
 
=== What is the something extra? ===
The console will display the word search game, along with the words inputted so that the player can print the game, or play it on their screen. It will also check to see if the user inputted enough words, so that it can fill in some words if needed. If time allows, we'll add the additional challenge of having words displayed backwards in the grid.

=== What did we learn from doing this? ===
The main thing that we learned from this project is how to write a complex program in a language with no side effects. Both of us were very used to programming in Python so in our original code we had a lot of functions that would return an IO as an input to another function expecting to parse the output. However, we realized very quickly that this approach would cause many type issues in our code. Instead we had to use work arounds when we wanted to have a function return a piece of information such as a tuple or other data type. After creating this program we decided that while was possible to do, functional programming made our word search generation much harder since it relies on random numbers being generated. It it were written in a language with side effects a lot of the IO issues we dealt with wouldn't have existed. 
