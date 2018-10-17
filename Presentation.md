Final Project: Predict the next word
========================================================
author: Kwatanwa17
date: October 17 2018
autosize: true
width: 1440
height: 900

Index
========================================================

For more information about the code please see my Github repository <https://github.com/Kwatanwa17/Capstone>.

- Corpus description
- Algorithm
- Shiny Application

Corpus description
========================================================

This shiny application is trained by the text data sets provided by Swiftkey for the Data Science Capstone course. 

Actually, we sampled only one fifth of the hole data set to train the algorthm because of the CPU capacity.

Collected text were toknized from 1-gram to 5-gram to implement the Katz's Back Off Model.

Used packeges:
- shiny
- quanteda
- dplyr
- stringr
- shinymaterial

Algorithm
========================================================

There are two functions:

- cleanSentenceUpdated: this function is used for cleaning the input text. Firstly, the iput text is checked if it contains four or more words. If so, this function takes the last four words and creates 4-gram, 3-gram, 2-gram and 1-gram tokens. If the text length is less than four, it makes the maximum number of n-gram tokens depending on the text length. For example, if the input is a word, this function will retun a 1-gram token, which is exactly the input word.

- predNextWordUpdated: this is the predict function. First of all, this function calls cleanSentenceUpdated to clean the input text. Then if the input text has 4-gram token, this function will check the 5-gram token data. If not, it will reduce the number of tokens and check the 4-gram token data until the 1-gram token data. If the length of the input text is less than four, this fucntion always adjusts to the used token data. For example, if the text input is a word, this fucntion will check directly the 2-gram token data.


Shiny Application "Predict the next word"
========================================================

Shiny application link <https://kwatanwa17.shinyapps.io/capstoneproject/>

This shiny application is very straightforward.

All you have to do is type some text in "Your text here" and click "SEE THE NEXT WORD".

It will be appreared the predict word below.

Thank you for using the app and enjoy !


