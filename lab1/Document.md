Lexical Analysis
===================

> Lab1 of Compilers, FDU, 2016, Modern Compiler Implementation in C.


comment handling
--------------------
 - As we know from the book, comment in tiger-language is wrapped in ___/*...*/___ formula, so we need to use regex ___/*___ to detect if we encounter the start of a comment string.
 - We define a ___IN_COMMENT___ state, when we find comment happens using the ___/*___ regex, we start the ___IN_COMMENT___ state.
 - In my design, if we encounter more ___/*___ in the ___IN_COMMENT___ state, most programming languages don't allow nested comment. As there are no requirement of that in the textbook and testcases, I implement both cases.
 - If we allow nested comment, we use a variable ___commentStateNum___, inside the ___IN_COMMENT___ state, if we meet another ___/*___, we increase the variable by 1, if we meet one ___*/___, we decrease the variable by 1 on contrast. When the variable is equal to zero, then there are no nested comment, and we just quit to the ___INITIAL___ state.
 - If nested comment is not allowed, we just ignore any ___/*___ in the comment state, and quit to the ___INITIAL___ state when we meet the first ___*/___
 - If we meet ___*/___ inside ___INITIAL_STATE___, we regard it as illegal since we are not in the comment state.
 - I have also handled some other common status inside  ___IN_COMMENT___ state, like the ___EOF___ and ___/n___ state, in order to pass the testcases. Other inputs are all considered legal.


 string handling
--------------------
 - The way we use to store the string content is by using a ___buffer___ variable, which is a char array(buffer). The default size of the ___buffer___ is set to be 16, using const int to make it immutable.
 - When the buffer length comes up to the limit, we just double the buffer size and copy the old content to the new buffer to continue storing. 
 - Specially, we also maintain a ___len___ variable to store the starting char position. Every time we call ___adjust()___, the ___EM_tokPos___ increases. If we calculate the starting position using string length, sometimes it will come to incorrect results. 
 - Regex ___/"___ is used to identify the start of string literals. There are several special situations inside ___IN_STRING___ state:
 - ___EOF___: Cause error as it illegally terminate the state.
 - Escape character like ___\\n, \\t, ...___: push it's real character, for example if we encounter '\\n' symbol, we just append '\n' symbol to the buffer.
 - ___"___: It means the end of string. Return "(null)" to satisfy the test cases when we meet an empty string, it's a special case. 
 - The other symbols we read are all consider legal in my design.


error handling
--------------------
 - In my design, most of the regular errors are all handled, including:
 - ___EOF___ sign in ___IN_COMMENT___ and ___IN_STRING___ states.
 - Try to dangerously close a comment when we are not in ___IN_COMMENT___ state.
 - Illegal tokens.


file end(EOF) handling
--------------------
 - ___EOF___ is handled in ___IN_STRING___ and ___IN_COMMENT___ states. When we encounter it in such states, we consume it as error as it forces to terminate special state while it's running, so we throw an error and terminate. 
 - The program will definitely end at an ___EOF___ sign, so we don't need to handle the EOF sign in the regular occasions.


extra tests
--------------------
 - Test some other extra escape characters, for example: "\a", "\v", "\t", \b", "\f", "\r" and so on. They are not used in testcases but also perform right.
