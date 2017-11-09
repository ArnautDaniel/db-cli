#db-cli

A silly program in racket to access a sqlite3 database and search a field in real time.

Was an interesting little experiment to spend a few hours with.  Curses is pretty cool.

Still more work to do if I ever want to make this a nice command line tool.  Like being able to switch tables/columns being searched; and support for postgre and mysql.
 
I'm kinda curious what this would look like in chicken scheme.  raco exe puts this in at 8mb which is quite a bit too large for a cli tool.

The demo code for charterm was used as a base to provide input.  I think it could be a fairly nice library to use but it's rather bare.  Writing some helper functions for it would probably make it quite fun to make curses programs in.
