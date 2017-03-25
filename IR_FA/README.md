# Intermediate Representation and Frame Analysis 
Run with:
    Main.IR "filename"
## Debugging things 
- We changed temp.sml so that we can have named registers which are guaranteed to be "unique". 
- We also added named labels which are guaranteed to be "unique".
- We added an extra parameter to seq() function for debugging purposes. 

## Future TODO
- bounds checking [done]
- need to emit tiger_main frag [done]
- instruction to move RV to register on function exit [done]
- check all transerror and change those used in place of nil or unit [done]
- possibly convert tree.sml to safe alternative
- test 6: in which direction is memory read and written ie. MEM[-4] means -4 to -8 or 0 to -4?
- test 11: edge case when for loop tries to assign to loop variable, this should not be allowed.

## Happy birthday Adithya!
[yaya adithya](https://www.dropbox.com/s/r0dz6hum7im02ek/17342866_10154601088338985_3403718265296467886_n.jpg?dl=0)
