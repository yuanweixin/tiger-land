template doAssert*(a,b,action) = 
    if a != b:
        action(a,b)
