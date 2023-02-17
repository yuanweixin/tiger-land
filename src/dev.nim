template devAssert*(cond, msgs: varargs[string, `$`]) = 
    when defined(tigerdevel):
        if not cond:
            doAssert false, ' '.join(msgs)

template devEcho*(things: varargs[string, `$`]) = 
    when defined(tigerdevel):
        echo things