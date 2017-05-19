module FastQ where
    import Queue
    data FastQ q = Record [q] [q]
    instance Queue FastQ where
        push x (Record [] []) = Record [x] []
        push x (Record pushStack popStack) = Record  (x:pushStack) popStack
        
        pop (Record pushStack []) = pop (Record [] pushStack)
        pop (Record pushStack popStack) = Record pushStack (tail popStack)
         
        top (Record pushStack popStack) = head popStack
        
        emp (Record pushStack popStack) = null popStack && null pushStack
        
        create x = Record [x] []
        
    instance Show a => Show (FastQ a) where
        show (Record pushStack popStack) = show ((reverse pushStack) ++ popStack)