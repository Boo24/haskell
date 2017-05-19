module BestQueue where
    import Queue
    data BestQueue a = Record [a] [a] Int Int
    
    instance Queue BestQueue where
        push x (Record [] [] 0 0) = Record [] [x] 0 1
        push x (Record pushStack popStack pushLen popLen) = if pushLen <= (popLen+1) then Record (x:pushStack) popStack (pushLen + 1) popLen else 
                    push x (Record [] (popStack ++ (reverse pushStack)) 0 (popLen + pushLen))

        pop (Record pushStack popStack pushLen popLen) = if pushLen <= (popLen+1) then Record pushStack (tail popStack) pushLen (popLen-1) else
                   pop (Record [] (popStack ++ (reverse pushStack)) 0 (popLen + pushLen))
                   
        top (Record pushStack popStack pushLen popLen) = head popStack
        emp (Record pushStack popStack pushLen popLen) = null pushStack && null popStack
        
        create x = Record [] [x] 0 1
        
        
    instance Show a => Show (BestQueue a) where
        show (Record pushStack popStack pushLen popLen) = show (popStack ++ (reverse pushStack))