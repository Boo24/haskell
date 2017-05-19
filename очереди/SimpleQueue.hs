module SimpleQueue where
    import Queue
    data SimpleQueue a = Record [a]
    
    instance Queue SimpleQueue where
        push x (Record v) = Record (v++[x])
        pop (Record v) = Record (tail v)
        top (Record v) = head v
        emp (Record v) = null v
        create x = Record [x]

    instance Show a => Show (SimpleQueue a) where
        show (Record v) = show v