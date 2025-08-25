one := 1
num := 36
zero := 0
snum :^ (num, 0.5)
iter {
    div := 2
    ismod :% (num, div)
    isdiv :@ (ismod, zero)
    if isdiv { // skip a number of items after testing (add/remove bit mask to instrtuction set)
        `div // print the number
    }
    cond  :< (div,snum)
    if cond {
        iter:loop next // if the loop spawning is done once, then it becomes invalidated
        divplusone :+ (div, 1)
        iter.div = divplusone
    }
}

// we get snum steps to spawn the computations

