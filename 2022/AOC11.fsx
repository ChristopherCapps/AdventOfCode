

type Monkey = {
    id: int
    operation: int -> int
    startingItems: int list
    test: int -> bool
    onTrue: int
    onFalse: int
}

let monkey0 = { 
    id = 0;
    operation = fun worry -> worry * 3;
    startingItems = [50; 70; 54; 83; 52; 78]; 
    test = fun worry -> worry % 11 = 0;
    onTrue = 2;
    onFalse = 7
}

let monkey1 = { 
    id = 1;
    operation = fun worry -> worry * worry;
    startingItems = [71; 52; 58; 60; 71]; 
    test = fun worry -> worry % 7 = 0;
    onTrue = 0;
    onFalse = 2
}

let monkey2 = { 
    id = 2;
    operation = fun worry -> worry + 1;
    startingItems = [71; 52; 58; 60; 71]; 
    test = fun worry -> worry % 7 = 0;
    onTrue = 0;
    onFalse = 2
}