
// 99 problems

// 1
// find the last element of a list

function myLast(arr) {
    return arr[arr.length - 1];
}

// 2
// find the second to last element of a list

function myLastButOne(arr) {
    return arr[arr.length - 2];
}

// 3
// find the nth item, 1 based, of a list

function myNth(arr, index) {
    return arr[index - 1];
}

// 4
// Find the number of elements in a list

function myLength(arr) {
    return arr.length
}

// 5
// Reverse a list

function myReverse(arr) {
    var newArr = [];
    for (var i = arr.length - 1; i >= 0; i--) {
        newArr.push(arr[i]);
    }
    return newArr;
}

// 6
// Find out whether a list is a palindrome

function palindromeP(arr) {
    for (var i = 0, j = arr.length - 1; i < j; i++, j--) {
        if (arr[i] != arr[j]) {
            return false;
        }
    }
    return true;
}

// 7
// Flatten nested list structure

function flatten(arr) {
    var newArr = [];
    var pushAllNewArr = function (x) {x.map(function (y) {newArr.push(y);});};
    for (var i = 0; i < arr.length; i ++) {
        if (arr[i] instanceof Array) {
            pushAllNewArr(flatten(arr[i]));
        } else {
            newArr.push(arr[i]);
        }
    }
    return newArr;
}

// 8
// eliminate consecutive elements of a list
function compress(arr) {
    if (arr.length == 0) {
        return [];
    }
    var newArr = [arr[0]];
    var last = arr[0];
    for (var i = 1; i < arr.length; i++) {
        if (arr[i] != last) {
            last = arr[i];
            newArr.push(last);
        }
    }
    return newArr;
}

// 9
// pack consecutive duplicates of a list into sublists
function pack(arr) {
    if (arr.length == 0) {
        return [];
    }
    var newArr = [];
    var tempArr = [arr[0]];
    var last = arr[0];
    for (var i = 1; i < arr.length; i++) {
        if (arr[i] == last) {
            tempArr.push(last);
        } else {
            newArr.push(tempArr);
            tempArr = [arr[i]];
            last = arr[i];
        }
    }
    newArr.push(tempArr);
    return newArr;
}

// 10
// run-length encoding of list
function encode(arr) {
    var trans = function (x) {return [x.length, x[0]]};
    return pack(arr).map(trans);
}

// 11
// modified run-length -- if an element only exists once, just put it directly into the list

function encodeModified(arr) {
    return encode(arr).map(function (x) {return x[0] == 1 ? x[1] : x;});
}

// 12
// decode run-length encoded list
function decode(arr) {
    function mkArr(x) {
        var a = [];
        for (var i = 0; i < x[0]; i++) {
            a.push(x[1]);
        }
        return a;
    }
    return flatten(arr.map(mkArr));
}

// 13
// runlength encode directly - no calling the pack function
// also, make it put single elements in the list directly, as in 11

function encodeDirect(arr) {
    if (arr.length == 0) {
        return [];
    }
    var newArr = [];
    var last = arr[0];
    var count = 0;
    for (var i = 0; i < arr.length; i++) {
        if (arr[i] == last) {
            count++;
        } else {
            newArr.push([count, last]);
            count = 1;
            last = arr[i];
        }
    }
    newArr.push([count, last]);
    return newArr.map(function (x) {return x[0] == 1 ? x[1] : x;});
}

// 14
// duplicate the elements of a list -- [a,b] becomes [a,a,b,b]

function duplicate(arr) {
    var newArr = [];
    for (var i = 0; i < arr.length; i++) {
        newArr.push(arr[i]);
        newArr.push(arr[i]);
    }
    return newArr;
}

