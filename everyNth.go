package main

import "fmt"
import "strconv"

func everyNth(l []string, n int) []string {
    if n < 1 || n > len(l) {
        return []string{}
    }
    e := make([]string, len(l)/n)
    for i, j := n-1, 0; i < len(l); i, j = i+n, j+1 {
        e[j] = l[i]
    }
    return e
}

func makeRange(min, max int) []string {
    a := make([]string, max-min+1)
    for i := range a {
        a[i] = strconv.Itoa(min + i)
    }
    return a
}

func main() {
    l := makeRange(0,999)
    e := everyNth(l,109)
    for n := range e {
        fmt.Println(e[n])
        n++
    }
}