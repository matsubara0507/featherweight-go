package main

type Function(type a Any, b Any) interface {
    Apply(x a) b
}

type incr struct { n int }
func (this incr) Apply(x int) int {
    return this.n.add(x)
}

type pos struct {}
func (this pos) Apply(x int) bool {
    return x.lt(zero)
}

type compose(type a Any, b Any, c Any) struct {
    f Function(a, b)
    g Function(b, c)
}
func (this compose(type a Any, b Any, c Any)) Apply(x a) c {
    return this.g.Apply(this.f.Apply(x))
}

func main(){
    _ = compose(int, int, bool){incr{x}, pos{}}.Apply(y)
}
