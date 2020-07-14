package main

type Any interface {}
type Function interface {
    Apply(x Any) Any
}

type incr struct { n int }
func (this incr) Apply(x Any) Any {
    return this.n.add(x.(int))
}

type pos struct {}
func (this pos) Apply(x Any) Any {
    return x.(int).lt(zero)
}

type compose struct {
    f Function
    g Function
}
func (this compose) Apply(x Any) Any {
    return this.g.Apply(this.f.Apply(x))
}

func main(){
    _ = compose{incr{x}, pos{}}.Apply(y).(bool)
}
