(*whole expression*)
x = (x)
(*abstraction body extends as far as possible:*)
𝜆𝑥.abc...z = 𝜆𝑥.(abc...z) 
(*abstarction -> association to rigth*)
𝜆x1.𝜆x2. ... . 𝜆𝑥𝑛.f(x1,x2, ... xn) = 𝜆x1.(𝜆x2.(... .𝜆xn.f(x1,x2, ... xn)))
(*application -> association to left*)
x1x2...xn = (((x1)x2)...)xn
(*parenthness -> exercises*)
x = (x)
xy = ((x)y)
𝜆x.x = (𝜆x.(x))
𝜆xz.x y = (𝜆xz.((x)y))
(𝜆x.x y)5 = ((𝜆x.((x)y))5)
abc = (((a)b)c)
𝜆x.xz.𝜆y.ab = (𝜆x.(((x)z)𝜆y.((a)b)))
((xy)(𝜆y.(𝜆z.(z(xy))))) = xy𝜆y.𝜆z.z(xy)
(*free and bound vaiables*)
𝜆x.xy
(𝜆x.x)z
x
(𝜆x.xz)xz
(𝜆x.x)(𝜆y.yx)x
𝜆x.zy𝜆y.yx
(*alpha-conversion*)
(*problem*)
(𝜆x.𝜆y.x+y)z ~ 𝜆y.z+y
(𝜆x.𝜆y.x+y)z (=) 𝜆y.z+y (*(=) -> not equal*)
(*solution*)
(𝜆x.𝜆y.x+y)y = (𝜆x.𝜆z.x+y)y ~ 𝜆z.y+z
(*simplify the expressions*)
(𝜆f.𝜆x.f(x))(𝜆y.y+x) = (𝜆f.𝜆z.f(fz))(𝜆y.y+x) ~ 𝜆z.((𝜆y.y+x)((𝜆y.y+x)z)) ~ 𝜆z.z+x+x
(𝜆xy.xyy)(𝜆y.y)y = (𝜆xy1.xy1.y1)(𝜆y2.y2)y ~ (𝜆y1.(𝜆y2.y2)y1y1)y ~ (𝜆y2.y2)yy ~ yy
(*logical value and operations*)
true = 𝜆t.𝜆f.t
false = 𝜆t.𝜆f.f
if = 𝜆p.𝜆m.𝜆n(p n m)
and = 𝜆p.𝜆q.(p q p)
or = 𝜆p.𝜆q.(p p q)
not = 𝜆p.(p false true)
(*pprove thet "and" expression yields the logical AND table*)
and t t = (𝜆p.𝜆q.pqp) t t ~ (𝜆p.t q t) t ~ t t t = (𝜆t.𝜆f.t) t t ~ t
and t f = (𝜆p.𝜆q.pqp) t f ~ t f t = (𝜆t.𝜆f.t) f t ~ f
and f t = (𝜆p.𝜆q.pqp) f t ~ f t f = (𝜆t.𝜆f.f) t f ~ f
and f f = (𝜆p.𝜆q.pqp) f f ~ f f f = (𝜆t.𝜆f.f) f f ~ f