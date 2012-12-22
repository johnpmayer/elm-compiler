
import Mouse

static = svg 40 40 $ sCircle 20

dynamic = \r -> svg 400 400 $ sCircle r

rad = \(x,y) -> sqrt (x * x + y * y)

view = \pos -> flow down [ dynamic $ rad pos, static ]

main = view <~ Mouse.position
