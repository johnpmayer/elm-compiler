
import Mouse

main = (\x -> flow down [svg 101 100 $ sCircle x, svg 100 100 $ sCircle 20]) <~ Mouse.x
