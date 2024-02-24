x1 = 100
y1 = 98
x2 = 98
y2 = 95

dx = abs(x2 - x1) + 1
dy = abs(y2 - y1) + 1
dp = sqrt(dx^2 + dy^2)
dt = 1.0 / dp / 100.0

println("dx: $dx, dy: $dy, dp: $dp, dt: $dt")

for t in 0:dt:1
    x = x1 + t * (x2 - x1)
    y = y1 + t * (y2 - y1)
    #println("($x, $y)")
end