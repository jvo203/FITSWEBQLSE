a = ones(5)
b = [1 3 5]

c = reinterpret(UInt8, a)
println(c)

z1 = map(x -> reinterpret(UInt8, [x]), a)
z2 = map(x -> reverse(reinterpret(UInt8, [x])), a)

println(z1)
println(z2)