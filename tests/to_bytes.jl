using BitConverter;

a = ones(5)
b = [1 3 5]

c = reinterpret(UInt8, a)
println(c)