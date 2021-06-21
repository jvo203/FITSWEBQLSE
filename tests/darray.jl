@everywhere using DistributedArrays

println(workers())

n = length(workers())
println("#workers: $n")

# pixels = dzeros((3, 3), workers(), [1,n])
pixels = dzeros(Float32, (3, 3, n), workers())
mask = dfill(false, (3, 3, n), workers())

println(pixels)
println(mask)

println("indices:", [@fetchfrom p localindices(pixels) for p in workers()])