@everywhere using DistributedArrays

println(workers())

n = length(workers())
println("#workers: $n")

@everywhere function create_array(dimx, dimy)
    pid = myid()

    fill(pid, (dimx, dimy))
end

ras = [@spawnat w create_array(3, 4) for w in workers()]
#res = fetch(ras)
#println(res)

ras = reshape(ras, (3, 4))
pixels = DArray(ras)

println("pixels:", pixels)

println("indices:", [@fetchfrom p localindices(pixels) for p in workers()])

r1 = DistributedArrays.remotecall(() -> rand(10, 10), workers()[1])
r2 = DistributedArrays.remotecall(() -> rand(10, 10), workers()[2])
r3 = DistributedArrays.remotecall(() -> rand(10, 10), workers()[3])
r4 = DistributedArrays.remotecall(() -> rand(10, 10), workers()[4])
D = DArray(reshape([r1 r2 r3 r4], (2, 2)))
