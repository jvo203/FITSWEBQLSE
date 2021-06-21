@everywhere using DistributedArrays

println(workers())

n = length(workers())
println("#workers: $n")

# pixels = dzeros((3, 3), workers(), [1,n])
pixels = dzeros(Int32, (3, 3, n), workers())
mask = dfill(false, (3, 3, n), workers())

@everywhere function fill_array(d::DArray)
    pid = myid()

    la = localpart(d)
    la .= pid

    # println(localpart(d))
end

@sync for w in workers()
    @spawnat w fill_array(pixels)
end

println("pixels:", pixels)
println("mask:", mask)

println("indices:", [@fetchfrom p localindices(pixels) for p in workers()])