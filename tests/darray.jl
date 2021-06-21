@everywhere using DistributedArrays

println(workers())

n = length(workers())
println("#workers: $n")

# pixels = dzeros((3, 3), workers(), [1,n])
pixels = dzeros(Float32, (3, 3, n), workers())
mask = dfill(false, (3, 3, n), workers())

@everywhere function fill_array(d::DArray)
    pid = myid()

    # d[1,1,pid] = Float32(pid)
    println(localpart(d))

    la = localpart(d)
    la .= Float32(pid)
end

@sync for w in workers()
    @spawnat w fill_array(pixels)
end

println(pixels)
println(mask)

println("indices:", [@fetchfrom p localindices(pixels) for p in workers()])