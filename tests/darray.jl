@everywhere using DistributedArrays

println(workers())

n = length(workers())
println("#workers: $n")

# pixels = dzeros((3, 4), workers(), [1,n])
# pixels = dzeros(Int32, (3, 4, n))

chunks = (1, 1, n)
pixels = DArray(I -> zeros(Int32, map(length, I)), (3, 4, n), workers(), chunks)

# mask = dfill(false, (3, 4, n), workers())
mask = DArray(I -> fill(false, map(length, I)), (3, 4, n), workers(), chunks)

@everywhere function fill_array(d::DArray)
    pid = myid()

    la = localpart(d)
    la .= pid
    la[1,1] = pid - 1

    println("size(la) = $(size(la))")
end

@sync for w in workers()
    @spawnat w fill_array(pixels)
end

println("pixels:", pixels)
println("mask:", mask)

println("indices:", [@fetchfrom p localindices(pixels) for p in workers()])