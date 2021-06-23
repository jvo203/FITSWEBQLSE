using Distances
using LinearAlgebra
using Distributions
using Mmap

width = 5
height = 10

function write_mmap(width, height)
    io = open("/tmp/mmap.bin", "w+")
    compressed_pixels = Mmap.mmap(io, Matrix{Float32}, (width, height))
    compressed_pixels = rand(Float32, width, height)
    println("write_mmap: ", compressed_pixels)
    Mmap.sync!(compressed_pixels)
    close(io)
end

function write_mmap2(width, height)
    io = open("/tmp/mmap.bin", "w+")
    compressed_pixels = rand(Float32, width, height)
    println("write_mmap: ", compressed_pixels)
    write(io, compressed_pixels)
    close(io)
end

function read_mmap(width, height)
    io = open("/tmp/mmap.bin")
    compressed_pixels = Mmap.mmap(io, Matrix{Float32}, (width, height))
    close(io)

    println("read_mmap: ", compressed_pixels)
end

write_mmap2(width, height)
read_mmap(width, height)

function testmmap()
    data = Float32.(rand(10000, 15))
    Eucldist = pairwise(Euclidean(), data, dims = 1)
    D = maximum(Eucldist .^ 2)
    sigma2hat = mean(((Eucldist .^ 2)./D)[tril!(trues(size((Eucldist .^ 2) ./ D)), -1)])
    L = exp.(-(Eucldist .^ 2 / D) / (2 * sigma2hat))
    s = open("/tmp/mmap.bin", "w+")
    write(s, size(L, 1))
    write(s, size(L, 2))
    write(s, L)
    close(s)

    # deref and gc collect
    Eucldist = data = L = zeros(Float32, 2, 2)
    GC.gc()

    s = open("/tmp/mmap.bin", "r+") # allow read and write
    m = read(s, Int)
    n = read(s, Int)
    L = Mmap.mmap(s, Matrix{Float32}, (m, n))  # now L references the file contents
    K = eigen(L)
    println(K)
end

# testmmap()
# @time testmmap()  # 109.657995 seconds (17.48 k allocations: 4.673 GiB, 0.73% gc time)
