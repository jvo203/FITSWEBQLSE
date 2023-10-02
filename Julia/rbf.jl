using LinearAlgebra

function sqnorm(a, b)
    return dot(a, a) + dot(b, b) - 2 * dot(a, b)
end

function rbf_kernel(x, y, γ)
    return exp(-γ * sqnorm(x, y))
end

function read_dat_vector(filename::String)
    values = Float64[]

    f = open(filename)

    while !eof(f)
        push!(values, parse(Float64, readline(f)))
    end

    close(f)

    return values
end

wts = read_dat_vector("wts.dat")
testout = read_dat_vector("testout.dat")

# print the vector
println(wts)
println(testout)

# read the centres three Float64 values per line
# and store them in an array of arrays
centres = []

f = open("centres.dat")
while !eof(f)
    line = readline(f)
    push!(centres, parse.(Float64, split(line)))
end

# close the file
close(f)

# process test inputs
testdata = []

f = open("testdata.dat")
while !eof(f)
    line = readline(f)
    push!(testdata, parse.(Float64, split(line)))
end

# close the file
close(f)

# go through each element in testdata
for entry in testdata
    x = entry[1:3]
    target = entry[end]

    println("x = $x, target = $target")

    # map the input to the hidden layer
    hh = [rbf_kernel(x, c, 1.0) for c in centres]

    # prepend a 1.0 to the hidden layer
    # and store it in a new array
    hout = [1.0; hh]

    # calculate the output of the network
    # and clamp it to between 0.0 and 1.0
    out = clamp(dot(hout, wts), 0.0, 1.0)

    println("out: $out")

    #=
    for j in 1:length(centres)
        # calculate the rbf kernel
        kernel = rbf_kernel(testdata[i], centres[j], 0.1)
        # multiply the kernel by the weight
        testout[i] += kernel * wts[j]
    end
    =#
end